-- | A connection to a Spotify access point: the TCP socket, the Diffie–Hellman
-- handshake, and the Shannon-encrypted packet channel that follows it.
--
-- After 'connectAccessPoint' returns, use 'sendPacket' / 'recvPacket' to
-- exchange @(cmd, payload)@ packets. Each packet is independently keyed with a
-- per-direction, monotonically increasing nonce, encrypted, and authenticated
-- with a 4-byte Shannon MAC.
module Spotify.Connection
  ( Connection
  , ConnectionError (..)
  , connectAccessPoint
  , connectAccessPointRetry
  , sendPacket
  , recvPacket
  , close
  ) where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.MVar      (MVar, newMVar, withMVar)
import           Control.Exception            (Exception, SomeException, catch, fromException,
                                               throwIO, try)
import           Control.Monad                (unless, when)
import           Data.Bits                    (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.IORef
import           Data.Word                    (Word32, Word8)
import qualified Network.Socket               as N
import qualified Network.Socket.ByteString    as NB
import           System.Entropy               (getEntropy)

import qualified Spotify.Crypto.DiffieHellman as DH
import           Spotify.Crypto.Keys          (HandshakeKeys (..), computeKeys,
                                               verifyServerSignature)
import qualified Spotify.Crypto.Shannon       as Sh
import           Spotify.Proto.Keyexchange

-- | An established, authenticated-channel-ready connection.
data Connection = Connection
  { connSock      :: !N.Socket
  , connSend      :: !Sh.Shannon
  , connRecv      :: !Sh.Shannon
  , connSendNonce :: !(IORef Word32)
  , connRecvNonce :: !(IORef Word32)
  , connSendLock  :: !(MVar ())   -- serialises concurrent senders
  }

-- | Things that can go wrong at the transport/handshake layer.
data ConnectionError
  = SocketResolveFailed String
  | ConnectionClosed
  | ServerSignatureInvalid          -- ^ possible man-in-the-middle
  | HandshakeRejected Int           -- ^ access point refused during handshake
  | ProtocolError String
  | MacMismatch
  deriving (Eq, Show)

instance Exception ConnectionError

-- ---------------------------------------------------------------------------
-- Connect + handshake
-- ---------------------------------------------------------------------------

-- | Open a TCP connection to @host:port@ and perform the DH handshake, leaving
-- a ready-to-use encrypted channel.
connectAccessPoint :: String -> Int -> IO Connection
connectAccessPoint host port = do
  sock <- openTcp host port
  keys <- DH.generate
  let gc = DH.publicKeyBytes keys
  clientNonce <- getEntropy 16

  -- ClientHello, framed as: 00 04 | u32be totalSize | protobuf
  let helloBody   = buildClientHello gc clientNonce
      helloFramed = BS.pack [0x00, 0x04]
                 <> word32BE (fromIntegral (2 + 4 + BS.length helloBody))
                 <> helloBody
  NB.sendAll sock helloFramed

  -- APResponseMessage, framed as: u32be totalSize | protobuf
  header <- recvExact sock 4
  let total = fromIntegral (beWord32 header)
  when (total < 4) $ throwIO (ProtocolError "APResponse: bad length")
  body <- recvExact sock (total - 4)
  let accumulator = helloFramed <> header <> body

  case parseAPResponse body of
    Left e            -> throwIO (ProtocolError e)
    Right (APFailed c) -> throwIO (HandshakeRejected c)
    Right (APChallenge gs gsSig) -> do
      unless (verifyServerSignature gs gsSig) $ throwIO ServerSignatureInvalid

      let shared = DH.sharedSecret keys gs
          HandshakeKeys challenge sendKey recvKey = computeKeys shared accumulator

      -- ClientResponsePlaintext, framed as: u32be totalSize | protobuf
      let respBody   = buildClientResponsePlaintext challenge
          respFramed = word32BE (fromIntegral (4 + BS.length respBody)) <> respBody
      NB.sendAll sock respFramed

      sendC <- Sh.new sendKey
      recvC <- Sh.new recvKey
      sn    <- newIORef 0
      rn    <- newIORef 0
      lock  <- newMVar ()
      pure Connection
        { connSock = sock, connSend = sendC, connRecv = recvC
        , connSendNonce = sn, connRecvNonce = rn, connSendLock = lock }

-- | Like 'connectAccessPoint', but transparently retry transient failures
-- (the access point sometimes drops a connection mid-handshake). A failed
-- server-signature check is never retried — that would be a security issue.
connectAccessPointRetry :: Int -> String -> Int -> IO Connection
connectAccessPointRetry maxTries host port = go 1
  where
    -- Transient resets surface either as our ConnectionClosed or as a raw
    -- IOException ("Connection reset by peer"); retry both. A bad server
    -- signature is a security failure and is never retried.
    go n = do
      r <- try (connectAccessPoint host port) :: IO (Either SomeException Connection)
      case r of
        Right c -> pure c
        Left e
          | Just ServerSignatureInvalid <- fromException e -> throwIO e
          | n >= maxTries -> throwIO e
          | otherwise     -> threadDelay (200000 * n) >> go (n + 1)

-- ---------------------------------------------------------------------------
-- The encrypted packet channel
-- ---------------------------------------------------------------------------

-- | Send one @(cmd, payload)@ packet.
sendPacket :: Connection -> Word8 -> ByteString -> IO ()
sendPacket c cmd payload = do
  let len = BS.length payload
  when (len > 0xffff) $ throwIO (ProtocolError "payload exceeds 65535 bytes")
  let buf = BS.pack [cmd, fromIntegral (len `shiftR` 8), fromIntegral (len .&. 0xff)]
         <> payload
  withMVar (connSendLock c) $ \_ -> do
    n <- readIORef (connSendNonce c)
    writeIORef (connSendNonce c) (n + 1)
    Sh.nonceU32 (connSend c) n
    enc <- Sh.encrypt (connSend c) buf
    mac <- Sh.finish  (connSend c) 4
    NB.sendAll (connSock c) (enc <> mac)

-- | Receive one @(cmd, payload)@ packet (blocks until a full packet arrives).
recvPacket :: Connection -> IO (Word8, ByteString)
recvPacket c = do
  n <- readIORef (connRecvNonce c)
  writeIORef (connRecvNonce c) (n + 1)
  Sh.nonceU32 (connRecv c) n

  encHeader <- recvExact (connSock c) 3
  header    <- Sh.decrypt (connRecv c) encHeader
  let cmd  = BS.index header 0
      size = (fromIntegral (BS.index header 1) `shiftL` 8)
         .|.  fromIntegral (BS.index header 2)

  rest <- recvExact (connSock c) (size + 4)
  let (encPayload, mac) = BS.splitAt size rest
  payload <- Sh.decrypt (connRecv c) encPayload
  ok      <- Sh.checkMac (connRecv c) mac
  unless ok $ throwIO MacMismatch
  pure (cmd, payload)

-- | Close the underlying socket.
close :: Connection -> IO ()
close = N.close . connSock

-- ---------------------------------------------------------------------------
-- Socket helpers
-- ---------------------------------------------------------------------------

openTcp :: String -> Int -> IO N.Socket
openTcp host port = do
  let hints = N.defaultHints { N.addrSocketType = N.Stream }
  addrs <- N.getAddrInfo (Just hints) (Just host) (Just (show port))
  go addrs
  where
    go []       = throwIO (SocketResolveFailed (host <> ":" <> show port))
    go (a : as) = do
      s <- N.socket (N.addrFamily a) (N.addrSocketType a) (N.addrProtocol a)
      r <- tryConnect s (N.addrAddress a)
      case r of
        Right () -> pure s
        Left _   -> N.close s >> if null as then throwIO (SocketResolveFailed host) else go as

    tryConnect :: N.Socket -> N.SockAddr -> IO (Either IOError ())
    tryConnect s addr =
      (Right <$> N.connect s addr) `catch` (pure . Left)

-- read exactly n bytes, or fail if the peer closes early
recvExact :: N.Socket -> Int -> IO ByteString
recvExact sock = loop BS.empty
  where
    loop acc remaining
      | remaining <= 0 = pure acc
      | otherwise = do
          chunk <- NB.recv sock remaining
          if BS.null chunk
            then throwIO ConnectionClosed
            else loop (acc <> chunk) (remaining - BS.length chunk)

word32BE :: Word32 -> ByteString
word32BE w = BS.pack
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

beWord32 :: ByteString -> Word32
beWord32 bs =
  (fromIntegral (BS.index bs 0) `shiftL` 24) .|.
  (fromIntegral (BS.index bs 1) `shiftL` 16) .|.
  (fromIntegral (BS.index bs 2) `shiftL` 8)  .|.
   fromIntegral (BS.index bs 3)
