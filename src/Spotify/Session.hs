-- | The friendly entry point: turn credentials into a logged-in 'Session', and
-- once logged in, talk to the backend.
--
-- A live session owns a background reader thread that services the encrypted
-- channel: it answers keep-alive pings, records the account's country and
-- product info that the server pushes after login, and routes Mercury responses
-- back to the requests that are waiting for them.
module Spotify.Session
  ( -- * Credentials
    Credentials (..)
  , withAccessToken
  , storedCredentials
    -- * Configuration
  , SessionConfig (..)
  , defaultConfig
    -- * Sessions
  , Session
  , sessionUsername
  , sessionDeviceId
  , sessionClientId
  , AuthError (..)
  , connect
  , disconnect
  , reusableCredentials
    -- * Account info & Mercury
  , sessionCountry
  , sessionProduct
  , awaitCountry
  , awaitProduct
  , MercuryResponse (..)
  , mercuryGet
  , Token (..)
  , getToken
    -- * Audio keys
  , requestAudioKey
    -- * Channels (low-level plumbing for audio fetch)
  , ChannelMsg (..)
  , allocChannel
  , closeChannel
  , sessionSend
  ) where

import           Control.Concurrent             (ThreadId, forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception              (Exception, SomeException, bracketOnError,
                                                 catch, fromException, throwIO, try)
import           Data.Aeson                     (eitherDecodeStrict, withObject, (.:), (.:?))
import           Data.Aeson.Types               (parseEither)
import           Data.Bits                      (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BC
import           Data.IORef
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import qualified Data.Text.Encoding             as TE
import           Data.Word                      (Word16, Word32, Word64, Word8)
import           System.Entropy                 (getEntropy)
import           System.IO                      (hPutStrLn, stderr)
import           System.Timeout                 (timeout)

import           Spotify.Auth.OAuth             (keymasterClientId)
import           Spotify.Connection
import           Spotify.Mercury
import           Spotify.Net.ApResolve          (resolveAccessPoint)
import           Spotify.Proto.Authentication

-- | A set of credentials to log in with.
data Credentials = Credentials
  { credUsername :: Maybe ByteString
  , credAuthType :: AuthType
  , credAuthData :: ByteString
  } deriving (Eq, Show)

-- | Log in with an OAuth access token.
withAccessToken :: ByteString -> Credentials
withAccessToken token = Credentials Nothing AuthSpotifyToken token

-- | Log in by reusing a stored credentials blob (see 'reusableCredentials').
storedCredentials :: ByteString -> ByteString -> Credentials
storedCredentials username blob =
  Credentials (Just username) AuthStoredCredentials blob

-- | Tweakables for a session.
data SessionConfig = SessionConfig
  { cfgDeviceId :: Maybe ByteString  -- ^ stable device id; a random one is made if absent
  , cfgClientId :: ByteString        -- ^ client id used for token requests
  }

defaultConfig :: SessionConfig
defaultConfig = SessionConfig { cfgDeviceId = Nothing, cfgClientId = keymasterClientId }

-- | A live, authenticated session.
data Session = Session
  { sessionConnection :: Connection
  , sessionUsername   :: ByteString   -- ^ the canonical username Spotify assigned
  , sessionDeviceId   :: ByteString
  , sessionClientId   :: ByteString
  , sessionReusable   :: Credentials  -- ^ cache these to skip OAuth next time
  , sessionCountryV   :: TVar (Maybe ByteString)
  , sessionProductV   :: TVar (Maybe ByteString)
  , sessionSeq        :: IORef Word64
  , sessionPending    :: TVar (Map Word64 PendingReq)
  , sessionKeySeq     :: IORef Word32
  , sessionKeyPending :: TVar (Map Word32 (MVar (Either String ByteString)))
  , sessionChanSeq    :: IORef Word16
  , sessionChannels   :: TVar (Map Word16 (TQueue ChannelMsg))
  , sessionReader     :: ThreadId
  }

-- a Mercury request awaiting its (possibly multi-packet) response
data PendingReq = PendingReq
  { prParts   :: ![ByteString]
  , prPartial :: !(Maybe ByteString)
  , prResult  :: !(MVar (Either String MercuryResponse))
  }

-- | A message delivered to a data channel (used by the audio fetcher).
data ChannelMsg
  = ChannelData !ByteString  -- ^ a chunk of payload (headers + data, framed)
  | ChannelEnd               -- ^ the server closed the channel
  | ChannelFailed !String    -- ^ a ChannelError packet
  deriving (Show)

-- | Login failures reported by the access point.
data AuthError
  = LoginFailed Int String
  | UnexpectedReply String
  deriving (Eq, Show)

instance Exception AuthError

-- ---------------------------------------------------------------------------
-- Connect
-- ---------------------------------------------------------------------------

-- | Resolve an access point, connect, authenticate, and start servicing the
-- channel. Transient access-point resets are retried; a genuine login failure
-- or a bad server signature is reported immediately.
connect :: SessionConfig -> Credentials -> IO Session
connect cfg creds = do
  deviceId            <- maybe newDeviceId pure (cfgDeviceId cfg)
  (conn, user, reuse) <- connectAndAuth creds deviceId
  buildSession cfg conn user reuse deviceId

connectAndAuth :: Credentials -> ByteString -> IO (Connection, ByteString, Credentials)
connectAndAuth creds deviceId = go (1 :: Int)
  where
    maxAttempts = 6
    go n = do
      r <- try attempt :: IO (Either SomeException (Connection, ByteString, Credentials))
      case r of
        Right ok -> pure ok
        Left e
          | fatal e          -> throwIO e
          | n >= maxAttempts -> throwIO e
          | otherwise        -> do
              hPutStrLn stderr ("access point reset, retrying (" ++ show n ++ "/"
                                ++ show (maxAttempts - 1) ++ ") ...")
              threadDelay (300000 * n)
              go (n + 1)

    attempt =
      resolveAccessPoint >>= \(host, port) ->
        bracketOnError (connectAccessPoint host port) close $ \conn -> do
          (user, reuse) <- authenticate conn creds deviceId
          pure (conn, user, reuse)

    fatal e
      | Just (LoginFailed _ _)      <- fromException e = True
      | Just ServerSignatureInvalid <- fromException e = True
      | otherwise                                      = False

buildSession :: SessionConfig -> Connection -> ByteString -> Credentials -> ByteString -> IO Session
buildSession cfg conn user reuse deviceId = do
  countryV <- newTVarIO Nothing
  productV <- newTVarIO Nothing
  seqRef   <- newIORef 0
  pendingV <- newTVarIO Map.empty
  keySeq   <- newIORef 0
  keyPend  <- newTVarIO Map.empty
  chanSeq  <- newIORef 0
  chans    <- newTVarIO Map.empty
  tid      <- forkIO (dispatcher conn pendingV countryV productV keyPend chans)
  pure Session
    { sessionConnection = conn
    , sessionUsername   = user
    , sessionDeviceId   = deviceId
    , sessionClientId   = cfgClientId cfg
    , sessionReusable   = reuse
    , sessionCountryV   = countryV
    , sessionProductV   = productV
    , sessionSeq        = seqRef
    , sessionPending    = pendingV
    , sessionKeySeq     = keySeq
    , sessionKeyPending = keyPend
    , sessionChanSeq    = chanSeq
    , sessionChannels   = chans
    , sessionReader     = tid
    }

-- | Stop the reader thread and close the connection.
disconnect :: Session -> IO ()
disconnect sess = killThread (sessionReader sess) >> close (sessionConnection sess)

-- | The reusable credentials returned by the server at login time.
reusableCredentials :: Session -> Credentials
reusableCredentials = sessionReusable

-- ---------------------------------------------------------------------------
-- Authentication
-- ---------------------------------------------------------------------------

authenticate :: Connection -> Credentials -> ByteString -> IO (ByteString, Credentials)
authenticate conn creds deviceId = do
  let body = buildClientResponseEncrypted
               (credUsername creds) (credAuthType creds) (credAuthData creds) deviceId
  sendPacket conn cmdLogin body
  await
  where
    await = do
      (cmd, dat) <- recvPacket conn
      if | cmd == cmdAPWelcome ->
             case parseAPWelcome dat of
               Left e  -> throwIO (UnexpectedReply ("APWelcome parse: " <> e))
               Right w -> pure (welcomeUsername w, reuse w)
         | cmd == cmdAuthFailure ->
             case parseLoginFailedCode dat of
               Left e   -> throwIO (UnexpectedReply ("AuthFailure parse: " <> e))
               Right co -> throwIO (LoginFailed co (loginErrorMessage co))
         | otherwise -> await

    reuse w = Credentials
      { credUsername = Just (welcomeUsername w)
      , credAuthType = authTypeFromCode (welcomeReusableType w)
      , credAuthData = welcomeReusableData w
      }

-- ---------------------------------------------------------------------------
-- The background dispatcher
-- ---------------------------------------------------------------------------

dispatcher
  :: Connection
  -> TVar (Map Word64 PendingReq)
  -> TVar (Maybe ByteString)
  -> TVar (Maybe ByteString)
  -> TVar (Map Word32 (MVar (Either String ByteString)))
  -> TVar (Map Word16 (TQueue ChannelMsg))
  -> IO ()
dispatcher conn pendingV countryV productV keyPendingV channelsV = loop `catch` onErr
  where
    loop = do
      (cmd, payload) <- recvPacket conn
      handlePacket cmd payload
      loop

    handlePacket cmd payload = case cmd of
      0x04 -> sendPacket conn 0x49 (BS.pack [0, 0, 0, 0])              -- Ping -> Pong
      0x1b -> atomically (writeTVar countryV (Just payload))           -- CountryCode
      0x50 -> atomically (writeTVar productV (Just (productType payload)))  -- ProductInfo
      0xb2 -> handleMercury pendingV payload                           -- Mercury response
      0x0d -> handleKey keyPendingV payload True                       -- AesKey
      0x0e -> handleKey keyPendingV payload False                      -- AesKeyError
      0x09 -> handleChannel channelsV payload False                    -- StreamChunkRes
      0x0a -> handleChannel channelsV payload True                     -- ChannelError
      _    -> pure ()

    onErr (e :: SomeException) = do
      let reason = "connection closed: " <> show e
      m <- atomically (swapTVar pendingV Map.empty)
      mapM_ (\pr -> tryPutMVar (prResult pr) (Left reason)) (Map.elems m)
      km <- atomically (swapTVar keyPendingV Map.empty)
      mapM_ (\mv -> tryPutMVar mv (Left reason)) (Map.elems km)
      cm <- atomically (swapTVar channelsV Map.empty)
      mapM_ (\q -> atomically (writeTQueue q (ChannelFailed reason))) (Map.elems cm)

handleMercury :: TVar (Map Word64 PendingReq) -> ByteString -> IO ()
handleMercury pendingV payload = case parseMercuryPacket payload of
  Left _ -> pure ()
  Right (MercuryPacket sq flags parts) -> do
    toDeliver <- atomically $ do
      m <- readTVar pendingV
      case Map.lookup sq m of
        Nothing -> pure Nothing
        Just pr -> do
          let pr' = mergeParts pr flags parts
          if flags == 1
            then writeTVar pendingV (Map.delete sq m) >> pure (Just pr')
            else writeTVar pendingV (Map.insert sq pr' m) >> pure Nothing
    case toDeliver of
      Nothing  -> pure ()
      Just pr' -> putMVar (prResult pr') (Right (toResponse pr'))

mergeParts :: PendingReq -> Word8 -> [ByteString] -> PendingReq
mergeParts pr0 flags parts = go pr0 (zip [0 ..] parts)
  where
    lastIx = length parts - 1
    go pr [] = pr
    go pr ((i, part) : rest) =
      let merged = maybe part (<> part) (prPartial pr)
          pr1    = pr { prPartial = Nothing }
      in if i == lastIx && flags == 2
           then go pr1 { prPartial = Just merged } rest
           else go pr1 { prParts = prParts pr1 ++ [merged] } rest

toResponse :: PendingReq -> MercuryResponse
toResponse pr = case prParts pr of
  (hdr : rest) -> let (uri, status) = parseHeaderStatus hdr in MercuryResponse uri status rest
  []           -> MercuryResponse "" 0 []

-- An AesKey (ok) / AesKeyError response: seq (u32 BE) then the 16-byte key.
handleKey :: TVar (Map Word32 (MVar (Either String ByteString))) -> ByteString -> Bool -> IO ()
handleKey keyV payload isOk = do
  let seqN = beWord32 (BS.take 4 payload)
      rest = BS.drop 4 payload
  mmv <- atomically $ do
    m <- readTVar keyV
    writeTVar keyV (Map.delete seqN m)
    pure (Map.lookup seqN m)
  case mmv of
    Nothing -> pure ()
    Just mv -> putMVar mv $
      if isOk then Right (BS.take 16 rest)
              else Left ("audio key error " ++ show (BS.unpack (BS.take 2 rest)))

-- A StreamChunkRes / ChannelError packet: 2-byte channel id then the payload.
handleChannel :: TVar (Map Word16 (TQueue ChannelMsg)) -> ByteString -> Bool -> IO ()
handleChannel channelsV payload isErr = do
  let chId = beWord16 (BS.take 2 payload)
      rest = BS.drop 2 payload
  mq <- atomically (Map.lookup chId <$> readTVar channelsV)
  case mq of
    Nothing -> pure ()
    Just q  -> atomically $ writeTQueue q $
      if isErr        then ChannelFailed ("channel error " ++ show (BS.unpack (BS.take 2 rest)))
      else if BS.null rest then ChannelEnd
      else                 ChannelData rest

-- ---------------------------------------------------------------------------
-- Mercury requests
-- ---------------------------------------------------------------------------

-- | Issue a Mercury @GET@ and wait for the response.
mercuryGet :: Session -> ByteString -> IO (Either String MercuryResponse)
mercuryGet sess uri = do
  sq     <- atomicModifyIORef' (sessionSeq sess) (\s -> (s + 1, s))
  result <- newEmptyMVar
  atomically $ modifyTVar' (sessionPending sess)
                           (Map.insert sq (PendingReq [] Nothing result))
  sendPacket (sessionConnection sess) 0xb2 (encodeGetRequest sq uri)
  r <- timeout (10 * 1000000) (takeMVar result)
  case r of
    Just res -> pure res
    Nothing  -> do
      atomically (modifyTVar' (sessionPending sess) (Map.delete sq))
      pure (Left "mercury: request timed out")

-- | A Web API access token minted for this session (via Mercury keymaster).
data Token = Token
  { tokenAccessToken :: ByteString
  , tokenExpiry      :: Int
  , tokenScopes      :: [ByteString]
  } deriving (Eq, Show)

-- | Request a Web API token from keymaster for the given (comma-joined) scopes.
getToken :: Session -> [ByteString] -> IO (Either String Token)
getToken sess scopes = do
  let uri = "hm://keymaster/token/authenticated?scope=" <> BS.intercalate "," scopes
         <> "&client_id=" <> sessionClientId sess
         <> "&device_id=" <> sessionDeviceId sess
  r <- mercuryGet sess uri
  pure $ case r of
    Left e -> Left e
    Right resp
      | mrStatus resp >= 400 -> Left ("token request failed (status " <> show (mrStatus resp) <> ")")
      | otherwise -> case mrPayload resp of
          (json : _) -> parseToken json
          []         -> Left "token: empty payload"

parseToken :: ByteString -> Either String Token
parseToken json = do
  v <- eitherDecodeStrict json
  parseEither tokenParser v
  where
    tokenParser = withObject "token" $ \o -> do
      acc <- o .:  "accessToken"
      exp <- o .:? "expiresIn" >>= maybe (pure 3600) pure
      scs <- o .:? "scope" >>= maybe (pure []) pure
      pure (Token (TE.encodeUtf8 acc) exp (map TE.encodeUtf8 scs))

-- ---------------------------------------------------------------------------
-- Audio keys
-- ---------------------------------------------------------------------------

-- | Request the 16-byte AES key for a track's audio file. @track@ is the 16 raw
-- id bytes, @file@ the 20 raw file-id bytes.
requestAudioKey :: Session -> ByteString -> ByteString -> IO (Either String ByteString)
requestAudioKey sess track file = do
  seqN <- atomicModifyIORef' (sessionKeySeq sess) (\s -> (s + 1, s))
  mv   <- newEmptyMVar
  atomically $ modifyTVar' (sessionKeyPending sess) (Map.insert seqN mv)
  let payload = file <> track <> word32BE seqN <> BS.pack [0, 0]
  sendPacket (sessionConnection sess) 0x0c payload
  r <- timeout (3 * 1000000) (takeMVar mv)
  case r of
    Just res -> pure res
    Nothing  -> do
      atomically (modifyTVar' (sessionKeyPending sess) (Map.delete seqN))
      pure (Left "audio key: request timed out")

beWord32 :: ByteString -> Word32
beWord32 = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0

word32BE :: Word32 -> ByteString
word32BE w = BS.pack
  [ fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8),  fromIntegral w ]

beWord16 :: ByteString -> Word16
beWord16 = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0

-- | Allocate a data channel; returns its id and the queue its packets arrive on.
allocChannel :: Session -> IO (Word16, TQueue ChannelMsg)
allocChannel sess = do
  chId <- atomicModifyIORef' (sessionChanSeq sess) (\s -> (s + 1, s))
  q    <- newTQueueIO
  atomically $ modifyTVar' (sessionChannels sess) (Map.insert chId q)
  pure (chId, q)

-- | Forget a channel once its data has been fully received.
closeChannel :: Session -> Word16 -> IO ()
closeChannel sess chId =
  atomically $ modifyTVar' (sessionChannels sess) (Map.delete chId)

-- | Send a raw @(cmd, payload)@ packet on the session's connection.
sessionSend :: Session -> Word8 -> ByteString -> IO ()
sessionSend sess = sendPacket (sessionConnection sess)

-- ---------------------------------------------------------------------------
-- Account info pushed by the server after login
-- ---------------------------------------------------------------------------

sessionCountry :: Session -> IO (Maybe ByteString)
sessionCountry = readTVarIO . sessionCountryV

sessionProduct :: Session -> IO (Maybe ByteString)
sessionProduct = readTVarIO . sessionProductV

-- | Block (up to ~3s) until the country code arrives, then return it.
awaitCountry :: Session -> IO (Maybe ByteString)
awaitCountry sess = awaitTVar (sessionCountryV sess)

-- | Block (up to ~3s) until the product info arrives, then return it.
awaitProduct :: Session -> IO (Maybe ByteString)
awaitProduct sess = awaitTVar (sessionProductV sess)

awaitTVar :: TVar (Maybe a) -> IO (Maybe a)
awaitTVar v = timeout (3 * 1000000) $ atomically $ readTVar v >>= maybe retry pure

-- pull <type>…</type> out of the ProductInfo XML
productType :: ByteString -> ByteString
productType = maybe "" id . extractTag "type"

extractTag :: ByteString -> ByteString -> Maybe ByteString
extractTag tag bs =
  let open  = "<" <> tag <> ">"
      close = "</" <> tag <> ">"
      after = snd (BS.breakSubstring open bs)
  in if BS.null after
       then Nothing
       else Just (fst (BS.breakSubstring close (BS.drop (BS.length open) after)))

-- ---------------------------------------------------------------------------

cmdLogin, cmdAPWelcome, cmdAuthFailure :: Word8
cmdLogin       = 0xab
cmdAPWelcome   = 0xac
cmdAuthFailure = 0xad

loginErrorMessage :: Int -> String
loginErrorMessage = \case
  0x0  -> "protocol error"
  0x2  -> "try another access point"
  0x5  -> "bad connection id"
  0x9  -> "travel restriction"
  0xb  -> "premium account required"
  0xc  -> "bad credentials"
  0xd  -> "could not validate credentials"
  0xe  -> "account exists"
  0xf  -> "extra verification required"
  0x10 -> "invalid app key"
  0x11 -> "application banned"
  _    -> "unknown error"

newDeviceId :: IO ByteString
newDeviceId = toHex <$> getEntropy 20
  where
    toHex = BS.concatMap (\b -> BC.pack [hexDigit (b `shiftR` 4), hexDigit (b .&. 0xf)])
    hexDigit n | n < 10    = toEnum (fromIntegral n + fromEnum '0')
               | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')
