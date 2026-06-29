{-# LANGUAGE BangPatterns #-}

-- | The Shannon stream cipher (Hawkes & Rose), with its built-in MAC.
--
-- This is the cipher that protects the Spotify access-point channel once the
-- Diffie–Hellman handshake has produced the per-direction keys. Each packet is
-- re-keyed with a fresh 32-bit nonce, encrypted, and followed by a 4-byte MAC
-- produced by 'finish'.
--
-- The implementation is a faithful, self-contained port of the reference
-- word-oriented algorithm. State is mutable (an 'IOUArray' register plus a few
-- 'IORef's) and threaded through 'IO'; a single 'Shannon' value is used for one
-- direction of one connection.
--
-- Correctness is pinned by test vectors generated from the original
-- implementation (see @test/Spec.hs@).
module Spotify.Crypto.Shannon
  ( Shannon
  , new
  , nonce
  , nonceU32
  , encrypt
  , decrypt
  , finish
  , checkMac
  ) where

import           Data.Array.IO
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.IORef
import           Data.Word

-- Algorithm constants.
nN :: Int
nN = 16

keyp :: Int
keyp = 13

foldN :: Int
foldN = nN

initkonst :: Word32
initkonst = 0x6996c53a

-- | One direction of a Shannon-keyed channel. Mutable; not thread-safe.
data Shannon = Shannon
  { sR     :: !(IOUArray Int Word32)  -- ^ the linear feedback register
  , sCRC   :: !(IOUArray Int Word32)  -- ^ the parallel CRC register (for the MAC)
  , sInitR :: !(IOUArray Int Word32)  -- ^ a saved copy of @R@ after key setup
  , sKonst :: !(IORef Word32)
  , sSbuf  :: !(IORef Word32)         -- ^ current keystream word
  , sMbuf  :: !(IORef Word32)         -- ^ partial-word MAC accumulator
  , sNbuf  :: !(IORef Int)            -- ^ bits still needed to fill a partial word
  }

-- ---------------------------------------------------------------------------
-- Core transforms
-- ---------------------------------------------------------------------------

sbox1 :: Word32 -> Word32
sbox1 w0 =
  let w1 = w0 `xor` (rotateL w0 5  .|. rotateL w0 7)
  in      w1 `xor` (rotateL w1 19 .|. rotateL w1 22)

sbox2 :: Word32 -> Word32
sbox2 w0 =
  let w1 = w0 `xor` (rotateL w0 7 .|. rotateL w0 22)
  in      w1 `xor` (rotateL w1 5 .|. rotateL w1 19)

modifyArr :: IOUArray Int Word32 -> Int -> (Word32 -> Word32) -> IO ()
modifyArr arr i f = readArray arr i >>= writeArray arr i . f
{-# INLINE modifyArr #-}

copyArr :: IOUArray Int Word32 -> IOUArray Int Word32 -> IO ()
copyArr src dst = mapM_ (\i -> readArray src i >>= writeArray dst i) [0 .. nN - 1]

-- | Cycle the register once, producing the next keystream word in 'sSbuf'.
cycleS :: Shannon -> IO ()
cycleS s = do
  r12 <- readArray (sR s) 12
  r13 <- readArray (sR s) 13
  k   <- readIORef  (sKonst s)
  r0  <- readArray (sR s) 0
  let t0 = sbox1 (r12 `xor` r13 `xor` k) `xor` rotateL r0 1
  -- shift the register left by one word
  mapM_ (\i -> readArray (sR s) i >>= writeArray (sR s) (i - 1)) [1 .. nN - 1]
  writeArray (sR s) (nN - 1) t0
  r2   <- readArray (sR s) 2
  r15  <- readArray (sR s) 15
  let t1 = sbox2 (r2 `xor` r15)
  modifyArr (sR s) 0 (`xor` t1)
  r8   <- readArray (sR s) 8
  r12' <- readArray (sR s) 12
  writeIORef (sSbuf s) (t1 `xor` r8 `xor` r12')

diffuse :: Shannon -> IO ()
diffuse s = mapM_ (const (cycleS s)) [1 .. foldN]

genkonst :: Shannon -> IO ()
genkonst s = readArray (sR s) 0 >>= writeIORef (sKonst s)

savestate :: Shannon -> IO ()
savestate s = copyArr (sR s) (sInitR s)

reloadstate :: Shannon -> IO ()
reloadstate s = copyArr (sInitR s) (sR s)

-- | Fold key (or nonce) material into the register, then make it irreversible.
loadkey :: Shannon -> ByteString -> IO ()
loadkey s key = do
  mapM_ (\w -> modifyArr (sR s) keyp (`xor` le32pad w) >> cycleS s) (chunk4 key)
  modifyArr (sR s) keyp (`xor` fromIntegral (BS.length key))
  cycleS s
  copyArr (sR s) (sCRC s)
  diffuse s
  mapM_ (\i -> readArray (sCRC s) i >>= \c -> modifyArr (sR s) i (`xor` c)) [0 .. nN - 1]

crcfunc :: Shannon -> Word32 -> IO ()
crcfunc s i = do
  c0  <- readArray (sCRC s) 0
  c2  <- readArray (sCRC s) 2
  c15 <- readArray (sCRC s) 15
  let t = c0 `xor` c2 `xor` c15 `xor` i
  mapM_ (\j -> readArray (sCRC s) j >>= writeArray (sCRC s) (j - 1)) [1 .. nN - 1]
  writeArray (sCRC s) (nN - 1) t

macfunc :: Shannon -> Word32 -> IO ()
macfunc s i = do
  crcfunc s i
  modifyArr (sR s) keyp (`xor` i)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Create a cipher initialised with a key.
new :: ByteString -> IO Shannon
new key = do
  r     <- newArray (0, nN - 1) 0 :: IO (IOUArray Int Word32)
  crc   <- newArray (0, nN - 1) 0 :: IO (IOUArray Int Word32)
  initr <- newArray (0, nN - 1) 0 :: IO (IOUArray Int Word32)
  konst <- newIORef initkonst
  sbuf  <- newIORef 0
  mbuf  <- newIORef 0
  nbuf  <- newIORef 0
  let s = Shannon r crc initr konst sbuf mbuf nbuf
  -- register seeded with Fibonacci numbers
  writeArray r 0 1
  writeArray r 1 1
  mapM_ (\i -> do a <- readArray r (i - 1); b <- readArray r (i - 2); writeArray r i (a + b))
        [2 .. nN - 1]
  loadkey s key
  genkonst s
  savestate s
  pure s

-- | Re-key the cipher with a nonce (re-using the post-key-setup register state).
nonce :: Shannon -> ByteString -> IO ()
nonce s n = do
  reloadstate s
  writeIORef (sKonst s) initkonst
  loadkey s n
  genkonst s
  writeIORef (sNbuf s) 0

-- | Re-key with a 32-bit (big-endian) nonce — what the AP packet codec uses.
nonceU32 :: Shannon -> Word32 -> IO ()
nonceU32 s w = nonce s (be32 w)

-- | Encrypt a buffer in place (and fold its plaintext into the running MAC).
encrypt :: Shannon -> ByteString -> IO ByteString
encrypt s = withBuf $ \arr len -> process s arr len encWord encByte
  where
    encWord t = do
      macfunc s t
      sb <- readIORef (sSbuf s)
      pure (t `xor` sb)
    encByte b nb = do
      sb <- readIORef (sSbuf s)
      modifyIORef' (sMbuf s) (`xor` (fromIntegral b `shiftL` (32 - nb)))
      pure (b `xor` fromIntegral ((sb `shiftR` (32 - nb)) .&. 0xff))

-- | Decrypt a buffer in place (and fold the recovered plaintext into the MAC).
decrypt :: Shannon -> ByteString -> IO ByteString
decrypt s = withBuf $ \arr len -> process s arr len decWord decByte
  where
    decWord t = do
      sb <- readIORef (sSbuf s)
      let p = t `xor` sb
      macfunc s p
      pure p
    decByte b nb = do
      sb <- readIORef (sSbuf s)
      let p = b `xor` fromIntegral ((sb `shiftR` (32 - nb)) .&. 0xff)
      modifyIORef' (sMbuf s) (`xor` (fromIntegral p `shiftL` (32 - nb)))
      pure p

-- | Finish the current MAC and emit @n@ bytes of it.
finish :: Shannon -> Int -> IO ByteString
finish s n = do
  nb <- readIORef (sNbuf s)
  if nb /= 0 then readIORef (sMbuf s) >>= macfunc s else pure ()
  cycleS s
  modifyArr (sR s) keyp (`xor` (initkonst `xor` (fromIntegral nb `shiftL` 3)))
  writeIORef (sNbuf s) 0
  mapM_ (\i -> readArray (sCRC s) i >>= \c -> modifyArr (sR s) i (`xor` c)) [0 .. nN - 1]
  diffuse s
  out <- newArray (0, n - 1) 0 :: IO (IOUArray Int Word8)
  let outLoop c
        | c >= n    = pure ()
        | otherwise = do
            cycleS s
            sb <- readIORef (sSbuf s)
            let r = n - c
            if r >= 4
              then writeWord32LE out c sb >> outLoop (c + 4)
              else do
                mapM_ (\i -> writeArray out (c + i) (fromIntegral ((sb `shiftR` (8 * i)) .&. 0xff)))
                      [0 .. r - 1]
                outLoop (c + r)
  outLoop 0
  BS.pack <$> getElems out

-- | Finish the MAC and compare it (in full) to an expected value.
checkMac :: Shannon -> ByteString -> IO Bool
checkMac s expected = (== expected) <$> finish s (BS.length expected)

-- ---------------------------------------------------------------------------
-- The shared processing skeleton for encrypt/decrypt
-- ---------------------------------------------------------------------------

type WordOp = Word32 -> IO Word32      -- transform one little-endian word
type ByteOp = Word8 -> Int -> IO Word8 -- transform one byte, given current nbuf

-- | Walk the buffer exactly like the reference: complete any partial word left
-- over from a previous call, process whole 4-byte words, then buffer a trailing
-- partial word for the next call (or 'finish').
process :: Shannon -> IOUArray Int Word8 -> Int -> WordOp -> ByteOp -> IO ()
process s arr len wordOp byteOp = do
  cur <- newIORef (0 :: Int)
  -- (1) finish a partial word buffered by a previous call
  let complete = do
        nb <- readIORef (sNbuf s)
        if nb > 0
          then do
            c <- readIORef cur
            if c < len
              then do
                b  <- readArray arr c
                b' <- byteOp b nb
                writeArray arr c b'
                writeIORef cur (c + 1)
                writeIORef (sNbuf s) (nb - 8)
                readIORef (sMbuf s) >>= macfunc s
                complete
              else pure ()
          else pure ()
  startNb <- readIORef (sNbuf s)
  if startNb /= 0 then complete else pure ()
  -- (2) whole words
  c0 <- readIORef cur
  let wholeEnd = c0 + ((len - c0) .&. complement 3)
      wordLoop c
        | c >= wholeEnd = writeIORef cur c
        | otherwise = do
            cycleS s
            t  <- readWord32LE arr c
            t' <- wordOp t
            writeWord32LE arr c t'
            wordLoop (c + 4)
  wordLoop c0
  -- (3) trailing partial word
  c1 <- readIORef cur
  if len - c1 > 0
    then do
      cycleS s
      writeIORef (sMbuf s) 0
      writeIORef (sNbuf s) 32
      let trailLoop c
            | c >= len  = writeIORef cur c
            | otherwise = do
                nb <- readIORef (sNbuf s)
                b  <- readArray arr c
                b' <- byteOp b nb
                writeArray arr c b'
                writeIORef (sNbuf s) (nb - 8)
                trailLoop (c + 1)
      trailLoop c1
    else pure ()

-- ---------------------------------------------------------------------------
-- Byte helpers
-- ---------------------------------------------------------------------------

withBuf :: (IOUArray Int Word8 -> Int -> IO ()) -> ByteString -> IO ByteString
withBuf act bs = do
  let len = BS.length bs
  arr <- newListArray (0, len - 1) (BS.unpack bs) :: IO (IOUArray Int Word8)
  act arr len
  BS.pack <$> getElems arr

readWord32LE :: IOUArray Int Word8 -> Int -> IO Word32
readWord32LE arr c = do
  b0 <- readArray arr c
  b1 <- readArray arr (c + 1)
  b2 <- readArray arr (c + 2)
  b3 <- readArray arr (c + 3)
  pure $ fromIntegral b0
     .|. (fromIntegral b1 `shiftL` 8)
     .|. (fromIntegral b2 `shiftL` 16)
     .|. (fromIntegral b3 `shiftL` 24)

writeWord32LE :: IOUArray Int Word8 -> Int -> Word32 -> IO ()
writeWord32LE arr c w = do
  writeArray arr c       (fromIntegral (w               .&. 0xff))
  writeArray arr (c + 1) (fromIntegral ((w `shiftR` 8)  .&. 0xff))
  writeArray arr (c + 2) (fromIntegral ((w `shiftR` 16) .&. 0xff))
  writeArray arr (c + 3) (fromIntegral ((w `shiftR` 24) .&. 0xff))

-- read up to four bytes as a little-endian word, zero-padded
le32pad :: ByteString -> Word32
le32pad bs =
  b 0 .|. (b 1 `shiftL` 8) .|. (b 2 `shiftL` 16) .|. (b 3 `shiftL` 24)
  where b i = if i < BS.length bs then fromIntegral (BS.index bs i) else 0

be32 :: Word32 -> ByteString
be32 w = BS.pack
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

chunk4 :: ByteString -> [ByteString]
chunk4 bs
  | BS.null bs = []
  | otherwise  = let (a, b) = BS.splitAt 4 bs in a : chunk4 b
