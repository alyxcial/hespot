-- | Fetch an encrypted audio file over the access-point data channel.
--
-- A file is requested in 128 KiB chunks: each chunk request opens a channel and
-- the server streams back a short header block (one header, id @0x3@, carries
-- the total file size in 32-bit words) followed by the chunk's bytes. We fetch
-- chunk 0 to learn the size, then the remaining chunks, and concatenate.
module Spotify.Audio.Fetch
  ( fetchEncryptedFile
  ) where

import           Control.Concurrent.STM
import           Data.Bits               (shiftL, (.|.))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Word               (Word16, Word32, Word8)
import           System.Timeout          (timeout)

import           Spotify.Id              (FileId, fileIdRaw)
import           Spotify.Session

chunkBytes :: Int
chunkBytes = 0x20000   -- 128 KiB

chunkWords :: Word32
chunkWords = fromIntegral (chunkBytes `div` 4)

-- | Fetch a whole encrypted audio file. @onProgress got total@ is called after
-- each chunk so callers can show progress.
fetchEncryptedFile :: Session -> FileId -> (Int -> Int -> IO ()) -> IO (Either String ByteString)
fetchEncryptedFile sess file onProgress = do
  r0 <- fetchChunk sess file 0
  case r0 of
    Left e -> pure (Left e)
    Right (hdrs, dat0) -> case lookup 0x3 hdrs of
      Nothing        -> pure (Left "audio fetch: missing size header (0x3)")
      Just sizeWords -> do
        let total = fromIntegral (beWord32 sizeWords) * 4 :: Int
        onProgress (BS.length dat0) total
        loop total [dat0] (BS.length dat0)
  where
    loop total acc got
      | got >= total = pure (Right (BS.take total (BS.concat (reverse acc))))
      | otherwise = do
          r <- fetchChunk sess file (got `div` chunkBytes)
          case r of
            Left e -> pure (Left e)
            Right (_, dat)
              | BS.null dat -> pure (Right (BS.concat (reverse acc)))
              | otherwise   -> do
                  let got' = got + BS.length dat
                  onProgress got' total
                  loop total (dat : acc) got'

-- Request one chunk; return its (headers, data).
fetchChunk :: Session -> FileId -> Int -> IO (Either String ([(Word8, ByteString)], ByteString))
fetchChunk sess file idx = do
  (chId, q) <- allocChannel sess
  let start = fromIntegral idx * chunkWords
      end   = start + chunkWords
  sessionSend sess 0x08 (streamChunkReq chId (fileIdRaw file) start end)
  res <- timeout (20 * 1000000) (collect q [])
  closeChannel sess chId
  pure $ case res of
    Nothing         -> Left "audio fetch: chunk timed out"
    Just (Left e)   -> Left e
    Just (Right bs) -> Right (parseHeaders bs)
  where
    collect q acc = do
      msg <- atomically (readTQueue q)
      case msg of
        ChannelData d   -> collect q (d : acc)
        ChannelEnd      -> pure (Right (BS.concat (reverse acc)))
        ChannelFailed e -> pure (Left e)

-- The legacy StreamChunk request framing (offsets are in 32-bit words).
streamChunkReq :: Word16 -> ByteString -> Word32 -> Word32 -> ByteString
streamChunkReq chId fileId start end = BL.toStrict $ B.toLazyByteString $
     B.word16BE chId
  <> B.word8 0
  <> B.word8 1
  <> B.word16BE 0x0000
  <> B.word32BE 0x00000000
  <> B.word32BE 0x00009c40
  <> B.word32BE 0x00020000
  <> B.byteString fileId
  <> B.word32BE start
  <> B.word32BE end

-- Split the leading length-prefixed headers (terminated by a zero length) from
-- the chunk data.
parseHeaders :: ByteString -> ([(Word8, ByteString)], ByteString)
parseHeaders = go []
  where
    go hdrs bs
      | BS.length bs < 2 = (reverse hdrs, BS.empty)
      | otherwise =
          let len  = fromIntegral (beWord16 (BS.take 2 bs))
              rest = BS.drop 2 bs
          in if len == 0
               then (reverse hdrs, rest)
               else if BS.length rest < len
                      then (reverse hdrs, BS.empty)
                      else go ((BS.head rest, BS.take (len - 1) (BS.drop 1 rest)) : hdrs)
                              (BS.drop len rest)

beWord16 :: ByteString -> Word16
beWord16 = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0

beWord32 :: ByteString -> Word32
beWord32 = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0
