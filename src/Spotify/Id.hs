-- | Spotify identifiers: the 128-bit item id (tracks, albums, …) and the
-- 160-bit audio 'FileId'. Items are usually written in base62 (the 22-character
-- form in @spotify:track:…@ URIs); on the wire they travel as raw bytes.
module Spotify.Id
  ( SpotifyId
  , idFromBase62
  , idFromRaw
  , parseTrackUri
  , idToHex
  , idToBase62
  , idToRaw
  , FileId
  , fileIdFromHex
  , fileIdFromRaw
  , fileIdRaw
  , fileIdHex
  ) where

import           Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (chr)
import           Data.Word             (Word8)

-- | A 128-bit Spotify item id.
newtype SpotifyId = SpotifyId Integer
  deriving (Eq, Ord)

instance Show SpotifyId where
  show = BC.unpack . idToHex

-- | Decode a 22-character base62 id.
idFromBase62 :: ByteString -> Either String SpotifyId
idFromBase62 s
  | BS.length s /= 22 = Left "spotify id: expected 22 base62 characters"
  | otherwise         = SpotifyId <$> BS.foldl' step (Right 0) s
  where
    step acc c = do
      n <- acc
      d <- digit c
      pure (n * 62 + fromIntegral d)
    digit c
      | c >= 0x30 && c <= 0x39 = Right (c - 0x30)        -- 0-9
      | c >= 0x61 && c <= 0x7a = Right (c - 0x61 + 10)   -- a-z
      | c >= 0x41 && c <= 0x5a = Right (c - 0x41 + 36)   -- A-Z
      | otherwise              = Left "spotify id: invalid base62 character"

-- | Accept a bare base62 id, a @spotify:track:…@ / @spotify:album:…@ URI, or an
-- @open.spotify.com/…/<id>@ URL.
parseTrackUri :: ByteString -> Either String SpotifyId
parseTrackUri raw =
  let noQuery = BC.takeWhile (/= '?') raw
  in idFromBase62 (lastSeg '/' (lastSeg ':' noQuery))
  where
    lastSeg c b = case BC.split c b of [] -> b; xs -> last xs

-- | Build a 'SpotifyId' from its 16 raw big-endian bytes (e.g. a metadata @gid@).
idFromRaw :: ByteString -> Either String SpotifyId
idFromRaw b
  | BS.length b == 16 = Right (SpotifyId (BS.foldl' (\acc w -> acc `shiftL` 8 .|. fromIntegral w) 0 b))
  | otherwise         = Left "spotify id: expected 16 raw bytes"

-- | The id as 32 lowercase hex characters.
idToHex :: SpotifyId -> ByteString
idToHex (SpotifyId n) = BS.pack (concatMap byteHex (rawBytes n 16))

-- | The id as a canonical 22-character base62 string.
idToBase62 :: SpotifyId -> ByteString
idToBase62 (SpotifyId n) = BC.pack (replicate (22 - length ds) '0' ++ ds)
  where
    ds = if n == 0 then "0" else reverse (go n)
    go 0 = ""
    go k = (alphabet !! fromIntegral (k `mod` 62)) : go (k `div` 62)
    alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" :: String

-- | The id as 16 raw big-endian bytes (what the audio-key request expects).
idToRaw :: SpotifyId -> ByteString
idToRaw (SpotifyId n) = BS.pack (rawBytes n 16)

-- | A 160-bit (20-byte) audio file id.
newtype FileId = FileId ByteString
  deriving (Eq, Ord)

instance Show FileId where
  show = BC.unpack . fileIdHex

fileIdFromHex :: ByteString -> Either String FileId
fileIdFromHex hx
  | BS.length raw == 20 = Right (FileId raw)
  | otherwise           = Left "file id: expected 40 hex characters"
  where raw = unhex hx

-- | Build a 'FileId' from its 20 raw bytes (as they appear in metadata).
fileIdFromRaw :: ByteString -> Either String FileId
fileIdFromRaw raw
  | BS.length raw == 20 = Right (FileId raw)
  | otherwise           = Left "file id: expected 20 raw bytes"

-- | The 20 raw bytes (what the audio-key request expects).
fileIdRaw :: FileId -> ByteString
fileIdRaw (FileId b) = b

fileIdHex :: FileId -> ByteString
fileIdHex (FileId b) = BS.pack (concatMap byteHex (BS.unpack b))

-- ---------------------------------------------------------------------------

rawBytes :: Integer -> Int -> [Word8]
rawBytes n size = [ fromIntegral (n `shiftR` (8 * (size - 1 - i)) .&. 0xff) | i <- [0 .. size - 1] ]

byteHex :: Word8 -> [Word8]
byteHex b = [ hexDigit (b `shiftR` 4), hexDigit (b .&. 0xf) ]
  where hexDigit d = fromIntegral (fromEnum (chr (if d < 10 then fromIntegral d + 0x30
                                                            else fromIntegral d - 10 + 0x61)))

unhex :: ByteString -> ByteString
unhex = BS.pack . go . BS.unpack
  where
    go (a : b : rest) = (hv a * 16 + hv b) : go rest
    go _              = []
    hv c | c >= 0x30 && c <= 0x39 = c - 0x30
         | c >= 0x61 && c <= 0x66 = c - 0x61 + 10
         | c >= 0x41 && c <= 0x46 = c - 0x41 + 10
         | otherwise              = 0
