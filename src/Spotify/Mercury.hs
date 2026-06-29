-- | The Mercury request/response bus that rides on top of the encrypted packet
-- channel. Mercury is how a client asks the backend for things (tokens, some
-- metadata, …): a request carries a sequence number, a small protobuf
-- 'Header' (uri + method), and zero or more payload parts; the matching
-- response comes back tagged with the same sequence number.
module Spotify.Mercury
  ( MercuryResponse (..)
  , encodeGetRequest
  , MercuryPacket (..)
  , parseMercuryPacket
  , parseHeaderStatus
  ) where

import           Data.Bits
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Builder  as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Word
import qualified Spotify.Proto.Wire       as W

-- | A completed Mercury response: the echoed uri, a status code, and the
-- payload parts (the 'Header' part has already been split off).
data MercuryResponse = MercuryResponse
  { mrUri     :: !ByteString
  , mrStatus  :: !Int
  , mrPayload :: ![ByteString]
  } deriving (Eq, Show)

-- | Encode the body of a Mercury @GET@ request (sent as packet command 0xb2).
encodeGetRequest :: Word64 -> ByteString -> ByteString
encodeGetRequest sq uri =
  BL.toStrict $ B.toLazyByteString $
       B.word16BE 8                                        -- sequence length
    <> B.word64BE sq                                       -- 8-byte sequence number
    <> B.word8 1                                           -- flags = FINAL
    <> B.word16BE 1                                        -- part count (just the header)
    <> B.word16BE (fromIntegral (BS.length header))
    <> B.byteString header
  where
    header = W.encode [ W.string 1 uri, W.string 3 "GET" ] -- Header { uri = 1, method = 3 }

-- | The raw framing of an incoming Mercury packet (before parts are merged
-- across continuation packets).
data MercuryPacket = MercuryPacket
  { mpSeq   :: !Word64
  , mpFlags :: !Word8
  , mpParts :: ![ByteString]
  } deriving (Eq, Show)

parseMercuryPacket :: ByteString -> Either String MercuryPacket
parseMercuryPacket bs0 = do
  (seqLen, bs1)   <- u16 bs0
  (seqBytes, bs2) <- takeN seqLen bs1
  (flags, bs3)    <- u8 bs2
  (count, bs4)    <- u16 bs3
  parts           <- parseParts count bs4
  pure (MercuryPacket (beWord64 seqBytes) flags parts)
  where
    parseParts 0 _  = Right []
    parseParts k bs = do
      (plen, r1) <- u16 bs
      (part, r2) <- takeN plen r1
      (part :) <$> parseParts (k - 1) r2

-- | Extract @(uri, status_code)@ from a Mercury 'Header' protobuf. @status_code@
-- is a protobuf @sint32@, so it is zig-zag decoded.
parseHeaderStatus :: ByteString -> (ByteString, Int)
parseHeaderStatus hdr = case W.decode hdr of
  Left _  -> ("", 0)
  Right m -> ( maybe "" id  (W.getBytes  1 m)
             , maybe 0 zigzag (W.getVarint 4 m) )

zigzag :: Word64 -> Int
zigzag w = fromIntegral ((w `shiftR` 1) `xor` negate (w .&. 1))

-- ---------------------------------------------------------------------------

u8 :: ByteString -> Either String (Word8, ByteString)
u8 bs = maybe (Left "mercury: short u8") Right (BS.uncons bs)

u16 :: ByteString -> Either String (Int, ByteString)
u16 bs
  | BS.length bs >= 2 = Right ( fromIntegral (BS.index bs 0) `shiftL` 8
                            .|. fromIntegral (BS.index bs 1)
                              , BS.drop 2 bs )
  | otherwise         = Left "mercury: short u16"

takeN :: Int -> ByteString -> Either String (ByteString, ByteString)
takeN n bs
  | BS.length bs >= n = Right (BS.splitAt n bs)
  | otherwise         = Left "mercury: short bytes"

beWord64 :: ByteString -> Word64
beWord64 = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0
