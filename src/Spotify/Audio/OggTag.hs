{-# LANGUAGE BangPatterns #-}

-- | Rewrite an Ogg Vorbis file's comment header to carry our tags (and an
-- embedded cover picture) — done from scratch, without any external tool.
--
-- We parse the Ogg pages, pull out the three Vorbis header packets
-- (identification, comment, setup), replace the comment packet with a freshly
-- built one, re-paginate the header, and (only if the header grew by a page)
-- renumber the audio pages — recomputing the Ogg CRC on every page we touch.
module Spotify.Audio.OggTag
  ( retagOgg
  ) where

import           Data.Array.Unboxed
import           Data.Bits
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Word

-- ---------------------------------------------------------------------------
-- Ogg CRC32 (polynomial 0x04c11db7, init 0, not reflected, no final xor)
-- ---------------------------------------------------------------------------

crcTable :: UArray Int Word32
crcTable = listArray (0, 255) [ gen (fromIntegral i `shiftL` 24) | i <- [0 .. 255 :: Int] ]
  where
    gen r = iterate stepBit r !! 8
    stepBit r = if testBit r 31 then (r `shiftL` 1) `xor` 0x04c11db7 else r `shiftL` 1

oggCrc :: ByteString -> Word32
oggCrc = BS.foldl' step 0
  where step !crc b = (crc `shiftL` 8)
                  `xor` (crcTable ! fromIntegral (((crc `shiftR` 24) `xor` fromIntegral b) .&. 0xff))

-- ---------------------------------------------------------------------------
-- Pages
-- ---------------------------------------------------------------------------

data Page = Page
  { pType    :: !Word8
  , pGranule :: !ByteString  -- 8 raw little-endian bytes
  , pSerial  :: !Word32
  , pSeq     :: !Word32
  , pSegs    :: ![Word8]
  , pData    :: !ByteString
  }

serialize :: Page -> ByteString
serialize p =
  let nseg = length (pSegs p)
      body = BS.concat
        [ BS.pack [0x4f, 0x67, 0x67, 0x53, 0, pType p]   -- "OggS", version 0, header type
        , pGranule p
        , le32 (pSerial p), le32 (pSeq p)
        , BS.pack [0, 0, 0, 0]                            -- CRC placeholder
        , BS.singleton (fromIntegral nseg), BS.pack (pSegs p)
        , pData p ]
      crc = oggCrc body
  in BS.take 22 body <> le32 crc <> BS.drop 26 body

parsePages :: ByteString -> Either String [Page]
parsePages bs
  | BS.null bs = Right []
  | not (BS.isPrefixOf "OggS" bs) = Left "ogg: bad page magic"
  | BS.length bs < 27 = Left "ogg: truncated page header"
  | otherwise =
      let nseg    = fromIntegral (BS.index bs 26)
          segs    = BS.unpack (BS.take nseg (BS.drop 27 bs))
          paylen  = sum (map fromIntegral segs)
          hdrlen  = 27 + nseg
          page    = Page (BS.index bs 5) (BS.take 8 (BS.drop 6 bs))
                         (le32at bs 14) (le32at bs 18) segs
                         (BS.take paylen (BS.drop hdrlen bs))
      in (page :) <$> parsePages (BS.drop (hdrlen + paylen) bs)

-- ---------------------------------------------------------------------------
-- Retag
-- ---------------------------------------------------------------------------

-- | Replace the Vorbis comments (the full @KEY=value@ list) of an Ogg Vorbis
-- stream. Values may include a @METADATA_BLOCK_PICTURE@ for cover art.
retagOgg :: [(ByteString, ByteString)] -> ByteString -> Either String ByteString
retagOgg comments ogg = do
  pages <- parsePages ogg
  (hdrPages, audioPages, k) <- splitHeader pages
  case hdrPages of
    [] -> Left "ogg: no header pages"
    (p0 : _) -> do
      let serial   = pSerial p0
          segAll   = concatMap pSegs hdrPages
          payAll   = BS.concat (map pData hdrPages)
      case extractPackets segAll payAll of
        (idP : _ : setupP : _) -> do
          let newComment = buildComment comments
              -- page 0: the identification packet alone (beginning of stream)
              page0      = Page 0x02 zero8 serial 0 (lacing (BS.length idP)) idP
              restPay    = newComment <> setupP
              restSegs   = lacing (BS.length newComment) ++ lacing (BS.length setupP)
              hdrNew     = page0 : paginate serial 1 restSegs restPay
              m          = length hdrNew
              delta      = fromIntegral (m - k) :: Word32
              hdrByteLen = sum [ 27 + length (pSegs p) + BS.length (pData p) | p <- hdrPages ]
              audioOut   = if delta == 0
                             then BS.drop hdrByteLen ogg            -- untouched: keep verbatim
                             else BS.concat [ serialize p { pSeq = pSeq p + delta } | p <- audioPages ]
          Right (BS.concat (map serialize hdrNew) <> audioOut)
        _ -> Left "ogg: fewer than three Vorbis header packets"

-- the header pages are those carrying the first three packets
splitHeader :: [Page] -> Either String ([Page], [Page], Int)
splitHeader pages = go 0 0 pages
  where
    go _ _ [] = Left "ogg: header packets not found"
    go pkts k (p : ps) =
      let pkts' = pkts + length (filter (< 255) (pSegs p))   -- packets ending in this page
      in if pkts' >= 3
           then Right (take (k + 1) pages, drop (k + 1) pages, k + 1)
           else go pkts' (k + 1) ps

-- split a concatenated (segment-table, payload) into its packets
extractPackets :: [Word8] -> ByteString -> [ByteString]
extractPackets [] _ = []
extractPackets segs payload =
  let (full, rest) = span (== 255) segs
      (these, more) = case rest of (x : xs) -> (full ++ [x], xs); [] -> (full, [])
      sz           = sum (map fromIntegral these)
      (pkt, pl')   = BS.splitAt sz payload
  in pkt : extractPackets more pl'

-- build a Vorbis comment packet from a full comment list
buildComment :: [(ByteString, ByteString)] -> ByteString
buildComment comments = BS.concat
  [ BS.singleton 0x03, "vorbis"                        -- comment header magic
  , le32 (fromIntegral (BS.length vendor)), vendor
  , le32 (fromIntegral (length entries))
  , BS.concat [ le32 (fromIntegral (BS.length e)) <> e | e <- entries ]
  , BS.singleton 0x01                                  -- framing bit
  ]
  where
    vendor  = "hespot"
    entries = [ k <> "=" <> v | (k, v) <- comments ]

-- paginate a (segment-table, payload) into pages of at most 255 segments
paginate :: Word32 -> Word32 -> [Word8] -> ByteString -> [Page]
paginate serial = go False
  where
    go _ _ [] _ = []
    go cont sq ss pl =
      let (pageSegs, restSegs) = splitAt 255 ss
          plen                 = sum (map fromIntegral pageSegs)
          (pagePl, restPl)     = BS.splitAt plen pl
          page                 = Page (if cont then 0x01 else 0x00) zero8 serial sq pageSegs pagePl
          nextCont             = not (null pageSegs) && last pageSegs == 255
      in page : if null restSegs then [] else go nextCont (sq + 1) restSegs restPl

-- lacing values for a packet of the given length
lacing :: Int -> [Word8]
lacing l = replicate (l `div` 255) 255 ++ [fromIntegral (l `mod` 255)]

-- ---------------------------------------------------------------------------

zero8 :: ByteString
zero8 = BS.replicate 8 0

le32 :: Word32 -> ByteString
le32 w = BS.pack
  [ fromIntegral w, fromIntegral (w `shiftR` 8)
  , fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 24) ]

le32at :: ByteString -> Int -> Word32
le32at bs i =
      fromIntegral (BS.index bs i)
  .|. (fromIntegral (BS.index bs (i + 1)) `shiftL` 8)
  .|. (fromIntegral (BS.index bs (i + 2)) `shiftL` 16)
  .|. (fromIntegral (BS.index bs (i + 3)) `shiftL` 24)
