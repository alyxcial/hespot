-- | Track metadata: the human-readable bits (title, artists, album, duration)
-- and the audio file list. Fetched over Mercury (@hm://metadata/4/track/<hex>@),
-- which needs no token.
module Spotify.Metadata
  ( AudioFileInfo (..)
  , TrackInfo (..)
  , formatName
  , isOgg
  , fetchTrack
  , fetchTrackFiles
  , fetchAlbumTracks
  , pickBestOgg
  ) where

import           Data.Bits             (shiftR, xor, (.&.))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.List             (maximumBy, sortBy)
import           Data.Ord              (Down (..), comparing)
import           Data.Word             (Word64, Word8)
import           Numeric               (showHex)

import           Spotify.Id
import qualified Spotify.Proto.Wire    as W
import           Spotify.Session       (MercuryResponse (..), Session, mercuryGet)

-- | One playable audio file of a track.
data AudioFileInfo = AudioFileInfo
  { afFileId :: !FileId
  , afFormat :: !Int
  } deriving (Show)

-- | The interesting metadata of a track.
data TrackInfo = TrackInfo
  { tiName        :: !ByteString
  , tiArtists     :: ![ByteString]
  , tiAlbum       :: !ByteString
  , tiAlbumArtists :: ![ByteString]
  , tiDurationMs  :: !Int
  , tiTrackNo     :: !Int
  , tiDiscNo      :: !Int
  , tiIsrc        :: !ByteString  -- ^ International Standard Recording Code
  , tiUpc         :: !ByteString  -- ^ album Universal Product Code
  , tiReleaseDate :: !ByteString  -- ^ @YYYY@ / @YYYY-MM@ / @YYYY-MM-DD@
  , tiLabel       :: !ByteString  -- ^ record label
  , tiLicensorId  :: !ByteString  -- ^ licensor UUID
  , tiAlbumGid    :: !ByteString  -- ^ raw 16-byte album id (for a follow-up album fetch)
  , tiCoverId     :: !ByteString  -- ^ largest cover image file-id (hex), from the album
  , tiFiles       :: ![AudioFileInfo]
  } deriving (Show)

formatName :: Int -> String
formatName f = case f of
  0  -> "OGG_VORBIS_96"
  1  -> "OGG_VORBIS_160"
  2  -> "OGG_VORBIS_320"
  3  -> "MP3_256"
  4  -> "MP3_320"
  5  -> "MP3_160"
  6  -> "MP3_96"
  8  -> "AAC_24"
  9  -> "AAC_48"
  16 -> "FLAC"
  22 -> "FLAC_24BIT"
  _  -> "FORMAT_" ++ show f

isOgg :: Int -> Bool
isOgg f = f `elem` [0, 1, 2]

-- | Fetch a track's full metadata. The UPC lives in the album metadata, so when
-- it is missing we do a quick follow-up album fetch to fill it in.
fetchTrack :: Session -> SpotifyId -> IO (Either String TrackInfo)
fetchTrack sess sid = do
  r <- mercuryGet sess ("hm://metadata/4/track/" <> idToHex sid)
  case r of
    Left e -> pure (Left e)
    Right resp
      | mrStatus resp >= 400 -> pure (Left ("metadata: status " ++ show (mrStatus resp)))
      | otherwise -> case mrPayload resp of
          []          -> pure (Left "metadata: empty payload")
          (track : _) -> case parseTrack track of
            Left e   -> pure (Left e)
            Right ti
              | tiAlbumGid ti == "" -> pure (Right ti)
              | otherwise -> do
                  (upc, cover) <- fetchAlbumExtras sess (tiAlbumGid ti)
                  pure (Right ti
                    { tiUpc     = if tiUpc ti == ""     then upc   else tiUpc ti
                    , tiCoverId = if tiCoverId ti == "" then cover else tiCoverId ti
                    })

-- Fetch the UPC and the largest cover-image id from an album's metadata.
fetchAlbumExtras :: Session -> ByteString -> IO (ByteString, ByteString)
fetchAlbumExtras sess gid = do
  r <- mercuryGet sess ("hm://metadata/4/album/" <> hexOfBytes gid)
  pure $ case r of
    Right resp
      | mrStatus resp < 400
      , (a : _) <- mrPayload resp
      , Right am <- W.decode a -> (findExternalId "upc" am, albumCoverId am)
    _ -> ("", "")

-- the file-id (hex) of the largest cover image in an Album's cover_group (17)
albumCoverId :: W.Message -> ByteString
albumCoverId am = case W.getMessage 17 am of
  Right (Just ig) ->
    let imgs = [ (maybe 0 zigzag (W.getVarint 3 im), fid)
               | (1, W.VBytes s) <- ig, Right im <- [W.decode s]
               , Just fid <- [W.getBytes 1 im] ]
    in if null imgs then "" else hexOfBytes (snd (maximumBy (comparing fst) imgs))
  _ -> ""

-- | Just the audio file list (kept for the @track@ command).
fetchTrackFiles :: Session -> SpotifyId -> IO (Either String [AudioFileInfo])
fetchTrackFiles sess sid = fmap (fmap tiFiles) (fetchTrack sess sid)

-- | Fetch an album's name and its ordered list of track ids.
fetchAlbumTracks :: Session -> SpotifyId -> IO (Either String (ByteString, [SpotifyId]))
fetchAlbumTracks sess sid = do
  r <- mercuryGet sess ("hm://metadata/4/album/" <> idToHex sid)
  pure $ case r of
    Left e -> Left e
    Right resp
      | mrStatus resp >= 400 -> Left ("album metadata: status " ++ show (mrStatus resp))
      | otherwise -> case mrPayload resp of
          (a : _) -> parseAlbumTracks a
          []      -> Left "album metadata: empty payload"

parseAlbumTracks :: ByteString -> Either String (ByteString, [SpotifyId])
parseAlbumTracks bs = do
  m      <- W.decode bs
  gids   <- concat <$> mapM discTracks [ s | (11, W.VBytes s) <- m ]   -- repeated Disc (11)
  tracks <- mapM idFromRaw gids
  pure (maybe "" id (W.getString 2 m), tracks)
  where
    discTracks ds = do
      dm <- W.decode ds
      pure [ gid | (3, W.VBytes tm) <- dm, Right tmm <- [W.decode tm]   -- repeated Track (3)
                 , Just gid <- [W.getBytes 1 tmm] ]                     -- Track.gid (1)

-- | The highest-quality Ogg Vorbis file available, if any.
pickBestOgg :: [AudioFileInfo] -> Maybe AudioFileInfo
pickBestOgg files =
  case sortBy (comparing (Down . afFormat)) (filter (isOgg . afFormat) files) of
    (x : _) -> Just x
    []      -> Nothing

-- ---------------------------------------------------------------------------

parseTrack :: ByteString -> Either String TrackInfo
parseTrack bs = do
  m       <- W.decode bs
  artists <- mapM parseName [ s | (4, W.VBytes s) <- m ]   -- repeated Artist (name = 2)
  files   <- mapM parseAudioFile [ s | (12, W.VBytes s) <- m ]
  malbum  <- W.getMessage 3 m                              -- Album
  mlic    <- W.getMessage 21 m                             -- Licensor
  albArts <- case malbum of
               Just am -> mapM parseName [ s | (3, W.VBytes s) <- am ]
               Nothing -> Right []
  pure TrackInfo
    { tiName         = strOf 2 m
    , tiArtists      = artists
    , tiAlbum        = maybe "" (strOf 2) malbum
    , tiAlbumArtists = albArts
    , tiDurationMs   = maybe 0 zigzag (W.getVarint 7 m)
    , tiTrackNo      = maybe 0 zigzag (W.getVarint 5 m)
    , tiDiscNo       = maybe 0 zigzag (W.getVarint 6 m)
    , tiIsrc         = findExternalId "isrc" m
    , tiUpc          = maybe "" (findExternalId "upc") malbum
    , tiReleaseDate  = maybe "" albumDate malbum
    , tiLabel        = maybe "" (strOf 5) malbum
    , tiLicensorId   = maybe "" id (mlic >>= (fmap formatUuid . W.getBytes 1))
    , tiAlbumGid     = maybe "" id (malbum >>= W.getBytes 1)
    , tiCoverId      = maybe "" albumCoverId malbum
    , tiFiles        = files
    }
  where strOf fn msg = maybe "" id (W.getString fn msg)

parseName :: ByteString -> Either String ByteString
parseName bs = do
  m <- W.decode bs
  pure (maybe "" id (W.getString 2 m))

-- the id of an external_id (field 10) of a given type, if present
findExternalId :: ByteString -> W.Message -> ByteString
findExternalId typ m = case ids of (x : _) -> x; [] -> ""
  where
    ids = [ i
          | s <- [ x | (10, W.VBytes x) <- m ]
          , Right em <- [W.decode s]
          , W.getString 1 em == Just typ
          , Just i <- [W.getString 2 em] ]

-- Album.date (field 6) as YYYY[-MM[-DD]]
albumDate :: W.Message -> ByteString
albumDate am = case W.getMessage 6 am of
  Right (Just dm) ->
    let y  = maybe 0 zigzag (W.getVarint 1 dm)
        mo = maybe 0 zigzag (W.getVarint 2 dm)
        d  = maybe 0 zigzag (W.getVarint 3 dm)
    in if y <= 0
         then ""
         else BC.pack (show y ++ part mo (part d ""))
  _ -> ""
  where
    part n rest | n > 0     = "-" ++ pad2 n ++ rest
                | otherwise = ""
    pad2 n = if n < 10 then '0' : show n else show n

-- a 16-byte UUID as 8-4-4-4-12 hex
formatUuid :: ByteString -> ByteString
formatUuid b = BC.pack (dashify (concatMap hx (BS.unpack b)))
  where
    hx :: Word8 -> String
    hx w = let s = showHex w "" in if length s == 1 then '0' : s else s
    dashify h
      | length h == 32 = let (a, r1) = splitAt 8 h; (c, r2) = splitAt 4 r1
                             (d, r3) = splitAt 4 r2; (e, f) = splitAt 4 r3
                         in a ++ "-" ++ c ++ "-" ++ d ++ "-" ++ e ++ "-" ++ f
      | otherwise      = h

parseAudioFile :: ByteString -> Either String AudioFileInfo
parseAudioFile bs = do
  m   <- W.decode bs
  fb  <- maybe (Left "AudioFile: missing file_id") Right (W.getBytes 1 m)
  fid <- fileIdFromRaw fb
  pure (AudioFileInfo fid (maybe (-1) fromIntegral (W.getVarint 2 m)))

hexOfBytes :: ByteString -> ByteString
hexOfBytes = BC.pack . concatMap hx . BS.unpack
  where hx w = let s = showHex w "" in if length s == 1 then '0' : s else s

-- protobuf sint32 zig-zag decode
zigzag :: Word64 -> Int
zigzag w = fromIntegral ((w `shiftR` 1) `xor` negate (w .&. 1))
