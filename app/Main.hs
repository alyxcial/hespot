-- | A small CLI to exercise the library.
module Main (main) where

import           Control.Exception         (SomeException, try)
import           Control.Monad             (forM_, when)
import           Data.Bits                 (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import qualified Data.Text                 as T
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           Numeric                   (showHex)
import           System.Directory          (createDirectoryIfMissing, doesDirectoryExist,
                                            doesFileExist, removeFile)
import           System.Environment        (getArgs)
import           System.Exit               (ExitCode (..), exitFailure)
import           System.FilePath           (takeExtension, (</>))
import           System.IO                 (BufferMode (LineBuffering), hFlush, hSetBuffering,
                                            stdout)
import           System.Process            (createProcess, proc, readProcessWithExitCode,
                                            waitForProcess)

import           Spotify
import           Spotify.Auth.Cache        (defaultCachePath, defaultTokenPath, loadCredentials,
                                            loadToken, saveCredentials, saveToken)
import           Spotify.Auth.ClientToken  (getClientToken)
import           Spotify.Auth.Login5       (login5Token)
import           Spotify.Auth.OAuth        (OAuthToken (..), defaultOAuthConfig, keymasterClientId,
                                            obtainToken, refreshAccessToken)
import           Spotify.Audio.Decrypt     (audioDecrypt)
import           Spotify.Audio.Fetch       (fetchEncryptedFile)
import           Spotify.Audio.OggTag      (retagOgg)
import           Spotify.Connect           (connectDevice)
import           Spotify.Connection        (close, connectAccessPointRetry)
import           Spotify.Id                (SpotifyId, fileIdHex, fileIdRaw, idToBase62, idToHex,
                                            idToRaw, parseTrackUri)
import           Spotify.Metadata          (AudioFileInfo, TrackInfo (..), afFileId, afFormat,
                                            fetchAlbumTracks, fetchTrack, formatName, pickBestOgg)
import           Spotify.Net.ApResolve     (resolveSpclient)
import           Spotify.Proto.Keyexchange (buildClientHello)
import           Spotify.WebApi            (Profile (..), fetchImage, fetchLyrics, getMe)

main :: IO ()
main = hSetBuffering stdout LineBuffering >> getArgs >>= \case
  ["handshake"]    -> handshakeDemo
  ["login", tok]   -> loginDemo (BC.pack tok)
  ["oauth-login"]  -> oauthLogin
  ["login-cached"] -> loginCached
  ["whoami"]            -> whoami
  ["track", uri]        -> trackInfo (BC.pack uri)
  ("download" : rest)   -> download rest
  ("album" : rest)      -> album rest
  ("play" : rest)       -> play rest
  ("lyrics" : rest)     -> lyrics rest
  ["client-token"]      -> clientTokenTest
  ["login5"]            -> login5Test
  ["connect"]           -> deviceConnect
  ["debug-hello"]       -> BC.putStrLn (toHex (buildClientHello (BS.replicate 96 1) (BS.replicate 16 2)))
  _                     -> usage

usage :: IO ()
usage = mapM_ putStrLn
  [ "hespot — a clean-room Spotify client core"
  , ""
  , "usage:"
  , "  hespot handshake        resolve an access point and run the crypto handshake"
  , "  hespot oauth-login      log in via your browser (OAuth), then cache credentials"
  , "  hespot login-cached     log in by reusing previously cached credentials"
  , "  hespot login <token>    log in with an access token you already have"
  , "  hespot whoami           log in (cached) and show your real account info"
  , "  hespot track <uri>      list a track's audio files (id + format)"
  , "  hespot download <uri> <out|dir|auto> [--quality 320|160|96|flac]"
  , "                          [--format ogg|mp3|flac|wav] [--no-cover]"
  , "                          download a track: tagged, cover art, auto-named, any format"
  , "  hespot album <album-uri> <dir> [flags]   download a whole album into a folder"
  , "  hespot play <uri> [--quality ...]        stream and play a track (ffplay/mpv)"
  , "  hespot lyrics <uri> [out.lrc]            download synced lyrics as .lrc"
  , "  hespot connect                           appear as a Spotify Connect device"
  ]

handshakeDemo :: IO ()
handshakeDemo = do
  (host, port) <- resolveAccessPoint
  putStrLn ("Resolved access point: " <> host <> ":" <> show port)
  r <- try (connectAccessPointRetry 5 host port) :: IO (Either SomeException Connection)
  case r of
    Left e     -> putStrLn ("Handshake failed: " <> show e) >> exitFailure
    Right conn -> do
      putStrLn "Handshake OK — Shannon-encrypted channel established."
      close conn

loginDemo :: ByteString -> IO ()
loginDemo token = report $ do
  putStrLn "Connecting and authenticating with access token..."
  session <- connect defaultConfig (withAccessToken token)
  putStrLn ("Logged in as: " <> BC.unpack (sessionUsername session))
  disconnect session

oauthLogin :: IO ()
oauthLogin = report $ do
  token <- obtainToken defaultOAuthConfig
  putStrLn "Got access token — connecting to Spotify..."
  -- cache the OAuth token so future Web API calls can refresh without a browser
  tpath <- defaultTokenPath
  now   <- round <$> getPOSIXTime
  saveToken tpath (tokenAccess token) (maybe "" id (tokenRefresh token))
            (now + fromIntegral (tokenExpiresIn token))
  session <- connect defaultConfig (withAccessToken (tokenAccess token))
  putStrLn ("Logged in as: " <> BC.unpack (sessionUsername session))
  path <- defaultCachePath
  saveCredentials path (reusableCredentials session)
  putStrLn ("Cached reusable credentials at " <> path)
  putStrLn ("Cached OAuth token at " <> tpath)
  showProfile (tokenAccess token)
  disconnect session

loginCached :: IO ()
loginCached = report $ do
  path   <- defaultCachePath
  mCreds <- loadCredentials path
  case mCreds of
    Nothing    -> putStrLn ("No cached credentials at " <> path <> " — run: hespot oauth-login")
    Just creds -> do
      session <- connect defaultConfig creds
      putStrLn ("Logged in (from cache) as: " <> BC.unpack (sessionUsername session))
      saveCredentials path (reusableCredentials session)
      disconnect session

whoami :: IO ()
whoami = report $ do
  path   <- defaultCachePath
  mCreds <- loadCredentials path
  case mCreds of
    Nothing    -> putStrLn ("No cached credentials at " <> path <> " — run: hespot oauth-login")
    Just creds -> do
      session <- connect defaultConfig creds
      country <- awaitCountry session
      prod    <- awaitProduct session
      putStrLn ("Username : " <> BC.unpack (sessionUsername session))
      putStrLn ("Country  : " <> maybe "?" BC.unpack country <> "   (pushed by the AP)")
      putStrLn ("Product  : " <> maybe "?" BC.unpack prod)
      etok <- ensureToken
      case etok of
        Left e    -> putStrLn ("Web API token: " <> e)
        Right tok -> showProfile tok
      disconnect session

trackInfo :: ByteString -> IO ()
trackInfo uriRaw = report $
  case parseTrackUri uriRaw of
    Left e    -> putStrLn ("bad track id: " <> e)
    Right sid -> do
      putStrLn ("Track id : " <> BC.unpack (idToHex sid))
      path   <- defaultCachePath
      mCreds <- loadCredentials path
      case mCreds of
        Nothing    -> putStrLn ("No cached credentials at " <> path <> " — run: hespot oauth-login")
        Just creds -> do
          session <- connect defaultConfig creds
          etrack  <- fetchTrack session sid
          case etrack of
            Left e   -> putStrLn ("metadata error: " <> e)
            Right ti -> do
              printTrackInfo ti
              putStrLn (show (length (tiFiles ti)) <> " audio file(s):")
              mapM_ (\f -> putStrLn ("  " <> formatName (afFormat f)
                                     <> "  " <> BC.unpack (fileIdHex (afFileId f)))) (tiFiles ti)
          disconnect session

data DlOpts = DlOpts
  { dlQuality :: Maybe Int   -- requested AudioFile format code (Nothing = best Ogg)
  , dlFormat  :: String      -- output container: ogg / mp3 / flac / wav / m4a
  , dlCover   :: Bool
  }

defaultDlOpts :: DlOpts
defaultDlOpts = DlOpts Nothing "ogg" True

parseDlOpts :: [String] -> DlOpts
parseDlOpts = go defaultDlOpts
  where
    go o []                      = o
    go o ("--quality" : q : r)   = go o { dlQuality = qcode q } r
    go o ("--format"  : f : r)   = go o { dlFormat  = f } r
    go o ("--no-cover" : r)      = go o { dlCover = False } r
    go o (_ : r)                 = go o r
    qcode q = case q of "320" -> Just 2; "160" -> Just 1; "96" -> Just 0; "flac" -> Just 16; _ -> Nothing

download :: [String] -> IO ()
download (uriS : outS : flags) = report $
  case parseTrackUri (BC.pack uriS) of
    Left e    -> putStrLn ("bad track id: " <> e)
    Right sid -> do
      path   <- defaultCachePath
      mCreds <- loadCredentials path
      case mCreds of
        Nothing    -> putStrLn ("No cached credentials at " <> path <> " — run: hespot oauth-login")
        Just creds -> do
          session <- connect defaultConfig creds
          _ <- downloadTrack session sid outS (parseDlOpts flags)
          disconnect session
download _ = putStrLn
  "usage: hespot download <uri> <out|dir|auto> [--quality 320|160|96|flac] [--format ogg|mp3|flac|wav] [--no-cover]"

-- | Download one track; returns the saved path (used by the album command too).
downloadTrack :: Session -> SpotifyId -> FilePath -> DlOpts -> IO (Maybe FilePath)
downloadTrack session sid outArg opts = do
  etrack <- fetchTrack session sid
  case etrack of
    Left e   -> putStrLn ("metadata error: " <> e) >> pure Nothing
    Right ti -> case pickFile opts (tiFiles ti) of
      Nothing   -> putStrLn "No matching audio file for this track." >> pure Nothing
      Just best -> do
        printTrackInfo ti
        putStrLn ("Quality  : " <> formatName (afFormat best))
        ekey <- requestAudioKey session (idToRaw sid) (fileIdRaw (afFileId best))
        case ekey of
          Left e    -> putStrLn ("key error: " <> e) >> pure Nothing
          Right key -> do
            efile <- fetchEncryptedFile session (afFileId best) progressLine
            putStrLn ""
            case efile of
              Left e    -> putStrLn ("fetch error: " <> e) >> pure Nothing
              Right enc -> case audioDecrypt key enc of
                Left e    -> putStrLn ("decrypt error: " <> e) >> pure Nothing
                Right dec -> do
                  let audio = maybe dec (`BS.drop` dec) (findVorbisStart dec)
                  out    <- resolveOut outArg ti (outExt (dlFormat opts))
                  mcover <- if dlCover opts && tiCoverId ti /= ""
                              then fetchCoverTmp (tiCoverId ti) else pure Nothing
                  ok <- finalize audio out mcover ti opts
                  maybe (pure ()) removeIfExists mcover
                  if ok
                    then do putStrLn ("Saved    : " <> out
                                      <> maybe "  (tagged)" (const "  (tagged + cover)") mcover)
                            pure (Just out)
                    else do BS.writeFile out audio
                            putStrLn ("Saved    : " <> out <> "  (raw; ffmpeg unavailable)")
                            pure (Just out)

pickFile :: DlOpts -> [AudioFileInfo] -> Maybe AudioFileInfo
pickFile o files = case dlQuality o of
  Nothing   -> pickBestOgg files
  Just code -> case filter ((== code) . afFormat) files of
    (x : _) -> Just x
    []      -> case [ f | f <- files, afFormat f `elem` [16, 22], code == 16 ] of
                 (x : _) -> Just x
                 _       -> pickBestOgg files

outExt :: String -> String
outExt f = case f of "mp3" -> "mp3"; "flac" -> "flac"; "wav" -> "wav"; "m4a" -> "m4a"; _ -> "ogg"

codecArgs :: String -> [String]
codecArgs f = case f of
  "mp3"  -> ["-c:a", "libmp3lame", "-q:a", "0"]
  "flac" -> ["-c:a", "flac"]
  "wav"  -> ["-c:a", "pcm_s16le"]
  "m4a"  -> ["-c:a", "aac", "-b:a", "256k"]
  _      -> ["-c:a", "copy"]

fetchCoverTmp :: ByteString -> IO (Maybe FilePath)
fetchCoverTmp hexId = do
  r <- fetchImage hexId
  case r of
    Right bytes -> let fp = "/tmp/hespot-cover.jpg" in BS.writeFile fp bytes >> pure (Just fp)
    Left _      -> pure Nothing

-- run ffmpeg to write the final file (format + tags + optional cover); False if ffmpeg fails
finalize :: ByteString -> FilePath -> Maybe FilePath -> TrackInfo -> DlOpts -> IO Bool
finalize audio out mcover ti opts
  | dlFormat opts == "ogg" = do
      -- native: rewrite the Vorbis comment packet (text tags + cover), no ffmpeg
      mjpeg <- maybe (pure Nothing) (fmap Just . BS.readFile) mcover
      let comments = textComments ti
                  ++ maybe [] (\j -> [("METADATA_BLOCK_PICTURE", BC.pack (pictureBlock j))]) mjpeg
      case retagOgg comments audio of
        Right tagged -> BS.writeFile out tagged >> pure True
        Left _       -> BS.writeFile out audio  >> pure True   -- fall back to untagged
  | otherwise = do
      -- transcode + tag (+ cover) via ffmpeg
      let tmpIn = out <> ".in.ogg"
      BS.writeFile tmpIn audio
      ok <- ffmpegFinalize tmpIn out mcover ti opts
      removeIfExists tmpIn
      pure ok

-- standard Vorbis comment field names, dropping empty values
textComments :: TrackInfo -> [(ByteString, ByteString)]
textComments ti = filter (not . BS.null . snd)
  [ ("TITLE",       tiName ti)
  , ("ARTIST",      BS.intercalate ", " (tiArtists ti))
  , ("ALBUM",       tiAlbum ti)
  , ("ALBUMARTIST", BS.intercalate ", " (tiAlbumArtists ti))
  , ("DATE",        tiReleaseDate ti)
  , ("LABEL",       tiLabel ti)
  , ("ISRC",        tiIsrc ti)
  , ("UPC",         tiUpc ti)
  , ("TRACKNUMBER", BC.pack (show (tiTrackNo ti)))
  , ("DISCNUMBER",  BC.pack (show (tiDiscNo ti)))
  ]

ffmpegFinalize :: FilePath -> FilePath -> Maybe FilePath -> TrackInfo -> DlOpts -> IO Bool
ffmpegFinalize audioIn out mc ti opts = do
  let fmt        = dlFormat opts
      header     = ["-y", "-loglevel", "error", "-i", audioIn]
      meta k v   = ["-metadata", k <> "=" <> v]
      metaIf k v = if BS.null v then [] else meta k (BC.unpack v)
      tags = meta   "title"        (BC.unpack (tiName ti))
          <> meta   "artist"       (BC.unpack (BS.intercalate ", " (tiArtists ti)))
          <> meta   "album"        (BC.unpack (tiAlbum ti))
          <> metaIf "album_artist" (BS.intercalate ", " (tiAlbumArtists ti))
          <> metaIf "date"         (tiReleaseDate ti)
          <> metaIf "label"        (tiLabel ti)
          <> metaIf "ISRC"         (tiIsrc ti)
          <> metaIf "UPC"          (tiUpc ti)
          <> meta   "track"        (show (tiTrackNo ti))
          <> meta   "disc"         (show (tiDiscNo ti))
      noCov = header <> ["-map", "0:a"] <> codecArgs fmt <> tags <> [out]
      args  = case mc of
        Nothing -> noCov
        Just c  -> header <> ["-i", c, "-map", "0:a", "-map", "1:v", "-c:v", "copy",
                              "-disposition:v:0", "attached_pic"]
                          <> codecArgs fmt <> tags <> [out]
  ok <- runOnce args
  if ok then pure True else runOnce noCov
  where
    runOnce as = do
      r <- try (readProcessWithExitCode "ffmpeg" as "")
             :: IO (Either SomeException (ExitCode, String, String))
      pure (case r of Right (ExitSuccess, _, _) -> True; _ -> False)

progressLine :: Int -> Int -> IO ()
progressLine got total = do
  let pct = if total <= 0 then 0 else got * 100 `div` total
  putStr ("\r  fetching " <> show pct <> "%  ("
          <> show (got `div` 1024) <> " / " <> show (total `div` 1024) <> " KiB)   ")
  hFlush stdout

fmtDuration :: Int -> String
fmtDuration ms = let s = ms `div` 1000 in show (s `div` 60) <> ":" <> pad2 (s `mod` 60)
  where pad2 n = if n < 10 then '0' : show n else show n

printTrackInfo :: TrackInfo -> IO ()
printTrackInfo ti = mapM_ putStrLn
  [ "Title    : " <> BC.unpack (tiName ti)
  , "Artist   : " <> BC.unpack (BS.intercalate ", " (tiArtists ti))
  , "Album    : " <> BC.unpack (tiAlbum ti) <> albArt
  , "Released : " <> orDash (tiReleaseDate ti)
  , "Label    : " <> orDash (tiLabel ti)
  , "Duration : " <> fmtDuration (tiDurationMs ti)
  , "Track #  : " <> show (tiTrackNo ti) <> "  (disc " <> show (tiDiscNo ti) <> ")"
  , "ISRC     : " <> orDash (tiIsrc ti)
  , "UPC      : " <> orDash (tiUpc ti)
  , "Licensor : " <> orDash (tiLicensorId ti)
  ]
  where albArt = case tiAlbumArtists ti of
          [] -> ""
          as -> "  (by " <> BC.unpack (BS.intercalate ", " as) <> ")"

orDash :: ByteString -> String
orDash b = if BS.null b then "-" else BC.unpack b

resolveOut :: FilePath -> TrackInfo -> String -> IO FilePath
resolveOut outArg ti ext = do
  isDir <- doesDirectoryExist outArg
  if outArg == "auto" || isDir
    then let dir  = if outArg == "auto" then "." else outArg
             base = sanitize (BC.unpack (BS.intercalate ", " (tiArtists ti)) <> " - "
                              <> BC.unpack (tiName ti))
         in pure (dir </> (base <> "." <> ext))
    else pure (if takeExtension outArg == "" then outArg <> "." <> ext else outArg)

sanitize :: String -> String
sanitize = map (\c -> if c `elem` ("/\\:*?\"<>|" :: String) then '_' else c)

-- | Download a whole album into a folder named after it.
album :: [String] -> IO ()
album (uriS : outDir : flags) = report $
  case parseTrackUri (BC.pack uriS) of
    Left e    -> putStrLn ("bad album id: " <> e)
    Right sid -> do
      path   <- defaultCachePath
      mCreds <- loadCredentials path
      case mCreds of
        Nothing    -> putStrLn ("No cached credentials at " <> path <> " — run: hespot oauth-login")
        Just creds -> do
          session <- connect defaultConfig creds
          er <- fetchAlbumTracks session sid
          case er of
            Left e               -> putStrLn ("album error: " <> e)
            Right (name, tracks) -> do
              let dir = outDir </> sanitize (BC.unpack name)
                  n   = length tracks
              putStrLn ("Album    : " <> BC.unpack name <> "  (" <> show n <> " tracks)")
              putStrLn ("Folder   : " <> dir)
              createDirectoryIfMissing True dir
              forM_ (zip [1 :: Int ..] tracks) $ \(i, t) -> do
                putStrLn ("\n[" <> show i <> "/" <> show n <> "] -----------------------------")
                _ <- downloadTrack session t dir (parseDlOpts flags)
                pure ()
              putStrLn ("\nDone: " <> show n <> " tracks saved to " <> dir)
          disconnect session
album _ = putStrLn
  "usage: hespot album <album-uri> <out-dir> [--quality 320|160|96|flac] [--format ogg|mp3|flac|wav] [--no-cover]"

-- | Fetch, decrypt, and play a track through ffplay (or mpv).
play :: [String] -> IO ()
play (uriS : flags) = report $
  case parseTrackUri (BC.pack uriS) of
    Left e    -> putStrLn ("bad track id: " <> e)
    Right sid -> do
      path   <- defaultCachePath
      mCreds <- loadCredentials path
      case mCreds of
        Nothing    -> putStrLn ("No cached credentials at " <> path <> " — run: hespot oauth-login")
        Just creds -> do
          session <- connect defaultConfig creds
          etrack  <- fetchTrack session sid
          case etrack of
            Left e   -> putStrLn ("metadata error: " <> e)
            Right ti -> case pickFile (parseDlOpts flags) (tiFiles ti) of
              Nothing   -> putStrLn "No matching audio file."
              Just best -> do
                putStrLn ("Now playing: " <> BC.unpack (BS.intercalate ", " (tiArtists ti))
                          <> " - " <> BC.unpack (tiName ti)
                          <> "  (" <> fmtDuration (tiDurationMs ti) <> ")")
                ekey <- requestAudioKey session (idToRaw sid) (fileIdRaw (afFileId best))
                case ekey of
                  Left e    -> putStrLn ("key error: " <> e)
                  Right key -> do
                    efile <- fetchEncryptedFile session (afFileId best) progressLine
                    putStrLn ""
                    case efile of
                      Left e    -> putStrLn ("fetch error: " <> e)
                      Right enc -> case audioDecrypt key enc of
                        Left e    -> putStrLn ("decrypt error: " <> e)
                        Right dec -> playOgg (maybe dec (`BS.drop` dec) (findVorbisStart dec))
          disconnect session
play _ = putStrLn "usage: hespot play <uri> [--quality 320|160|96]"

playOgg :: ByteString -> IO ()
playOgg ogg = do
  let tmp = "/tmp/hespot-play.ogg"
  BS.writeFile tmp ogg
  ok1 <- tryPlayer "ffplay" ["-loglevel", "error", "-nodisp", "-autoexit", tmp]
  ok2 <- if ok1 then pure True else tryPlayer "mpv" ["--no-video", "--really-quiet", tmp]
  when (not (ok1 || ok2)) (putStrLn "No audio player found — install ffplay (ffmpeg) or mpv.")
  removeIfExists tmp

tryPlayer :: String -> [String] -> IO Bool
tryPlayer cmd args = do
  r <- try (do (_, _, _, ph) <- createProcess (proc cmd args); waitForProcess ph)
         :: IO (Either SomeException ExitCode)
  pure (case r of Right ExitSuccess -> True; _ -> False)

removeIfExists :: FilePath -> IO ()
removeIfExists fp = do e <- doesFileExist fp; when e (removeFile fp)

-- Spotify prepends a ~167-byte Ogg page before the real Vorbis stream; find the
-- OggS page that carries the \x01vorbis identification header.
findVorbisStart :: ByteString -> Maybe Int
findVorbisStart d =
  let (pre, post) = BS.breakSubstring "\1vorbis" d
  in if BS.null post then Nothing else Just (scanBack (BS.length pre - 4))
  where
    scanBack i
      | i <= 0                            = 0
      | BS.isPrefixOf "OggS" (BS.drop i d) = i
      | otherwise                          = scanBack (i - 1)

-- | Print the user's Web API profile, given a bearer token.
showProfile :: ByteString -> IO ()
showProfile token = do
  eprof <- getMe token
  case eprof of
    Left e  -> putStrLn ("  profile error: " <> e)
    Right p -> mapM_ putStrLn
      [ "── Web API /v1/me ──"
      , "  id       : " <> T.unpack (profileId p)
      , "  display  : " <> maybe "-" T.unpack (profileDisplayName p)
      , "  email    : " <> maybe "-" T.unpack (profileEmail p)
      , "  country  : " <> maybe "-" T.unpack (profileCountry p)
      , "  product  : " <> maybe "-" T.unpack (profileProduct p)
      , "  followers: " <> maybe "-" show (profileFollowers p)
      ]

-- | Return a currently-valid Web API access token, refreshing it via the cached
-- refresh token when the cached access token has (almost) expired.
ensureToken :: IO (Either String ByteString)
ensureToken = do
  tpath <- defaultTokenPath
  mtok  <- loadToken tpath
  now   <- round <$> getPOSIXTime
  case mtok of
    Nothing -> pure (Left "no cached token — run: hespot oauth-login")
    Just (acc, refr, expiresAt)
      | expiresAt > now + 60 -> pure (Right acc)
      | BS.null refr         -> pure (Left "token expired, no refresh token — run: hespot oauth-login")
      | otherwise            -> do
          r <- try (refreshAccessToken defaultOAuthConfig refr)
                 :: IO (Either SomeException OAuthToken)
          case r of
            Left e    -> pure (Left ("refresh failed: " <> show e))
            Right tok -> do
              now2 <- round <$> getPOSIXTime
              saveToken tpath (tokenAccess tok) (maybe refr id (tokenRefresh tok))
                        (now2 + fromIntegral (tokenExpiresIn tok))
              pure (Right (tokenAccess tok))

-- run an action, turning any exception into a tidy message + non-zero exit
report :: IO () -> IO ()
report act = do
  r <- try act :: IO (Either SomeException ())
  case r of
    Left e  -> putStrLn ("Error: " <> show e) >> exitFailure
    Right _ -> pure ()

-- | Download a track's synced lyrics as an .lrc file (via spclient).
lyrics :: [String] -> IO ()
lyrics (uriS : rest) = report $
  case parseTrackUri (BC.pack uriS) of
    Left e    -> putStrLn ("bad track id: " <> e)
    Right sid -> do
      putStrLn "Getting a login5 token (client-token + hash-cash) ..."
      emt <- getModernToken
      case emt of
        Left e          -> putStrLn ("token: " <> e)
        Right (tok, ct) -> do
          (host, _) <- resolveSpclient
          r <- fetchLyrics tok ct host (idToBase62 sid)
          case r of
            Left e   -> putStrLn ("lyrics: " <> e)
            Right [] -> putStrLn "No lyrics available for this track."
            Right ls -> do
              let out = case rest of (o : _) -> o; [] -> "lyrics.lrc"
                  lrc = unlines [ "[" <> fmtLrc ms <> "]" <> T.unpack w | (ms, w) <- ls ]
              writeFile out lrc
              putStrLn ("Saved " <> show (length ls) <> " synced lines -> " <> out)
lyrics _ = putStrLn "usage: hespot lyrics <uri> [out.lrc]"

-- a login5 access token + client-token, derived from the cached stored credentials
getModernToken :: IO (Either String (ByteString, ByteString))
getModernToken = do
  path   <- defaultCachePath
  mCreds <- loadCredentials path
  case mCreds of
    Nothing -> pure (Left "no cached credentials — run: hespot oauth-login")
    Just creds -> case credUsername creds of
      Nothing   -> pure (Left "cached credentials have no username")
      Just user -> do
        ect <- getClientToken keymasterClientId devId
        case ect of
          Left e   -> pure (Left ("client-token: " <> e))
          Right ct -> do
            etok <- login5Token keymasterClientId devId ct user (credAuthData creds)
            pure (either (Left . ("login5: " <>)) (\t -> Right (t, ct)) etok)
  where devId = "0123456789abcdef0123456789abcdef01234567"

fmtLrc :: Int -> String
fmtLrc ms = pad2 (cs `div` 6000) <> ":" <> pad2 ((cs `div` 100) `mod` 60) <> "." <> pad2 (cs `mod` 100)
  where cs = ms `div` 10
        pad2 n = if n < 10 then '0' : show n else show n

clientTokenTest :: IO ()
clientTokenTest = report $ do
  putStrLn "Requesting a client-token (solving the hash-cash challenge) ..."
  r <- getClientToken keymasterClientId "0123456789abcdef0123456789abcdef01234567"
  case r of
    Left e   -> putStrLn ("client-token error: " <> e)
    Right ct -> putStrLn ("client-token OK: " <> BC.unpack (BS.take 28 ct)
                          <> "...  (" <> show (BS.length ct) <> " chars)")

login5Test :: IO ()
login5Test = report $ do
  putStrLn "Getting a login5 access token (client-token + stored credentials) ..."
  emt <- getModernToken
  case emt of
    Left e          -> putStrLn ("error: " <> e)
    Right (tok, _ ) -> putStrLn ("login5 access token OK: " <> BC.unpack (BS.take 28 tok)
                                 <> "...  (" <> show (BS.length tok) <> " chars)")

-- | Spotify Connect: register hespot as a device and stay connected.
deviceConnect :: IO ()
deviceConnect = report $ do
  emt <- getModernToken
  case emt of
    Left e          -> putStrLn ("token: " <> e)
    Right (tok, ct) -> do
      putStrLn "Registering hespot as a Spotify Connect device ..."
      connectDevice tok ct "0123456789abcdef0123456789abcdef01234567" "hespot"

toHex :: ByteString -> ByteString
toHex = BC.pack . concatMap byte . BS.unpack
  where byte b = let h = showHex b "" in if length h == 1 then '0' : h else h

-- standard base64 (with padding)
base64std :: ByteString -> String
base64std = enc . BS.unpack
  where
    ch i = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !! i
    enc (a : b : c : rest) = let n = w a 16 .|. w b 8 .|. fromIntegral c
                             in ch (sh n 18) : ch (sh n 12) : ch (sh n 6) : ch (n .&. 63) : enc rest
    enc [a, b] = let n = w a 16 .|. w b 8 in [ch (sh n 18), ch (sh n 12), ch (sh n 6), '=']
    enc [a]    = let n = w a 16          in [ch (sh n 18), ch (sh n 12), '=', '=']
    enc []     = []
    w x s = (fromIntegral x `shiftL` s) :: Int
    sh n s = (n `shiftR` s) .&. 63

-- a Vorbis METADATA_BLOCK_PICTURE (front cover, JPEG), base64-encoded
pictureBlock :: ByteString -> String
pictureBlock jpeg = base64std $ BS.concat
  [ be32 3                                  -- picture type: front cover
  , be32 (BS.length mime), mime             -- MIME type
  , be32 0                                   -- description (empty)
  , be32 0, be32 0, be32 0, be32 0          -- width, height, depth, #colors (0 = unspecified)
  , be32 (BS.length jpeg), jpeg             -- image data
  ]
  where mime = "image/jpeg"

be32 :: Int -> ByteString
be32 n = BS.pack
  [ fromIntegral (n `shiftR` 24), fromIntegral (n `shiftR` 16)
  , fromIntegral (n `shiftR` 8),  fromIntegral n ]
