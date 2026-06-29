-- | A thin wrapper over Spotify's Web API (the HTTPS @api.spotify.com@ surface),
-- authenticated with a bearer token obtained from the session.
module Spotify.WebApi
  ( apiGet
  , Profile (..)
  , getMe
  , fetchImage
  , spclientGet
  , fetchLyrics
  ) where

import           Control.Concurrent       (threadDelay)
import           Data.Aeson
import           Data.Aeson.Types         (Parser, parseEither)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           System.IO                (hPutStrLn, stderr)
import           Text.Read                (readMaybe)

-- | Perform an authenticated @GET@ against @api.spotify.com@ and decode the JSON.
-- Transparently waits out an HTTP 429 (rate limit), honouring @Retry-After@.
apiGet :: ByteString -> String -> IO (Either String Value)
apiGet token path = newManager tlsManagerSettings >>= \mgr -> go mgr (0 :: Int)
  where
    go mgr attempt = do
      req0 <- parseRequest ("https://api.spotify.com" <> path)
      let req = req0
            { requestHeaders = [ ("Authorization", "Bearer " <> token)
                               , ("Accept", "application/json") ] }
      resp <- httpLbs req mgr
      let body = responseBody resp
          code = statusCode (responseStatus resp)
      if | code >= 200 && code < 300 ->
             pure (maybe (Left "Web API: invalid JSON") Right (decode body))
         | code == 429 && attempt < 3 -> do
             let wait = min 20 (max 1 (maybe 3 id (retryAfter resp)))
             hPutStrLn stderr ("  Web API rate-limited (429); waiting " <> show wait <> "s ...")
             threadDelay (wait * 1000000)
             go mgr (attempt + 1)
         | otherwise ->
             pure (Left ("Web API: HTTP " <> show code <> ": " <> take 300 (BLC.unpack body)))

    retryAfter resp = lookup "Retry-After" (responseHeaders resp) >>= readMaybe . BC.unpack

-- | Download a cover image (JPEG bytes) by its hex file-id from Spotify's public
-- image CDN — no authentication needed.
fetchImage :: ByteString -> IO (Either String ByteString)
fetchImage hexId = do
  mgr  <- newManager tlsManagerSettings
  req  <- parseRequest ("https://i.scdn.co/image/" <> BC.unpack hexId)
  resp <- httpLbs req mgr
  let code = statusCode (responseStatus resp)
  pure $ if code >= 200 && code < 300
           then Right (BL.toStrict (responseBody resp))
           else Left ("cover image: HTTP " <> show code)

-- | An authenticated GET against an spclient host (needs both an access token
-- and a client-token). Returns the raw response body.
spclientGet :: ByteString -> ByteString -> String -> String -> IO (Either String ByteString)
spclientGet token clientToken host path = do
  mgr  <- newManager tlsManagerSettings
  req0 <- parseRequest ("https://" <> host <> path)
  let req = req0
        { requestHeaders = [ ("Authorization", "Bearer " <> token)
                           , ("client-token", clientToken)
                           , ("app-platform", "WebPlayer")
                           , ("Accept", "application/json") ] }
  resp <- httpLbs req mgr
  let body = responseBody resp
      code = statusCode (responseStatus resp)
  pure $ if code >= 200 && code < 300
           then Right (BL.toStrict body)
           else Left ("spclient: HTTP " <> show code <> ": " <> take 200 (BLC.unpack body))

-- | Fetch a track's (synced) lyrics as @(startTimeMs, words)@ lines, via the
-- spclient color-lyrics endpoint. Needs an access token and a client-token.
fetchLyrics :: ByteString -> ByteString -> String -> ByteString -> IO (Either String [(Int, Text)])
fetchLyrics token clientToken host base62 = do
  r <- spclientGet token clientToken host
         ("/color-lyrics/v2/track/" <> BC.unpack base62
          <> "?format=json&vocalRemoval=false&market=from_token")
  pure (r >>= parseLyrics)

parseLyrics :: ByteString -> Either String [(Int, Text)]
parseLyrics bs = either Left (parseEither linesP) (eitherDecodeStrictBytes bs)
  where
    eitherDecodeStrictBytes = eitherDecode . BL.fromStrict
    linesP = withObject "lyrics" $ \o -> do
      lyr <- o   .: "lyrics"
      lns <- lyr .: "lines"
      mapM lineP lns
    lineP = withObject "line" $ \l -> do
      ms <- l .: "startTimeMs"
      w  <- l .: "words"
      pure (maybe 0 id (readMaybe (T.unpack ms)), w)

-- | The interesting bits of @/v1/me@.
data Profile = Profile
  { profileId          :: Text
  , profileDisplayName :: Maybe Text
  , profileEmail       :: Maybe Text
  , profileCountry     :: Maybe Text
  , profileProduct     :: Maybe Text
  , profileFollowers   :: Maybe Int
  } deriving (Eq, Show)

-- | Fetch the current user's profile.
getMe :: ByteString -> IO (Either String Profile)
getMe token = (>>= parseEither parseProfile) <$> apiGet token "/v1/me"

parseProfile :: Value -> Parser Profile
parseProfile = withObject "me" $ \o -> do
  pid  <- o .:  "id"
  dn   <- o .:? "display_name"
  em   <- o .:? "email"
  co   <- o .:? "country"
  pr   <- o .:? "product"
  mFol <- o .:? "followers" :: Parser (Maybe Object)
  fol  <- maybe (pure Nothing) (.:? "total") mFol
  pure (Profile pid dn em co pr fol)
