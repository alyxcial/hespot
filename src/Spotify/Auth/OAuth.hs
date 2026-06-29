-- | Obtain a Spotify access token via the OAuth 2.0 authorization-code flow
-- with PKCE — the same flow librespot uses.
--
-- This is interactive: we print (and optionally open) an authorization URL, the
-- user logs in with their browser, and Spotify redirects back to a tiny local
-- listener we run, handing us a one-time code we exchange for an access token.
module Spotify.Auth.OAuth
  ( OAuthConfig (..)
  , defaultOAuthConfig
  , OAuthToken (..)
  , keymasterClientId
  , base64url
  , sha256
  , obtainToken
  , refreshAccessToken
  ) where

import           Control.Exception        (SomeException, bracket, try)
import           Control.Monad            (void)
import           Crypto.Hash              (SHA256 (..), hashWith)
import qualified Data.ByteArray           as BA
import           Data.Aeson               (FromJSON (..), eitherDecode, withObject, (.:), (.:?))
import           Data.Bits                (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as BL
import           Data.Word                (Word8)
import qualified Network.HTTP.Client      as H
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Types.URI   (urlEncode)
import qualified Network.Socket           as N
import qualified Network.Socket.ByteString as NB
import           System.Entropy           (getEntropy)
import           System.Process           (spawnProcess)

-- | OAuth flow configuration. 'defaultOAuthConfig' is the usual choice.
data OAuthConfig = OAuthConfig
  { oauthClientId     :: ByteString    -- ^ defaults to Spotify's keymaster id
  , oauthRedirectPort :: Int           -- ^ local port the browser redirects to
  , oauthScopes       :: [ByteString]  -- ^ requested scopes
  , oauthOpenBrowser  :: Bool          -- ^ try to launch the browser automatically
  }

-- | The token returned by Spotify.
data OAuthToken = OAuthToken
  { tokenAccess    :: ByteString
  , tokenRefresh   :: Maybe ByteString
  , tokenExpiresIn :: Int
  } deriving (Eq, Show)

-- | Spotify's well-known desktop ("keymaster") client id — the one with enough
-- privilege for an access-point login. Override only if you know you need to.
keymasterClientId :: ByteString
keymasterClientId = "65b708073fc0480ea92a077233ca87bd"

defaultOAuthConfig :: OAuthConfig
defaultOAuthConfig = OAuthConfig
  { oauthClientId     = keymasterClientId
  , oauthRedirectPort = 5588
  , oauthScopes       =
      [ "streaming"
      , "user-read-email", "user-read-private"
      , "user-read-playback-state", "user-modify-playback-state"
      , "user-read-currently-playing"
      , "playlist-read-private", "playlist-read-collaborative"
      , "user-library-read", "user-top-read", "user-read-recently-played"
      ]
  , oauthOpenBrowser  = True
  }

-- | Run the full interactive flow and return a fresh access token.
obtainToken :: OAuthConfig -> IO OAuthToken
obtainToken cfg = do
  verifier <- base64url <$> getEntropy 32
  state    <- base64url <$> getEntropy 16
  let challenge   = base64url (sha256 verifier)
      redirectUri = "http://127.0.0.1:" <> BC.pack (show (oauthRedirectPort cfg)) <> "/login"
      authUrl     = buildAuthUrl cfg redirectUri challenge state

  putStrLn "\nTo authorize hespot, open this URL in your browser and log in:\n"
  BC.putStrLn authUrl
  putStrLn ""
  maybeOpenBrowser cfg authUrl

  code <- waitForCode (oauthRedirectPort cfg)
  exchangeCode cfg redirectUri verifier code

-- ---------------------------------------------------------------------------
-- Authorization URL
-- ---------------------------------------------------------------------------

buildAuthUrl :: OAuthConfig -> ByteString -> ByteString -> ByteString -> ByteString
buildAuthUrl cfg redirectUri challenge state =
  "https://accounts.spotify.com/authorize?" <> query
  where
    query = BS.intercalate "&"
      [ "client_id="             <> oauthClientId cfg
      , "response_type=code"
      , "redirect_uri="          <> enc redirectUri
      , "scope="                 <> enc (BS.intercalate " " (oauthScopes cfg))
      , "code_challenge_method=S256"
      , "code_challenge="        <> challenge
      , "state="                 <> state
      ]
    enc = urlEncode True

maybeOpenBrowser :: OAuthConfig -> ByteString -> IO ()
maybeOpenBrowser cfg url
  | oauthOpenBrowser cfg =
      void (try (void (spawnProcess "xdg-open" [BC.unpack url])) :: IO (Either SomeException ()))
  | otherwise = pure ()

-- ---------------------------------------------------------------------------
-- Local redirect listener
-- ---------------------------------------------------------------------------

-- | Bind a one-shot HTTP listener on @127.0.0.1:port@ and return the @code@
-- from the single request Spotify's redirect makes.
waitForCode :: Int -> IO ByteString
waitForCode port = do
  let hints = N.defaultHints { N.addrFlags = [N.AI_PASSIVE], N.addrSocketType = N.Stream }
  addr : _ <- N.getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
  bracket (openListener addr) N.close $ \lsock -> do
    putStrLn ("Waiting for the Spotify redirect on " <> show port <> " ...")
    bracket (fst <$> N.accept lsock) N.close $ \conn -> do
      request <- NB.recv conn 8192
      let code = extractCode request
      void (NB.send conn httpResponse)
      maybe (ioError (userError "OAuth: no 'code' in redirect")) pure code
  where
    openListener addr = do
      s <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
      N.setSocketOption s N.ReuseAddr 1
      N.bind s (N.addrAddress addr)
      N.listen s 1
      pure s

httpResponse :: ByteString
httpResponse =
  "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nConnection: close\r\n\r\n"
  <> "<html><body style='font-family:sans-serif'><h2>hespot</h2>"
  <> "<p>Authorization received - you can close this tab and return to your terminal.</p>"
  <> "</body></html>"

-- pull the code out of the request line: "GET /login?code=...&state=... HTTP/1.1"
extractCode :: ByteString -> Maybe ByteString
extractCode request =
  case BC.words (BC.takeWhile (/= '\r') (firstLine request)) of
    (_ : path : _) -> lookup "code" (queryPairs path)
    _              -> Nothing
  where
    firstLine = BC.takeWhile (/= '\n')

queryPairs :: ByteString -> [(ByteString, ByteString)]
queryPairs path =
  case BC.break (== '?') path of
    (_, q) | not (BS.null q) -> map pair (BC.split '&' (BS.drop 1 q))
    _                        -> []
  where
    pair kv = case BC.break (== '=') kv of
      (k, v) -> (k, BS.drop 1 v)

-- ---------------------------------------------------------------------------
-- Token exchange
-- ---------------------------------------------------------------------------

data TokenResponse = TokenResponse ByteString (Maybe ByteString) Int

instance FromJSON TokenResponse where
  parseJSON = withObject "token" $ \o ->
    TokenResponse
      <$> (BC.pack <$> o .:  "access_token")
      <*> (fmap BC.pack <$> o .:? "refresh_token")
      <*> (o .:? "expires_in" >>= maybe (pure 3600) pure)

exchangeCode :: OAuthConfig -> ByteString -> ByteString -> ByteString -> IO OAuthToken
exchangeCode cfg redirectUri verifier code =
  postTokenEndpoint
    [ ("grant_type",    "authorization_code")
    , ("code",          code)
    , ("redirect_uri",  redirectUri)
    , ("client_id",     oauthClientId cfg)
    , ("code_verifier", verifier)
    ]

-- | Exchange a refresh token for a fresh access token — no browser needed.
-- Spotify may omit a new refresh token, in which case the old one is kept.
refreshAccessToken :: OAuthConfig -> ByteString -> IO OAuthToken
refreshAccessToken cfg refresh = do
  tok <- postTokenEndpoint
    [ ("grant_type",    "refresh_token")
    , ("refresh_token", refresh)
    , ("client_id",     oauthClientId cfg)
    ]
  pure tok { tokenRefresh = maybe (Just refresh) Just (tokenRefresh tok) }

-- POST an x-www-form-urlencoded body to the token endpoint and parse the result.
postTokenEndpoint :: [(ByteString, ByteString)] -> IO OAuthToken
postTokenEndpoint form = do
  mgr  <- H.newManager tlsManagerSettings
  req0 <- H.parseRequest "https://accounts.spotify.com/api/token"
  let req = H.urlEncodedBody form req0
  resp <- H.httpLbs req mgr
  case eitherDecode (H.responseBody resp) of
    Right (TokenResponse acc refr expiry) -> pure (OAuthToken acc refr expiry)
    Left e ->
      let body = BC.unpack (BL.toStrict (H.responseBody resp))
      in ioError (userError ("OAuth token endpoint failed: " <> e <> " — body: " <> take 200 body))

-- ---------------------------------------------------------------------------
-- base64url (no padding) + SHA-256
-- ---------------------------------------------------------------------------

-- | URL-safe base64 without padding (used for PKCE).
base64url :: ByteString -> ByteString
base64url = BS.pack . go . BS.unpack
  where
    enc :: Int -> Word8
    enc i = BS.index alphabet i
    go (a : b : c : rest) =
      let n = (i a `shiftL` 16) .|. (i b `shiftL` 8) .|. i c
      in enc (sh n 18) : enc (sh n 12) : enc (sh n 6) : enc (n .&. 63) : go rest
    go [a, b] =
      let n = (i a `shiftL` 16) .|. (i b `shiftL` 8)
      in [enc (sh n 18), enc (sh n 12), enc (sh n 6)]
    go [a] =
      let n = i a `shiftL` 16
      in [enc (sh n 18), enc (sh n 12)]
    go [] = []
    i = fromIntegral :: Word8 -> Int
    sh n k = (n `shiftR` k) .&. 63

alphabet :: ByteString
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

-- | SHA-256 digest as raw bytes.
sha256 :: ByteString -> ByteString
sha256 bs = BA.convert (hashWith SHA256 bs)
