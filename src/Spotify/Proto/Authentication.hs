-- | The encrypted login messages: our @ClientResponseEncrypted@ and the
-- server's @APWelcome@ / @APLoginFailed@.
module Spotify.Proto.Authentication
  ( AuthType (..)
  , authTypeCode
  , authTypeFromCode
  , buildClientResponseEncrypted
  , APWelcome (..)
  , parseAPWelcome
  , parseLoginFailedCode
  ) where

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Word          (Word64)
import qualified Spotify.Proto.Wire as W

-- | The credential kinds we support.
data AuthType
  = AuthUserPass            -- ^ AUTHENTICATION_USER_PASS (legacy)
  | AuthStoredCredentials   -- ^ AUTHENTICATION_STORED_SPOTIFY_CREDENTIALS
  | AuthSpotifyToken        -- ^ AUTHENTICATION_SPOTIFY_TOKEN (OAuth access token)
  deriving (Eq, Show)

authTypeCode :: AuthType -> Word64
authTypeCode AuthUserPass          = 0
authTypeCode AuthStoredCredentials = 1
authTypeCode AuthSpotifyToken      = 3

authTypeFromCode :: Int -> AuthType
authTypeFromCode 0 = AuthUserPass
authTypeFromCode 1 = AuthStoredCredentials
authTypeFromCode _ = AuthSpotifyToken

-- | Build a @ClientResponseEncrypted@ login packet body.
buildClientResponseEncrypted
  :: Maybe ByteString  -- ^ username (omitted for token logins)
  -> AuthType          -- ^ credential kind
  -> ByteString        -- ^ auth_data (token bytes or a stored blob)
  -> ByteString        -- ^ device id
  -> ByteString
buildClientResponseEncrypted mUser authType authData deviceId = W.encode
  [ W.message 0xa $                              -- login_credentials
      maybe [] (\u -> [W.string 0xa u]) mUser ++ --   username?
      [ W.uint64 0x14 (authTypeCode authType)   --   typ
      , W.bytes  0x1e authData                  --   auth_data
      ]
  , W.message 0x32                               -- system_info
      [ W.uint64 0xa  2                          --   cpu_family = CPU_X86_64
      , W.uint64 0x3c 5                          --   os = OS_LINUX
      , W.string 0x5a "hespot-0.1.0"             --   system_information_string
      , W.string 0x64 deviceId                   --   device_id
      ]
  , W.string 0x46 "hespot 0.1.0"                 -- version_string
  ]

-- | The successful login reply.
data APWelcome = APWelcome
  { welcomeUsername     :: ByteString  -- ^ canonical username
  , welcomeReusableType :: Int         -- ^ reusable_auth_credentials_type
  , welcomeReusableData :: ByteString  -- ^ reusable_auth_credentials (cache this!)
  } deriving (Eq, Show)

parseAPWelcome :: ByteString -> Either String APWelcome
parseAPWelcome bs = do
  m    <- W.decode bs
  user <- maybe (Left "APWelcome: no canonical_username") Right (W.getString 0xa m)
  pure APWelcome
    { welcomeUsername     = user
    , welcomeReusableType = maybe 1 fromIntegral (W.getVarint 0x1e m)
    , welcomeReusableData = maybe BS.empty id    (W.getBytes  0x28 m)
    }

-- | Pull the error code out of an @APLoginFailed@ packet.
parseLoginFailedCode :: ByteString -> Either String Int
parseLoginFailedCode bs = do
  m <- W.decode bs
  pure (maybe (-1) fromIntegral (W.getVarint 0xa m))
