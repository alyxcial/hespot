-- | Persist and reload the reusable credentials Spotify hands back at login,
-- so subsequent runs can skip the interactive OAuth dance.
module Spotify.Auth.Cache
  ( defaultCachePath
  , saveCredentials
  , loadCredentials
  , defaultTokenPath
  , saveToken
  , loadToken
  ) where

import           Control.Exception            (SomeException, try)
import           Data.Aeson                   (Value, decode, encode, object,
                                               withObject, (.:), (.:?), (.=))
import           Data.Aeson.Types             (parseMaybe)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BC
import qualified Data.ByteString.Lazy         as BL
import           Numeric                      (readHex, showHex)
import           System.Directory             (createDirectoryIfMissing, doesFileExist,
                                               getHomeDirectory)
import           System.FilePath              (takeDirectory, (</>))

import           Data.ByteString              (ByteString)

import           Spotify.Proto.Authentication (authTypeCode, authTypeFromCode)
import           Spotify.Session              (Credentials (..))

-- | @~/.cache/hespot/credentials.json@
defaultCachePath :: IO FilePath
defaultCachePath = do
  home <- getHomeDirectory
  pure (home </> ".cache" </> "hespot" </> "credentials.json")

-- | Write credentials to disk (creating the directory if needed).
saveCredentials :: FilePath -> Credentials -> IO ()
saveCredentials path creds = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encode (toValue creds))

-- | Load credentials, returning 'Nothing' if the file is missing or unreadable.
loadCredentials :: FilePath -> IO (Maybe Credentials)
loadCredentials path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      r <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
      pure $ case r of
        Left _   -> Nothing
        Right bs -> decode bs >>= fromValue

toValue :: Credentials -> Value
toValue c = object
  [ "username"  .= fmap BC.unpack (credUsername c)
  , "auth_type" .= (fromIntegral (authTypeCode (credAuthType c)) :: Int)
  , "auth_data" .= toHex (credAuthData c)
  ]

fromValue :: Value -> Maybe Credentials
fromValue = parseMaybe $ withObject "credentials" $ \o -> do
  mUser <- o .:? "username"
  ty    <- o .:  "auth_type"
  hx    <- o .:  "auth_data"
  pure Credentials
    { credUsername = fmap BC.pack mUser
    , credAuthType = authTypeFromCode ty
    , credAuthData = fromHex hx
    }

-- | @~/.cache/hespot/token.json@ — the cached OAuth access + refresh token.
defaultTokenPath :: IO FilePath
defaultTokenPath = do
  home <- getHomeDirectory
  pure (home </> ".cache" </> "hespot" </> "token.json")

-- | Persist an OAuth token: access token, refresh token, and absolute expiry
-- (unix seconds).
saveToken :: FilePath -> ByteString -> ByteString -> Integer -> IO ()
saveToken path acc refr expiresAt = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path $ encode $ object
    [ "access_token"  .= BC.unpack acc
    , "refresh_token" .= BC.unpack refr
    , "expires_at"    .= expiresAt
    ]

-- | Load a cached OAuth token as @(access, refresh, expiresAt)@.
loadToken :: FilePath -> IO (Maybe (ByteString, ByteString, Integer))
loadToken path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      r <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
      pure $ case r of
        Left _   -> Nothing
        Right bs -> decode bs >>= parseMaybe tokenP
  where
    tokenP = withObject "token" $ \o -> do
      acc   <- o .: "access_token"
      refr  <- o .: "refresh_token"
      expAt <- o .: "expires_at"
      pure (BC.pack acc, BC.pack refr, expAt)

toHex :: BS.ByteString -> String
toHex = concatMap byte . BS.unpack
  where byte b = let h = showHex b "" in if length h == 1 then '0' : h else h

fromHex :: String -> BS.ByteString
fromHex = BS.pack . go
  where
    go (a : b : rest) = case readHex [a, b] of
      [(n, _)] -> fromIntegral (n :: Int) : go rest
      _        -> []
    go _ = []
