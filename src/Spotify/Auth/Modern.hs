-- | Mint the modern token pair (login5 access token + client-token) from stored
-- credentials — the auth the spclient / CDN / partner endpoints expect.
module Spotify.Auth.Modern
  ( modernTokens
  ) where

import           Data.ByteString          (ByteString)

import           Spotify.Auth.ClientToken (getClientToken)
import           Spotify.Auth.Login5      (login5Token)
import           Spotify.Auth.OAuth       (keymasterClientId)
import           Spotify.Session          (Credentials (..))

-- | @(login5 access token, client-token)@ for the given device id and stored
-- credentials. Solves the client-token hash-cash and runs the login5 exchange.
modernTokens :: ByteString -> Credentials -> IO (Either String (ByteString, ByteString))
modernTokens deviceId creds = case credUsername creds of
  Nothing   -> pure (Left "credentials have no username")
  Just user -> do
    ect <- getClientToken keymasterClientId deviceId
    case ect of
      Left e   -> pure (Left ("client-token: " <> e))
      Right ct -> do
        etok <- login5Token keymasterClientId deviceId ct user (credAuthData creds)
        pure (either (Left . ("login5: " <>)) (\t -> Right (t, ct)) etok)
