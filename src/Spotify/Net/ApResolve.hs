-- | Resolve Spotify endpoints (access points, spclient) over HTTPS.
--
-- Spotify publishes lists of hosts per service at @apresolve.spotify.com@. Each
-- entry is @host:port@; the access-point protocol itself is raw (non-TLS), while
-- spclient is HTTPS — we only use HTTPS here to discover where to connect.
module Spotify.Net.ApResolve
  ( resolveAccessPoint
  , resolveSpclient
  , resolveDealer
  ) where

import           Control.Exception       (SomeException, try)
import           Data.Aeson              (eitherDecode)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as M
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Text.Read               (readMaybe)

-- | A raw (non-TLS) access point @(host, port)@.
resolveAccessPoint :: IO (String, Int)
resolveAccessPoint = resolveType "accesspoint" ("ap.spotify.com", 443)

-- | An spclient @(host, port)@ for the modern HTTPS API surface.
resolveSpclient :: IO (String, Int)
resolveSpclient = resolveType "spclient" ("spclient.wg.spotify.com", 443)

-- | A dealer @(host, port)@ — the WebSocket push channel used by Spotify Connect.
resolveDealer :: IO (String, Int)
resolveDealer = resolveType "dealer" ("gae2-dealer.spotify.com", 443)

resolveType :: String -> (String, Int) -> IO (String, Int)
resolveType typ fallback = do
  r <- try (fetch typ) :: IO (Either SomeException [String])
  pure $ case r of
    Right (h : _) | Just hp <- splitHostPort h -> hp
    _                                          -> fallback
  where
    fetch t = do
      mgr  <- newManager tlsManagerSettings
      req  <- parseRequest ("https://apresolve.spotify.com/?type=" <> t)
      resp <- httpLbs req mgr
      case eitherDecode (responseBody resp) :: Either String (Map String [String]) of
        Right m -> pure (M.findWithDefault [] t m)
        Left  e -> ioError (userError ("apresolve: " <> e))

splitHostPort :: String -> Maybe (String, Int)
splitHostPort s = case break (== ':') s of
  (h, ':' : p) | not (null h), Just n <- readMaybe p -> Just (h, n)
  _                                                  -> Nothing
