-- | Fetch a track's encrypted audio over the CDN (fast HTTPS) instead of the
-- legacy access-point channel.
--
-- @storage-resolve@ (an spclient endpoint, needing a login5 access token and a
-- client-token) hands back one or more pre-signed CDN URLs for a file; we then
-- download the file straight from the CDN over HTTPS. The bytes are the same
-- AES-CTR-encrypted Ogg as the AP channel returns, so decrypt them as usual —
-- this is just a much faster transport.
module Spotify.Audio.Cdn
  ( fetchEncryptedFileCdn
  ) where

import           Control.Exception         (SomeException, try)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BL
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)

import           Spotify.Id                (FileId, fileIdHex)
import qualified Spotify.Proto.Wire        as W

-- | Resolve a file to a CDN URL and download it. Returns the encrypted bytes.
fetchEncryptedFileCdn
  :: ByteString  -- ^ login5 access token
  -> ByteString  -- ^ client-token
  -> String      -- ^ spclient host
  -> FileId
  -> IO (Either String ByteString)
fetchEncryptedFileCdn token clientToken spHost fileId = do
  r <- try go :: IO (Either SomeException (Either String ByteString))
  pure (either (Left . ("cdn: " <>) . show) id r)
  where
    go = do
      mgr  <- newManager tlsManagerSettings
      req0 <- parseRequest ("https://" <> spHost <> "/storage-resolve/files/audio/interactive/"
                            <> BC.unpack (fileIdHex fileId))
      let req = req0 { requestHeaders = [ ("Authorization", "Bearer " <> token)
                                        , ("client-token", clientToken) ] }
      resp <- httpLbs req mgr
      let code = statusCode (responseStatus resp)
      if code /= 200
        then pure (Left ("storage-resolve: HTTP " <> show code))
        else case storageUrls (BL.toStrict (responseBody resp)) of
          Left e        -> pure (Left e)
          Right []      -> pure (Left "storage-resolve: no CDN url (restricted track?)")
          Right (u : _) -> do
            cdnReq <- parseRequest u
            r2 <- httpLbs cdnReq mgr
            let c2 = statusCode (responseStatus r2)
            pure $ if c2 >= 200 && c2 < 300
                     then Right (BL.toStrict (responseBody r2))
                     else Left ("cdn download: HTTP " <> show c2)

-- the repeated `cdnurl` strings (field 2) of a StorageResolveResponse
storageUrls :: ByteString -> Either String [String]
storageUrls bs = do
  m <- W.decode bs
  pure [ BC.unpack u | (2, W.VBytes u) <- m ]
