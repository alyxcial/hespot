-- | The login5 token path: exchange stored credentials for a modern access token
-- at @login5.spotify.com@.
--
-- This is the access token the spclient and partner endpoints accept (the
-- keymaster/OAuth token does not). The request carries a client-token header and
-- may be answered with a hash-cash challenge (seeded by the @login_context@),
-- which we solve and resubmit. Protobufs are built with the wire codec.
module Spotify.Auth.Login5
  ( login5Token
  ) where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as BL
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)

import           Spotify.Crypto.Hashcash   (solveHashcash)
import qualified Spotify.Proto.Wire        as W

-- | Get a login5 access token from stored credentials (username + blob), given a
-- client-token. Returns the access-token string bytes.
login5Token
  :: ByteString  -- ^ client id
  -> ByteString  -- ^ device id
  -> ByteString  -- ^ client-token
  -> ByteString  -- ^ username
  -> ByteString  -- ^ stored credential blob
  -> IO (Either String ByteString)
login5Token clientId deviceId clientToken username blob = do
  mgr <- newManager tlsManagerSettings
  go mgr (loginRequest clientId deviceId username blob Nothing []) (4 :: Int)
  where
    go _ _ 0 = pure (Left "login5: too many challenge rounds")
    go mgr body n = do
      r <- post mgr clientToken body
      case r >>= parseResponse of
        Left e                       -> pure (Left e)
        Right (Ok tok)               -> pure (Right tok)
        Right (Err code)             -> pure (Left ("login5 error " <> show code))
        Right (Chall ctx prefix len) ->
          let suffix = solveHashcash ctx prefix len
          in go mgr (loginRequest clientId deviceId username blob (Just ctx) [suffix]) (n - 1)

post :: Manager -> ByteString -> ByteString -> IO (Either String ByteString)
post mgr clientToken body = do
  req0 <- parseRequest "https://login5.spotify.com/v3/login"
  let req = req0
        { method = "POST"
        , requestHeaders = [ ("Content-Type", "application/x-protobuf")
                           , ("Accept", "application/x-protobuf")
                           , ("client-token", clientToken) ]
        , requestBody = RequestBodyBS body }
  resp <- httpLbs req mgr
  let code = statusCode (responseStatus resp)
  pure $ if code >= 200 && code < 300
           then Right (BL.toStrict (responseBody resp))
           else Left ("login5: HTTP " <> show code)

-- ---------------------------------------------------------------------------
-- Protobuf (spotify.login5.v3)
-- ---------------------------------------------------------------------------

loginRequest :: ByteString -> ByteString -> ByteString -> ByteString
             -> Maybe ByteString -> [ByteString] -> ByteString
loginRequest clientId deviceId username blob mCtx suffixes = W.encode $
     [ W.message 1 [ W.string 1 clientId, W.string 2 deviceId ] ]   -- client_info
  ++ maybe [] (\ctx -> [ W.bytes 2 ctx ]) mCtx                      -- login_context
  ++ [ W.message 3 [ solution s | s <- suffixes ] | not (null suffixes) ]  -- challenge_solutions
  ++ [ W.message 100 [ W.string 1 username, W.bytes 2 blob ] ]      -- stored_credential (field 100)
  where
    solution suffix = W.message 1                 -- ChallengeSolution
      [ W.message 1                               --   hashcash (HashcashSolution)
          [ W.bytes 1 suffix                      --     suffix
          , W.message 2 [ W.uint64 1 1 ]          --     duration { seconds = 1 }
          ] ]

data L5 = Ok ByteString | Err Int | Chall ByteString ByteString Int

parseResponse :: ByteString -> Either String L5
parseResponse bs = do
  m <- W.decode bs
  let loginCtx = maybe "" id (W.getBytes 5 m)
  resolve m loginCtx
  where
    getMsg fn msg = either (const Nothing) id (W.getMessage fn msg)
    resolve m ctx
      | Just ok  <- getMsg 1 m       = maybe (Left "login5: ok without access_token")
                                             (Right . Ok) (W.getBytes 2 ok)
      | Just ch  <- getMsg 3 m       = challenge ctx ch
      | Just err <- W.getVarint 2 m  = Right (Err (fromIntegral err))
      | otherwise                    = Left "login5: unexpected response"
    challenge ctx ch = case [ s | (1, W.VBytes s) <- ch ] of
      (c : _) -> do
        cm <- W.decode c
        case getMsg 1 cm of                                -- hashcash (field 1)
          Just hc -> Right (Chall ctx
                                  (maybe "" id  (W.getBytes  1 hc))   -- prefix
                                  (maybe 0 fromIntegral (W.getVarint 2 hc)))  -- length
          Nothing -> Left "login5: challenge is not hash-cash"
      [] -> Left "login5: empty challenge list"
