-- | Obtain a Spotify @client-token@ from @clienttoken.spotify.com@.
--
-- The endpoint answers the first (client-data) request with a hash-cash
-- challenge; we solve it (see "Spotify.Crypto.Hashcash") and resubmit to get the
-- granted token. The client-token is required, alongside an access token, by the
-- modern spclient / partner endpoints. Protobuf bodies are built with the
-- hand-rolled "Spotify.Proto.Wire" codec.
module Spotify.Auth.ClientToken
  ( getClientToken
  ) where

import           Data.Bits                 (shiftR, (.&.))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (toUpper)
import           Data.Word                 (Word8)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)

import           Spotify.Crypto.Hashcash   (solveHashcash)
import qualified Spotify.Proto.Wire        as W

-- | Get a client-token for the given client id and device id.
getClientToken :: ByteString -> ByteString -> IO (Either String ByteString)
getClientToken clientId deviceId = do
  mgr <- newManager tlsManagerSettings
  r1  <- post mgr (clientDataRequest clientId deviceId)
  case r1 >>= parseResponse of
    Left e              -> pure (Left e)
    Right (Granted tok) -> pure (Right tok)
    Right (Challenge state prefixHex len) -> do
      let suffix = solveHashcash "" (unhex prefixHex) len
      r2 <- post mgr (challengeAnswer state (hexUpper suffix))
      pure $ case r2 >>= parseResponse of
        Right (Granted tok) -> Right tok
        Right _             -> Left "client-token: no granted token after solving challenge"
        Left e              -> Left e

post :: Manager -> ByteString -> IO (Either String ByteString)
post mgr body = do
  req0 <- parseRequest "https://clienttoken.spotify.com/v1/clienttoken"
  let req = req0
        { method = "POST"
        , requestHeaders = [ ("Content-Type", "application/x-protobuf")
                           , ("Accept", "application/x-protobuf") ]
        , requestBody = RequestBodyBS body }
  resp <- httpLbs req mgr
  let code = statusCode (responseStatus resp)
  pure $ if code >= 200 && code < 300
           then Right (BL.toStrict (responseBody resp))
           else Left ("client-token: HTTP " <> show code)

-- ---------------------------------------------------------------------------
-- Protobuf messages (spotify.clienttoken.http.v0)
-- ---------------------------------------------------------------------------

clientDataRequest :: ByteString -> ByteString -> ByteString
clientDataRequest clientId deviceId = W.encode
  [ W.uint64 1 1                                  -- request_type = REQUEST_CLIENT_DATA_REQUEST
  , W.message 2                                   -- client_data (ClientDataRequest)
      [ W.string 1 "1.2.52.442.gace0ef26"         --   client_version
      , W.string 2 clientId                       --   client_id
      , W.message 3                               --   connectivity_sdk_data
          [ W.message 1                           --     platform_specific_data
              [ W.message 5                       --       desktop_linux (NativeDesktopLinuxData)
                  [ W.string 1 "Linux"            --         system_name
                  , W.string 2 "6.0.0"            --         system_release
                  , W.string 3 "#1 SMP"           --         system_version
                  , W.string 4 "x86_64" ]         --         hardware
              ]
          , W.string 2 deviceId                   --     device_id
          ]
      ]
  ]

challengeAnswer :: ByteString -> ByteString -> ByteString
challengeAnswer state suffixHex = W.encode
  [ W.uint64 1 2                                  -- request_type = REQUEST_CHALLENGE_ANSWERS_REQUEST
  , W.message 3                                   -- challenge_answers (ChallengeAnswersRequest)
      [ W.string 1 state                          --   state
      , W.message 2                               --   answers[0] (ChallengeAnswer)
          [ W.uint64 1 3                          --     ChallengeType = CHALLENGE_HASH_CASH
          , W.message 4 [ W.string 1 suffixHex ]  --     hash_cash { suffix }
          ]
      ]
  ]

data CTResponse
  = Granted ByteString
  | Challenge ByteString ByteString Int   -- state, prefix (hex), length

parseResponse :: ByteString -> Either String CTResponse
parseResponse bs = do
  m <- W.decode bs
  case maybe 0 fromIntegral (W.getVarint 1 m) :: Int of
    1 -> do
      gt  <- need "granted_token" =<< W.getMessage 2 m
      tok <- need "token"         (W.getString 1 gt)
      Right (Granted tok)
    2 -> do
      ch <- need "challenges" =<< W.getMessage 3 m
      let state = maybe "" id (W.getString 1 ch)
      first <- case [ s | (2, W.VBytes s) <- ch ] of
                 (c : _) -> W.decode c
                 []      -> Left "client-token: empty challenge list"
      hcp <- need "hashcash params" =<< W.getMessage 4 first
      Right (Challenge state
                        (maybe "" id (W.getString 2 hcp))
                        (maybe 0 fromIntegral (W.getVarint 1 hcp)))
    other -> Left ("client-token: unexpected response type " <> show other)
  where
    need what = maybe (Left ("client-token: missing " <> what)) Right

-- ---------------------------------------------------------------------------

hexUpper :: ByteString -> ByteString
hexUpper = BC.pack . concatMap byte . BS.unpack
  where
    byte b = [hd (b `shiftR` 4), hd (b .&. 0xf)]
    hd n | n < 10    = toEnum (fromIntegral n + fromEnum '0')
         | otherwise = toUpper (toEnum (fromIntegral n - 10 + fromEnum 'a'))

unhex :: ByteString -> ByteString
unhex = BS.pack . go . BS.unpack
  where
    go (a : b : rest) = (hv a * 16 + hv b) : go rest
    go _              = []
    hv :: Word8 -> Word8
    hv c | c >= 0x30 && c <= 0x39 = c - 0x30
         | c >= 0x61 && c <= 0x66 = c - 0x61 + 10
         | c >= 0x41 && c <= 0x46 = c - 0x41 + 10
         | otherwise              = 0
