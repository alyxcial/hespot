-- | Spotify Connect — first light.
--
-- Registers hespot as a Connect device so it appears in the user's Spotify app:
-- open the dealer WebSocket (@wss://<dealer>/?access_token=…@), read the
-- @Spotify-Connection-Id@ the dealer hands out, then @PUT@ a 'PutStateRequest'
-- protobuf to @/connect-state/v1/devices/<id>@. The socket is kept alive (a ping
-- every 30s) and incoming dealer frames are logged. Playback control is future work.
module Spotify.Connect
  ( connectDevice
  ) where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Exception         (SomeException, try)
import           Control.Monad             (forever)
import           Data.Aeson                (eitherDecodeStrict, withObject, (.!=), (.:?))
import           Data.Aeson.Types          (parseEither)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           Data.Word                 (Word64)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           Network.WebSockets        (receiveData, sendTextData)
import           Wuss                      (runSecureClient)

import           Spotify.Net.ApResolve     (resolveDealer, resolveSpclient)
import qualified Spotify.Proto.Wire        as W

keymasterClientId :: ByteString
keymasterClientId = "65b708073fc0480ea92a077233ca87bd"

-- | Connect to the dealer, register as a device, then stay connected (logging
-- dealer frames) until the socket drops.
connectDevice :: ByteString  -- ^ login5 access token
              -> ByteString  -- ^ client-token
              -> String      -- ^ device id
              -> String      -- ^ device name (shown in the app)
              -> IO ()
connectDevice token clientToken deviceId deviceName = do
  (dealerHost, _) <- resolveDealer
  (spHost, _)     <- resolveSpclient
  putStrLn ("Dealer   : " <> dealerHost)
  putStrLn ("spclient : " <> spHost)
  runSecureClient dealerHost 443 ("/?access_token=" <> BC.unpack token) (app spHost)
  where
    app spHost conn = do
      putStrLn "Connected to the dealer; waiting for a connection-id ..."
      connId <- waitConnId conn
      putStrLn ("Connection-Id: " <> take 28 connId <> "…")
      (ok, code) <- putConnectState token clientToken spHost deviceId deviceName connId
      putStrLn $ if ok
        then "Registered (HTTP " <> show code <> ") — \"" <> deviceName
             <> "\" should now appear in your Spotify app (Connect to a device)."
        else "Registration failed: HTTP " <> show code
      _ <- forkIO (pingLoop conn)
      listen conn

    waitConnId conn = do
      raw <- receiveData conn :: IO ByteString
      maybe (waitConnId conn) (pure . T.unpack) (connIdOf raw)

    pingLoop conn = forever $ do
      threadDelay (30 * 1000000)
      sendTextData conn ("{\"type\":\"ping\"}" :: ByteString)

    listen conn = do
      r <- try (receiveData conn :: IO ByteString) :: IO (Either SomeException ByteString)
      case r of
        Left _    -> putStrLn "Dealer connection closed."
        Right msg -> logFrame msg >> listen conn

-- | Pull @headers["Spotify-Connection-Id"]@ out of a dealer JSON frame.
connIdOf :: ByteString -> Maybe Text
connIdOf raw = either (const Nothing) id (eitherDecodeStrict raw >>= parseEither p)
  where
    p = withObject "frame" $ \o -> do
      mh <- o .:? "headers"
      maybe (pure Nothing) (.:? "Spotify-Connection-Id") mh

-- | One-line log of a dealer frame (its type and uri).
logFrame :: ByteString -> IO ()
logFrame raw = case eitherDecodeStrict raw >>= parseEither p of
    Right (ty, uri)
      | ty == "ping" || ty == "pong" -> pure ()
      | otherwise -> putStrLn ("  dealer ◂ " <> T.unpack ty
                               <> if T.null uri then "" else "  " <> T.unpack uri)
    Left _ -> pure ()
  where
    p = withObject "frame" $ \o -> do
      ty  <- o .:? "type" .!= ""
      uri <- o .:? "uri"  .!= ""
      pure (ty :: Text, uri :: Text)

putConnectState :: ByteString -> ByteString -> String -> String -> String -> String
                -> IO (Bool, Int)
putConnectState token clientToken spHost deviceId deviceName connId = do
  now <- getPOSIXTime
  let nowMs = round (now * 1000) :: Word64
  mgr  <- newManager tlsManagerSettings
  req0 <- parseRequest ("https://" <> spHost <> "/connect-state/v1/devices/" <> deviceId)
  let req = req0
        { method = "PUT"
        , requestHeaders =
            [ ("Authorization", "Bearer " <> token)
            , ("client-token", clientToken)
            , ("X-Spotify-Connection-Id", BC.pack connId)
            , ("Content-Type", "application/x-protobuf")
            , ("Accept", "application/x-protobuf") ]
        , requestBody = RequestBodyBS (putStateRequest deviceId deviceName nowMs) }
  resp <- httpLbs req mgr
  let code = statusCode (responseStatus resp)
  pure (code >= 200 && code < 300, code)

-- | Build a minimal @PutStateRequest@ (spotify.connectstate) describing the device.
putStateRequest :: String -> String -> Word64 -> ByteString
putStateRequest deviceId deviceName nowMs = W.encode
  [ W.message 2 device       -- device
  , W.uint64 3 2             -- member_type = CONNECT_STATE
  , W.uint64 5 3             -- put_state_reason = NEW_DEVICE
  , W.uint64 12 nowMs        -- client_side_timestamp
  ]
  where
    device = [ W.message 1 deviceInfo ]
    deviceInfo =
      [ W.uint64 1 1                            -- can_play
      , W.uint64 2 65535                        -- volume
      , W.string 3 (BC.pack deviceName)         -- name
      , W.message 4 capabilities                -- capabilities
      , W.string 6 "hespot 0.1.0"               -- device_software_version
      , W.uint64 7 1                            -- device_type = COMPUTER
      , W.string 10 (BC.pack deviceId)          -- device_id
      , W.string 13 keymasterClientId           -- client_id
      , W.string 14 "hespot"                    -- brand
      ]
    capabilities =
      [ W.uint64 2 1                            -- can_be_player
      , W.uint64 7 1                            -- is_observable
      , W.uint64 8 64                           -- volume_steps
      , W.string 9 "audio/track"                -- supported_types (repeated)
      , W.string 9 "audio/episode"
      , W.uint64 16 1                           -- is_controllable
      , W.uint64 19 1                           -- supports_transfer_command
      , W.uint64 20 1                           -- supports_command_request
      ]
