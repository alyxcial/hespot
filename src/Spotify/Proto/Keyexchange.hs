-- | The handshake (key-exchange) messages, built and parsed with
-- "Spotify.Proto.Wire". Field numbers are written in hex to mirror the
-- reference @keyexchange.proto@ exactly.
module Spotify.Proto.Keyexchange
  ( spotifyVersion
  , buildClientHello
  , buildClientResponsePlaintext
  , APResponse (..)
  , parseAPResponse
  ) where

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Word          (Word64)
import qualified Spotify.Proto.Wire as W

-- | The Spotify protocol version we advertise.
spotifyVersion :: Word64
spotifyVersion = 124200290

-- enum values we use
productClient, platformLinuxX86_64, cryptoSuiteShannon :: Word64
productClient       = 0   -- PRODUCT_CLIENT
platformLinuxX86_64 = 8   -- PLATFORM_LINUX_X86_64
cryptoSuiteShannon  = 0   -- CRYPTO_SUITE_SHANNON

-- | Build a @ClientHello@ given our DH public key @gc@ and a 16-byte nonce.
buildClientHello :: ByteString -> ByteString -> ByteString
buildClientHello gc clientNonce = W.encode
  [ W.message 0xa                                -- build_info
      [ W.uint64 0xa  productClient              --   product
      , W.uint64 0x14 0                          --   product_flags = PRODUCT_FLAG_NONE
      , W.uint64 0x1e platformLinuxX86_64        --   platform
      , W.uint64 0x28 spotifyVersion             --   version
      ]
  , W.uint64 0x1e cryptoSuiteShannon             -- cryptosuites_supported
  , W.message 0x32                               -- login_crypto_hello
      [ W.message 0xa                            --   diffie_hellman
          [ W.bytes  0xa  gc                     --     gc
          , W.uint32 0x14 1                      --     server_keys_known
          ]
      ]
  , W.bytes 0x3c clientNonce                     -- client_nonce
  , W.bytes 0x46 (BS.singleton 0x1e)             -- padding
  ]

-- | Build a @ClientResponsePlaintext@ carrying the derived login challenge.
buildClientResponsePlaintext :: ByteString -> ByteString
buildClientResponsePlaintext challenge = W.encode
  [ W.message 0xa                                -- login_crypto_response
      [ W.message 0xa [ W.bytes 0xa challenge ]  --   diffie_hellman { hmac }
      ]
  , W.message 0x14 []                            -- pow_response {}
  , W.message 0x1e []                            -- crypto_response {}
  ]

-- | The server's reply to @ClientHello@.
data APResponse
  = APChallenge ByteString ByteString  -- ^ @gs@, @gs_signature@
  | APFailed Int                       -- ^ login-failed error code
  deriving (Eq, Show)

-- | Parse an @APResponseMessage@.
parseAPResponse :: ByteString -> Either String APResponse
parseAPResponse bs = do
  m <- W.decode bs
  W.getMessage 0xa m >>= \case          -- challenge
    Just ch -> do
      lcc <- need "login_crypto_challenge" =<< W.getMessage 0xa ch
      dh  <- need "diffie_hellman"        =<< W.getMessage 0xa lcc
      gs  <- need "gs"                    (W.getBytes 0xa  dh)
      sig <- need "gs_signature"          (W.getBytes 0x1e dh)
      Right (APChallenge gs sig)
    Nothing ->
      W.getMessage 0x1e m >>= \case       -- login_failed
        Just f  -> Right (APFailed (maybe (-1) fromIntegral (W.getVarint 0xa f)))
        Nothing -> Left "APResponse: neither challenge nor login_failed present"
  where
    need what = maybe (Left ("APResponse: missing " <> what)) Right
