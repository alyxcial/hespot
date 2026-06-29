-- | Spotify audio is AES-128 in CTR mode with a fixed IV; the key is the
-- per-file key fetched over the access-point channel. Decrypting is just XORing
-- the keystream over the ciphertext.
module Spotify.Audio.Decrypt
  ( audioDecrypt
  , aes128ctr
  ) where

import           Crypto.Cipher.AES    (AES128)
import           Crypto.Cipher.Types  (IV, cipherInit, ctrCombine, makeIV)
import           Crypto.Error         (CryptoFailable (..))
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS

-- the fixed IV Spotify uses for audio files
audioIV :: ByteString
audioIV = BS.pack
  [ 0x72,0xe0,0x67,0xfb,0xdd,0xcb,0xcf,0x77,0xeb,0xe8,0xbc,0x64,0x3f,0x63,0x0d,0x93 ]

-- | AES-128-CTR over @input@ with a 16-byte @key@ and 16-byte @iv@ (counter).
aes128ctr :: ByteString -> ByteString -> ByteString -> Either String ByteString
aes128ctr key iv input =
  case cipherInit key :: CryptoFailable AES128 of
    CryptoFailed e -> Left ("aes: " <> show e)
    CryptoPassed c -> case makeIV iv :: Maybe (IV AES128) of
      Nothing  -> Left "aes: bad IV length"
      Just iv' -> Right (ctrCombine (c :: AES128) iv' input)

-- | Decrypt a whole Spotify audio file (starting at block 0) with its key.
audioDecrypt :: ByteString -> ByteString -> Either String ByteString
audioDecrypt key = aes128ctr key audioIV
