-- | Diffie–Hellman key agreement over the 768-bit MODP group Spotify uses.
--
-- The local private key is 95 random bytes interpreted little-endian; the public
-- key and the shared secret are exchanged as big-endian byte strings (with no
-- leading zero padding, matching the reference).
module Spotify.Crypto.DiffieHellman
  ( DhKeys (..)
  , generate
  , publicKeyBytes
  , sharedSecret
  ) where

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           System.Entropy  (getEntropy)
import           Data.Word       (Word8)

-- | A freshly generated local key pair.
data DhKeys = DhKeys
  { dhPrivate :: !Integer
  , dhPublic  :: !Integer
  }

dhGenerator :: Integer
dhGenerator = 2

-- The well-known 768-bit MODP prime (Oakley/RFC 2409 group 1).
dhPrime :: Integer
dhPrime = os2ipBE $ BS.pack
  [ 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xc9,0x0f,0xda,0xa2,0x21,0x68,0xc2,0x34
  , 0xc4,0xc6,0x62,0x8b,0x80,0xdc,0x1c,0xd1,0x29,0x02,0x4e,0x08,0x8a,0x67,0xcc,0x74
  , 0x02,0x0b,0xbe,0xa6,0x3b,0x13,0x9b,0x22,0x51,0x4a,0x08,0x79,0x8e,0x34,0x04,0xdd
  , 0xef,0x95,0x19,0xb3,0xcd,0x3a,0x43,0x1b,0x30,0x2b,0x0a,0x6d,0xf2,0x5f,0x14,0x37
  , 0x4f,0xe1,0x35,0x6d,0x6d,0x51,0xc2,0x45,0xe4,0x85,0xb5,0x76,0x62,0x5e,0x7e,0xc6
  , 0xf4,0x4c,0x42,0xe9,0xa6,0x3a,0x36,0x20,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
  ]

-- | Generate a local key pair using system entropy.
generate :: IO DhKeys
generate = do
  bytes <- getEntropy 95
  let priv = os2ipLE bytes
      pub  = expModp dhGenerator priv
  pure (DhKeys priv pub)

-- | The local public key as big-endian bytes (this is @gc@ in the handshake).
publicKeyBytes :: DhKeys -> ByteString
publicKeyBytes = i2ospBE . dhPublic

-- | Compute the shared secret from the remote public key (@gs@) bytes.
sharedSecret :: DhKeys -> ByteString -> ByteString
sharedSecret keys remote = i2ospBE (expModp (os2ipBE remote) (dhPrivate keys))

-- modular exponentiation in the DH group (square-and-multiply)
expModp :: Integer -> Integer -> Integer
expModp base0 exp0 = go (base0 `mod` dhPrime) exp0 1
  where
    go _ 0 !acc = acc
    go b e !acc =
      let acc' = if testBit e 0 then (acc * b) `mod` dhPrime else acc
      in go ((b * b) `mod` dhPrime) (e `shiftR` 1) acc'

-- ---------------------------------------------------------------------------
-- byte <-> integer helpers
-- ---------------------------------------------------------------------------

os2ipBE :: ByteString -> Integer
os2ipBE = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0

os2ipLE :: ByteString -> Integer
os2ipLE = os2ipBE . BS.reverse

-- minimal big-endian encoding (no leading zero byte for positive values)
i2ospBE :: Integer -> ByteString
i2ospBE n
  | n <= 0    = BS.singleton 0
  | otherwise = BS.pack (reverse (unfold n))
  where
    unfold :: Integer -> [Word8]
    unfold 0 = []
    unfold k = fromIntegral (k .&. 0xff) : unfold (k `shiftR` 8)
