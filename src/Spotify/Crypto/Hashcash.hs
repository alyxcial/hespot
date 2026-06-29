{-# LANGUAGE BangPatterns #-}

-- | Spotify's hash-cash proof-of-work.
--
-- The client-token (and login5) endpoints can answer a request with a hash-cash
-- challenge: a @prefix@ and a difficulty @length@. The answer is a 16-byte
-- @suffix@ such that the 64-bit big-endian integer at bytes 12..20 of
-- @SHA1(prefix ‖ suffix)@ has at least @length@ trailing zero bits. The suffix
-- is built from a counter seeded by @SHA1(ctx)@.
--
-- This is the reusable proof-of-work core shared by the modern token paths.
module Spotify.Crypto.Hashcash
  ( solveHashcash
  , hashcashTrailingZeros
  ) where

import           Crypto.Hash            (hashWith)
import           Crypto.Hash.Algorithms (SHA1 (..))
import qualified Data.ByteArray        as BA
import           Data.Bits        (countTrailingZeros, shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as BS
import           Data.Int         (Int64)
import           Data.Word        (Word64)

sha1 :: ByteString -> ByteString
sha1 = BA.convert . hashWith SHA1

-- | Solve a hash-cash challenge, returning the 16-byte suffix. @ctx@ is empty
-- for the client-token flow.
solveHashcash
  :: ByteString  -- ^ context (usually empty)
  -> ByteString  -- ^ challenge prefix
  -> Int         -- ^ required trailing zero bits
  -> ByteString  -- ^ 16-byte suffix answer
solveHashcash ctx prefix len = go 0
  where
    target = beI64 (sha1 ctx) 12
    go !counter =
      let suffix = i64be (target + counter) <> i64be counter
      in if hashcashTrailingZeros prefix suffix >= len
           then suffix
           else go (counter + 1)

-- | Trailing zero bits of the 64-bit value at bytes 12..20 of
-- @SHA1(prefix ‖ suffix)@ — the quantity the difficulty is measured against.
hashcashTrailingZeros :: ByteString -> ByteString -> Int
hashcashTrailingZeros prefix suffix =
  countTrailingZeros (beI64 (sha1 (prefix <> suffix)) 12)

-- read a signed 64-bit big-endian integer at an offset
beI64 :: ByteString -> Int -> Int64
beI64 bs off =
  fromIntegral (BS.foldl' (\a w -> a `shiftL` 8 .|. fromIntegral w) (0 :: Word64)
                          (BS.take 8 (BS.drop off bs)))

i64be :: Int64 -> ByteString
i64be n = BS.pack [ fromIntegral ((n `shiftR` (8 * (7 - i))) .&. 0xff) | i <- [0 .. 7] ]
