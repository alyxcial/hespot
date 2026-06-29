-- | A tiny, dependency-free Protocol Buffers wire codec.
--
-- This implements just enough of the protobuf wire format
-- (<https://protobuf.dev/programming-guides/encoding/>) to build and parse the
-- handful of messages the Spotify handshake and login use. We deliberately do
-- not model schemas or generate code: messages are built from a list of
-- 'Field's and parsed into a flat, ordered '[(FieldNumber, RawValue)]' that
-- typed wrappers (see "Spotify.Proto.Keyexchange" / "Spotify.Proto.Authentication")
-- interpret.
module Spotify.Proto.Wire
  ( -- * Field numbers
    FieldNumber
    -- * Encoding
  , Field
  , encode
  , varint
  , int32
  , uint32
  , uint64
  , bool
  , bytes
  , string
  , message
    -- * Decoding
  , RawValue (..)
  , Message
  , decode
  , getVarint
  , getBytes
  , getString
  , getMessage
  , getAll
  ) where

import           Data.Bits
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Builder  as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Word

type FieldNumber = Int

-- ---------------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------------

-- | One encoded field. Build these with the smart constructors below and hand a
-- list to 'encode'. Optional fields are naturally expressed as @[]@ vs @[f]@.
newtype Field = Field B.Builder

-- | Encode a message (a list of fields) to its protobuf byte representation.
encode :: [Field] -> ByteString
encode fs = BL.toStrict (B.toLazyByteString (foldMap unField fs))
  where unField (Field b) = b

-- wire types
wtVarint, wtLen :: Word64
wtVarint = 0
wtLen    = 2

tag :: FieldNumber -> Word64 -> B.Builder
tag n wt = varintB (fromIntegral n `shiftL` 3 .|. wt)

varintB :: Word64 -> B.Builder
varintB = go
  where
    go w
      | w < 0x80  = B.word8 (fromIntegral w)
      | otherwise = B.word8 (fromIntegral (w .&. 0x7f) .|. 0x80) <> go (w `shiftR` 7)

-- | A raw varint field.
varint :: FieldNumber -> Word64 -> Field
varint n w = Field (tag n wtVarint <> varintB w)

-- | A 32-bit signed integer (encoded, like protobuf, as a 64-bit varint with
-- sign extension — matching @int32@ semantics for the small enums we use).
int32 :: FieldNumber -> Int -> Field
int32 n v = varint n (fromIntegral (fromIntegral v :: Word64))

uint32 :: FieldNumber -> Word32 -> Field
uint32 n v = varint n (fromIntegral v)

uint64 :: FieldNumber -> Word64 -> Field
uint64 = varint

bool :: FieldNumber -> Bool -> Field
bool n b = varint n (if b then 1 else 0)

-- | A length-delimited @bytes@ field.
bytes :: FieldNumber -> ByteString -> Field
bytes n bs =
  Field (tag n wtLen <> varintB (fromIntegral (BS.length bs)) <> B.byteString bs)

-- | A length-delimited UTF-8 @string@ field.
string :: FieldNumber -> ByteString -> Field
string = bytes

-- | An embedded (length-delimited) sub-message.
message :: FieldNumber -> [Field] -> Field
message n fs = bytes n (encode fs)

-- ---------------------------------------------------------------------------
-- Decoding
-- ---------------------------------------------------------------------------

-- | A decoded field value. Only the wire types we actually receive are kept.
data RawValue
  = VVarint !Word64
  | VBytes  !ByteString
  | VFixed32 !Word32
  | VFixed64 !Word64
  deriving (Eq, Show)

-- | A decoded message: fields in the order they appeared (repeated fields keep
-- all occurrences). Singular lookups take the last occurrence, per protobuf.
type Message = [(FieldNumber, RawValue)]

-- | Parse a protobuf message. Unknown wire types are an error rather than being
-- skipped, because for the messages we handle they only indicate corruption.
decode :: ByteString -> Either String Message
decode = go []
  where
    go acc bs
      | BS.null bs = Right (reverse acc)
      | otherwise  = do
          (key, r1) <- takeVarint bs
          let fn = fromIntegral (key `shiftR` 3)
              wt = key .&. 0x7
          (val, r2) <- case wt of
            0 -> do (v, r) <- takeVarint r1
                    Right (VVarint v, r)
            2 -> do (len, r) <- takeVarint r1
                    let n = fromIntegral len
                    if BS.length r < n
                      then Left "wire: truncated length-delimited field"
                      else Right (VBytes (BS.take n r), BS.drop n r)
            5 -> do (v, r) <- takeFixed 4 r1
                    Right (VFixed32 (leWord (BS.unpack v)), r)
            1 -> do (v, r) <- takeFixed 8 r1
                    Right (VFixed64 (leWord (BS.unpack v)), r)
            _ -> Left ("wire: unsupported wire type " <> show wt)
          go ((fn, val) : acc) r2

    leWord :: (Bits a, Num a) => [Word8] -> a
    leWord = foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) 0 . reverse

takeVarint :: ByteString -> Either String (Word64, ByteString)
takeVarint = go 0 0
  where
    go !shft !acc bs = case BS.uncons bs of
      Nothing -> Left "wire: truncated varint"
      Just (b, rest)
        | shft >= 64 -> Left "wire: varint overflow"
        | otherwise  ->
            let acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` shft)
            in if b .&. 0x80 /= 0
                 then go (shft + 7) acc' rest
                 else Right (acc', rest)

takeFixed :: Int -> ByteString -> Either String (ByteString, ByteString)
takeFixed n bs
  | BS.length bs < n = Left "wire: truncated fixed field"
  | otherwise        = Right (BS.splitAt n bs)

-- | The last 'VVarint' for a field number, if any.
getVarint :: FieldNumber -> Message -> Maybe Word64
getVarint n m = lastOf [v | (k, VVarint v) <- m, k == n]

-- | The last @bytes@/@string@ value for a field number, if any.
getBytes :: FieldNumber -> Message -> Maybe ByteString
getBytes n m = lastOf [v | (k, VBytes v) <- m, k == n]

-- | Same as 'getBytes', kept for intent at the call site.
getString :: FieldNumber -> Message -> Maybe ByteString
getString = getBytes

-- | Decode the last embedded sub-message for a field number.
getMessage :: FieldNumber -> Message -> Either String (Maybe Message)
getMessage n m = case getBytes n m of
  Nothing -> Right Nothing
  Just bs -> Just <$> decode bs

-- | All raw values for a (repeated) field number, in order.
getAll :: FieldNumber -> Message -> [RawValue]
getAll n m = [v | (k, v) <- m, k == n]

lastOf :: [a] -> Maybe a
lastOf [] = Nothing
lastOf xs = Just (last xs)
