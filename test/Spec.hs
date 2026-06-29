-- | Test vectors and round-trips for the crypto and protobuf layers.
--
-- The Shannon vectors are ground-truth values produced by the original
-- reference implementation, so a passing run means our port is bit-exact.
module Main (main) where

import           Control.Monad      (forM_)
import           Data.Char          (digitToInt)
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.IORef
import           System.Exit        (exitFailure, exitSuccess)

import           Spotify.Audio.Decrypt        (aes128ctr)
import           Spotify.Auth.OAuth           (base64url, sha256)
import qualified Spotify.Crypto.DiffieHellman as DH
import           Spotify.Crypto.Hashcash      (hashcashTrailingZeros, solveHashcash)
import           Spotify.Id                   (idFromBase62, idToHex, idToRaw)
import           Spotify.Crypto.Keys          (hmacSha1)
import qualified Spotify.Crypto.Shannon       as Sh
import qualified Spotify.Proto.Wire           as W

main :: IO ()
main = do
  fails <- newIORef (0 :: Int)
  let check name ok = do
        putStrLn ((if ok then "  ok   " else "  FAIL ") ++ name)
        if ok then pure () else modifyIORef' fails (+ 1)
      checkIO name act = act >>= check name

  putStrLn "== Shannon (vs. reference vectors) =="
  forM_ shannonVectors $ \(label, ptH, ctH, macH) -> do
    let pt  = unhex ptH; ct = unhex ctH; mac = unhex macH
    checkIO ("encrypt len=" ++ label) $ do
      s   <- Sh.new shannonKey
      Sh.nonceU32 s 0
      ct' <- Sh.encrypt s pt
      m'  <- Sh.finish s 4
      pure (ct' == ct && m' == mac)
    checkIO ("decrypt len=" ++ label) $ do
      s   <- Sh.new shannonKey
      Sh.nonceU32 s 0
      pt' <- Sh.decrypt s ct
      ok  <- Sh.checkMac s mac
      pure (pt' == pt && ok)

  putStrLn "== Shannon split decrypt (header then payload, as the codec does) =="
  checkIO "split decrypt + mac" $ do
    let pt = unhex "ab000a0102030405060708090a"
        ct = unhex "5092b2a954e9fc4d65b36ce6a0"
        mac = unhex "da670cad"
    s <- Sh.new shannonKey
    Sh.nonceU32 s 5
    h  <- Sh.decrypt s (BS.take 3 ct)   -- 3-byte header
    p  <- Sh.decrypt s (BS.drop 3 ct)   -- payload
    ok <- Sh.checkMac s mac
    pure ((h <> p) == pt && ok)

  putStrLn "== Diffie-Hellman =="
  checkIO "shared secret agrees both ways" $ do
    a <- DH.generate
    b <- DH.generate
    let sa = DH.sharedSecret a (DH.publicKeyBytes b)
        sb = DH.sharedSecret b (DH.publicKeyBytes a)
    pure (sa == sb && not (BS.null sa))

  putStrLn "== HMAC-SHA1 (RFC 2202 #1) =="
  check "hmac-sha1 known answer" $
    hmacSha1 (BS.replicate 20 0x0b) "Hi There"
      == unhex "b617318655057264e28bc0b6fb378c8ef146be00"

  putStrLn "== Protobuf wire codec =="
  let encoded = W.encode
        [ W.uint32 1 300
        , W.string 2 "hi"
        , W.message 3 [ W.bool 1 True, W.bytes 2 "\xde\xad" ]
        ]
  case W.decode encoded of
    Left e  -> check ("decode error: " ++ e) False
    Right m -> do
      check "varint field"  (W.getVarint 1 m == Just 300)
      check "string field"  (W.getString 2 m == Just "hi")
      case W.getMessage 3 m of
        Right (Just sub) -> do
          check "nested bool"  (W.getVarint 1 sub == Just 1)
          check "nested bytes" (W.getBytes  2 sub == Just "\xde\xad")
        _ -> check "nested message" False

  putStrLn "== base64url (RFC 4648) =="
  mapM_ (\(inp, out) -> check ("base64url " ++ show inp) (base64url inp == out))
    [ ("",       "")
    , ("f",      "Zg")
    , ("fo",     "Zm8")
    , ("foo",    "Zm9v")
    , ("foob",   "Zm9vYg")
    , ("fooba",  "Zm9vYmE")
    , ("foobar", "Zm9vYmFy")
    ]

  putStrLn "== PKCE challenge (RFC 7636 appendix B) =="
  check "S256 code_challenge" $
    base64url (sha256 "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
      == "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

  putStrLn "== Hashcash proof-of-work =="
  let hcPrefix = "hespot-hashcash-test"
      hcSuffix = solveHashcash "" hcPrefix 12
  check "suffix is 16 bytes"          (BS.length hcSuffix == 16)
  check "solution meets difficulty 12" (hashcashTrailingZeros hcPrefix hcSuffix >= 12)

  putStrLn "== SpotifyId base62 (librespot vectors) =="
  case idFromBase62 "5sWHDYs0csV6RS48xBl0tH" of
    Left e    -> check ("idFromBase62: " ++ e) False
    Right sid -> do
      check "base62 -> hex" (idToHex sid == "b39fe8081e1f4c54be38e8d6f9f12bb9")
      check "base62 -> raw" (idToRaw sid == unhex "b39fe8081e1f4c54be38e8d6f9f12bb9")
  case idFromBase62 "0000000000000000000000" of
    Left e    -> check ("idFromBase62 zero: " ++ e) False
    Right sid -> check "base62 zero -> hex" (idToHex sid == "00000000000000000000000000000000")
  check "idFromBase62 rejects bad length" (either (const True) (const False) (idFromBase62 "abc"))

  putStrLn "== AES-128-CTR (NIST SP800-38A F.5.1) =="
  check "aes128ctr matches NIST vector" $
    aes128ctr (unhex "2b7e151628aed2a6abf7158809cf4f3c")
              (unhex "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff")
              (unhex "6bc1bee22e409f96e93d7e117393172a")
      == Right (unhex "874d6191b620e3261bef6864990db6ce")

  n <- readIORef fails
  if n == 0
    then putStrLn "\nALL TESTS PASSED" >> exitSuccess
    else putStrLn ("\n" ++ show n ++ " TEST(S) FAILED") >> exitFailure

-- | The 32-byte key used for every Shannon vector (0x01 .. 0x20).
shannonKey :: ByteString
shannonKey = unhex "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20"

-- | (label, plaintext, ciphertext, mac) at nonce 0, from the reference crate.
shannonVectors :: [(String, String, String, String)]
shannonVectors =
  [ ("0",  "",                                          "",                                          "e9426b14")
  , ("3",  "030a11",                                    "1b6b98",                                    "422a6de7")
  , ("4",  "030a1118",                                  "1b6b9879",                                  "6f676e2e")
  , ("7",  "030a11181f262d",                            "1b6b987995733b",                            "58b91fda")
  , ("13", "030a11181f262d343b42495057",                "1b6b987995733b2ebd1b45aaf1",                "d48712ea")
  , ("16", "030a11181f262d343b424950575e656c",          "1b6b987995733b2ebd1b45aaf1e8b1bd",          "896c72a1")
  , ("20", "030a11181f262d343b424950575e656c737a8188",  "1b6b987995733b2ebd1b45aaf1e8b1bd4f1d5509",  "51e4e255")
  ]

unhex :: String -> ByteString
unhex = BS.pack . go
  where
    go (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _              = []
