{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Address
  ( addressTests
  )
where

import Cardano.Crypto.Hash.Class (Hash (..), HashAlgorithm (..))
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as LB16
import qualified Data.ByteString.Lazy as LBS
import Hedgehog (Gen)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as H
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..))
import Shelley.Spec.Ledger.Keys (pattern KeyHash)
import Shelley.Spec.Ledger.Scripts (pattern ScriptHash)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as C
import Test.Tasty (TestTree)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Test.Tasty.Hedgehog as T

addressTests :: TestTree
addressTests = T.testGroup "Address binary and golden tests" [goldenTests, roundTripTests]

goldenTests :: TestTree
goldenTests =
  T.testGroup
    "golden tests"
    [ golden "keyHash" putCredential keyHash "01020304",
      golden "scriptHash" putCredential scriptHash "05060708",
      golden "ptr" putPtr ptr "81000203",
      golden
        "addrBaseKK"
        putAddr
        (Addr keyHash (StakeRefBase keyHash))
        "020102030401020304",
      golden
        "addrBaseSK"
        putAddr
        (Addr scriptHash (StakeRefBase keyHash))
        "120506070801020304",
      golden
        "addrBaseKS"
        putAddr
        (Addr keyHash (StakeRefBase scriptHash))
        "220102030405060708",
      golden
        "addrBaseSS"
        putAddr
        (Addr scriptHash (StakeRefBase scriptHash))
        "320506070805060708",
      golden
        "addrPtrK"
        putAddr
        (Addr keyHash (StakeRefPtr ptr))
        "420102030481000203",
      golden
        "addrPtrS"
        putAddr
        (Addr scriptHash (StakeRefPtr ptr))
        "520506070881000203",
      golden
        "addrEnterpriseK"
        putAddr
        (Addr keyHash StakeRefNull)
        "6201020304",
      golden
        "addrEnterpriseS"
        putAddr
        (Addr scriptHash StakeRefNull)
        "7205060708"
    ]

golden :: String -> (a -> B.Put) -> a -> LBS.ByteString -> TestTree
golden name put value expected =
  T.testCase name $
    T.assertEqual name expected (LB16.encode . B.runPut . put $ value)

keyHash :: C.Credential kh
keyHash =
  KeyHashObj . KeyHash . UnsafeHash . fst $
    B16.decode "01020304"

scriptHash :: C.Credential kh
scriptHash =
  ScriptHashObj . ScriptHash . UnsafeHash . fst $
    B16.decode "05060708"

ptr :: Ptr
ptr = Ptr (SlotNo 128) 2 3

roundTripTests :: TestTree
roundTripTests =
  T.testGroup
    "round trip tests"
    [ roundTripAddress,
      putGet "keyhash" genKeyHash putCredential getKeyHash,
      putGet "scripthash" genScriptHash putCredential getScriptHash,
      putGet "ptr" genPtr putPtr getPtr,
      putGet "nat" genNat putVariableLengthNat getVariableLengthNat,
      roundTripNatWord7s
    ]

roundTripAddress :: TestTree
roundTripAddress = T.testProperty "address_bytes" $ H.property $ do
  addr <- H.forAll genAddr
  H.tripping addr serialiseAddr deserialiseAddr

putGet :: (Show a, Eq a) => String -> Gen a -> (a -> B.Put) -> B.Get a -> TestTree
putGet name gen put get = T.testProperty (name <> "_bytes") $ H.property $ do
  value <- H.forAll gen
  H.tripping value (LB16.encode . B.runPut . put) (execGet get . fst . LB16.decode)
  where
    execGet :: B.Get a -> LBS.ByteString -> Maybe a
    execGet g bytes = case B.runGetOrFail g bytes of
      Left _ -> Nothing
      Right (_, _, result) -> Just result

genAddr :: Gen C.Addr
genAddr = Addr <$> genCredential <*> genStakeReference
  where
    genCredential = H.choice [genKeyHash, genScriptHash]
    genStakeReference =
      H.choice
        [ StakeRefBase <$> genCredential,
          StakeRefPtr <$> genPtr,
          pure StakeRefNull
        ]

genKeyHash :: Gen (C.Credential kr)
genKeyHash = KeyHashObj . KeyHash <$> genHash

genScriptHash :: Gen (C.Credential kr)
genScriptHash = ScriptHashObj . ScriptHash <$> genHash

genHash :: forall h a. HashAlgorithm h => Gen (Hash h a)
genHash = UnsafeHash . BS.pack <$> genWords numBytes
  where
    numBytes = fromIntegral $ sizeHash ([] @h)

genWords :: Natural -> Gen [B.Word8]
genWords n
  | n > 0 = (:) <$> H.word8 H.constantBounded <*> genWords (n -1)
  | otherwise = pure []

genPtr :: Gen Ptr
genPtr = Ptr <$> (SlotNo <$> genNat) <*> genNat <*> genNat

genNat :: Integral a => Gen a
genNat =
  H.choice
    [ fromIntegral <$> H.word8 H.constantBounded,
      fromIntegral <$> H.word16 H.constantBounded,
      fromIntegral <$> H.word32 H.constantBounded,
      fromIntegral <$> H.word64 H.constantBounded
    ]

roundTripNatWord7s :: TestTree
roundTripNatWord7s = T.testProperty "nat_word7s" $ H.property $ do
  nat <- H.forAll genNat
  H.tripping nat natToWord7s (Just . word7sToNat)
