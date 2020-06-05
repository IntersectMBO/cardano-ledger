{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Address
  ( addressTests,
  )
where

import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import Cardano.Crypto.Hash.Class (Hash (..), HashAlgorithm (..))
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.VRF.Simple (SimpleVRF)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as LB16
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import Hedgehog (Gen)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as H
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.BaseTypes (Network (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ptr (..),
    StakeReference (..),
  )
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Keys (pattern KeyHash)
import Shelley.Spec.Ledger.Scripts (pattern ScriptHash)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Test.Shelley.Spec.Ledger.Address.Bootstrap (bootstrapTest, genBootstrapAddress)
import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as C
import Test.Tasty (TestTree)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Test.Tasty.Hedgehog as T

addressTests :: TestTree
addressTests = T.testGroup "Address golden tests" [goldenTests, testsWithOtherCrypto, roundTripTests, bootstrapTest]

goldenTests :: TestTree
goldenTests =
  T.testGroup
    "ConcreteCrypto golden tests"
    [ golden "keyHash" putCredential keyHash "01020304",
      golden "scriptHash" putCredential scriptHash "05060708",
      golden "ptr" putPtr ptr "81000203",
      golden
        "addrBaseKK"
        putAddr
        (Addr Testnet keyHash (StakeRefBase keyHash))
        "000102030401020304",
      golden
        "addrBaseSK"
        putAddr
        (Addr Testnet scriptHash (StakeRefBase keyHash))
        "100506070801020304",
      golden
        "addrBaseKS"
        putAddr
        (Addr Testnet keyHash (StakeRefBase scriptHash))
        "200102030405060708",
      golden
        "addrBaseSS"
        putAddr
        (Addr Testnet scriptHash (StakeRefBase scriptHash))
        "300506070805060708",
      golden
        "addrPtrK"
        putAddr
        (Addr Testnet keyHash (StakeRefPtr ptr))
        "400102030481000203",
      golden
        "addrPtrS"
        putAddr
        (Addr Testnet scriptHash (StakeRefPtr ptr))
        "500506070881000203",
      golden
        "addrEnterpriseK"
        putAddr
        (Addr Testnet keyHash StakeRefNull)
        "6001020304",
      golden
        "addrEnterpriseS"
        putAddr
        (Addr Testnet scriptHash StakeRefNull)
        "7005060708",
      golden
        "rewardAcntK"
        putRewardAcnt
        (RewardAcnt Testnet keyHash)
        "e001020304",
      golden
        "rewardAcntS"
        putRewardAcnt
        (RewardAcnt Testnet scriptHash)
        "f005060708"
    ]

testsWithOtherCrypto :: TestTree
testsWithOtherCrypto =
  T.testGroup
    "serialiseAddr tests with OtherCrypto"
    [ checkSerialiseAddr
        "addrEnterpriseK for network id = 0"
        (Addr Testnet (keyBlake2b224 paymentKey) StakeRefNull)
        "608a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4",
      checkSerialiseAddr
        "addrBaseKK for network id = 0"
        (Addr Testnet (keyBlake2b224 paymentKey) (StakeRefBase (keyBlake2b224 stakeKey)))
        "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      checkSerialiseAddr
        "addrPtrK for network id = 0"
        (Addr Testnet (keyBlake2b224 paymentKey) (StakeRefPtr ptr))
        "408a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203",
      checkSerialiseAddr
        "addrEnterpriseK for network id = 1"
        (Addr Mainnet (keyBlake2b224 paymentKey) StakeRefNull)
        "618a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4",
      checkSerialiseAddr
        "addrBaseKK for network id = 1"
        (Addr Mainnet (keyBlake2b224 paymentKey) (StakeRefBase (keyBlake2b224 stakeKey)))
        "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d408b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4",
      checkSerialiseAddr
        "addrPtrK for network id = 1"
        (Addr Mainnet (keyBlake2b224 paymentKey) (StakeRefPtr ptr))
        "418a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d481000203"
    ]

-- helper data to mimick crypto impl used in cardano-node
-- influenced by https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus-shelley/src/Ouroboros/Consensus/Shelley/Protocol/Crypto.hs
--
data OtherCrypto

instance Crypto OtherCrypto where
  type DSIGN OtherCrypto = Ed25519DSIGN
  type KES OtherCrypto = Sum7KES Ed25519DSIGN Blake2b_256
  type VRF OtherCrypto = SimpleVRF
  type HASH OtherCrypto = Blake2b_256

type OtherCredential kr = Credential kr OtherCrypto

checkSerialiseAddr ::
  String ->
  Addr OtherCrypto ->
  BS.ByteString ->
  TestTree
checkSerialiseAddr name value expected =
  T.testCase name $
    T.assertEqual name expected (B16.encode . serialiseAddr $ value)

paymentKey :: BS.ByteString
paymentKey = B16.encode "1a2a3a4a5a6a7a8a"

stakeKey :: BS.ByteString
stakeKey = B16.encode "1c2c3c4c5c6c7c8c"

-- 32-byte verification key is expected, vk, ie., public key without chain code.
-- The verification key undergoes Blake2b_224 hashing
-- and should be 28-byte in the aftermath
keyBlake2b224 :: BS.ByteString -> OtherCredential kh
keyBlake2b224 vk =
  KeyHashObj . KeyHash . UnsafeHash $ hk
  where
    hash = digest (Proxy :: Proxy Blake2b_224)
    vk' = invariantSize 32 vk
    hk =
      invariantSize
        (fromIntegral $ sizeHash (Proxy :: Proxy Blake2b_224))
        (hash vk')

invariantSize :: HasCallStack => Int -> BS.ByteString -> BS.ByteString
invariantSize expectedLength bytes
  | BS.length bytes == expectedLength = bytes
  | otherwise =
    error $
      "length was "
        ++ show (BS.length bytes)
        ++ ", but expected to be "
        ++ show expectedLength

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
      roundTripRewardAcnt,
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

roundTripRewardAcnt :: TestTree
roundTripRewardAcnt = T.testProperty "reward_account_bytes" $ H.property $ do
  ra <- H.forAll genRewardAcnt
  H.tripping ra serialiseRewardAcnt deserialiseRewardAcnt

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
genAddr =
  H.frequency
    [ (5, Addr Testnet <$> genCredential <*> genStakeReference),
      (1, AddrBootstrap <$> genBootstrapAddress)
    ]
  where
    genStakeReference =
      H.choice
        [ StakeRefBase <$> genCredential,
          StakeRefPtr <$> genPtr,
          pure StakeRefNull
        ]

genRewardAcnt :: Gen C.RewardAcnt
genRewardAcnt = RewardAcnt Testnet <$> genCredential

genCredential :: Gen (C.Credential kr)
genCredential = H.choice [genKeyHash, genScriptHash]

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
