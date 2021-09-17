{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Crypto.CBOR
  ( constantByteString,
    getBytes,
    tests,
  )
where

import Cardano.Binary (Dropper, ToCBOR, dropBytes, dropList, enforceSize)
import Cardano.Crypto
  ( AbstractHash,
    PassPhrase,
    ProtocolMagicId (..),
    RedeemSignature,
    SignTag (SignForTestingOnly),
    Signature,
    SigningKey (..),
    VerificationKey (..),
    redeemDeterministicKeyGen,
    redeemSign,
    serializeCborHash,
    sign,
  )
import Cardano.Crypto.Wallet (xprv, xpub)
import Cardano.Prelude
import Crypto.Hash (Blake2b_224, Blake2b_256, Blake2b_384, Blake2b_512, SHA1)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import Hedgehog (Gen, Property)
import qualified Hedgehog as H
import Test.Cardano.Binary.Helpers (SizeTestConfig (..), scfg, sizeTest)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( deprecatedGoldenDecode,
    goldenTestCBOR,
    roundTripsCBORBuildable,
    roundTripsCBORShow,
  )
import Test.Cardano.Crypto.Gen
import Test.Cardano.Prelude

--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

roundTripProtocolMagicAeson :: Property
roundTripProtocolMagicAeson = eachOf 1000 genProtocolMagic roundTripsAesonShow

--------------------------------------------------------------------------------
-- RequiresNetworkMagic
--------------------------------------------------------------------------------

roundTripRequiresNetworkMagicCBOR :: Property
roundTripRequiresNetworkMagicCBOR =
  eachOf 100 genRequiresNetworkMagic roundTripsCBORShow

--------------------------------------------------------------------------------
-- VerificationKey
--------------------------------------------------------------------------------

goldenVerificationKey :: Property
goldenVerificationKey = goldenTestCBOR vkey "test/golden/VerificationKey"
  where
    Right vkey = VerificationKey <$> xpub (getBytes 0 64)

roundTripVerificationKeyCBOR :: Property
roundTripVerificationKeyCBOR = eachOf 1000 genVerificationKey roundTripsCBORBuildable

roundTripVerificationKeyAeson :: Property
roundTripVerificationKeyAeson = eachOf 1000 genVerificationKey roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- VerificationKey
--------------------------------------------------------------------------------

roundTripCompactRedeemVerificationKeyCBOR :: Property
roundTripCompactRedeemVerificationKeyCBOR = eachOf 1000 genCompactRedeemVerificationKey roundTripsCBORShow

--------------------------------------------------------------------------------
-- SigningKey
--------------------------------------------------------------------------------

goldenSigningKey :: Property
goldenSigningKey = goldenTestCBOR skey "test/golden/SigningKey"
  where
    Right skey = SigningKey <$> xprv (getBytes 10 128)

roundTripSigningKeyCBOR :: Property
roundTripSigningKeyCBOR = eachOf 1000 genSigningKey roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

goldenSignature :: Property
goldenSignature = goldenTestCBOR sig "test/golden/Signature"
  where
    Right skey = SigningKey <$> xprv (getBytes 10 128)
    sig = sign (ProtocolMagicId 0) SignForTestingOnly skey ()

genUnitSignature :: Gen (Signature ())
genUnitSignature = do
  pm <- genProtocolMagicId
  genSignature pm (pure ())

roundTripSignatureCBOR :: Property
roundTripSignatureCBOR = eachOf 1000 genUnitSignature roundTripsCBORBuildable

roundTripSignatureAeson :: Property
roundTripSignatureAeson = eachOf 1000 genUnitSignature roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- RedeemVerificationKey
--------------------------------------------------------------------------------

goldenRedeemVerificationKey :: Property
goldenRedeemVerificationKey = goldenTestCBOR rvk "test/golden/RedeemVerificationKey"
  where
    Just rvk = fst <$> redeemDeterministicKeyGen (getBytes 0 32)

roundTripRedeemVerificationKeyCBOR :: Property
roundTripRedeemVerificationKeyCBOR =
  eachOf 1000 genRedeemVerificationKey roundTripsCBORBuildable

roundTripRedeemVerificationKeyAeson :: Property
roundTripRedeemVerificationKeyAeson =
  eachOf 1000 genRedeemVerificationKey roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- RedeemSigningKey
--------------------------------------------------------------------------------

goldenRedeemSigningKey :: Property
goldenRedeemSigningKey = goldenTestCBOR rsk "test/golden/RedeemSigningKey"
  where
    Just rsk = snd <$> redeemDeterministicKeyGen (getBytes 0 32)

roundTripRedeemSigningKeyCBOR :: Property
roundTripRedeemSigningKeyCBOR =
  eachOf 1000 genRedeemSigningKey roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- RedeemSignature
--------------------------------------------------------------------------------

goldenRedeemSignature :: Property
goldenRedeemSignature = goldenTestCBOR rsig "test/golden/RedeemSignature"
  where
    Just rsk = snd <$> redeemDeterministicKeyGen (getBytes 0 32)
    rsig = redeemSign (ProtocolMagicId 0) SignForTestingOnly rsk ()

genUnitRedeemSignature :: Gen (RedeemSignature ())
genUnitRedeemSignature = do
  pm <- genProtocolMagicId
  genRedeemSignature pm (pure ())

roundTripRedeemSignatureCBOR :: Property
roundTripRedeemSignatureCBOR =
  eachOf 1000 genUnitRedeemSignature roundTripsCBORBuildable

roundTripRedeemSignatureAeson :: Property
roundTripRedeemSignatureAeson =
  eachOf 1000 genUnitRedeemSignature roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- VssPublicKey
--------------------------------------------------------------------------------

goldenDeprecatedVssPublicKey :: Property
goldenDeprecatedVssPublicKey =
  deprecatedGoldenDecode "VssPublicKey" dropBytes "test/golden/VssPublicKey"

--------------------------------------------------------------------------------
-- DecShare
--------------------------------------------------------------------------------

goldenDeprecatedDecShare :: Property
goldenDeprecatedDecShare =
  deprecatedGoldenDecode "DecShare" dropBytes "test/golden/DecShare"

--------------------------------------------------------------------------------
-- EncShare
--------------------------------------------------------------------------------

goldenDeprecatedEncShare :: Property
goldenDeprecatedEncShare =
  deprecatedGoldenDecode "EncShare" dropBytes "test/golden/EncShare"

--------------------------------------------------------------------------------
-- Secret
--------------------------------------------------------------------------------

goldenDeprecatedSecret :: Property
goldenDeprecatedSecret =
  deprecatedGoldenDecode "Secret" dropBytes "test/golden/Secret"

--------------------------------------------------------------------------------
-- SecretProof
--------------------------------------------------------------------------------

goldenDeprecatedSecretProof :: Property
goldenDeprecatedSecretProof =
  deprecatedGoldenDecode
    "SecretProof"
    dropSecretProof
    "test/golden/SecretProof"
  where
    dropSecretProof :: Dropper s
    dropSecretProof = do
      enforceSize "SecretProof" 4
      replicateM_ 3 dropBytes
      dropList dropBytes

--------------------------------------------------------------------------------
-- AbstractHash
--------------------------------------------------------------------------------

goldenAbstractHash :: Property
goldenAbstractHash = goldenTestCBOR (serializeCborHash ()) "test/golden/AbstractHash"

genUnitAbstractHash :: Gen (AbstractHash Blake2b_256 ())
genUnitAbstractHash = genAbstractHash $ pure ()

roundTripAbstractHashCBOR :: Property
roundTripAbstractHashCBOR =
  eachOf 1000 genUnitAbstractHash roundTripsCBORBuildable

roundTripAbstractHashAeson :: Property
roundTripAbstractHashAeson =
  eachOf 1000 genUnitAbstractHash roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- PassPhrase
--------------------------------------------------------------------------------

goldenPassPhrase :: Property
goldenPassPhrase = goldenTestCBOR passphrase "test/golden/PassPhrase"
  where
    -- PassPhrase has to be 32 bytes in length
    passphrase = ByteArray.pack (BS.unpack $ getBytes 3 32) :: PassPhrase

roundTripPassPhraseCBOR :: Property
roundTripPassPhraseCBOR = eachOf 1000 genPassPhrase roundTripsCBORBuildable

--------------------------------------------------------------------------------

getBytes :: Int -> Int -> ByteString
getBytes offset len = BS.take len $ BS.drop offset constantByteString

-- | Changing existing values in this string will break existing golden
-- tests, but it us OK to append more data to the end.
constantByteString :: ByteString
constantByteString =
  "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
  \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
  \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
  \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
  \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
  \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

--------------------------------------------------------------------------------

sizeEstimates :: H.Group
sizeEstimates =
  let testPrecise :: forall a. (Show a, ToCBOR a) => Gen a -> Property
      testPrecise g = sizeTest $ scfg {gen = g, precise = True}
   in H.Group
        "Encoded size bounds for crypto types."
        [ ("VerificationKey", testPrecise genVerificationKey),
          ( "AbstractHash Blake2b_224 VerificationKey",
            testPrecise @(AbstractHash Blake2b_224 VerificationKey) $
              genAbstractHash genVerificationKey
          ),
          ( "AbstractHash Blake2b_256 VerificationKey",
            testPrecise @(AbstractHash Blake2b_256 VerificationKey) $
              genAbstractHash genVerificationKey
          ),
          ( "AbstractHash Blake2b_384 VerificationKey",
            testPrecise @(AbstractHash Blake2b_384 VerificationKey) $
              genAbstractHash genVerificationKey
          ),
          ( "AbstractHash Blake2b_512 VerificationKey",
            testPrecise @(AbstractHash Blake2b_512 VerificationKey) $
              genAbstractHash genVerificationKey
          ),
          ( "AbstractHash SHA1 VerificationKey",
            testPrecise @(AbstractHash SHA1 VerificationKey) $
              genAbstractHash genVerificationKey
          ),
          ("RedeemVerificationKey", testPrecise genRedeemVerificationKey),
          ("RedeemSigningKey", testPrecise genRedeemSigningKey),
          ( "RedeemSignature VerificationKey",
            testPrecise (genRedeemSignature (ProtocolMagicId 0) genVerificationKey)
          )
        ]

--------------------------------------------------------------------------------

tests :: IO Bool
tests =
  and
    <$> sequence
      [ H.checkSequential $$discoverGolden,
        H.checkParallel $$discoverRoundTrip,
        H.checkParallel sizeEstimates
      ]
