{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Crypto.Bi
  ( constantByteString
  , getBytes
  , tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Cardano.Crypto.Wallet (XPrv, unXPrv, xprv, xpub)
import Crypto.Hash (Blake2b_224, Blake2b_256, Blake2b_384, Blake2b_512, SHA1)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS

import Hedgehog (Gen, Property)
import qualified Hedgehog as H

import Cardano.Binary.Class (Bi, Dropper, dropBytes, dropList, enforceSize)
import Cardano.Crypto
  ( AbstractHash
  , PassPhrase
  , ProtocolMagicId(..)
  , ProxyCert
  , ProxySecretKey
  , PublicKey(..)
  , RedeemSignature
  , SecretKey(..)
  , SignTag(SignForTestingOnly)
  , Signature
  , createPsk
  , deriveHDPassphrase
  , hash
  , noPassEncrypt
  , noPassSafeSigner
  , packHDAddressAttr
  , proxySign
  , redeemDeterministicKeyGen
  , redeemSign
  , safeCreateProxyCert
  , sign
  , toPublic
  )

import Test.Cardano.Binary.Helpers (SizeTestConfig(..), scfg, sizeTest)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( deprecatedGoldenDecode
  , goldenTestBi
  , roundTripsBiBuildable
  , roundTripsBiShow
  )
import Test.Cardano.Crypto.Gen


--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

roundTripProtocolMagicAeson :: Property
roundTripProtocolMagicAeson = eachOf 1000 genProtocolMagic roundTripsAesonShow


--------------------------------------------------------------------------------
-- PublicKey
--------------------------------------------------------------------------------

goldenPublicKey :: Property
goldenPublicKey = goldenTestBi pkey "test/golden/PublicKey"
  where Right pkey = PublicKey <$> xpub (getBytes 0 64)

roundTripPublicKeyBi :: Property
roundTripPublicKeyBi = eachOf 1000 genPublicKey roundTripsBiBuildable

roundTripPublicKeyAeson :: Property
roundTripPublicKeyAeson = eachOf 1000 genPublicKey roundTripsAesonBuildable


--------------------------------------------------------------------------------
-- SecretKey
--------------------------------------------------------------------------------

goldenSecretKey :: Property
goldenSecretKey = goldenTestBi skey "test/golden/SecretKey"
  where Right skey = SecretKey <$> xprv (getBytes 10 128)

roundTripSecretKeyBi :: Property
roundTripSecretKeyBi = eachOf 1000 genSecretKey roundTripsBiBuildable


--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

goldenSignature :: Property
goldenSignature = goldenTestBi sig "test/golden/Signature"
 where
  Right skey = SecretKey <$> xprv (getBytes 10 128)
  sig        = sign (ProtocolMagicId 0) SignForTestingOnly skey ()

genUnitSignature :: Gen (Signature ())
genUnitSignature = do
  pm <- genProtocolMagicId
  genSignature pm (pure ())

roundTripSignatureBi :: Property
roundTripSignatureBi = eachOf 1000 genUnitSignature roundTripsBiBuildable

roundTripSignatureAeson :: Property
roundTripSignatureAeson = eachOf 1000 genUnitSignature roundTripsAesonBuildable


--------------------------------------------------------------------------------
-- EncryptedSecretKey
--------------------------------------------------------------------------------

-- | This instance is unsafe, as it allows a timing attack. But it's OK for
-- tests.
instance Eq XPrv where
   (==) = (==) `on` unXPrv

goldenEncryptedSecretKey :: Property
goldenEncryptedSecretKey = goldenTestBi esk "test/golden/EncryptedSecretKey"
  where Right esk = noPassEncrypt . SecretKey <$> xprv (getBytes 10 128)

roundTripEncryptedSecretKeysBi :: Property
roundTripEncryptedSecretKeysBi =
  eachOf 100 genEncryptedSecretKey roundTripsBiBuildable


--------------------------------------------------------------------------------
-- RedeemPublicKey
--------------------------------------------------------------------------------

goldenRedeemPublicKey :: Property
goldenRedeemPublicKey = goldenTestBi rpk "test/golden/RedeemPublicKey"
  where Just rpk = fst <$> redeemDeterministicKeyGen (getBytes 0 32)

roundTripRedeemPublicKeyBi :: Property
roundTripRedeemPublicKeyBi =
  eachOf 1000 genRedeemPublicKey roundTripsBiBuildable

roundTripRedeemPublicKeyAeson :: Property
roundTripRedeemPublicKeyAeson =
  eachOf 1000 genRedeemPublicKey roundTripsAesonBuildable


--------------------------------------------------------------------------------
-- RedeemSecretKey
--------------------------------------------------------------------------------

goldenRedeemSecretKey :: Property
goldenRedeemSecretKey = goldenTestBi rsk "test/golden/RedeemSecretKey"
  where Just rsk = snd <$> redeemDeterministicKeyGen (getBytes 0 32)

roundTripRedeemSecretKeyBi :: Property
roundTripRedeemSecretKeyBi =
  eachOf 1000 genRedeemSecretKey roundTripsBiBuildable


--------------------------------------------------------------------------------
-- RedeemSignature
--------------------------------------------------------------------------------

goldenRedeemSignature :: Property
goldenRedeemSignature = goldenTestBi rsig "test/golden/RedeemSignature"
 where
  Just rsk = snd <$> redeemDeterministicKeyGen (getBytes 0 32)
  rsig     = redeemSign (ProtocolMagicId 0) SignForTestingOnly rsk ()

genUnitRedeemSignature :: Gen (RedeemSignature ())
genUnitRedeemSignature = do
  pm <- genProtocolMagicId
  genRedeemSignature pm (pure ())

roundTripRedeemSignatureBi :: Property
roundTripRedeemSignatureBi =
  eachOf 1000 genUnitRedeemSignature roundTripsBiBuildable

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
-- ProxyCert
--------------------------------------------------------------------------------

goldenProxyCert :: Property
goldenProxyCert = goldenTestBi pcert "test/golden/ProxyCert"
 where
  Right pkey = PublicKey <$> xpub (getBytes 0 64)
  Right skey = SecretKey <$> xprv (getBytes 10 128)
  pcert =
    safeCreateProxyCert (ProtocolMagicId 0) (noPassSafeSigner skey) pkey ()

genUnitProxyCert :: Gen (ProxyCert ())
genUnitProxyCert = do
  pm <- genProtocolMagicId
  genProxyCert pm $ pure ()

roundTripProxyCertBi :: Property
roundTripProxyCertBi = eachOf 100 genUnitProxyCert roundTripsBiBuildable

roundTripProxyCertAeson :: Property
roundTripProxyCertAeson = eachOf 100 genUnitProxyCert roundTripsAesonBuildable


--------------------------------------------------------------------------------
-- ProxySecretKey
--------------------------------------------------------------------------------

goldenProxySecretKey :: Property
goldenProxySecretKey = goldenTestBi psk "test/golden/ProxySecretKey"
 where
  Right pkey = PublicKey <$> xpub (getBytes 0 64)
  Right skey = SecretKey <$> xprv (getBytes 10 128)
  psk        = createPsk (ProtocolMagicId 0) (noPassSafeSigner skey) pkey ()

genUnitProxySecretKey :: Gen (ProxySecretKey ())
genUnitProxySecretKey = do
  pm <- genProtocolMagicId
  genProxySecretKey pm $ pure ()

roundTripProxySecretKeyBi :: Property
roundTripProxySecretKeyBi =
  eachOf 100 genUnitProxySecretKey roundTripsBiBuildable

roundTripProxySecretKeyAeson :: Property
roundTripProxySecretKeyAeson =
  eachOf 100 genUnitProxySecretKey roundTripsAesonBuildable


--------------------------------------------------------------------------------
-- ProxySignature
--------------------------------------------------------------------------------

goldenProxySignature :: Property
goldenProxySignature = goldenTestBi psig "test/golden/ProxySignature"
 where
  Right skey = SecretKey <$> xprv (getBytes 10 128)
  psk =
    createPsk (ProtocolMagicId 0) (noPassSafeSigner skey) (toPublic skey) ()
  psig = proxySign (ProtocolMagicId 0) SignForTestingOnly skey psk ()

roundTripProxySignatureBi :: Property
roundTripProxySignatureBi = eachOf
  100
  genUnitProxySignature
  roundTripsBiBuildable
 where
  genUnitProxySignature = do
    pm <- genProtocolMagicId
    genProxySignature pm (pure ()) (pure ())


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
goldenDeprecatedSecretProof = deprecatedGoldenDecode
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
goldenAbstractHash = goldenTestBi (hash ()) "test/golden/AbstractHash"

genUnitAbstractHash :: Gen (AbstractHash Blake2b_256 ())
genUnitAbstractHash = genAbstractHash $ pure ()

roundTripAbstractHashBi :: Property
roundTripAbstractHashBi = eachOf 1000 genUnitAbstractHash roundTripsBiBuildable

roundTripAbstractHashAeson :: Property
roundTripAbstractHashAeson =
  eachOf 1000 genUnitAbstractHash roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- PassPhrase
--------------------------------------------------------------------------------

goldenPassPhrase :: Property
goldenPassPhrase = goldenTestBi passphrase "test/golden/PassPhrase"
  where
    -- PassPhrase has to be 32 bytes in length
        passphrase = ByteArray.pack (BS.unpack $ getBytes 3 32) :: PassPhrase

roundTripPassPhraseBi :: Property
roundTripPassPhraseBi = eachOf 1000 genPassPhrase roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HDAddressPayload
--------------------------------------------------------------------------------

goldenHDAddressPayload :: Property
goldenHDAddressPayload = goldenTestBi hdap "test/golden/HDAddressPayload"
 where
  Right hdap =
    flip packHDAddressAttr [] . deriveHDPassphrase . PublicKey <$> xpub
      (getBytes 0 64)

roundTripHDAddressPayloadBi :: Property
roundTripHDAddressPayloadBi = eachOf 1000 genHDAddressPayload roundTripsBiShow

roundTripHDAddressPayloadAeson :: Property
roundTripHDAddressPayloadAeson =
  eachOf 1000 genHDAddressPayload roundTripsAesonShow

--------------------------------------------------------------------------------

getBytes :: Int -> Int -> ByteString
getBytes offset len = BS.take len $ BS.drop offset constantByteString

-- | Changing existing values in this string will break existing golden
-- tests, but it us OK to append more data to the end.
constantByteString :: ByteString
constantByteString
  = "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

--------------------------------------------------------------------------------

sizeEstimates :: H.Group
sizeEstimates =
  let
    testPrecise :: forall a . (Show a, Bi a) => Gen a -> Property
    testPrecise g = sizeTest $ scfg { gen = g, precise = True }
  in H.Group
    "Encoded size bounds for crypto types."
    [ ("PublicKey", testPrecise genPublicKey)
    , ( "AbstractHash Blake2b_224 PublicKey"
      , testPrecise @(AbstractHash Blake2b_224 PublicKey)
        $ genAbstractHash genPublicKey
      )
    , ( "AbstractHash Blake2b_256 PublicKey"
      , testPrecise @(AbstractHash Blake2b_256 PublicKey)
        $ genAbstractHash genPublicKey
      )
    , ( "AbstractHash Blake2b_384 PublicKey"
      , testPrecise @(AbstractHash Blake2b_384 PublicKey)
        $ genAbstractHash genPublicKey
      )
    , ( "AbstractHash Blake2b_512 PublicKey"
      , testPrecise @(AbstractHash Blake2b_512 PublicKey)
        $ genAbstractHash genPublicKey
      )
    , ( "AbstractHash SHA1 PublicKey"
      , testPrecise @(AbstractHash SHA1 PublicKey)
        $ genAbstractHash genPublicKey
      )
    , ("RedeemPublicKey", testPrecise genRedeemPublicKey)
    , ("RedeemSecretKey", testPrecise genRedeemSecretKey)
    , ( "RedeemSignature PublicKey"
      , testPrecise (genRedeemSignature (ProtocolMagicId 0) genPublicKey)
      )
    ]

--------------------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
  [ H.checkSequential $$discoverGolden
  , H.checkSequential $$discoverRoundTrip
  , H.checkParallel sizeEstimates
  ]
