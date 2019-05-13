{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Crypto.Signing.Proxy
  ( tests
  )
where

import Cardano.Prelude

import qualified Data.ByteString.Lazy as BSL

import Hedgehog
  (Gen, Property, assert, checkParallel, discover, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Binary (decodeFull, serialize, slice)
import Cardano.Crypto.Signing
  ( AProxySignature(..)
  , AProxyVerificationKey(..)
  , ProxySignature
  , SignTag(..)
  , createPsk
  , proxySign
  , proxyVerify
  , safeToVerification
  , toVerification
  , validateProxyVerificationKey
  )

import qualified Test.Cardano.Crypto.Dummy as Dummy
import Test.Cardano.Crypto.Gen (genVerificationKey, genSafeSigner, genSigningKey)


--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover


--------------------------------------------------------------------------------
-- Proxy Signature Properties
--------------------------------------------------------------------------------

-- | Can verify a correct 'ProxySignature'
prop_proxySign :: Property
prop_proxySign = property $ do
  issuerSafeSigner <- forAll genSafeSigner
  delegateSK <- forAll genSigningKey
  omega      <-
    forAll
    $   (,)
    <$> Gen.int32 Range.constantBounded
    <*> Gen.int32 Range.constantBounded
  let
    psk = createPsk
      Dummy.protocolMagicId
      issuerSafeSigner
      (toVerification delegateSK)
      omega
  a <- forAll genData

  assert
    $ proxyVerify Dummy.protocolMagicId SignForTestingOnly (== omega) a
    $ proxySign Dummy.protocolMagicId SignForTestingOnly delegateSK psk a

-- | Cannot verify a 'ProxySignature' with an incorrect key
prop_proxySignDifferentKey :: Property
prop_proxySignDifferentKey = property $ do
  issuerSafeSigner  <- forAll genSafeSigner
  issuerSafeSigner' <- forAll $ Gen.filter
    ((/= safeToVerification issuerSafeSigner) . safeToVerification)
    genSafeSigner

  delegateSK <- forAll genSigningKey
  omega      <-
    forAll
    $   (,)
    <$> Gen.int32 Range.constantBounded
    <*> Gen.int32 Range.constantBounded

  let
    psk = createPsk
      Dummy.protocolMagicId
      issuerSafeSigner
      (toVerification delegateSK)
      omega
    psk' = createPsk
      Dummy.protocolMagicId
      issuerSafeSigner'
      (toVerification delegateSK)
      omega
    switchPsk :: ProxySignature w s -> ProxySignature (Int32, Int32) s
    switchPsk sig = sig { psigPsk = psk' }

  a <- forAll genData

  assert
    . not
    $ proxyVerify Dummy.protocolMagicId SignForTestingOnly (== omega) a
    $ switchPsk
    $ proxySign Dummy.protocolMagicId SignForTestingOnly delegateSK psk a

-- | Cannot verify a 'ProxySignature' with the wrong data
prop_proxySignDifferentData :: Property
prop_proxySignDifferentData = property $ do
  issuerSafeSigner <- forAll genSafeSigner
  delegateSK <- forAll genSigningKey
  omega      <-
    forAll
    $   (,)
    <$> Gen.int32 Range.constantBounded
    <*> Gen.int32 Range.constantBounded
  let
    psk = createPsk
      Dummy.protocolMagicId
      issuerSafeSigner
      (toVerification delegateSK)
      omega
  a <- forAll genData
  b <- forAll $ Gen.filter (/= a) genData

  assert
    . not
    $ proxyVerify Dummy.protocolMagicId SignForTestingOnly (== omega) b
    $ proxySign Dummy.protocolMagicId SignForTestingOnly delegateSK psk a


--------------------------------------------------------------------------------
-- Proxy Verification Key Properties
--------------------------------------------------------------------------------

-- | Can validate 'ProxyVerificationKey's produced by 'createPsk'
prop_proxyVerificationKeyCorrect :: Property
prop_proxyVerificationKeyCorrect = property $ do
  issuerSafeSigner <- forAll genSafeSigner
  delegateVK <- forAll genVerificationKey
  omega      <- forAll $ Gen.int32 Range.constantBounded

  let
    psk   = createPsk Dummy.protocolMagicId issuerSafeSigner delegateVK omega
    bytes = serialize psk

    annotatedPsk :: AProxyVerificationKey Int32 ByteString
    annotatedPsk =
      fmap (BSL.toStrict . slice bytes)
        . fromRight
            (panic
              "prop_proxyVerificationKeyCorrect: Round trip broken for ProxyVerificationKey"
            )
        $ decodeFull bytes

  assert . isRight $ validateProxyVerificationKey
    Dummy.annotatedProtocolMagicId
    annotatedPsk

-- | Cannot validate 'ProxyVerificationKey's with incorrect verification keys
prop_proxyVerificationKeyIncorrect :: Property
prop_proxyVerificationKeyIncorrect = property $ do
  issuerSafeSigner <- forAll genSafeSigner
  delegateVK  <- forAll genVerificationKey
  delegateVK' <- forAll $ Gen.filter (/= delegateVK) genVerificationKey
  omega       <- forAll $ Gen.int32 Range.constantBounded

  let
    psk = (createPsk Dummy.protocolMagicId issuerSafeSigner delegateVK omega)
      { pskDelegateVK = delegateVK'
      }
    bytes = serialize psk

    annotatedPsk :: AProxyVerificationKey Int32 ByteString
    annotatedPsk =
      fmap (BSL.toStrict . slice bytes)
        . fromRight
            (panic
              "prop_proxyVerificationKeyCorrect: Round trip broken for ProxyVerificationKey"
            )
        $ decodeFull bytes

  assert . isLeft $ validateProxyVerificationKey
    Dummy.annotatedProtocolMagicId
    annotatedPsk

genData :: Gen [Int32]
genData = Gen.list (Range.constant 0 50) (Gen.int32 Range.constantBounded)
