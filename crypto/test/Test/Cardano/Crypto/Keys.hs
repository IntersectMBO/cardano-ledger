{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Crypto.Keys
  ( tests
  )
where

import Cardano.Prelude

import Formatting (sformat)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Crypto.Signing
  ( deterministicKeyGen
  , encToPublic
  , fullPublicKeyF
  , parseFullPublicKey
  , redeemDeterministicKeyGen
  , redeemToPublic
  , safeDeterministicKeyGen
  , toPublic
  )

import Test.Cardano.Crypto.Gen (genPassPhrase, genPublicKey)


--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover


--------------------------------------------------------------------------------
-- Key Properties
--------------------------------------------------------------------------------

-- | Derived 'PublicKey' is the same as generated one
prop_pubKeyDerivedGenerated :: Property
prop_pubKeyDerivedGenerated = property $ do
  seed <- forAll $ Gen.bytes (Range.singleton 32)
  let (pk, sk) = deterministicKeyGen seed
  pk === toPublic sk

prop_pubKeyParsing :: Property
prop_pubKeyParsing = property $ do
  pk <- forAll genPublicKey
  parseFullPublicKey (sformat fullPublicKeyF pk) === Right pk

-- | Derived 'RedeemPublicKey' is the same as generated one
prop_redeemPubKeyDerivedGenerated :: Property
prop_redeemPubKeyDerivedGenerated = property $ do
  seed <- forAll $ Gen.bytes (Range.singleton 32)
  let
    (pk, sk) =
      fromMaybe (panic "redeem keygen failed") $ redeemDeterministicKeyGen seed
  pk === redeemToPublic sk

-- | Derived 'PublicKey' is the same as generated one
prop_safePubKeyDerivedGenerated :: Property
prop_safePubKeyDerivedGenerated = property $ do
  pp   <- forAll genPassPhrase
  seed <- forAll $ Gen.bytes (Range.singleton 32)
  let (pk, sk) = safeDeterministicKeyGen seed pp
  pk === encToPublic sk
