{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.Signing.Safe
  ( tests
  )
where

import Cardano.Prelude

import Hedgehog
  (Property, checkParallel, discover, forAll, property, (===))

import Cardano.Crypto.Signing
  ( noPassSafeSigner
  , safeToVerification
  , toVerification
  )

import Test.Cardano.Crypto.Gen (genSigningKey)


--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover


--------------------------------------------------------------------------------
-- Safe Signing Properties
--------------------------------------------------------------------------------

-- | Making a 'SafeSigner' from a 'SigningKey' preserves the 'VerificationKey'
prop_safeSignerPreservesVerificationKey :: Property
prop_safeSignerPreservesVerificationKey = property $ do
  sk <- forAll genSigningKey
  safeToVerification (noPassSafeSigner sk) === toVerification sk
