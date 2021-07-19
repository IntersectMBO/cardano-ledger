{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.Signing.Safe
  ( tests,
  )
where

import Cardano.Crypto.Signing
  ( noPassSafeSigner,
    safeToVerification,
    toVerification,
  )
import Cardano.Prelude
import Hedgehog
  ( Property,
    checkParallel,
    discover,
    forAll,
    property,
    (===),
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
--
-- Since SafeSigner was simplified, this is a completely trivial property.
--
-- Definitions:
--
-- > safeToVerification (SafeSigner sk _) = toVerification sk
-- > noPassSafeSigner sk = SafeSigner sk emptyPassphrase
--
-- Theorem:
--
-- > safeToVerification (noPassSafeSigner sk) = toVerification sk
--
-- Proof:
--
-- >   safeToVerification (noPassSafeSigner sk)
-- >
-- > = { by expanding definition of noPassSafeSigner }
-- >
-- >   safeToVerification (SafeSigner sk emptyPassphrase)
-- >
-- > = { by expanding definition of safeToVerification }
-- >
-- >   toVerification sk
prop_safeSignerPreservesVerificationKey :: Property
prop_safeSignerPreservesVerificationKey = property $ do
  sk <- forAll genSigningKey
  safeToVerification (noPassSafeSigner sk) === toVerification sk
