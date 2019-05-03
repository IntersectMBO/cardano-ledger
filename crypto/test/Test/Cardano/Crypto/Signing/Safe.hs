{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.Signing.Safe
  ( tests
  )
where

import Cardano.Prelude

import Hedgehog
  (Property, assert, checkParallel, discover, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen

import Cardano.Crypto.Signing
  ( changeEncPassphrase
  , encToVerification
  , noPassEncrypt
  , noPassSafeSigner
  , safeKeyGen
  , safeToVerification
  , toVerification
  , withSafeSigner
  )

import Test.Cardano.Crypto.Gen (genPassPhrase, genSigningKey)


--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover


--------------------------------------------------------------------------------
-- Safe Signing Properties
--------------------------------------------------------------------------------

-- | Constructing a 'SafeSigner' with a valid 'PassPhrase' works
prop_validPassPhraseGivesJustSigner :: Property
prop_validPassPhraseGivesJustSigner = property $ do
  passPhrase <- forAll genPassPhrase
  (_, key)   <- liftIO $ safeKeyGen passPhrase
  withSafeSigner key (pure passPhrase) (assert . isJust)

-- | Constructing a 'SafeSigner' with an invalid 'PassPhrase' gives 'Nothing'
prop_invalidPassPhraseGivesNothing :: Property
prop_invalidPassPhraseGivesNothing = property $ do
  passPhrase  <- forAll genPassPhrase
  passPhrase' <- forAll $ Gen.filter (/= passPhrase) genPassPhrase
  (_, key)    <- liftIO $ safeKeyGen passPhrase
  withSafeSigner key (pure passPhrase') (assert . isNothing)

-- | Changing the 'PassPhrase' of an 'EncryptedSigningKey' leaves the 'VerificationKey'
--   the same
prop_changingPassPhraseKeepsAddress :: Property
prop_changingPassPhraseKeepsAddress = property $ do
  passPhrase  <- forAll genPassPhrase
  passPhrase' <- forAll $ Gen.filter (/= passPhrase) genPassPhrase
  (_, oldKey) <- liftIO $ safeKeyGen passPhrase
  liftIO (changeEncPassphrase passPhrase passPhrase' oldKey) >>= \case
    Nothing     -> failure
    Just newKey -> encToVerification oldKey === encToVerification newKey

-- | Encrypting a 'SigningKey' preserves the corresponding 'VerificationKey'
prop_encryptionPreservesVerificationKey :: Property
prop_encryptionPreservesVerificationKey = property $ do
  sk <- forAll genSigningKey
  encToVerification (noPassEncrypt sk) === toVerification sk

-- | Making a 'SafeSigner' from a 'SigningKey' preserves the 'VerificationKey'
prop_safeSignerPreservesVerificationKey :: Property
prop_safeSignerPreservesVerificationKey = property $ do
  sk <- forAll genSigningKey
  safeToVerification (noPassSafeSigner sk) === toVerification sk
