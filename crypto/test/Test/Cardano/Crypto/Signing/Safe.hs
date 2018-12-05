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
  , encToPublic
  , noPassEncrypt
  , noPassSafeSigner
  , safeKeyGen
  , safeToPublic
  , toPublic
  , withSafeSigner
  )

import Test.Cardano.Crypto.Gen (genPassPhrase, genSecretKey)


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

-- | Changing the 'PassPhrase' of an 'EncryptedSecretKey' leaves the 'PublicKey'
--   the same
prop_changingPassPhraseKeepsAddress :: Property
prop_changingPassPhraseKeepsAddress = property $ do
  passPhrase  <- forAll genPassPhrase
  passPhrase' <- forAll $ Gen.filter (/= passPhrase) genPassPhrase
  (_, oldKey) <- liftIO $ safeKeyGen passPhrase
  liftIO (changeEncPassphrase passPhrase passPhrase' oldKey) >>= \case
    Nothing     -> failure
    Just newKey -> encToPublic oldKey === encToPublic newKey

-- | Encrypting a 'SecretKey' preserves the corresponding 'PublicKey'
prop_encryptionPreservesPublicKey :: Property
prop_encryptionPreservesPublicKey = property $ do
  sk <- forAll genSecretKey
  encToPublic (noPassEncrypt sk) === toPublic sk

-- | Making a 'SafeSigner' from a 'SecretKey' preserves the 'PublicKey'
prop_safeSignerPreservesPublicKey :: Property
prop_safeSignerPreservesPublicKey = property $ do
  sk <- forAll genSecretKey
  safeToPublic (noPassSafeSigner sk) === toPublic sk
