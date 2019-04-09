{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.HD
  ( tests
  )
where

import Cardano.Prelude

import Hedgehog
  (Gen, Property, assert, checkParallel, discover, forAll, property, tripping)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Crypto.HD
  ( HDPassphrase(..)
  , decryptChaChaPoly
  , encryptChaChaPoly
  , packHDAddressAttr
  , toEither
  , unpackHDAddressAttr
  )

import Test.Cardano.Crypto.Gen (genHDPassphrase)


--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover


--------------------------------------------------------------------------------
-- HD Properties
--------------------------------------------------------------------------------

-- | Packing and unpacking an HD address is the identity function
prop_packUnpackHDAddress :: Property
prop_packUnpackHDAddress = property $ do
  passPhrase <- forAll genHDPassphrase
  path       <- forAll
    $ Gen.list (Range.constant 0 10) (Gen.word32 Range.constantBounded)

  tripping path (packHDAddressAttr passPhrase) (unpackHDAddressAttr passPhrase)


-- | Encrypting then decrypting is the identity function
prop_encryptDecryptChaChaPoly :: Property
prop_encryptDecryptChaChaPoly = property $ do
  nonce            <- forAll genNonce
  HDPassphrase key <- forAll genHDPassphrase
  header           <- forAll genHeader
  plaintext        <- forAll genPlaintext

  tripping
    plaintext
    (toEither . encryptChaChaPoly nonce key header)
    (>>= decryptChaChaPoly nonce key header)


-- | Cannot decrypt with a different key than the encryption key
prop_encryptDecryptDifferentKey :: Property
prop_encryptDecryptDifferentKey = property $ do
  nonce                         <- forAll genNonce
  passphrase@(HDPassphrase key) <- forAll genHDPassphrase
  HDPassphrase key' <- forAll $ Gen.filter (/= passphrase) genHDPassphrase
  header                        <- forAll genHeader
  plaintext                     <- forAll genPlaintext

  assert
    .   isLeft
    $   toEither (encryptChaChaPoly nonce key header plaintext)
    >>= decryptChaChaPoly nonce key' header


-- | Cannot decrypt with an incorrect header
prop_encryptDecryptDifferentHeader :: Property
prop_encryptDecryptDifferentHeader = property $ do
  nonce            <- forAll genNonce
  HDPassphrase key <- forAll genHDPassphrase
  header           <- forAll genHeader
  header'          <- forAll $ Gen.filter (/= header) genHeader
  plaintext        <- forAll genPlaintext

  assert
    .   isLeft
    $   toEither (encryptChaChaPoly nonce key header plaintext)
    >>= decryptChaChaPoly nonce key header'


-- | Cannot decrypt with an incorrect nonce
prop_encryptDecryptDifferentNonce :: Property
prop_encryptDecryptDifferentNonce = property $ do
  nonce            <- forAll genNonce
  nonce'           <- forAll $ Gen.filter (/= nonce) genNonce
  HDPassphrase key <- forAll genHDPassphrase
  header           <- forAll genHeader
  plaintext        <- forAll genPlaintext

  assert
    .   isLeft
    $   toEither (encryptChaChaPoly nonce key header plaintext)
    >>= decryptChaChaPoly nonce' key header


--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genNonce :: Gen ByteString
genNonce = Gen.bytes (Range.singleton 12)

genHeader :: Gen ByteString
genHeader = Gen.bytes (Range.constant 0 20)

genPlaintext :: Gen ByteString
genPlaintext = Gen.bytes (Range.constant 0 100)
