{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Cardano.Crypto specification

module Test.Cardano.Crypto.CryptoSpec2
  ( spec
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Crypto.Hash (Blake2b_224, Blake2b_256)
import qualified Data.ByteString as BS
import Formatting (sformat)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  (Arbitrary(..), Property, ioProperty, property, vector, (===), (==>))

import Cardano.Binary.Class (Bi)
import Cardano.Crypto
  ( AbstractHash
  , HDPassphrase(..)
  , PassPhrase
  , PublicKey
  , RedeemPublicKey
  , RedeemSecretKey
  , SafeSigner
  , SecretKey
  , SignTag(SignForTestingOnly)
  , Signature
  , changeEncPassphrase
  , decryptChaChaPoly
  , encToPublic
  , encryptChaChaPoly
  , fullPublicKeyF
  , keyGen
  , noPassEncrypt
  , noPassSafeSigner
  , packHDAddressAttr
  , parseFullPublicKey
  , proxySign
  , proxyVerify
  , redeemSign
  , redeemToPublic
  , createPsk
  , safeKeyGen
  , safeToPublic
  , sign
  , toEither
  , toPublic
  , unpackHDAddressAttr
  , verifyRedeemSig
  , verifySignature
  , withSafeSigner
  )
import Cardano.Crypto.Limits (mlAbstractHash, mlPublicKey, mlSignature)

import Test.Cardano.Binary.Helpers (msgLenLimitedTest)
import Test.Cardano.Crypto.Arbitrary ()
import Test.Cardano.Crypto.Dummy (dummyProtocolMagicId)


spec :: Spec
spec =
  describe "Crypto"
    . describe "Signing"
    $ do
        describe "SafeSigning" $ do
          prop "passphrase matches"       matchingPassphraseWorks
          prop "passphrase doesn't match" mismatchingPassphraseFails
          prop -- if you see this case failing, then passphrase changing endpoint
               -- in wallets have to be reconsidered
            "passphrase change doesn't modify key address"
            passphraseChangeLeavesAddressUnmodified

        describe "Identity testing" $ describe "msgLenLimitedTest" $ do
          msgLenLimitedTest mlPublicKey
          msgLenLimitedTest @(Signature ()) mlSignature
          msgLenLimitedTest @(AbstractHash Blake2b_224 Void) mlAbstractHash
          msgLenLimitedTest @(AbstractHash Blake2b_256 Void) mlAbstractHash

        describe "keys" $ do
          it "derived pubkey equals to generated pubkey" keyDerivation
          prop "formatted key can be parsed back" keyParsing
        describe "signing" $ do
          prop
            "signed data can be verified successfully"
            (signThenVerify @[Int32])
          prop
            "signed data can't be verified by a different key"
            (signThenVerifyDifferentKey @[Int32])
          prop
            "modified data signature can't be verified"
            (signThenVerifyDifferentData @[Int32])
        describe "proxy signature scheme" $ do
          prop
            "signature can be verified successfully"
            (proxySignVerify @[Int32] @(Int32, Int32))
          -- prop
          --   "signature can't be verified with a different key"
          --   (proxySignVerifyDifferentKey @[Int32] @(Int32, Int32))
          prop
            "modified data signature can't be verified "
            (proxySignVerifyDifferentData @[Int32] @(Int32, Int32))
{- TODO: bring this back after validation rework
          prop
            "correct proxy signature schemes pass correctness check"
            (proxyVerificationKeyCheckCorrect @(Int32, Int32))
          prop
            "incorrect proxy signature schemes fails correctness check"
            (proxyVerificationKeyCheckIncorrect @(Int32, Int32))
-}
        describe "redeemer signatures" $ do
          prop
            "signature can be verified successfully"
            (redeemSignCheck @[Int32])
          prop
            "signature can't be verified with a different key"
            (redeemThenCheckDifferentKey @[Int32])
          prop
            "modified data signature can't be verified "
            (redeemThenCheckDifferentData @[Int32])

        describe "HD wallet" $ do
          prop "pack/unpack address payload"        packUnpackHDAddress
          prop "decryptChaCha . encryptChaCha = id" encrypyDecryptChaChaPoly
          prop
            "signed data can't be verified with a different key"
            encrypyDecryptChaChaDifferentKey
          prop
            "signed data can't be verified with a different header"
            encrypyDecryptChaChaDifferentHeader
          prop
            "signed data can't be verified with a different nonce"
            encrypyDecryptChaChaDifferentNonce

        describe "Safe Signing" $ do
          prop
            "turning a secret key into an encrypted secret key and this encrypted\
                 \ secret key into a public key is the same as turning the secret key\
                 \ into a public key"
            encToPublicToEnc
          prop
            "turning a secret key into an safe signer and this safe signer into a\
                 \ public key is the same as turning the secret key into a public key"
            skToSafeSigner

keyDerivation :: Expectation
keyDerivation = do
  (pk, sk) <- keyGen
  pk `shouldBe` toPublic sk

keyParsing :: PublicKey -> Property
keyParsing pk = parseFullPublicKey (sformat fullPublicKeyF pk) === Right pk

signThenVerify :: Bi a => SignTag -> SecretKey -> a -> Bool
signThenVerify t sk a =
  verifySignature dummyProtocolMagicId t (toPublic sk) a
    $ sign dummyProtocolMagicId t sk a

signThenVerifyDifferentKey
  :: Bi a => SignTag -> SecretKey -> PublicKey -> a -> Property
signThenVerifyDifferentKey t sk1 pk2 a = (toPublic sk1 /= pk2) ==> not
  ( verifySignature dummyProtocolMagicId t pk2 a
  $ sign dummyProtocolMagicId t sk1 a
  )

signThenVerifyDifferentData
  :: (Eq a, Bi a) => SignTag -> SecretKey -> a -> a -> Property
signThenVerifyDifferentData t sk a b = (a /= b) ==> not
  ( verifySignature dummyProtocolMagicId t (toPublic sk) b
  $ sign dummyProtocolMagicId t sk a
  )

{- TODO: bring this back after validation rework
proxyVerificationKeyCheckCorrect
  :: Bi w => SecretKey -> SecretKey -> w -> Bool
proxyVerificationKeyCheckCorrect issuerSk delegateSk w = isRight
  (validateProxyVerificationKey dummyProtocolMagic proxySk)
 where
  proxySk = createPsk
    dummyProtocolMagic
    issuerSk
    (toPublic delegateSk)
    w
-}

{- TODO: bring this back after validation rework
proxyVerificationKeyCheckIncorrect
  :: Bi w
  => SecretKey
  -> SecretKey
  -> PublicKey
  -> w
  -> Property
proxyVerificationKeyCheckIncorrect issuerSk delegateSk pk2 w = do
  let
    psk = createPsk
      dummyProtocolMagic
      issuerSk
      (toPublic delegateSk)
      w
    wrongPsk = unsafeProxyVerificationKey
      (pskOmega psk)
      pk2
      (pskDelegatePk psk)
      (pskCert psk)
    fromRight :: forall e a. Either e a -> a
    fromRight (Right x) = x
    badMessage = fromRight $ decodeFullAnnotatedBytes
      "proxy secret key"
      decodeAProxyVerificationKey
      (serialize wrongPsk)
      :: AProxyVerificationKey w ByteString
  (toPublic issuerSk /= pk2)
    ==> isLeft (validateProxyVerificationKey dummyProtocolMagic badMessage)
-}

proxySignVerify
  :: (Bi a, Bi w, Eq w) => SafeSigner -> SecretKey -> w -> a -> Bool
proxySignVerify issuerSafeSigner delegateSk w m = proxyVerify
  dummyProtocolMagicId
  SignForTestingOnly
  signature
  (== w)
  m
 where
  proxySk =
    createPsk dummyProtocolMagicId issuerSafeSigner (toPublic delegateSk) w
  signature =
    proxySign dummyProtocolMagicId SignForTestingOnly delegateSk proxySk m

-- TODO: Make this test redundant by disallowing invalid `ProxySignature`s
-- proxySignVerifyDifferentKey
--   :: forall w a
--    . (Bi a, Bi w, Eq w)
--   => SafeSigner
--   -> SecretKey
--   -> SafeSigner
--   -> w
--   -> a
--   -> Property
-- proxySignVerifyDifferentKey issuerSafeSigner delegateSk issuerSafeSigner' w m =
--   (safeToPublic issuerSafeSigner /= safeToPublic issuerSafeSigner') ==> not
--     (proxyVerify dummyProtocolMagic SignForTestingOnly sigBroken (== w) m)
--  where
--   psk = createPsk dummyProtocolMagic issuerSafeSigner (toPublic delegateSk) w
--   psk' = createPsk dummyProtocolMagic issuerSafeSigner' (toPublic delegateSk) w
--   signature =
--     proxySign dummyProtocolMagic SignForTestingOnly delegateSk proxySk m

--   sigBroken :: ProxySignature w a
--   sigBroken = signature { psigPsk = proxySk { pskIssuerPk = pk2 } }

proxySignVerifyDifferentData
  :: (Bi a, Eq a, Bi w, Eq w)
  => SafeSigner
  -> SecretKey
  -> w
  -> a
  -> a
  -> Property
proxySignVerifyDifferentData issuerSafeSigner delegateSk w m m2 =
  (m /= m2) ==> not
    (proxyVerify dummyProtocolMagicId SignForTestingOnly signature (== w) m2)
 where
  proxySk =
    createPsk dummyProtocolMagicId issuerSafeSigner (toPublic delegateSk) w
  signature =
    proxySign dummyProtocolMagicId SignForTestingOnly delegateSk proxySk m

redeemSignCheck :: Bi a => RedeemSecretKey -> a -> Bool
redeemSignCheck redeemerSK a =
  verifyRedeemSig dummyProtocolMagicId SignForTestingOnly redeemerPK a
    $ redeemSign dummyProtocolMagicId SignForTestingOnly redeemerSK a
  where redeemerPK = redeemToPublic redeemerSK

redeemThenCheckDifferentKey
  :: Bi a => RedeemSecretKey -> RedeemPublicKey -> a -> Property
redeemThenCheckDifferentKey sk1 pk2 a = (redeemToPublic sk1 /= pk2) ==> not
  ( verifyRedeemSig dummyProtocolMagicId SignForTestingOnly pk2 a
  $ redeemSign dummyProtocolMagicId SignForTestingOnly sk1 a
  )

redeemThenCheckDifferentData
  :: (Eq a, Bi a) => RedeemSecretKey -> a -> a -> Property
redeemThenCheckDifferentData sk a b = (a /= b) ==> not
  (verifyRedeemSig dummyProtocolMagicId SignForTestingOnly (redeemToPublic sk) b
  $ redeemSign dummyProtocolMagicId SignForTestingOnly sk a
  )

packUnpackHDAddress :: HDPassphrase -> [Word32] -> Bool
packUnpackHDAddress passphrase path =
  maybe False (== path)
    $ unpackHDAddressAttr passphrase (packHDAddressAttr passphrase path)

newtype Nonce = Nonce ByteString
    deriving (Show, Eq)

instance Arbitrary Nonce where
    arbitrary = Nonce . BS.pack <$> vector 12

encrypyDecryptChaChaPoly
  :: Nonce -> HDPassphrase -> ByteString -> ByteString -> Bool
encrypyDecryptChaChaPoly (Nonce nonce) (HDPassphrase key) header plaintext =
  (decrypt =<< (toEither . encrypt $ plaintext)) == Right plaintext
 where
  encrypt = encryptChaChaPoly nonce key header
  decrypt = decryptChaChaPoly nonce key header

encrypyDecryptChaChaDifferentKey
  :: Nonce
  -> HDPassphrase
  -> HDPassphrase
  -> ByteString
  -> ByteString
  -> Property
encrypyDecryptChaChaDifferentKey (Nonce nonce) (HDPassphrase key1) (HDPassphrase key2) header plaintext
  = (key1 /= key2) ==> qcIsLeft (decrypt =<< (toEither . encrypt $ plaintext))
 where
  encrypt = encryptChaChaPoly nonce key1 header
  decrypt = decryptChaChaPoly nonce key2 header

encrypyDecryptChaChaDifferentHeader
  :: Nonce -> HDPassphrase -> ByteString -> ByteString -> ByteString -> Property
encrypyDecryptChaChaDifferentHeader (Nonce nonce) (HDPassphrase key) header1 header2 plaintext
  = (header1 /= header2)
    ==> qcIsLeft (decrypt =<< (toEither . encrypt $ plaintext))
 where
  encrypt = encryptChaChaPoly nonce key header1
  decrypt = decryptChaChaPoly nonce key header2

encrypyDecryptChaChaDifferentNonce
  :: Nonce -> Nonce -> HDPassphrase -> ByteString -> ByteString -> Property
encrypyDecryptChaChaDifferentNonce (Nonce nonce1) (Nonce nonce2) (HDPassphrase key) header plaintext
  = (nonce1 /= nonce2)
    ==> qcIsLeft (decrypt =<< (toEither . encrypt $ plaintext))
 where
  encrypt = encryptChaChaPoly nonce1 key header
  decrypt = decryptChaChaPoly nonce2 key header

encToPublicToEnc :: SecretKey -> Property
encToPublicToEnc = encToPublic . noPassEncrypt .=. toPublic

skToSafeSigner :: SecretKey -> Property
skToSafeSigner = safeToPublic . noPassSafeSigner .=. toPublic

matchingPassphraseWorks :: PassPhrase -> Property
matchingPassphraseWorks passphrase = ioProperty $ do
  (_, key) <- safeKeyGen passphrase
  withSafeSigner key (return passphrase) (return . isJust)

mismatchingPassphraseFails :: PassPhrase -> PassPhrase -> Property
mismatchingPassphraseFails genPass signPass = ioProperty $ do
  (_, key) <- safeKeyGen genPass
  withSafeSigner key (return signPass)
    $ \signer -> return $ genPass /= signPass ==> property (isNothing signer)

passphraseChangeLeavesAddressUnmodified :: PassPhrase -> PassPhrase -> Property
passphraseChangeLeavesAddressUnmodified oldPass newPass = ioProperty $ do
  (_, oldKey) <- safeKeyGen oldPass
  newKey      <-
    fromMaybe (panic "Passphrase didn't match")
      <$> changeEncPassphrase oldPass newPass oldKey
  return $ encToPublic oldKey === encToPublic newKey
