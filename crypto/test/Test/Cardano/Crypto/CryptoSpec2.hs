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
import Data.Coerce (coerce)
import Formatting (sformat)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  (Arbitrary(..), Property, ioProperty, property, vector, (===), (==>))

import Cardano.Binary.Class (Bi, serialize')
import qualified Cardano.Crypto as Crypto
import Cardano.Crypto.Limits (mlAbstractHash, mlPublicKey, mlSignature)

import Test.Cardano.Binary.Helpers (msgLenLimitedTest)
import Test.Cardano.Crypto.Arbitrary ()
import Test.Cardano.Crypto.Dummy (dummyProtocolMagic)


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
          msgLenLimitedTest @(Crypto.Signature ()) mlSignature
          msgLenLimitedTest @(Crypto.AbstractHash Blake2b_224 Void)
            mlAbstractHash
          msgLenLimitedTest @(Crypto.AbstractHash Blake2b_256 Void)
            mlAbstractHash

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
          prop
            "signature can't be verified with a different key"
            (proxySignVerifyDifferentKey @[Int32] @(Int32, Int32))
          prop
            "modified data signature can't be verified "
            (proxySignVerifyDifferentData @[Int32] @(Int32, Int32))
{- TODO: bring this back after validation rework
          prop
            "correct proxy signature schemes pass correctness check"
            (proxySecretKeyCheckCorrect @(Int32, Int32))
          prop
            "incorrect proxy signature schemes fails correctness check"
            (proxySecretKeyCheckIncorrect @(Int32, Int32))
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

checkSig
  :: Bi a
  => Crypto.ProtocolMagic
  -> Crypto.SignTag
  -> Crypto.PublicKey
  -> a
  -> Crypto.Signature a
  -> Bool
checkSig pm t k x s =
  Crypto.checkSigRaw pm (Just t) k (serialize' x) (coerce s)

keyDerivation :: Expectation
keyDerivation = do
  (pk, sk) <- Crypto.keyGen
  pk `shouldBe` Crypto.toPublic sk

keyParsing :: Crypto.PublicKey -> Property
keyParsing pk =
  Crypto.parseFullPublicKey (sformat Crypto.fullPublicKeyF pk) === Right pk

signThenVerify :: Bi a => Crypto.SignTag -> Crypto.SecretKey -> a -> Bool
signThenVerify t sk a =
  checkSig dummyProtocolMagic t (Crypto.toPublic sk) a
    $ Crypto.sign dummyProtocolMagic t sk a

signThenVerifyDifferentKey
  :: Bi a
  => Crypto.SignTag
  -> Crypto.SecretKey
  -> Crypto.PublicKey
  -> a
  -> Property
signThenVerifyDifferentKey t sk1 pk2 a = (Crypto.toPublic sk1 /= pk2) ==> not
  (checkSig dummyProtocolMagic t pk2 a $ Crypto.sign dummyProtocolMagic t sk1 a)

signThenVerifyDifferentData
  :: (Eq a, Bi a) => Crypto.SignTag -> Crypto.SecretKey -> a -> a -> Property
signThenVerifyDifferentData t sk a b = (a /= b) ==> not
  ( checkSig dummyProtocolMagic t (Crypto.toPublic sk) b
  $ Crypto.sign dummyProtocolMagic t sk a
  )

{- TODO: bring this back after validation rework
proxySecretKeyCheckCorrect
  :: Bi w => Crypto.SecretKey -> Crypto.SecretKey -> w -> Bool
proxySecretKeyCheckCorrect issuerSk delegateSk w = isRight
  (Crypto.validateProxySecretKey dummyProtocolMagic proxySk)
 where
  proxySk = Crypto.createPsk
    dummyProtocolMagic
    issuerSk
    (Crypto.toPublic delegateSk)
    w
-}

{- TODO: bring this back after validation rework
proxySecretKeyCheckIncorrect
  :: Bi w
  => Crypto.SecretKey
  -> Crypto.SecretKey
  -> Crypto.PublicKey
  -> w
  -> Property
proxySecretKeyCheckIncorrect issuerSk delegateSk pk2 w = do
  let
    psk = Crypto.createPsk
      dummyProtocolMagic
      issuerSk
      (Crypto.toPublic delegateSk)
      w
    wrongPsk = Crypto.unsafeProxySecretKey
      (Crypto.pskOmega psk)
      pk2
      (Crypto.pskDelegatePk psk)
      (Crypto.pskCert psk)
    fromRight :: forall e a. Either e a -> a
    fromRight (Right x) = x
    badMessage = fromRight $ decodeFullAnnotatedBytes
      "proxy secret key"
      Crypto.decodeAProxySecretKey
      (serialize wrongPsk)
      :: Crypto.AProxySecretKey w ByteString
  (Crypto.toPublic issuerSk /= pk2)
    ==> isLeft (Crypto.validateProxySecretKey dummyProtocolMagic badMessage)
-}

proxySignVerify
  :: (Bi a, Bi w, Eq w)
  => Crypto.SecretKey
  -> Crypto.SecretKey
  -> w
  -> a
  -> Bool
proxySignVerify issuerSk delegateSk w m = Crypto.proxyVerify
  dummyProtocolMagic
  Crypto.SignForTestingOnly
  signature
  (== w)
  m
 where
  proxySk = Crypto.createPsk
    dummyProtocolMagic
    issuerSk
    (Crypto.toPublic delegateSk)
    w
  signature = Crypto.proxySign
    dummyProtocolMagic
    Crypto.SignForTestingOnly
    delegateSk
    proxySk
    m

proxySignVerifyDifferentKey
  :: forall w a
   . (Bi a, Bi w, Eq w)
  => Crypto.SecretKey
  -> Crypto.SecretKey
  -> Crypto.PublicKey
  -> w
  -> a
  -> Property
proxySignVerifyDifferentKey issuerSk delegateSk pk2 w m =
  (Crypto.toPublic issuerSk /= pk2) ==> not
    (Crypto.proxyVerify
      dummyProtocolMagic
      Crypto.SignForTestingOnly
      sigBroken
      (== w)
      m
    )
 where
  proxySk = Crypto.createPsk
    dummyProtocolMagic
    issuerSk
    (Crypto.toPublic delegateSk)
    w
  signature = Crypto.proxySign
    dummyProtocolMagic
    Crypto.SignForTestingOnly
    delegateSk
    proxySk
    m

  sigBroken :: Crypto.ProxySignature w a
  sigBroken =
    signature { Crypto.psigPsk = proxySk { Crypto.pskIssuerPk = pk2 } }

proxySignVerifyDifferentData
  :: (Bi a, Eq a, Bi w, Eq w)
  => Crypto.SecretKey
  -> Crypto.SecretKey
  -> w
  -> a
  -> a
  -> Property
proxySignVerifyDifferentData issuerSk delegateSk w m m2 = (m /= m2) ==> not
  (Crypto.proxyVerify
    dummyProtocolMagic
    Crypto.SignForTestingOnly
    signature
    (== w)
    m2
  )
 where
  proxySk = Crypto.createPsk
    dummyProtocolMagic
    issuerSk
    (Crypto.toPublic delegateSk)
    w
  signature = Crypto.proxySign
    dummyProtocolMagic
    Crypto.SignForTestingOnly
    delegateSk
    proxySk
    m

redeemSignCheck :: Bi a => Crypto.RedeemSecretKey -> a -> Bool
redeemSignCheck redeemerSK a =
  Crypto.redeemCheckSig
      dummyProtocolMagic
      Crypto.SignForTestingOnly
      redeemerPK
      a
    $ Crypto.redeemSign
        dummyProtocolMagic
        Crypto.SignForTestingOnly
        redeemerSK
        a
  where redeemerPK = Crypto.redeemToPublic redeemerSK

redeemThenCheckDifferentKey
  :: Bi a => Crypto.RedeemSecretKey -> Crypto.RedeemPublicKey -> a -> Property
redeemThenCheckDifferentKey sk1 pk2 a =
  (Crypto.redeemToPublic sk1 /= pk2) ==> not
    ( Crypto.redeemCheckSig dummyProtocolMagic Crypto.SignForTestingOnly pk2 a
    $ Crypto.redeemSign dummyProtocolMagic Crypto.SignForTestingOnly sk1 a
    )

redeemThenCheckDifferentData
  :: (Eq a, Bi a) => Crypto.RedeemSecretKey -> a -> a -> Property
redeemThenCheckDifferentData sk a b = (a /= b) ==> not
  ( Crypto.redeemCheckSig
      dummyProtocolMagic
      Crypto.SignForTestingOnly
      (Crypto.redeemToPublic sk)
      b
  $ Crypto.redeemSign dummyProtocolMagic Crypto.SignForTestingOnly sk a
  )

packUnpackHDAddress :: Crypto.HDPassphrase -> [Word32] -> Bool
packUnpackHDAddress passphrase path =
  maybe False (== path) $ Crypto.unpackHDAddressAttr
    passphrase
    (Crypto.packHDAddressAttr passphrase path)

newtype Nonce = Nonce ByteString
    deriving (Show, Eq)

instance Arbitrary Nonce where
    arbitrary = Nonce . BS.pack <$> vector 12

encrypyDecryptChaChaPoly
  :: Nonce -> Crypto.HDPassphrase -> ByteString -> ByteString -> Bool
encrypyDecryptChaChaPoly (Nonce nonce) (Crypto.HDPassphrase key) header plaintext
  = (decrypt =<< (Crypto.toEither . encrypt $ plaintext)) == Right plaintext
 where
  encrypt = Crypto.encryptChaChaPoly nonce key header
  decrypt = Crypto.decryptChaChaPoly nonce key header

encrypyDecryptChaChaDifferentKey
  :: Nonce
  -> Crypto.HDPassphrase
  -> Crypto.HDPassphrase
  -> ByteString
  -> ByteString
  -> Property
encrypyDecryptChaChaDifferentKey (Nonce nonce) (Crypto.HDPassphrase key1) (Crypto.HDPassphrase key2) header plaintext
  = (key1 /= key2)
    ==> qcIsLeft (decrypt =<< (Crypto.toEither . encrypt $ plaintext))
 where
  encrypt = Crypto.encryptChaChaPoly nonce key1 header
  decrypt = Crypto.decryptChaChaPoly nonce key2 header

encrypyDecryptChaChaDifferentHeader
  :: Nonce
  -> Crypto.HDPassphrase
  -> ByteString
  -> ByteString
  -> ByteString
  -> Property
encrypyDecryptChaChaDifferentHeader (Nonce nonce) (Crypto.HDPassphrase key) header1 header2 plaintext
  = (header1 /= header2)
    ==> qcIsLeft (decrypt =<< (Crypto.toEither . encrypt $ plaintext))
 where
  encrypt = Crypto.encryptChaChaPoly nonce key header1
  decrypt = Crypto.decryptChaChaPoly nonce key header2

encrypyDecryptChaChaDifferentNonce
  :: Nonce
  -> Nonce
  -> Crypto.HDPassphrase
  -> ByteString
  -> ByteString
  -> Property
encrypyDecryptChaChaDifferentNonce (Nonce nonce1) (Nonce nonce2) (Crypto.HDPassphrase key) header plaintext
  = (nonce1 /= nonce2)
    ==> qcIsLeft (decrypt =<< (Crypto.toEither . encrypt $ plaintext))
 where
  encrypt = Crypto.encryptChaChaPoly nonce1 key header
  decrypt = Crypto.decryptChaChaPoly nonce2 key header

encToPublicToEnc :: Crypto.SecretKey -> Property
encToPublicToEnc =
  Crypto.encToPublic . Crypto.noPassEncrypt .=. Crypto.toPublic

skToSafeSigner :: Crypto.SecretKey -> Property
skToSafeSigner = Crypto.safeToPublic . Crypto.fakeSigner .=. Crypto.toPublic

matchingPassphraseWorks :: Crypto.PassPhrase -> Property
matchingPassphraseWorks passphrase = ioProperty $ do
  (_, key) <- Crypto.safeKeyGen passphrase
  Crypto.withSafeSigner key (return passphrase) (return . isJust)

mismatchingPassphraseFails :: Crypto.PassPhrase -> Crypto.PassPhrase -> Property
mismatchingPassphraseFails genPass signPass = ioProperty $ do
  (_, key) <- Crypto.safeKeyGen genPass
  Crypto.withSafeSigner key (return signPass)
    $ \signer -> return $ genPass /= signPass ==> property (isNothing signer)

passphraseChangeLeavesAddressUnmodified
  :: Crypto.PassPhrase -> Crypto.PassPhrase -> Property
passphraseChangeLeavesAddressUnmodified oldPass newPass = ioProperty $ do
  (_, oldKey) <- Crypto.safeKeyGen oldPass
  newKey      <-
    fromMaybe (panic "Passphrase didn't match")
      <$> Crypto.changeEncPassphrase oldPass newPass oldKey
  return $ Crypto.encToPublic oldKey === Crypto.encToPublic newKey
