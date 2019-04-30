{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Crypto.Gen
  (
  -- * Protocol Magic Generator
    genProtocolMagic
  , genProtocolMagicId

  -- * Sign Tag Generator
  , genSignTag

  -- * Key Generators
  , genKeypair
  , genPublicKey
  , genSecretKey
  , genEncryptedSecretKey

  -- * Redeem Key Generators
  , genRedeemKeypair
  , genRedeemPublicKey
  , genRedeemSecretKey

  -- * Proxy Cert and Key Generators
  , genProxyCert
  , genProxyVerificationKey
  , genProxySignature

  -- * Signature Generators
  , genSignature
  , genSignatureEncoded
  , genRedeemSignature

  -- * Hash Generators
  , genAbstractHash

  -- * SafeSigner Generators
  , genSafeSigner

  -- * PassPhrase Generators
  , genPassPhrase

  -- * HD Generators
  , genHDPassphrase
  , genHDAddressPayload

  -- * Helper Generators
  , genHashRaw
  , genTextHash
  , feedPM
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.ByteArray as ByteArray
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Binary (Annotated(..), Raw(..), ToCBOR)
import Cardano.Crypto (PassPhrase)
import Cardano.Crypto.Hashing
  (AbstractHash(..), Hash, HashAlgorithm, abstractHash, hash)
import Cardano.Crypto.HD (HDAddressPayload(..), HDPassphrase(..))
import Cardano.Crypto.ProtocolMagic
  ( AProtocolMagic(..)
  , ProtocolMagic
  , ProtocolMagicId(..)
  , RequiresNetworkMagic(..)
  )
import Cardano.Crypto.Signing
  ( EncryptedSecretKey
  , ProxyCert
  , ProxyVerificationKey
  , ProxySignature
  , PublicKey
  , SafeSigner(..)
  , SecretKey
  , SignTag(..)
  , Signature
  , createPsk
  , deterministicKeyGen
  , emptyPassphrase
  , noPassEncrypt
  , proxySign
  , safeCreateProxyCert
  , sign
  , signEncoded
  , toPublic
  )
import Cardano.Crypto.Signing.Redeem
  ( RedeemPublicKey
  , RedeemSecretKey
  , RedeemSignature
  , redeemDeterministicKeyGen
  , redeemSign
  )


--------------------------------------------------------------------------------
-- Protocol Magic Generator
--------------------------------------------------------------------------------

genProtocolMagic :: Gen ProtocolMagic
genProtocolMagic =
  AProtocolMagic
    <$> (Annotated <$> genProtocolMagicId <*> pure ())
    <*> genRequiresNetworkMagic

genProtocolMagicId :: Gen ProtocolMagicId
genProtocolMagicId = ProtocolMagicId <$> Gen.word32 Range.constantBounded

genRequiresNetworkMagic :: Gen RequiresNetworkMagic
genRequiresNetworkMagic = Gen.element [RequiresNoMagic, RequiresMagic]

--------------------------------------------------------------------------------
-- Sign Tag Generator
--------------------------------------------------------------------------------

genSignTag :: Gen SignTag
genSignTag = Gen.element
  [ SignForTestingOnly
  , SignTx
  , SignRedeemTx
  , SignVssCert
  , SignUSProposal
  , SignCommitment
  , SignUSVote
  , SignMainBlock
  , SignMainBlockHeavy
  , SignProxyVK
  ]


--------------------------------------------------------------------------------
-- Key Generators
--------------------------------------------------------------------------------

genKeypair :: Gen (PublicKey, SecretKey)
genKeypair = deterministicKeyGen <$> gen32Bytes

genPublicKey :: Gen PublicKey
genPublicKey = fst <$> genKeypair

genSecretKey :: Gen SecretKey
genSecretKey = snd <$> genKeypair

genEncryptedSecretKey :: Gen EncryptedSecretKey
genEncryptedSecretKey = noPassEncrypt <$> genSecretKey


--------------------------------------------------------------------------------
-- Redeem Key Generators
--------------------------------------------------------------------------------

genRedeemKeypair :: Gen (RedeemPublicKey, RedeemSecretKey)
genRedeemKeypair = Gen.just $ redeemDeterministicKeyGen <$> gen32Bytes

genRedeemPublicKey :: Gen RedeemPublicKey
genRedeemPublicKey = fst <$> genRedeemKeypair

genRedeemSecretKey :: Gen RedeemSecretKey
genRedeemSecretKey = snd <$> genRedeemKeypair


--------------------------------------------------------------------------------
-- Proxy Cert and Key Generators
--------------------------------------------------------------------------------

genProxyCert :: ToCBOR w => ProtocolMagicId -> Gen w -> Gen (ProxyCert w)
genProxyCert pm genW =
  safeCreateProxyCert pm <$> genSafeSigner <*> genPublicKey <*> genW

genProxyVerificationKey
  :: ToCBOR w => ProtocolMagicId -> Gen w -> Gen (ProxyVerificationKey w)
genProxyVerificationKey pm genW =
  createPsk pm <$> genSafeSigner <*> genPublicKey <*> genW

genProxySignature
  :: (ToCBOR w, ToCBOR a)
  => ProtocolMagicId
  -> Gen a
  -> Gen w
  -> Gen (ProxySignature w a)
genProxySignature pm genA genW = do
  delegateSk <- genSecretKey
  issuerSafeSigner <- genSafeSigner
  w          <- genW
  a          <- genA
  let psk = createPsk pm issuerSafeSigner (toPublic delegateSk) w
  return $ proxySign pm SignProxyVK delegateSk psk a


--------------------------------------------------------------------------------
-- Signature Generators
--------------------------------------------------------------------------------

genSignature :: ToCBOR a => ProtocolMagicId -> Gen a -> Gen (Signature a)
genSignature pm genA = sign pm <$> genSignTag <*> genSecretKey <*> genA

genSignatureEncoded :: Gen ByteString -> Gen (Signature a)
genSignatureEncoded genB =
  signEncoded <$> genProtocolMagicId <*> genSignTag <*> genSecretKey <*> genB

genRedeemSignature
  :: ToCBOR a => ProtocolMagicId -> Gen a -> Gen (RedeemSignature a)
genRedeemSignature pm genA = redeemSign pm <$> gst <*> grsk <*> genA
 where
  gst  = genSignTag
  grsk = genRedeemSecretKey


--------------------------------------------------------------------------------
-- Hash Generators
--------------------------------------------------------------------------------

genAbstractHash
  :: (ToCBOR a, HashAlgorithm algo) => Gen a -> Gen (AbstractHash algo a)
genAbstractHash genA = abstractHash <$> genA


--------------------------------------------------------------------------------
-- PassPhrase Generators
--------------------------------------------------------------------------------

genPassPhrase :: Gen PassPhrase
genPassPhrase = ByteArray.pack <$> genWord8List
 where
  genWord8List :: Gen [Word8]
  genWord8List =
    Gen.list (Range.singleton 32) (Gen.word8 Range.constantBounded)


--------------------------------------------------------------------------------
-- SafeSigner Generators
--------------------------------------------------------------------------------

genSafeSigner :: Gen SafeSigner
genSafeSigner = SafeSigner <$> genEncryptedSecretKey <*> pure emptyPassphrase


--------------------------------------------------------------------------------
-- HD Generators
--------------------------------------------------------------------------------

genHDPassphrase :: Gen HDPassphrase
genHDPassphrase = HDPassphrase <$> gen32Bytes

genHDAddressPayload :: Gen HDAddressPayload
genHDAddressPayload = HDAddressPayload <$> gen32Bytes


--------------------------------------------------------------------------------
-- Helper Generators
--------------------------------------------------------------------------------

genHashRaw :: Gen (Hash Raw)
genHashRaw = genAbstractHash $ Raw <$> gen32Bytes

genTextHash :: Gen (Hash Text)
genTextHash = do
  sampleText <- Gen.text (Range.linear 0 10) Gen.alphaNum
  pure (hash sampleText :: Hash Text)

feedPM :: (ProtocolMagicId -> Gen a) -> Gen a
feedPM genA = genA =<< genProtocolMagicId
