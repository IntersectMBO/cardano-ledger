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
  , genVerificationKey
  , genSigningKey
  , genEncryptedSigningKey

  -- * Redeem Key Generators
  , genRedeemKeypair
  , genRedeemVerificationKey
  , genRedeemSigningKey

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
  ( EncryptedSigningKey
  , ProxyCert
  , ProxyVerificationKey
  , ProxySignature
  , VerificationKey
  , SafeSigner(..)
  , SigningKey
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
  , toVerification
  )
import Cardano.Crypto.Signing.Redeem
  ( RedeemVerificationKey
  , RedeemSigningKey
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

genKeypair :: Gen (VerificationKey, SigningKey)
genKeypair = deterministicKeyGen <$> gen32Bytes

genVerificationKey :: Gen VerificationKey
genVerificationKey = fst <$> genKeypair

genSigningKey :: Gen SigningKey
genSigningKey = snd <$> genKeypair

genEncryptedSigningKey :: Gen EncryptedSigningKey
genEncryptedSigningKey = noPassEncrypt <$> genSigningKey


--------------------------------------------------------------------------------
-- Redeem Key Generators
--------------------------------------------------------------------------------

genRedeemKeypair :: Gen (RedeemVerificationKey, RedeemSigningKey)
genRedeemKeypair = Gen.just $ redeemDeterministicKeyGen <$> gen32Bytes

genRedeemVerificationKey :: Gen RedeemVerificationKey
genRedeemVerificationKey = fst <$> genRedeemKeypair

genRedeemSigningKey :: Gen RedeemSigningKey
genRedeemSigningKey = snd <$> genRedeemKeypair


--------------------------------------------------------------------------------
-- Proxy Cert and Key Generators
--------------------------------------------------------------------------------

genProxyCert :: ToCBOR w => ProtocolMagicId -> Gen w -> Gen (ProxyCert w)
genProxyCert pm genW =
  safeCreateProxyCert pm <$> genSafeSigner <*> genVerificationKey <*> genW

genProxyVerificationKey
  :: ToCBOR w => ProtocolMagicId -> Gen w -> Gen (ProxyVerificationKey w)
genProxyVerificationKey pm genW =
  createPsk pm <$> genSafeSigner <*> genVerificationKey <*> genW

genProxySignature
  :: (ToCBOR w, ToCBOR a)
  => ProtocolMagicId
  -> Gen a
  -> Gen w
  -> Gen (ProxySignature w a)
genProxySignature pm genA genW = do
  delegateSk <- genSigningKey
  issuerSafeSigner <- genSafeSigner
  w          <- genW
  a          <- genA
  let psk = createPsk pm issuerSafeSigner (toVerification delegateSk) w
  return $ proxySign pm SignProxyVK delegateSk psk a


--------------------------------------------------------------------------------
-- Signature Generators
--------------------------------------------------------------------------------

genSignature :: ToCBOR a => ProtocolMagicId -> Gen a -> Gen (Signature a)
genSignature pm genA = sign pm <$> genSignTag <*> genSigningKey <*> genA

genSignatureEncoded :: Gen ByteString -> Gen (Signature a)
genSignatureEncoded genB =
  signEncoded <$> genProtocolMagicId <*> genSignTag <*> genSigningKey <*> genB

genRedeemSignature
  :: ToCBOR a => ProtocolMagicId -> Gen a -> Gen (RedeemSignature a)
genRedeemSignature pm genA = redeemSign pm <$> gst <*> grsk <*> genA
 where
  gst  = genSignTag
  grsk = genRedeemSigningKey


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
genSafeSigner = SafeSigner <$> genEncryptedSigningKey <*> pure emptyPassphrase


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
