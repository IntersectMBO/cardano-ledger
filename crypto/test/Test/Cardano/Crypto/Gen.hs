{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Crypto.Gen
  (
        -- Protocol Magic Generator
    genProtocolMagic

        -- Sign Tag Generator
  , genSignTag

        -- Key Generators
  , genKeypair
  , genPublicKey
  , genSecretKey
  , genEncryptedSecretKey

        -- Redeem Key Generators
  , genRedeemKeypair
  , genRedeemPublicKey
  , genRedeemSecretKey

        -- Proxy Cert and Key Generators
  , genProxyCert
  , genProxySecretKey
  , genProxySignature

        -- Signature Generators
  , genSignature
  , genSignatureEncoded
  , genSigned
  , genRedeemSignature

        -- Hash Generators
  , genAbstractHash

        -- SafeSigner Generators
  , genSafeSigner

        -- PassPhrase Generators
  , genPassPhrase

        -- HD Generators
  , genHDPassphrase
  , genHDAddressPayload
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

import Cardano.Binary.Class (Bi, Raw(..))
import Cardano.Crypto (PassPhrase)
import Cardano.Crypto.Hashing
  (AbstractHash(..), Hash, HashAlgorithm, abstractHash, hash)
import Cardano.Crypto.HD (HDAddressPayload(..), HDPassphrase(..))
import Cardano.Crypto.ProtocolMagic (ProtocolMagic(..))
import Cardano.Crypto.Signing
  ( EncryptedSecretKey
  , ProxyCert
  , ProxySecretKey
  , ProxySignature
  , PublicKey
  , SafeSigner(..)
  , SecretKey
  , SignTag(..)
  , Signature
  , Signed
  , createPsk
  , deterministicKeyGen
  , mkSigned
  , noPassEncrypt
  , proxySign
  , safeCreateProxyCert
  , safeCreatePsk
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
genProtocolMagic = ProtocolMagic <$> Gen.int32 Range.constantBounded


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
  , SignMainBlockLight
  , SignMainBlockHeavy
  , SignProxySK
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

genRedeemKeypair :: Gen (Maybe (RedeemPublicKey, RedeemSecretKey))
genRedeemKeypair = redeemDeterministicKeyGen <$> gen32Bytes

genRedeemPublicKey :: Gen RedeemPublicKey
genRedeemPublicKey = do
  rkp <- genRedeemKeypair
  case rkp of
    Nothing      -> panic "Error generating a RedeemPublicKey."
    Just (pk, _) -> return pk

genRedeemSecretKey :: Gen RedeemSecretKey
genRedeemSecretKey = do
  rkp <- genRedeemKeypair
  case rkp of
    Nothing      -> panic "Error generating a RedeemSecretKey."
    Just (_, sk) -> return sk


--------------------------------------------------------------------------------
-- Proxy Cert and Key Generators
--------------------------------------------------------------------------------

genProxyCert :: Bi w => ProtocolMagic -> Gen w -> Gen (ProxyCert w)
genProxyCert pm genW =
  safeCreateProxyCert pm <$> genSafeSigner <*> genPublicKey <*> genW

genProxySecretKey :: Bi w => ProtocolMagic -> Gen w -> Gen (ProxySecretKey w)
genProxySecretKey pm genW =
  safeCreatePsk pm <$> genSafeSigner <*> genPublicKey <*> genW

genProxySignature
  :: (Bi w, Bi a) => ProtocolMagic -> Gen a -> Gen w -> Gen (ProxySignature w a)
genProxySignature pm genA genW = do
  delegateSk <- genSecretKey
  issuerSk   <- genSecretKey
  w          <- genW
  a          <- genA
  let psk = createPsk pm issuerSk (toPublic delegateSk) w
  return $ proxySign pm SignProxySK delegateSk psk a


--------------------------------------------------------------------------------
-- Signature Generators
--------------------------------------------------------------------------------

genSignature :: Bi a => ProtocolMagic -> Gen a -> Gen (Signature a)
genSignature pm genA = sign pm <$> genSignTag <*> genSecretKey <*> genA

genSignatureEncoded :: Gen ByteString -> Gen (Signature a)
genSignatureEncoded genB =
  signEncoded <$> genProtocolMagic <*> genSignTag <*> genSecretKey <*> genB

genSigned :: Bi a => Gen a -> Gen (Signed a)
genSigned genA =
  mkSigned <$> genProtocolMagic <*> genSignTag <*> genSecretKey <*> genA

genRedeemSignature :: Bi a => ProtocolMagic -> Gen a -> Gen (RedeemSignature a)
genRedeemSignature pm genA = redeemSign pm <$> gst <*> grsk <*> genA
 where
  gst  = genSignTag
  grsk = genRedeemSecretKey


--------------------------------------------------------------------------------
-- Hash Generators
--------------------------------------------------------------------------------

genAbstractHash
  :: (Bi a, HashAlgorithm algo) => Gen a -> Gen (AbstractHash algo a)
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
genSafeSigner = Gen.choice gens
 where
  gens =
    [ SafeSigner <$> genEncryptedSecretKey <*> genPassPhrase
    , FakeSigner <$> genSecretKey
    ]


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

feedPM :: (ProtocolMagic -> Gen a) -> Gen a
feedPM genA = genA =<< genProtocolMagic
