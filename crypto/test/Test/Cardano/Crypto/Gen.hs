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
import Data.Coerce (coerce)
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
  , VerificationKey
  , SafeSigner(..)
  , SigningKey
  , SignTag(..)
  , Signature
  , deterministicKeyGen
  , emptyPassphrase
  , noPassEncrypt
  , sign
  , signRaw
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
genSignTag = Gen.choice
  [ pure SignForTestingOnly
  , pure SignTx
  , pure SignRedeemTx
  , pure SignVssCert
  , pure SignUSProposal
  , pure SignCommitment
  , pure SignUSVote
  , SignBlock <$> genVerificationKey
  , pure SignCertificate
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
-- Signature Generators
--------------------------------------------------------------------------------

genSignature :: ToCBOR a => ProtocolMagicId -> Gen a -> Gen (Signature a)
genSignature pm genA = sign pm <$> genSignTag <*> genSigningKey <*> genA

genSignatureEncoded :: Gen ByteString -> Gen (Signature a)
genSignatureEncoded genB =
  coerce . signRaw <$> genProtocolMagicId <*> (Just <$> genSignTag) <*> genSigningKey <*> genB

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
genTextHash = hash <$> Gen.text (Range.linear 0 10) Gen.alphaNum

feedPM :: (ProtocolMagicId -> Gen a) -> Gen a
feedPM genA = genA =<< genProtocolMagicId
