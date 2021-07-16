module Test.Cardano.Chain.UTxO.Gen
  ( genCompactTxId,
    genCompactTxIn,
    genCompactTxOut,
    genVKWitness,
    genRedeemWitness,
    genTx,
    genTxAttributes,
    genTxAux,
    genTxHash,
    genTxId,
    genTxIn,
    genTxInList,
    genTxInWitness,
    genTxOut,
    genTxOutList,
    genTxPayload,
    genUTxOConfiguration,
    genTxProof,
    genTxSig,
    genTxSigData,
    genTxValidationError,
    genTxWitness,
    genUTxO,
    genUTxOError,
    genUTxOValidationError,
  )
where

import Cardano.Chain.Common (makeNetworkMagic, mkAttributes)
import Cardano.Chain.UTxO
  ( CompactTxId,
    CompactTxIn,
    CompactTxOut,
    Tx (..),
    TxAttributes,
    TxAux,
    TxId,
    TxIn (..),
    TxInWitness (..),
    TxOut (..),
    TxPayload,
    TxProof (..),
    TxSig,
    TxSigData (..),
    TxValidationError (..),
    TxWitness,
    UTxO,
    UTxOConfiguration (..),
    UTxOError (..),
    UTxOValidationError (..),
    fromList,
    mkTxAux,
    mkTxPayload,
    mkUTxOConfiguration,
    toCompactTxId,
    toCompactTxIn,
    toCompactTxOut,
  )
import Cardano.Crypto
  ( Hash,
    ProtocolMagicId,
    decodeHash,
    getProtocolMagicId,
    sign,
  )
import Cardano.Prelude
import Data.ByteString.Base16 as B16
import Data.Coerce (coerce)
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Chain.Common.Gen
  ( genAddress,
    genLovelace,
    genLovelaceError,
    genMerkleRoot,
    genNetworkMagic,
  )
import Test.Cardano.Crypto.Gen
  ( genAbstractHash,
    genProtocolMagic,
    genRedeemSignature,
    genRedeemVerificationKey,
    genSignTag,
    genSigningKey,
    genTextHash,
    genVerificationKey,
  )
import Test.Cardano.Prelude

genCompactTxId :: Gen CompactTxId
genCompactTxId = toCompactTxId <$> genTxId

genCompactTxIn :: Gen CompactTxIn
genCompactTxIn = toCompactTxIn <$> genTxIn

genCompactTxOut :: Gen CompactTxOut
genCompactTxOut = toCompactTxOut <$> genTxOut

genVKWitness :: ProtocolMagicId -> Gen TxInWitness
genVKWitness pm = VKWitness <$> genVerificationKey <*> genTxSig pm

genRedeemWitness :: ProtocolMagicId -> Gen TxInWitness
genRedeemWitness pm =
  RedeemWitness <$> genRedeemVerificationKey <*> genRedeemSignature pm genTxSigData

genTx :: Gen Tx
genTx = UnsafeTx <$> genTxInList <*> genTxOutList <*> genTxAttributes

genTxAttributes :: Gen TxAttributes
genTxAttributes = pure $ mkAttributes ()

genTxAux :: ProtocolMagicId -> Gen TxAux
genTxAux pm = mkTxAux <$> genTx <*> (genTxWitness pm)

genTxHash :: Gen (Hash Tx)
genTxHash = coerce <$> genTextHash

genTxId :: Gen TxId
genTxId = genBase16Text >>= pure . decodeHash >>= either panic pure
  where
    genBase16Text = decodeUtf8 <$> genBase16Bs

genBase16Bs :: Gen ByteString
genBase16Bs = B16.encode <$> genBytes 32

genTxIn :: Gen TxIn
genTxIn = TxInUtxo <$> genTxId <*> genWord32

genTxInList :: Gen (NonEmpty TxIn)
genTxInList = Gen.nonEmpty (Range.linear 1 20) genTxIn

genTxOut :: Gen TxOut
genTxOut = TxOut <$> genAddress <*> genLovelace

genTxOutList :: Gen (NonEmpty TxOut)
genTxOutList = Gen.nonEmpty (Range.linear 1 100) genTxOut

genUTxOConfiguration :: Gen UTxOConfiguration
genUTxOConfiguration =
  mkUTxOConfiguration
    <$> Gen.list (Range.linear 0 50) genAddress

genTxPayload :: ProtocolMagicId -> Gen TxPayload
genTxPayload pm = mkTxPayload <$> Gen.list (Range.linear 0 10) (genTxAux pm)

genTxProof :: ProtocolMagicId -> Gen TxProof
genTxProof pm =
  TxProof <$> genWord32 <*> genMerkleRoot genTx
    <*> genAbstractHash
      (Gen.list (Range.linear 1 5) (genTxWitness pm))

genTxSig :: ProtocolMagicId -> Gen TxSig
genTxSig pm = sign pm <$> genSignTag <*> genSigningKey <*> genTxSigData

genTxSigData :: Gen TxSigData
genTxSigData = TxSigData <$> genTxHash

genTxValidationError :: Gen TxValidationError
genTxValidationError = do
  pm <- genProtocolMagic
  let pmi = getProtocolMagicId pm
      nm = makeNetworkMagic pm
  Gen.choice
    [ TxValidationLovelaceError
        <$> Gen.text (Range.constant 0 1000) Gen.alphaNum
        <*> genLovelaceError,
      TxValidationFeeTooSmall <$> genTx <*> genLovelace <*> genLovelace,
      TxValidationWitnessWrongSignature
        <$> genTxInWitness pmi
        <*> pure pmi
        <*> genTxSigData,
      TxValidationWitnessWrongKey <$> genTxInWitness pmi <*> genAddress,
      TxValidationMissingInput <$> genTxIn,
      TxValidationNetworkMagicMismatch <$> genNetworkMagic <*> pure nm,
      TxValidationTxTooLarge
        <$> Gen.integral (Range.constant 0 1000)
        <*> Gen.integral (Range.constant 0 1000),
      pure TxValidationUnknownAddressAttributes,
      pure TxValidationUnknownAttributes
    ]

genTxInWitness :: ProtocolMagicId -> Gen TxInWitness
genTxInWitness pm = Gen.choice [genVKWitness pm, genRedeemWitness pm]

genTxWitness :: ProtocolMagicId -> Gen TxWitness
genTxWitness pm =
  V.fromList <$> Gen.list (Range.linear 1 10) (genTxInWitness pm)

genUTxO :: Gen UTxO
genUTxO = fromList <$> Gen.list (Range.constant 0 1000) genTxInTxOut
  where
    genTxInTxOut :: Gen (TxIn, TxOut)
    genTxInTxOut = (,) <$> genTxIn <*> genTxOut

genUTxOError :: Gen UTxOError
genUTxOError =
  Gen.choice
    [ UTxOMissingInput <$> genTxIn,
      pure UTxOOverlappingUnion
    ]

genUTxOValidationError :: Gen UTxOValidationError
genUTxOValidationError =
  Gen.choice
    [ UTxOValidationTxValidationError <$> genTxValidationError,
      UTxOValidationUTxOError <$> genUTxOError
    ]
