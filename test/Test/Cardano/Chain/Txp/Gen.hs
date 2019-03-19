module Test.Cardano.Chain.Txp.Gen
  ( genPkWitness
  , genRedeemWitness
  , genTx
  , genTxAttributes
  , genTxAux
  , genTxHash
  , genTxId
  , genTxIn
  , genTxInList
  , genTxInWitness
  , genTxOut
  , genTxOutAux
  , genTxOutList
  , genTxpUndo
  , genTxPayload
  , genTxpConfiguration
  , genTxProof
  , genTxSig
  , genTxSigData
  , genTxUndo
  , genTxWitness
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.ByteString.Base16 as B16
import Data.Coerce (coerce)
import qualified Data.Set as S
import qualified Data.Vector as V

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Common (mkAttributes)
import Cardano.Chain.Txp
  ( Tx(..)
  , TxAttributes
  , TxAux
  , TxId
  , TxIn(..)
  , TxInWitness(..)
  , TxOut(..)
  , TxOutAux(..)
  , TxPayload
  , TxProof(..)
  , TxSig
  , TxSigData(..)
  , TxUndo
  , TxWitness
  , TxpConfiguration(..)
  , TxpUndo
  , mkTxAux
  , mkTxPayload
  )
import Cardano.Crypto (Hash, ProtocolMagicId, decodeHash, sign)

import Test.Cardano.Chain.Common.Gen (genAddress, genLovelace, genMerkleRoot)
import Test.Cardano.Crypto.Gen
  ( genAbstractHash
  , genPublicKey
  , genRedeemPublicKey
  , genRedeemSignature
  , genSecretKey
  , genSignTag
  , genTextHash
  )


genPkWitness :: ProtocolMagicId -> Gen TxInWitness
genPkWitness pm = PkWitness <$> genPublicKey <*> genTxSig pm

genRedeemWitness :: ProtocolMagicId -> Gen TxInWitness
genRedeemWitness pm =
  RedeemWitness <$> genRedeemPublicKey <*> genRedeemSignature pm genTxSigData

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
  where genBase16Text = decodeUtf8 <$> genBase16Bs

genBase16Bs :: Gen ByteString
genBase16Bs = B16.encode <$> genBytes 32

genTxIn :: Gen TxIn
genTxIn = TxInUtxo <$> genTxId <*> genWord32

genTxInList :: Gen (NonEmpty TxIn)
genTxInList = Gen.nonEmpty (Range.linear 1 20) genTxIn

genTxOut :: Gen TxOut
genTxOut = TxOut <$> genAddress <*> genLovelace

genTxOutAux :: Gen TxOutAux
genTxOutAux = TxOutAux <$> genTxOut

genTxOutList :: Gen (NonEmpty TxOut)
genTxOutList = Gen.nonEmpty (Range.linear 1 100) genTxOut

genTxpConfiguration :: Gen TxpConfiguration
genTxpConfiguration = do
  limit <- Gen.int (Range.constant 0 200)
  addrs <- Gen.list (Range.linear 0 50) genAddress
  return (TxpConfiguration limit (S.fromList addrs))

genTxpUndo :: Gen TxpUndo
genTxpUndo = Gen.list (Range.linear 1 50) genTxUndo

genTxPayload :: ProtocolMagicId -> Gen TxPayload
genTxPayload pm = mkTxPayload <$> Gen.list (Range.linear 0 10) (genTxAux pm)

genTxProof :: ProtocolMagicId -> Gen TxProof
genTxProof pm =
  TxProof <$> genWord32 <*> genMerkleRoot genTx <*> genAbstractHash
    (Gen.list (Range.linear 1 5) (genTxWitness pm))

genTxSig :: ProtocolMagicId -> Gen TxSig
genTxSig pm = sign pm <$> genSignTag <*> genSecretKey <*> genTxSigData

genTxSigData :: Gen TxSigData
genTxSigData = TxSigData <$> genTxHash

genTxInWitness :: ProtocolMagicId -> Gen TxInWitness
genTxInWitness pm = Gen.choice [genPkWitness pm, genRedeemWitness pm]

genTxUndo :: Gen TxUndo
genTxUndo = Gen.nonEmpty (Range.linear 1 10) $ Gen.maybe genTxOutAux

genTxWitness :: ProtocolMagicId -> Gen TxWitness
genTxWitness pm =
  V.fromList <$> Gen.list (Range.linear 1 10) (genTxInWitness pm)
