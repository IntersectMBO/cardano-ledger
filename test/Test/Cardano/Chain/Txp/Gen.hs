{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.Txp.Gen
       ( genPkWitness
       , genRedeemWitness
       , genScriptWitness
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
       , genTxProof
       , genTxSig
       , genTxSigData
       , genTxUndo
       , genTxWitness
       , genUnknownWitnessType
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Data.ByteString.Base16 as B16
import           Data.Coerce (coerce)
import qualified Data.Vector as V

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Chain.Common (mkAttributes)
import           Cardano.Chain.Txp (Tx (..), TxAttributes, TxAux (..), TxId,
                     TxIn (..), TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxPayload (..), TxProof (..), TxSig, TxSigData (..),
                     TxUndo, TxWitness, TxpUndo)
import           Cardano.Crypto (Hash, ProtocolMagic, decodeHash, sign)

import           Test.Cardano.Chain.Common.Gen (genAddress, genCoin,
                     genMerkleRoot, genScript)
import           Test.Cardano.Crypto.Gen (genAbstractHash, genPublicKey,
                     genRedeemPublicKey, genRedeemSignature, genSecretKey,
                     genSignTag, genTextHash)


genPkWitness :: ProtocolMagic -> Gen TxInWitness
genPkWitness pm = PkWitness <$> genPublicKey <*> genTxSig pm

genRedeemWitness :: ProtocolMagic -> Gen TxInWitness
genRedeemWitness pm =
    RedeemWitness <$> genRedeemPublicKey <*> genRedeemSignature pm genTxSigData

genScriptWitness :: Gen TxInWitness
genScriptWitness = ScriptWitness <$> genScript <*> genScript

genTx :: Gen Tx
genTx = UnsafeTx <$> genTxInList <*> genTxOutList <*> genTxAttributes

genTxAttributes :: Gen TxAttributes
genTxAttributes = pure $ mkAttributes ()

genTxAux :: ProtocolMagic -> Gen TxAux
genTxAux pm = TxAux <$> genTx <*> (genTxWitness pm)

genTxHash :: Gen (Hash Tx)
genTxHash = coerce <$> genTextHash

genTxId :: Gen TxId
genTxId = genBase16Text >>= pure . decodeHash >>= either error pure
    where
        genBase16Text = decodeUtf8 @Text @ByteString <$> genBase16Bs

genBase16Bs :: Gen ByteString
genBase16Bs = B16.encode <$> genBytes 32

genTxIn :: Gen TxIn
genTxIn = Gen.choice gens
  where
    gens = [ TxInUtxo <$> genTxId <*> genWord32
           -- 0 is reserved for TxInUtxo tag ----------+
           , TxInUnknown <$> Gen.word8 (Range.constant 1 255)
                         <*> gen32Bytes
           ]

genTxInList :: Gen (NonEmpty TxIn)
genTxInList = Gen.nonEmpty (Range.linear 1 20) genTxIn

genTxOut :: Gen TxOut
genTxOut = TxOut <$> genAddress <*> genCoin

genTxOutAux :: Gen TxOutAux
genTxOutAux = TxOutAux <$> genTxOut

genTxOutList :: Gen (NonEmpty TxOut)
genTxOutList = Gen.nonEmpty (Range.linear 1 100) genTxOut

genTxpUndo :: Gen TxpUndo
genTxpUndo = Gen.list (Range.linear 1 50) genTxUndo

genTxPayload :: ProtocolMagic -> Gen TxPayload
genTxPayload pm = TxPayload <$> Gen.list (Range.linear 0 10) (genTxAux pm)

genTxProof :: ProtocolMagic -> Gen TxProof
genTxProof pm =
    TxProof
        <$> genWord32
        <*> genMerkleRoot genTx
        <*> genAbstractHash (Gen.list (Range.linear 1 5) (genTxWitness pm))

genTxSig :: ProtocolMagic -> Gen TxSig
genTxSig pm =
    sign pm <$> genSignTag <*> genSecretKey <*> genTxSigData

genTxSigData :: Gen TxSigData
genTxSigData = TxSigData <$> genTxHash

genTxInWitness :: ProtocolMagic -> Gen TxInWitness
genTxInWitness pm = Gen.choice gens
  where
    gens = [ genPkWitness pm
           , genRedeemWitness pm
           , genScriptWitness
           , genUnknownWitnessType
           ]

genTxUndo :: Gen TxUndo
genTxUndo = Gen.nonEmpty (Range.linear 1 10) $ Gen.maybe genTxOutAux

genTxWitness :: ProtocolMagic -> Gen TxWitness
genTxWitness pm = V.fromList <$> Gen.list (Range.linear 1 10) (genTxInWitness pm)

genUnknownWitnessType :: Gen TxInWitness
genUnknownWitnessType =
    UnknownWitnessType <$> Gen.word8 (Range.constant 3 maxBound) <*> gen32Bytes
