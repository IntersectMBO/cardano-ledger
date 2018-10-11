module Test.Cardano.Chain.Block.Gen
       ( genBlockBodyAttributes
       , genBlockHeader
       , genBlockHeaderAttributes
       , genBlockSignature
       , genBoundaryBlockHeader
       , genBoundaryBody
       , genBoundaryConsensusData
       , genBoundaryProof
       , genHeaderHash
       , genMainBlockHeader
       , genMainBody
       , genMainConsensusData
       , genMainExtraBodyData
       , genMainExtraHeaderData
       , genMainProof
       , genMainToSign
       , genSlogUndo
       , genUndo
       ) where

import           Cardano.Prelude

import           Data.Coerce (coerce)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

import           Cardano.Chain.Block (BlockBodyAttributes, BlockHeader (..),
                     BlockHeaderAttributes, BlockSignature (..),
                     BoundaryBlockHeader, BoundaryBody (..),
                     BoundaryConsensusData (..), BoundaryProof (..),
                     HeaderHash, MainBlockHeader, MainBody (..),
                     MainConsensusData (..), MainExtraBodyData (..),
                     MainExtraHeaderData (..), MainProof (..), MainToSign (..),
                     SlogUndo (..), Undo (..), mkBoundaryHeader,
                     mkMainHeaderExplicit)
import           Cardano.Chain.Common (mkAttributes)
import           Cardano.Chain.Slotting (SlotCount)
import           Cardano.Crypto (ProtocolMagic)

import           Test.Cardano.Chain.Common.Gen (genChainDifficulty,
                     genSlotLeaders)
import qualified Test.Cardano.Chain.Delegation.Gen as Delegation
import           Test.Cardano.Chain.Slotting.Gen (genEpochIndex, genFlatSlotId,
                     genSlotId)
import           Test.Cardano.Chain.Ssc.Gen (genSscPayload, genSscProof)
import           Test.Cardano.Chain.Txp.Gen (genTxPayload, genTxProof,
                     genTxpUndo)
import qualified Test.Cardano.Chain.Update.Gen as Update
import           Test.Cardano.Crypto.Gen (genAbstractHash, genProxySignature,
                     genPublicKey, genSecretKey, genSignature, genTextHash)


genBlockBodyAttributes :: Gen BlockBodyAttributes
genBlockBodyAttributes = pure $ mkAttributes ()

genBlockHeader :: ProtocolMagic -> SlotCount -> Gen BlockHeader
genBlockHeader pm epochSlots = Gen.choice
  [ BlockHeaderBoundary <$> genBoundaryBlockHeader pm epochSlots
  , BlockHeaderMain <$> genMainBlockHeader pm epochSlots
  ]

genBlockHeaderAttributes :: Gen BlockHeaderAttributes
genBlockHeaderAttributes = pure $ mkAttributes ()

genBlockSignature :: ProtocolMagic -> SlotCount -> Gen BlockSignature
genBlockSignature pm epochSlots = Gen.choice
  [ BlockSignature <$> genSignature pm mts
  , BlockPSignatureLight
    <$> genProxySignature pm mts Delegation.genLightDlgIndices
  , BlockPSignatureHeavy
    <$> genProxySignature pm mts Delegation.genHeavyDlgIndex
  ]
  where mts = genMainToSign pm epochSlots

genBoundaryBlockHeader :: ProtocolMagic -> SlotCount -> Gen BoundaryBlockHeader
genBoundaryBlockHeader pm epochSlots = do
  epoch      <- genEpochIndex
  body       <- genBoundaryBody
  prevHeader <- BlockHeaderMain <$> genMainBlockHeader pm epochSlots
  pure $ mkBoundaryHeader pm (Right prevHeader) epoch body

genBoundaryBody :: Gen BoundaryBody
genBoundaryBody = BoundaryBody <$> genSlotLeaders

genBoundaryConsensusData :: Gen BoundaryConsensusData
genBoundaryConsensusData =
  BoundaryConsensusData <$> genEpochIndex <*> genChainDifficulty

genHeaderHash :: Gen HeaderHash
genHeaderHash = coerce <$> genTextHash

genBoundaryProof :: Gen BoundaryProof
genBoundaryProof = BoundaryProof <$> genAbstractHash genSlotLeaders

genMainBody :: ProtocolMagic -> Gen MainBody
genMainBody pm =
  MainBody
    <$> genTxPayload pm
    <*> genSscPayload pm
    <*> Delegation.genPayload pm
    <*> Update.genPayload pm

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
genMainBlockHeader :: ProtocolMagic -> SlotCount -> Gen MainBlockHeader
genMainBlockHeader pm epochSlots =
  mkMainHeaderExplicit pm
    <$> genHeaderHash
    <*> genChainDifficulty
    <*> genSlotId epochSlots
    <*> genSecretKey
    <*> pure Nothing
    <*> genMainBody pm
    <*> genMainExtraHeaderData

genMainConsensusData :: ProtocolMagic -> SlotCount -> Gen MainConsensusData
genMainConsensusData pm epochSlots =
  MainConsensusData
    <$> genSlotId epochSlots
    <*> genPublicKey
    <*> genChainDifficulty
    <*> genBlockSignature pm epochSlots


genMainExtraBodyData :: Gen MainExtraBodyData
genMainExtraBodyData = MainExtraBodyData <$> genBlockBodyAttributes

genMainExtraHeaderData :: Gen MainExtraHeaderData
genMainExtraHeaderData =
  MainExtraHeaderData
    <$> Update.genBlockVersion
    <*> Update.genSoftwareVersion
    <*> genBlockHeaderAttributes
    <*> genAbstractHash genMainExtraBodyData

genMainProof :: ProtocolMagic -> Gen MainProof
genMainProof pm =
  MainProof
    <$> genTxProof pm
    <*> genSscProof pm
    <*> genAbstractHash (Delegation.genPayload pm)
    <*> Update.genProof pm

genMainToSign :: ProtocolMagic -> SlotCount -> Gen MainToSign
genMainToSign pm epochSlots =
  MainToSign
    <$> genAbstractHash (genBlockHeader pm epochSlots)
    <*> genMainProof pm
    <*> genSlotId epochSlots
    <*> genChainDifficulty
    <*> genMainExtraHeaderData

genSlogUndo :: Gen SlogUndo
genSlogUndo = SlogUndo <$> Gen.maybe genFlatSlotId

genUndo :: ProtocolMagic -> SlotCount -> Gen Undo
genUndo pm epochSlots =
  Undo
    <$> genTxpUndo
    <*> Delegation.genUndo pm
    <*> Update.genUndo pm epochSlots
    <*> genSlogUndo
