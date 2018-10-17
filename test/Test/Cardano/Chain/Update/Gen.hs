module Test.Cardano.Chain.Update.Gen
       ( genApplicationName
       , genBlockVersion
       , genBlockVersionData
       , genBlockVersionModifier
       , genSoftforkRule
       , genSoftwareVersion
       , genSystemTag
       , genUpdateData
       , genPayload
       , genProof
       , genProposal
       , genProposalBody
       , genProposals
       , genUndo
       , genUpId
       , genUpsData
       , genVote
       , genVoteId
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Data.Coerce (coerce)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Chain.Block (HeaderHash)
import           Cardano.Chain.Common (mkAttributes)
import           Cardano.Chain.Slotting (SlotCount)
import           Cardano.Chain.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), BlockVersionModifier (..),
                     BlockVersionState (..), ConfirmedProposalState (..),
                     DecidedProposalState (..), DpsExtra (..), Payload (..),
                     PrevValue (..), Proof, Proposal (..), ProposalBody (..),
                     ProposalState (..), Proposals, SoftforkRule (..),
                     SoftwareVersion (..), SystemTag (..), USUndo (..),
                     UndecidedProposalState (..), UpId, UpdateData (..),
                     UpsExtra (..), Vote (..), VoteId, VoteState (..),
                     maybeToPrev, mkVote)
import           Cardano.Crypto (ProtocolMagic)

import           Test.Cardano.Chain.Common.Gen (genChainDifficulty, genCoin,
                     genCoinPortion, genScriptVersion, genStakeholderId,
                     genTxFeePolicy)
import           Test.Cardano.Chain.Slotting.Gen (genEpochIndex, genFlatSlotId,
                     genSlotId, genSlottingData)
import           Test.Cardano.Crypto.Gen (genAbstractHash, genHashRaw,
                     genPublicKey, genSecretKey, genSignature, genTextHash)


genApplicationName :: Gen ApplicationName
genApplicationName =
    ApplicationName <$> Gen.text (Range.constant 0 10) Gen.alphaNum

genBlockVersion :: Gen BlockVersion
genBlockVersion =
    BlockVersion
        <$> Gen.word16 Range.constantBounded
        <*> Gen.word16 Range.constantBounded
        <*> Gen.word8 Range.constantBounded

genBlockVersionData :: Gen BlockVersionData
genBlockVersionData =
    BlockVersionData
        <$> genScriptVersion
        <*> genNominalDiffTime
        <*> genNatural
        <*> genNatural
        <*> genNatural
        <*> genNatural
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genFlatSlotId
        <*> genSoftforkRule
        <*> genTxFeePolicy
        <*> genEpochIndex

genBlockVersionModifier :: Gen BlockVersionModifier
genBlockVersionModifier =
    BlockVersionModifier
        <$> Gen.maybe genScriptVersion
        <*> Gen.maybe genNominalDiffTime
        <*> Gen.maybe genNatural
        <*> Gen.maybe genNatural
        <*> Gen.maybe genNatural
        <*> Gen.maybe genNatural
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genFlatSlotId
        <*> Gen.maybe genSoftforkRule
        <*> Gen.maybe genTxFeePolicy
        <*> Gen.maybe genEpochIndex

genBlockVersionState :: Gen BlockVersionState
genBlockVersionState = BlockVersionState
    <$> genBlockVersionModifier
    <*> Gen.maybe genEpochIndex
    <*> Gen.set (Range.linear 0 10) genStakeholderId
    <*> Gen.set (Range.linear 0 10) genStakeholderId
    <*> Gen.maybe genHeaderHash
    <*> Gen.maybe genHeaderHash

genConfirmedProposalState :: ProtocolMagic -> Gen ConfirmedProposalState
genConfirmedProposalState pm = ConfirmedProposalState
    <$> genProposal pm
    <*> Gen.bool
    <*> genHeaderHash
    <*> genHeaderHash
    <*> genHeaderHash
    <*> Gen.maybe genHeaderHash
    <*> Gen.map (Range.linear 1 10) ((,) <$> genPublicKey <*> genVoteState)
    <*> genCoin
    <*> genCoin

genDecidedProposalState :: ProtocolMagic -> SlotCount -> Gen DecidedProposalState
genDecidedProposalState pm epochSlots = DecidedProposalState
    <$> Gen.bool
    <*> genUndecidedProposalState pm epochSlots
    <*> Gen.maybe genChainDifficulty
    <*> Gen.maybe genDpsExtra

genDpsExtra :: Gen DpsExtra
genDpsExtra = DpsExtra <$> genHeaderHash <*> Gen.bool

genPrevValue :: Gen a -> Gen (PrevValue a)
genPrevValue = fmap maybeToPrev . Gen.maybe

genProposalState :: ProtocolMagic -> SlotCount -> Gen ProposalState
genProposalState pm epochSlots = Gen.choice
  [ PSUndecided <$> genUndecidedProposalState pm epochSlots
  , PSDecided <$> genDecidedProposalState pm epochSlots
  ]

genSoftforkRule :: Gen SoftforkRule
genSoftforkRule =
    SoftforkRule <$> genCoinPortion <*> genCoinPortion <*> genCoinPortion

genSoftwareVersion :: Gen SoftwareVersion
genSoftwareVersion =
    SoftwareVersion <$> genApplicationName <*> Gen.word32 Range.constantBounded

genSystemTag :: Gen SystemTag
genSystemTag = SystemTag <$> Gen.text (Range.constant 0 10) Gen.alphaNum

genUndecidedProposalState
  :: ProtocolMagic -> SlotCount -> Gen UndecidedProposalState
genUndecidedProposalState pm epochSlots =
  UndecidedProposalState
    <$> Gen.map (Range.linear 0 10) ((,) <$> genPublicKey <*> genVoteState)
    <*> genProposal pm
    <*> genSlotId epochSlots
    <*> genCoin
    <*> genCoin
    <*> Gen.maybe genUpsExtra

genUndo :: ProtocolMagic -> SlotCount -> Gen USUndo
genUndo pm epochSlots =
  USUndo
    <$> Gen.map
          hmRange
          ((,) <$> genBlockVersion <*> genPrevValue genBlockVersionState)
    <*> Gen.maybe genBlockVersion
    <*> Gen.map
          hmRange
          ((,) <$> genUpId pm <*> genPrevValue (genProposalState pm epochSlots))
    <*> Gen.map
          hmRange
          ((,) <$> genApplicationName <*> genPrevValue
            (Gen.word32 Range.constantBounded)
          )
    <*> Gen.map
          hmRange
          ((,) <$> genSoftwareVersion <*> genPrevValue
            (genConfirmedProposalState pm)
          )
    <*> Gen.maybe (Gen.set (Range.linear 0 10) genStakeholderId)
    <*> Gen.maybe genSlottingData
  where hmRange = Range.linear 0 10

genUpdateData :: Gen UpdateData
genUpdateData =
    UpdateData <$> genHashRaw <*> genHashRaw <*> genHashRaw <*> genHashRaw

genPayload :: ProtocolMagic -> Gen Payload
genPayload pm =
    Payload <$> Gen.maybe (genProposal pm) <*> Gen.list
        (Range.linear 0 10)
        (genVote pm)

genProof :: ProtocolMagic -> Gen Proof
genProof pm = genAbstractHash (genPayload pm)

genProposal :: ProtocolMagic -> Gen Proposal
genProposal pm =
    Proposal
        <$> genProposalBody
        <*> genPublicKey
        <*> genSignature pm genProposalBody

genProposals :: ProtocolMagic -> Gen Proposals
genProposals pm =
    Gen.map (Range.linear 0 10) ((,) <$> genUpId pm <*> genProposal pm)

genProposalBody :: Gen ProposalBody
genProposalBody =
    ProposalBody
        <$> genBlockVersion
        <*> genBlockVersionModifier
        <*> genSoftwareVersion
        <*> genUpsData
        <*> pure (mkAttributes ())

genUpId :: ProtocolMagic -> Gen UpId
genUpId pm = genAbstractHash (genProposal pm)

genUpsData :: Gen (Map SystemTag UpdateData)
genUpsData = Gen.map (Range.linear 0 20) ((,) <$> genSystemTag <*> genUpdateData)

genUpsExtra :: Gen UpsExtra
genUpsExtra = UpsExtra <$> genHeaderHash

genVote :: ProtocolMagic -> Gen Vote
genVote pm = mkVote pm <$> genSecretKey <*> genUpId pm <*> Gen.bool

genVoteId :: ProtocolMagic -> Gen VoteId
genVoteId pm = (,,) <$> genUpId pm <*> genPublicKey <*> Gen.bool

genVoteState :: Gen VoteState
genVoteState =
    Gen.element [PositiveVote, NegativeVote, PositiveRevote, NegativeRevote]

-- | Copied here from "Cardano.Chain.Block.Gen" to avoid module cycle
genHeaderHash :: Gen HeaderHash
genHeaderHash = coerce <$> genTextHash
