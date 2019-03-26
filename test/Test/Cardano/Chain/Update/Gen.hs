module Test.Cardano.Chain.Update.Gen
  ( genCanonicalProtocolParameters
  , genApplicationName
  , genProtocolVersion
  , genProtocolParameters
  , genProtocolParametersUpdate
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
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.Coerce (coerce)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Block (HeaderHash)
import Cardano.Chain.Common (mkAttributes)
import Cardano.Chain.Slotting (EpochSlots)
import Cardano.Chain.Update
  ( ApplicationName(..)
  , ConfirmedProposalState(..)
  , DecidedProposalState(..)
  , DpsExtra(..)
  , Payload
  , PrevValue(..)
  , Proof
  , Proposal
  , ProposalBody(..)
  , ProposalState(..)
  , Proposals
  , ProtocolParametersUpdate(..)
  , ProtocolParameters(..)
  , ProtocolVersion(..)
  , ProtocolVersionState(..)
  , SoftforkRule(..)
  , SoftwareVersion(..)
  , SystemTag(..)
  , USUndo(..)
  , UndecidedProposalState(..)
  , UpId
  , UpdateData(..)
  , UpsExtra(..)
  , Vote
  , VoteId
  , VoteState(..)
  , applicationNameMaxLength
  , maybeToPrev
  , mkProposal
  , mkVote
  , payload
  , systemTagMaxLength
  )
import Cardano.Crypto (ProtocolMagicId)

import Test.Cardano.Chain.Common.Gen
  ( genCanonicalTxFeePolicy
  , genChainDifficulty
  , genLovelace
  , genLovelacePortion
  , genScriptVersion
  , genStakeholderId
  , genTxFeePolicy
  )
import Test.Cardano.Chain.Slotting.Gen
  (genEpochIndex, genFlatSlotId, genSlotId, genSlottingData)
import Test.Cardano.Crypto.Gen
  ( genAbstractHash
  , genHashRaw
  , genPublicKey
  , genSecretKey
  , genSignature
  , genTextHash
  )


genApplicationName :: Gen ApplicationName
genApplicationName =
  ApplicationName
    <$> Gen.text (Range.constant 0 applicationNameMaxLength) Gen.alphaNum

genCanonicalProtocolParameters :: Gen ProtocolParameters
genCanonicalProtocolParameters =
  ProtocolParameters
    <$> genScriptVersion
    <*> genNominalDiffTime
    <*> genNatural
    <*> genNatural
    <*> genNatural
    <*> genNatural
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genFlatSlotId
    <*> genSoftforkRule
    <*> genCanonicalTxFeePolicy
    <*> genEpochIndex

genProtocolVersion :: Gen ProtocolVersion
genProtocolVersion =
  ProtocolVersion
    <$> Gen.word16 Range.constantBounded
    <*> Gen.word16 Range.constantBounded
    <*> Gen.word8 Range.constantBounded

genProtocolParameters :: Gen ProtocolParameters
genProtocolParameters =
  ProtocolParameters
    <$> genScriptVersion
    <*> genNominalDiffTime
    <*> genNatural
    <*> genNatural
    <*> genNatural
    <*> genNatural
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion
    <*> genFlatSlotId
    <*> genSoftforkRule
    <*> genTxFeePolicy
    <*> genEpochIndex

genProtocolParametersUpdate :: Gen ProtocolParametersUpdate
genProtocolParametersUpdate =
  ProtocolParametersUpdate
    <$> Gen.maybe genScriptVersion
    <*> Gen.maybe genNominalDiffTime
    <*> Gen.maybe genNatural
    <*> Gen.maybe genNatural
    <*> Gen.maybe genNatural
    <*> Gen.maybe genNatural
    <*> Gen.maybe genLovelacePortion
    <*> Gen.maybe genLovelacePortion
    <*> Gen.maybe genLovelacePortion
    <*> Gen.maybe genLovelacePortion
    <*> Gen.maybe genFlatSlotId
    <*> Gen.maybe genSoftforkRule
    <*> Gen.maybe genTxFeePolicy
    <*> Gen.maybe genEpochIndex

genProtocolVersionState :: Gen ProtocolVersionState
genProtocolVersionState =
  ProtocolVersionState
    <$> genProtocolParametersUpdate
    <*> Gen.maybe genEpochIndex
    <*> Gen.set (Range.linear 0 10) genStakeholderId
    <*> Gen.set (Range.linear 0 10) genStakeholderId
    <*> Gen.maybe genHeaderHash
    <*> Gen.maybe genHeaderHash

genConfirmedProposalState :: ProtocolMagicId -> Gen ConfirmedProposalState
genConfirmedProposalState pm =
  ConfirmedProposalState
    <$> genProposal pm
    <*> Gen.bool
    <*> genHeaderHash
    <*> genHeaderHash
    <*> genHeaderHash
    <*> Gen.maybe genHeaderHash
    <*> Gen.map (Range.linear 1 10) ((,) <$> genPublicKey <*> genVoteState)
    <*> genLovelace
    <*> genLovelace

genDecidedProposalState
  :: ProtocolMagicId -> EpochSlots -> Gen DecidedProposalState
genDecidedProposalState pm epochSlots =
  DecidedProposalState
    <$> Gen.bool
    <*> genUndecidedProposalState pm epochSlots
    <*> Gen.maybe genChainDifficulty
    <*> Gen.maybe genDpsExtra

genDpsExtra :: Gen DpsExtra
genDpsExtra = DpsExtra <$> genHeaderHash <*> Gen.bool

genPrevValue :: Gen a -> Gen (PrevValue a)
genPrevValue = fmap maybeToPrev . Gen.maybe

genProposalState :: ProtocolMagicId -> EpochSlots -> Gen ProposalState
genProposalState pm epochSlots = Gen.choice
  [ PSUndecided <$> genUndecidedProposalState pm epochSlots
  , PSDecided <$> genDecidedProposalState pm epochSlots
  ]

genSoftforkRule :: Gen SoftforkRule
genSoftforkRule =
  SoftforkRule
    <$> genLovelacePortion
    <*> genLovelacePortion
    <*> genLovelacePortion

genSoftwareVersion :: Gen SoftwareVersion
genSoftwareVersion =
  SoftwareVersion <$> genApplicationName <*> Gen.word32 Range.constantBounded

genSystemTag :: Gen SystemTag
genSystemTag =
  SystemTag <$> Gen.text (Range.constant 0 systemTagMaxLength) Gen.alphaNum

genUndecidedProposalState
  :: ProtocolMagicId -> EpochSlots -> Gen UndecidedProposalState
genUndecidedProposalState pm epochSlots =
  UndecidedProposalState
    <$> Gen.map (Range.linear 0 10) ((,) <$> genPublicKey <*> genVoteState)
    <*> genProposal pm
    <*> genSlotId epochSlots
    <*> genLovelace
    <*> genLovelace
    <*> Gen.maybe genUpsExtra

genUndo :: ProtocolMagicId -> EpochSlots -> Gen USUndo
genUndo pm epochSlots =
  USUndo
    <$> Gen.map
          hmRange
          ((,) <$> genProtocolVersion <*> genPrevValue genProtocolVersionState)
    <*> Gen.maybe genProtocolVersion
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
 where
  hmRange :: Range Int
  hmRange = Range.linear 0 10

genUpdateData :: Gen UpdateData
genUpdateData =
  UpdateData <$> genHashRaw <*> genHashRaw <*> genHashRaw <*> genHashRaw

genPayload :: ProtocolMagicId -> Gen Payload
genPayload pm = payload <$> Gen.maybe (genProposal pm) <*> Gen.list
  (Range.linear 0 10)
  (genVote pm)

genProof :: ProtocolMagicId -> Gen Proof
genProof pm = genAbstractHash (genPayload pm)

genProposal :: ProtocolMagicId -> Gen Proposal
genProposal pm =
  mkProposal
    <$> genProposalBody
    <*> genPublicKey
    <*> genSignature pm genProposalBody

genProposals :: ProtocolMagicId -> Gen Proposals
genProposals pm =
  Gen.map (Range.linear 0 10) ((,) <$> genUpId pm <*> genProposal pm)

genProposalBody :: Gen ProposalBody
genProposalBody =
  ProposalBody
    <$> genProtocolVersion
    <*> genProtocolParametersUpdate
    <*> genSoftwareVersion
    <*> genUpsData
    <*> pure (mkAttributes ())

genUpId :: ProtocolMagicId -> Gen UpId
genUpId pm = genAbstractHash (genProposal pm)

genUpsData :: Gen (Map SystemTag UpdateData)
genUpsData =
  Gen.map (Range.linear 0 20) ((,) <$> genSystemTag <*> genUpdateData)

genUpsExtra :: Gen UpsExtra
genUpsExtra = UpsExtra <$> genHeaderHash

genVote :: ProtocolMagicId -> Gen Vote
genVote pm = mkVote pm <$> genSecretKey <*> genUpId pm <*> Gen.bool

genVoteId :: ProtocolMagicId -> Gen VoteId
genVoteId pm = (,,) <$> genUpId pm <*> genPublicKey <*> Gen.bool

genVoteState :: Gen VoteState
genVoteState =
  Gen.element [PositiveVote, NegativeVote, PositiveRevote, NegativeRevote]

-- | Copied here from "Cardano.Chain.Block.Gen" to avoid module cycle
genHeaderHash :: Gen HeaderHash
genHeaderHash = coerce <$> genTextHash
