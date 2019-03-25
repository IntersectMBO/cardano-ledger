{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Chain.Update.Undo
  (
  -- * Proposal state
    UndecidedProposalState(..)
  , DecidedProposalState(..)
  , ProposalState(..)
  , UpsExtra(..)
  , DpsExtra(..)
  , ConfirmedProposalState(..)
  , cpsProtocolVersion
  , cpsSoftwareVersion
  , propStateToEither
  , psProposal
  , psVotes
  , mkUProposalState

  -- * ProtocolVersion state
  , ProtocolVersionState(..)
  , bvsIsConfirmed
  , bvsScriptVersion
  , bvsSlotDuration
  , bvsMaxBlockSize

  -- * Rollback
  , PrevValue(..)
  , maybeToPrev
  , USUndo(..)
  , unChangedSVL
  , unChangedPropsL
  , unChangedBVL
  , unLastAdoptedBVL
  , unChangedConfPropsL
  , unPrevProposersL
  , unSlottingDataL

  -- * VoteState
  , StakeholderVotes
  , LocalVotes
  , VoteState(..)
  , canCombineVotes
  , combineVotes
  , isPositiveVote
  , newVoteState
  )
where

import Cardano.Prelude

import Control.Lens (makeLensesFor)
import Data.Time (NominalDiffTime)
import Formatting.Buildable (Buildable(..))

import Cardano.Binary.Class
  (Bi(..), DecoderError(..), decodeListLen, encodeListLen, enforceSize)
import Cardano.Chain.Block.Header (HeaderHash)
import Cardano.Chain.Common
  (ChainDifficulty, Lovelace, StakeholderId, mkKnownLovelace)
import Cardano.Chain.Slotting (EpochIndex, SlotId, SlottingData)
import Cardano.Chain.Update.ApplicationName (ApplicationName)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.ProtocolParametersUpdate
  (ProtocolParametersUpdate(..))
import Cardano.Chain.Update.SoftwareVersion
  (NumSoftwareVersion, SoftwareVersion)
import Cardano.Chain.Update.Vote
  (Proposal, ProposalBody(..), UpId, Vote, proposalBody)
import Cardano.Crypto (PublicKey)


--------------------------------------------------------------------------------
-- VoteState
--------------------------------------------------------------------------------

-- | This type represents summary of votes issued by stakeholder
data VoteState
  = PositiveVote
  -- ^ Stakeholder voted once positively.
  | NegativeVote
  -- ^ Stakeholder voted once positively.
  | PositiveRevote
  -- ^ Stakeholder voted negatively, then positively.
  | NegativeRevote
  -- ^ Stakeholder voted positively, then negatively.
  deriving (Show, Generic, Eq)
  deriving anyclass NFData

instance Buildable VoteState where
  build PositiveVote   = "PositiveVote"
  build NegativeVote   = "NegativeVote"
  build PositiveRevote = "PositiveRevote"
  build NegativeRevote = "NegativeRevote"

instance Bi VoteState where
  encode = \case
    PositiveVote   -> encodeListLen 1 <> encode (0 :: Word8)
    NegativeVote   -> encodeListLen 1 <> encode (1 :: Word8)
    PositiveRevote -> encodeListLen 1 <> encode (2 :: Word8)
    NegativeRevote -> encodeListLen 1 <> encode (3 :: Word8)

  decode = do
    enforceSize "VoteState" 1
    decode >>= \case
      0 -> pure PositiveVote
      1 -> pure NegativeVote
      2 -> pure PositiveRevote
      3 -> pure NegativeRevote
      t -> cborError $ DecoderErrorUnknownTag "VoteState" t

-- | Create new VoteState from bool, which is simple vote, not revote
newVoteState :: Bool -> VoteState
newVoteState True  = PositiveVote
newVoteState False = NegativeVote

isPositiveVote :: VoteState -> Bool
isPositiveVote PositiveVote = True
isPositiveVote PositiveRevote = True
isPositiveVote _ = False

-- | Check whether given decision is a valid vote if applied to existing vote
--   (which may not exist)
canCombineVotes :: Bool -> Maybe VoteState -> Bool
canCombineVotes _     Nothing = True
canCombineVotes True (Just NegativeVote) = True
canCombineVotes False (Just PositiveVote) = True
canCombineVotes _     _       = False

-- | Apply decision to given vote (or Nothing). This function returns 'Nothing'
--   if decision can't be applied. 'canCombineVotes' can be used to check
--   whether it will be successful.
combineVotes :: Bool -> Maybe VoteState -> Maybe VoteState
combineVotes decision oldVote = case (decision, oldVote) of
  (True , Nothing) -> Just PositiveVote
  (False, Nothing) -> Just NegativeVote
  (True, Just NegativeVote) -> Just PositiveRevote
  (False, Just PositiveVote) -> Just NegativeRevote
  (_    , Just _ ) -> Nothing

-- | Type alias for set of votes from stakeholders
type StakeholderVotes = Map PublicKey VoteState
type LocalVotes = Map UpId (Map PublicKey Vote)


--------------------------------------------------------------------------------
-- Proposal State
--------------------------------------------------------------------------------

-- | Extra data required by wallet, stored in UndecidedProposalState
data UpsExtra = UpsExtra
  { ueProposedBlk :: !HeaderHash
  -- ^ Block in which this update was proposed
  } deriving (Show, Generic, Eq)
    deriving anyclass NFData

instance Bi UpsExtra where
  encode extra = encodeListLen 1 <> encode (ueProposedBlk extra)
  decode = enforceSize "UpsExtra" 1 >> UpsExtra <$> decode

-- | State of Proposal which can't be classified as approved or rejected
data UndecidedProposalState = UndecidedProposalState
  { upsVotes         :: !StakeholderVotes
  -- ^ Votes given for this proposal.
  , upsProposal      :: !Proposal
  -- ^ Proposal itself.
  , upsSlot          :: !SlotId
  -- ^ SlotId from block in which update was proposed.
  , upsPositiveStake :: !Lovelace
  -- ^ Total stake of all positive votes.
  , upsNegativeStake :: !Lovelace
  -- ^ Total stake of all negative votes.
  , upsExtra         :: !(Maybe UpsExtra)
  -- ^ Extra data
  } deriving (Show, Generic, Eq)
    deriving anyclass NFData

instance Bi UndecidedProposalState where
  encode ups =
    encodeListLen 6
      <> encode (upsVotes ups)
      <> encode (upsProposal ups)
      <> encode (upsSlot ups)
      <> encode (upsPositiveStake ups)
      <> encode (upsNegativeStake ups)
      <> encode (upsExtra ups)

  decode = do
    enforceSize "UndecidedProposalState" 6
    UndecidedProposalState
      <$> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode

-- | Extra data required by wallet, stored in DecidedProposalState
data DpsExtra = DpsExtra
  { deDecidedBlk :: !HeaderHash
  -- ^ HeaderHash  of block in which this update was approved/rejected
  , deImplicit   :: !Bool
  -- ^ Which way we approve/reject this update proposal: implicit or explicit
  } deriving (Show, Generic, Eq)
    deriving anyclass NFData

instance Bi DpsExtra where
  encode de =
    encodeListLen 2 <> encode (deDecidedBlk de) <> encode (deImplicit de)

  decode = do
    enforceSize "DpsExtra" 2
    DpsExtra <$> decode <*> decode

-- | State of Proposal which can be classified as approved or rejected
data DecidedProposalState = DecidedProposalState
  { dpsDecision   :: !Bool
  -- ^ Whether proposal is approved.
  , dpsUndecided  :: !UndecidedProposalState
  -- ^ Corresponding UndecidedProposalState
  , dpsDifficulty :: !(Maybe ChainDifficulty)
  -- ^ Difficulty at which this proposal became approved/rejected. Can be
  --   Nothing in temporary state.
  , dpsExtra      :: !(Maybe DpsExtra)
  -- ^ Extra data
  } deriving (Show, Generic, Eq)
    deriving anyclass NFData


instance Bi DecidedProposalState where
  encode dps =
    encodeListLen 4
      <> encode (dpsDecision dps)
      <> encode (dpsUndecided dps)
      <> encode (dpsDifficulty dps)
      <> encode (dpsExtra dps)

  decode = do
    enforceSize "DecidedProposalState" 4
    DecidedProposalState <$> decode <*> decode <*> decode <*> decode

-- | Information about confirmed proposals stored in DB
data ConfirmedProposalState = ConfirmedProposalState
  { cpsUpdateProposal :: !Proposal
  , cpsImplicit       :: !Bool
  , cpsProposed       :: !HeaderHash
  , cpsDecided        :: !HeaderHash
  , cpsConfirmed      :: !HeaderHash
  , cpsAdopted        :: !(Maybe HeaderHash)
  , cpsVotes          :: !StakeholderVotes
  , cpsPositiveStake  :: !Lovelace
  , cpsNegativeStake  :: !Lovelace
  } deriving (Show, Generic, Eq)
    deriving anyclass NFData

instance Bi ConfirmedProposalState where
  encode cps =
    encodeListLen 9
      <> encode (cpsUpdateProposal cps)
      <> encode (cpsImplicit cps)
      <> encode (cpsProposed cps)
      <> encode (cpsDecided cps)
      <> encode (cpsConfirmed cps)
      <> encode (cpsAdopted cps)
      <> encode (cpsVotes cps)
      <> encode (cpsPositiveStake cps)
      <> encode (cpsNegativeStake cps)

  decode = do
    enforceSize "ConfirmedProposalState" 9
    ConfirmedProposalState
      <$> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode

-- | Get 'ProtocolVersion' from 'ConfirmedProposalState'
cpsProtocolVersion :: ConfirmedProposalState -> ProtocolVersion
cpsProtocolVersion = pbProtocolVersion . proposalBody . cpsUpdateProposal

-- | Get 'SoftwareVersion' from 'ConfirmedProposalState'
cpsSoftwareVersion :: ConfirmedProposalState -> SoftwareVersion
cpsSoftwareVersion = pbSoftwareVersion . proposalBody . cpsUpdateProposal

-- | State of Proposal
data ProposalState
  = PSUndecided !UndecidedProposalState
  | PSDecided !DecidedProposalState
  deriving (Eq, Generic, Show)
  deriving anyclass NFData

instance Bi ProposalState where
  encode = \case
    PSUndecided ups -> encodeListLen 2 <> encode (0 :: Word8) <> encode ups
    PSDecided   dps -> encodeListLen 2 <> encode (1 :: Word8) <> encode dps

  decode = do
    enforceSize "ProposalState" 2
    decode >>= \case
      0 -> PSUndecided <$> decode
      1 -> PSDecided <$> decode
      t -> cborError $ DecoderErrorUnknownTag "ProposalState" t

propStateToEither
  :: ProposalState -> Either UndecidedProposalState DecidedProposalState
propStateToEither (PSUndecided ups) = Left ups
propStateToEither (PSDecided   dps) = Right dps

psProposal :: ProposalState -> Proposal
psProposal (PSUndecided ups) = upsProposal ups
psProposal (PSDecided   dps) = upsProposal (dpsUndecided dps)

psVotes :: ProposalState -> StakeholderVotes
psVotes (PSUndecided ups) = upsVotes ups
psVotes (PSDecided   dps) = upsVotes (dpsUndecided dps)

-- | Make UndecidedProposalState from immutable data, i. e. SlotId and Proposal
mkUProposalState :: SlotId -> Proposal -> UndecidedProposalState
mkUProposalState slot proposal = UndecidedProposalState
  { upsVotes         = mempty
  , upsProposal      = proposal
  , upsSlot          = slot
  , upsPositiveStake = mkKnownLovelace @0
  , upsNegativeStake = mkKnownLovelace @0
  , upsExtra         = Nothing
  }


--------------------------------------------------------------------------------
-- ProtocolVersion state
--------------------------------------------------------------------------------

-- | State of ProtocolVersion from update proposal
data ProtocolVersionState = ProtocolVersionState
  { bvsModifier          :: !ProtocolParametersUpdate
  -- ^ 'ProtocolParametersUpdate' associated with this block version
  , bvsConfirmedEpoch    :: !(Maybe EpochIndex)
  -- ^ Epoch when proposal which generated this block version was confirmed
  , bvsIssuersStable     :: !(Set StakeholderId)
  -- ^ Identifiers of stakeholders which issued stable blocks with this
  --   'ProtocolVersion'. Stability is checked by the same rules as used in LRC.
  --   That is, 'SlotId' is considered. If block is created after crucial slot
  --   of 'i'-th epoch, it is not stable when 'i+1'-th epoch starts.
  , bvsIssuersUnstable   :: !(Set StakeholderId)
  -- ^ Identifiers of stakeholders which issued unstable blocks with this
  --   'ProtocolVersion'. See description of 'bvsIssuersStable' for details.
  , bvsLastBlockStable   :: !(Maybe HeaderHash)
  -- ^ Identifier of last block which modified set of 'bvsIssuersStable'
  , bvsLastBlockUnstable :: !(Maybe HeaderHash)
  -- ^ Identifier of last block which modified set of 'bvsIssuersUnstable'
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance Bi ProtocolVersionState where
  encode bvs =
    encodeListLen 6
      <> encode (bvsModifier bvs)
      <> encode (bvsConfirmedEpoch bvs)
      <> encode (bvsIssuersStable bvs)
      <> encode (bvsIssuersUnstable bvs)
      <> encode (bvsLastBlockStable bvs)
      <> encode (bvsLastBlockUnstable bvs)

  decode = do
    enforceSize "ProtocolVersionState" 6
    ProtocolVersionState
      <$> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode

-- | Check whether proposal which generated given 'ProtocolVersionState' is
--   confirmed
bvsIsConfirmed :: ProtocolVersionState -> Bool
bvsIsConfirmed = isJust . bvsConfirmedEpoch

bvsScriptVersion :: ProtocolVersionState -> Maybe Word16
bvsScriptVersion = ppuScriptVersion . bvsModifier

bvsSlotDuration :: ProtocolVersionState -> Maybe NominalDiffTime
bvsSlotDuration = ppuSlotDuration . bvsModifier

bvsMaxBlockSize :: ProtocolVersionState -> Maybe Natural
bvsMaxBlockSize = ppuMaxBlockSize . bvsModifier


--------------------------------------------------------------------------------
-- Undo
--------------------------------------------------------------------------------

-- | Previous value of something that could be missing
data PrevValue a
  = PrevValue a
  | NoExist
  deriving (Generic, Show, Eq)
  deriving anyclass NFData

instance Bi a => Bi (PrevValue a) where
  encode (PrevValue a) = encodeListLen 1 <> encode a
  encode NoExist       = encodeListLen 0

  decode = decodeListLen >>= \case
    1   -> PrevValue <$> decode
    0   -> pure NoExist
    len -> cborError $ DecoderErrorUnknownTag "PrevValue" (fromIntegral len)

maybeToPrev :: Maybe a -> PrevValue a
maybeToPrev (Just x) = PrevValue x
maybeToPrev Nothing  = NoExist

-- | Data necessary to unapply US data
data USUndo = USUndo
  { unChangedBV :: !(Map ProtocolVersion (PrevValue ProtocolVersionState))
  , unLastAdoptedBV :: !(Maybe ProtocolVersion)
  , unChangedProps :: !(Map UpId (PrevValue ProposalState))
  , unChangedSV :: !(Map ApplicationName (PrevValue NumSoftwareVersion))
  , unChangedConfProps :: !(Map SoftwareVersion (PrevValue ConfirmedProposalState))
  , unPrevProposers :: !(Maybe (Set StakeholderId))
  , unSlottingData :: !(Maybe SlottingData)
  -- ^ 'SlottingData' which should be modified as the result of this rollback
  } deriving (Generic, Show, Eq)
    deriving anyclass NFData

instance Bi USUndo where
  encode undo =
    encodeListLen 7
      <> encode (unChangedBV undo)
      <> encode (unLastAdoptedBV undo)
      <> encode (unChangedProps undo)
      <> encode (unChangedSV undo)
      <> encode (unChangedConfProps undo)
      <> encode (unPrevProposers undo)
      <> encode (unSlottingData undo)

  decode = do
    enforceSize "Update.Undo" 7
    USUndo
      <$> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode
      <*> decode

makeLensesFor
  [ ("unChangedBV", "unChangedBVL")
  , ("unLastAdoptedBV", "unLastAdoptedBVL")
  , ("unChangedProps", "unChangedPropsL")
  , ("unChangedSV", "unChangedSVL")
  , ("unChangedConfProps", "unChangedConfPropsL")
  , ("unPrevProposers", "unPrevProposersL")
  , ("unSlottingData", "unSlottingDataL")
  ]
  ''USUndo

instance Buildable USUndo where
  build _ = "BSUndo"
