{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Governance (
  EraGov (..),
  EnactState (..),
  RatifyState (..),
  RatifyEnv (..),
  RatifySignal (..),
  ConwayGovState (..),
  Committee (..),
  GovAction (..),
  GovActionState (..),
  GovActionIx (..),
  GovActionId (..),
  GovActionPurpose (..),
  PrevGovActionIds (..),
  PrevGovActionIdsChildren (..),
  PrevGovActionId (..),
  DRepPulsingState (..),
  DRepPulser (..),
  govActionIdToText,
  Voter (..),
  Vote (..),
  VotingProcedure (..),
  VotingProcedures (..),
  ProposalProcedure (..),
  GovProcedures (..),
  Anchor (..),
  AnchorData (..),
  indexedGovProps,
  Constitution (..),
  ConwayEraGov (..),
  votingStakePoolThreshold,
  votingDRepThreshold,
  votingCommitteeThreshold,
  isStakePoolVotingAllowed,
  isDRepVotingAllowed,
  isCommitteeVotingAllowed,
  Proposals,
  proposalsActions,
  proposalsAddVote,
  proposalsAddProposal,
  proposalsIds,
  proposalsRemoveIds,
  proposalsLookupId,
  proposalsGovActionStates,
  fromGovActionStateSeq,
  isConsistent_,
  -- Lenses
  cgProposalsL,
  cgEnactStateL,
  cgDRepPulsingStateL,
  ensCommitteeL,
  ensConstitutionL,
  ensCurPParamsL,
  ensPrevPParamsL,
  ensWithdrawalsL,
  ensTreasuryL,
  ensPrevGovActionIdsL,
  ensPrevPParamUpdateL,
  ensPrevHardForkL,
  ensPrevCommitteeL,
  ensPrevConstitutionL,
  ensProtVerL,
  rsEnactStateL,
  curPParamsConwayGovStateL,
  prevPParamsConwayGovStateL,
  constitutionScriptL,
  constitutionAnchorL,
  gasDepositL,
  gasCommitteeVotesL,
  gasDRepVotesL,
  gasExpiresAfterL,
  gasStakePoolVotesL,
  utxosGovStateL,
  newEpochStateDRepPulsingStateL,
  epochStateDRepPulsingStateL,
  epochStateStakeDistrL,
  epochStateIncrStakeDistrL,
  epochStateRegDrepL,
  epochStateUMapL,
  reDRepDistrL,
  pulseDRepPulsingState,
  completeDRepPulsingState,
  extractDRepPulsingState,
  finishDRepPulser,
  computeDrepDistr,
  getRatifyState,
  conwayGovStateDRepDistrG,
  psDRepDistrG,
  dormantEpoch,
  PulsingSnapshot (..),
  setCompleteDRepPulsingState,
  setFreshDRepPulsingState,
  psProposalsL,
  psDRepDistrL,
  psDRepStateL,
  RunConwayRatify (..),
  ensPrevGovActionIdsChildrenL,
  pgacPParamUpdateL,
  pgacHardForkL,
  pgacCommitteeL,
  pgacConstitutionL,

  -- * Exported for testing
  pparamsUpdateThreshold,
) where

import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  Globals (..),
  ProtVer (..),
  StrictMaybe (..),
  UnitInterval,
  isSJust,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  decNoShareCBOR,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.CertState (CommitteeState, Obligations (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  dvtPPEconomicGroupL,
  dvtPPGovGroupL,
  dvtPPNetworkGroupL,
  dvtPPTechnicalGroupL,
 )
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayRATIFY)
import Cardano.Ledger.Conway.Governance.Procedures (
  Anchor (..),
  AnchorData (..),
  Committee (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionPurpose (..),
  GovActionState (..),
  GovProcedures (..),
  PrevGovActionId (..),
  ProposalProcedure (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  gasCommitteeVotesL,
  gasDRepVotesL,
  gasDepositL,
  gasExpiresAfterL,
  gasStakePoolVotesL,
  govActionIdToText,
  indexedGovProps,
 )
import Cardano.Ledger.Conway.Governance.Proposals (
  PrevGovActionIds (..),
  PrevGovActionIdsChildren (..),
  Proposals,
  fromGovActionStateSeq,
  isConsistent_,
  pgacCommitteeL,
  pgacConstitutionL,
  pgacHardForkL,
  pgacPParamUpdateL,
  proposalsActions,
  proposalsAddProposal,
  proposalsAddVote,
  proposalsGovActionStates,
  proposalsIds,
  proposalsLookupId,
  proposalsRemoveIds,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams (..),
  DRepVotingThresholds (..),
  PPGroup (..),
  PoolVotingThresholds (..),
  ppCommitteeMinSizeL,
  ppDRepVotingThresholdsL,
  ppPoolVotingThresholdsL,
 )
import Cardano.Ledger.Core (
  Era (EraCrypto),
  EraPParams (..),
  PParams (..),
  PParamsUpdate,
  emptyPParams,
  fromEraCBOR,
  ppProtocolVersionL,
  toEraCBOR,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  NewEpochState (..),
  certDState,
  certVState,
  credMap,
  dsUnified,
  epochStateGovStateL,
  epochStateIncrStakeDistrL,
  epochStateRegDrepL,
  epochStateStakeDistrL,
  epochStateTreasuryL,
  epochStateUMapL,
  esLStateL,
  lsCertState,
  lsUTxOState,
  lsUTxOStateL,
  nesEsL,
  utxosGovStateL,
  utxosStakeDistr,
  vsCommitteeState,
  vsDReps,
 )
import Cardano.Ledger.UMap
import qualified Cardano.Ledger.UMap as UMap
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Trans.Reader (Reader, ReaderT, ask, runReader)
import Control.State.Transition.Extended
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (Pulsable (..), pulse)
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..), allNoThunks)

-- | A snapshot of information from the previous epoch stored inside the Pulser.
--   After the pulser completes, but before the epoch turns, this information
--   is store in the 'DRComplete' constructor of the 'DRepPulsingState'
--   These are the values at the start of the current epoch. This allows the API
--   To access these "previous" values, both during and after pulsing.
data PulsingSnapshot era = PulsingSnapshot
  { psProposals :: !(StrictSeq (GovActionState era))
  , psDRepDistr :: !(Map (DRep (EraCrypto era)) (CompactForm Coin))
  , psDRepState :: !(Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
  }
  deriving (Generic)

psProposalsL :: Lens' (PulsingSnapshot era) (StrictSeq (GovActionState era))
psProposalsL = lens psProposals (\x y -> x {psProposals = y})

psDRepDistrL :: Lens' (PulsingSnapshot era) (Map (DRep (EraCrypto era)) (CompactForm Coin))
psDRepDistrL = lens psDRepDistr (\x y -> x {psDRepDistr = y})

psDRepStateL :: Lens' (PulsingSnapshot era) (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
psDRepStateL = lens psDRepState (\x y -> x {psDRepState = y})

deriving instance EraPParams era => Eq (PulsingSnapshot era)

deriving instance EraPParams era => Show (PulsingSnapshot era)

instance EraPParams era => NFData (PulsingSnapshot era)

instance EraPParams era => NoThunks (PulsingSnapshot era)

toPulsingSnapshotsPairs :: (KeyValue e a, EraPParams era) => PulsingSnapshot era -> [a]
toPulsingSnapshotsPairs gas@(PulsingSnapshot _ _ _) =
  let (PulsingSnapshot {..}) = gas
   in [ "psProposals" .= psProposals
      , "psDRepDistr" .= psDRepDistr
      , "psDRepState" .= psDRepState
      ]

instance EraPParams era => ToJSON (PulsingSnapshot era) where
  toJSON = object . toPulsingSnapshotsPairs
  toEncoding = pairs . mconcat . toPulsingSnapshotsPairs

instance Default (PulsingSnapshot era) where
  def = PulsingSnapshot mempty def def

instance EraPParams era => EncCBOR (PulsingSnapshot era) where
  encCBOR PulsingSnapshot {..} =
    encode $
      Rec PulsingSnapshot
        !> To psProposals
        !> To psDRepDistr
        !> To psDRepState

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (PulsingSnapshot era) where
  decShareCBOR _ =
    decode $
      RecD PulsingSnapshot
        <! From
        <! From
        <! From

instance EraPParams era => DecCBOR (PulsingSnapshot era) where
  decCBOR =
    decode $
      RecD PulsingSnapshot
        <! From
        <! From
        <! From

instance EraPParams era => ToCBOR (PulsingSnapshot era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (PulsingSnapshot era) where
  fromCBOR = fromEraCBOR @era

data EnactState era = EnactState
  { ensCommittee :: !(StrictMaybe (Committee era))
  -- ^ Constitutional Committee
  , ensConstitution :: !(Constitution era)
  -- ^ Constitution
  , ensCurPParams :: !(PParams era)
  , ensPrevPParams :: !(PParams era)
  , ensTreasury :: !Coin
  , ensWithdrawals :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , ensPrevGovActionIds :: !(PrevGovActionIds era)
  -- ^ Last enacted GovAction Ids
  , ensPrevGovActionIdsChildren :: !(PrevGovActionIdsChildren era) -- TODO: @aniketd Move this inside Proposals
  }
  deriving (Generic)

ensCommitteeL :: Lens' (EnactState era) (StrictMaybe (Committee era))
ensCommitteeL = lens ensCommittee (\x y -> x {ensCommittee = y})

ensConstitutionL :: Lens' (EnactState era) (Constitution era)
ensConstitutionL = lens ensConstitution (\x y -> x {ensConstitution = y})

ensProtVerL :: EraPParams era => Lens' (EnactState era) ProtVer
ensProtVerL = ensCurPParamsL . ppProtocolVersionL

ensCurPParamsL :: Lens' (EnactState era) (PParams era)
ensCurPParamsL = lens ensCurPParams (\es x -> es {ensCurPParams = x})

ensPrevPParamsL :: Lens' (EnactState era) (PParams era)
ensPrevPParamsL = lens ensPrevPParams (\es x -> es {ensPrevPParams = x})

ensTreasuryL :: Lens' (EnactState era) Coin
ensTreasuryL = lens ensTreasury $ \es x -> es {ensTreasury = x}

ensWithdrawalsL :: Lens' (EnactState era) (Map (Credential 'Staking (EraCrypto era)) Coin)
ensWithdrawalsL = lens ensWithdrawals $ \es x -> es {ensWithdrawals = x}

ensPrevGovActionIdsL :: Lens' (EnactState era) (PrevGovActionIds era)
ensPrevGovActionIdsL = lens ensPrevGovActionIds (\es x -> es {ensPrevGovActionIds = x})

ensPrevGovActionIdsChildrenL :: Lens' (EnactState era) (PrevGovActionIdsChildren era)
ensPrevGovActionIdsChildrenL =
  lens ensPrevGovActionIdsChildren (\es x -> es {ensPrevGovActionIdsChildren = x})

ensPrevPParamUpdateL ::
  Lens' (EnactState era) (StrictMaybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
ensPrevPParamUpdateL =
  lens
    (pgaPParamUpdate . ensPrevGovActionIds)
    (\es x -> es {ensPrevGovActionIds = (ensPrevGovActionIds es) {pgaPParamUpdate = x}})

ensPrevHardForkL ::
  Lens' (EnactState era) (StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
ensPrevHardForkL =
  lens
    (pgaHardFork . ensPrevGovActionIds)
    (\es x -> es {ensPrevGovActionIds = (ensPrevGovActionIds es) {pgaHardFork = x}})

ensPrevCommitteeL ::
  Lens' (EnactState era) (StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
ensPrevCommitteeL =
  lens
    (pgaCommittee . ensPrevGovActionIds)
    (\es x -> es {ensPrevGovActionIds = (ensPrevGovActionIds es) {pgaCommittee = x}})

ensPrevConstitutionL ::
  Lens' (EnactState era) (StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
ensPrevConstitutionL =
  lens
    (pgaConstitution . ensPrevGovActionIds)
    (\es x -> es {ensPrevGovActionIds = (ensPrevGovActionIds es) {pgaConstitution = x}})

instance EraPParams era => ToJSON (EnactState era) where
  toJSON = object . toEnactStatePairs
  toEncoding = pairs . mconcat . toEnactStatePairs

toEnactStatePairs :: (KeyValue e a, EraPParams era) => EnactState era -> [a]
toEnactStatePairs cg@(EnactState _ _ _ _ _ _ _ _) =
  let EnactState {..} = cg
   in [ "committee" .= ensCommittee
      , "constitution" .= ensConstitution
      , "curPParams" .= ensCurPParams
      , "prevPParams" .= ensPrevPParams
      , "prevGovActionIds" .= ensPrevGovActionIds
      , "prevGovActionIdsChilren" .= ensPrevGovActionIdsChildren
      ]

deriving instance Eq (PParams era) => Eq (EnactState era)

deriving instance Show (PParams era) => Show (EnactState era)

instance EraPParams era => Default (EnactState era) where
  def =
    EnactState
      def
      def
      def
      def
      (Coin 0)
      def
      def
      def

instance EraPParams era => DecCBOR (EnactState era) where
  decCBOR = decNoShareCBOR

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (EnactState era) where
  decShareCBOR _ =
    decode $
      RecD EnactState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance EraPParams era => EncCBOR (EnactState era) where
  encCBOR EnactState {..} =
    encode $
      Rec EnactState
        !> To ensCommittee
        !> To ensConstitution
        !> To ensCurPParams
        !> To ensPrevPParams
        !> To ensTreasury
        !> To ensWithdrawals
        !> To ensPrevGovActionIds
        !> To ensPrevGovActionIdsChildren

instance EraPParams era => ToCBOR (EnactState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (EnactState era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => NFData (EnactState era)

instance EraPParams era => NoThunks (EnactState era)

-- ========================================

data RatifyState era = RatifyState
  { rsEnactState :: !(EnactState era)
  , rsRemoved :: !(Set (GovActionId (EraCrypto era)))
  , rsEnacted :: !(Set (GovActionId (EraCrypto era)))
  , rsDelayed :: !Bool
  }
  deriving (Generic)

deriving instance EraPParams era => Eq (RatifyState era)

deriving instance EraPParams era => Show (RatifyState era)

rsEnactStateL :: Lens' (RatifyState era) (EnactState era)
rsEnactStateL = lens rsEnactState (\x y -> x {rsEnactState = y})

instance EraPParams era => Default (RatifyState era)

instance EraPParams era => NFData (RatifyState era)

instance EraPParams era => NoThunks (RatifyState era)

instance EraPParams era => ToJSON (RatifyState era) where
  toJSON = object . toRatifyStatePairs
  toEncoding = pairs . mconcat . toRatifyStatePairs

toRatifyStatePairs :: (KeyValue e a, EraPParams era) => RatifyState era -> [a]
toRatifyStatePairs cg@(RatifyState _ _ _ _) =
  let RatifyState {..} = cg
   in [ "nextEnactState" .= rsEnactState
      , "removedGovActions" .= rsRemoved
      , "enactedGovActions" .= rsEnacted
      , "ratificationDelayed" .= rsDelayed
      ]

-- =============================================
data ConwayGovState era = ConwayGovState
  { cgProposals :: !(Proposals era)
  , cgEnactState :: !(EnactState era)
  , cgDRepPulsingState :: !(DRepPulsingState era)
  -- ^ The 'cgDRepPulsingState' field is a pulser that incrementally computes the stake distribution of the DReps
  --   over the Epoch following the close of voting at end of the previous Epoch. It assembles this with some of
  --   its other internal components into a (RatifyEnv era) when it completes, and then calls the RATIFY rule
  --   and eventually returns the updated RatifyState. The pulser is created at the Epoch boundary, but does
  --   no work until it is pulsed in the 'NEWEPOCH' rule, whenever the system is NOT at the epoch boundary.
  }
  deriving (Generic, Show)

deriving instance EraPParams era => Eq (ConwayGovState era)

cgProposalsL :: Lens' (ConwayGovState era) (Proposals era)
cgProposalsL = lens cgProposals (\x y -> x {cgProposals = y})

cgEnactStateL :: Lens' (ConwayGovState era) (EnactState era)
cgEnactStateL = lens cgEnactState (\x y -> x {cgEnactState = y})

cgDRepPulsingStateL :: Lens' (ConwayGovState era) (DRepPulsingState era)
cgDRepPulsingStateL = lens cgDRepPulsingState (\x y -> x {cgDRepPulsingState = y})

curPParamsConwayGovStateL :: Lens' (ConwayGovState era) (PParams era)
curPParamsConwayGovStateL = cgEnactStateL . ensCurPParamsL

prevPParamsConwayGovStateL :: Lens' (ConwayGovState era) (PParams era)
prevPParamsConwayGovStateL = cgEnactStateL . ensPrevPParamsL

conwayGovStateDRepDistrG :: SimpleGetter (ConwayGovState era) (Map (DRep (EraCrypto era)) (CompactForm Coin))
conwayGovStateDRepDistrG = to (\govst -> (psDRepDistr . fst) $ finishDRepPulser (cgDRepPulsingState govst))

getRatifyState :: ConwayGovState era -> RatifyState era
getRatifyState (ConwayGovState {cgDRepPulsingState}) = snd $ finishDRepPulser cgDRepPulsingState

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (ConwayGovState era) where
  decShareCBOR _ =
    decode $
      RecD ConwayGovState
        <! From
        <! From
        <! From

instance EraPParams era => DecCBOR (ConwayGovState era) where
  decCBOR =
    decode $
      RecD ConwayGovState
        <! From
        <! From
        <! From

instance EraPParams era => EncCBOR (ConwayGovState era) where
  encCBOR ConwayGovState {..} =
    encode $
      Rec ConwayGovState
        !> To cgProposals
        !> To cgEnactState
        !> To cgDRepPulsingState

instance EraPParams era => ToCBOR (ConwayGovState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovState era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => Default (ConwayGovState era) where
  def = ConwayGovState def def (DRComplete def def)

instance EraPParams era => NFData (ConwayGovState era)

instance EraPParams era => NoThunks (ConwayGovState era)

instance EraPParams era => ToJSON (ConwayGovState era) where
  toJSON = object . toConwayGovPairs
  toEncoding = pairs . mconcat . toConwayGovPairs

toConwayGovPairs :: (KeyValue e a, EraPParams era) => ConwayGovState era -> [a]
toConwayGovPairs cg@(ConwayGovState _ _ _) =
  let ConwayGovState {..} = cg
   in [ "proposals" .= cgProposals
      , "enactState" .= cgEnactState
      , "nextRatifyState" .= extractDRepPulsingState cgDRepPulsingState
      ]

instance EraPParams (ConwayEra c) => EraGov (ConwayEra c) where
  type GovState (ConwayEra c) = ConwayGovState (ConwayEra c)

  getConstitution g = Just $ g ^. cgEnactStateL . ensConstitutionL

  getCommitteeMembers g =
    case g ^. cgEnactStateL . ensCommitteeL of
      SJust Committee {..} -> Just (committeeMembers, committeeQuorum)
      SNothing -> Nothing

  curPParamsGovStateL = curPParamsConwayGovStateL

  prevPParamsGovStateL = prevPParamsConwayGovStateL

  obligationGovState st =
    Obligations
      { oblProposal = foldMap' gasDeposit $ proposalsActions (st ^. cgProposalsL)
      , oblDRep = Coin 0
      , oblStake = Coin 0
      , oblPool = Coin 0
      }

  getDRepDistr govst = psDRepDistr . fst $ finishDRepPulser (govst ^. drepPulsingStateGovStateL)

class EraGov era => ConwayEraGov era where
  constitutionGovStateL :: Lens' (GovState era) (Constitution era)
  proposalsGovStateL :: Lens' (GovState era) (Proposals era)
  drepPulsingStateGovStateL :: Lens' (GovState era) (DRepPulsingState era)
  enactStateGovStateL :: Lens' (GovState era) (EnactState era)

instance Crypto c => ConwayEraGov (ConwayEra c) where
  constitutionGovStateL = cgEnactStateL . ensConstitutionL
  proposalsGovStateL = cgProposalsL
  drepPulsingStateGovStateL = cgDRepPulsingStateL
  enactStateGovStateL = cgEnactStateL

pparamsUpdateThreshold ::
  forall era.
  ConwayEraPParams era =>
  PParams era ->
  PParamsUpdate era ->
  UnitInterval
pparamsUpdateThreshold pp ppu =
  let thresholdLens = \case
        NetworkGroup -> dvtPPNetworkGroupL
        GovGroup -> dvtPPGovGroupL
        TechnicalGroup -> dvtPPTechnicalGroupL
        EconomicGroup -> dvtPPEconomicGroupL
      lookupGroupThreshold grp =
        pp ^. ppDRepVotingThresholdsL . thresholdLens grp
   in Set.foldr' max minBound $
        Set.map lookupGroupThreshold $
          modifiedPPGroups @era ppu

data VotingThreshold
  = -- | This is the actual threshold. It is lazy, because upon proposal we only care if
    -- the voting is allowed or not, instead of getting the actual threshold value.
    VotingThreshold UnitInterval -- <- lazy on purpose
  | -- | Does not have a threshold, therefore an action can not be ratified
    NoVotingThreshold
  | -- | Some GovActions are not allowed to be voted by some entities
    NoVotingAllowed

toRatifyVotingThreshold :: VotingThreshold -> StrictMaybe UnitInterval
toRatifyVotingThreshold = \case
  VotingThreshold t -> SJust t -- concrete threshold
  NoVotingThreshold -> SNothing -- no voting threshold prevents ratification
  NoVotingAllowed -> SJust minBound -- votes should not count, set threshold to zero

isVotingAllowed :: VotingThreshold -> Bool
isVotingAllowed = \case
  VotingThreshold {} -> True
  NoVotingThreshold -> True
  NoVotingAllowed -> False

isStakePoolVotingAllowed ::
  ConwayEraPParams era =>
  GovAction era ->
  Bool
isStakePoolVotingAllowed =
  isVotingAllowed . votingStakePoolThresholdInternal pp isElectedCommittee
  where
    -- Information about presence of committe or values in PParams are irrelevant for
    -- knowing if voting is allowed or not:
    pp = emptyPParams
    isElectedCommittee = False

votingStakePoolThreshold ::
  ConwayEraPParams era =>
  RatifyState era ->
  GovAction era ->
  StrictMaybe UnitInterval
votingStakePoolThreshold ratifyState =
  toRatifyVotingThreshold . votingStakePoolThresholdInternal pp isElectedCommittee
  where
    pp = ratifyState ^. rsEnactStateL . ensCurPParamsL
    isElectedCommittee = isSJust $ ratifyState ^. rsEnactStateL . ensCommitteeL

votingStakePoolThresholdInternal ::
  ConwayEraPParams era =>
  PParams era ->
  Bool ->
  GovAction era ->
  VotingThreshold
votingStakePoolThresholdInternal pp isElectedCommittee action =
  let PoolVotingThresholds
        { pvtCommitteeNoConfidence
        , pvtCommitteeNormal
        , pvtHardForkInitiation
        } = pp ^. ppPoolVotingThresholdsL
   in case action of
        NoConfidence {} -> VotingThreshold pvtCommitteeNoConfidence
        UpdateCommittee {} ->
          VotingThreshold $
            if isElectedCommittee
              then pvtCommitteeNormal
              else pvtCommitteeNoConfidence
        NewConstitution {} -> NoVotingAllowed
        HardForkInitiation {} -> VotingThreshold pvtHardForkInitiation
        ParameterChange {} -> NoVotingAllowed
        TreasuryWithdrawals {} -> NoVotingAllowed
        InfoAction {} -> NoVotingThreshold

isCommitteeVotingAllowed :: ConwayEraPParams era => GovAction era -> Bool
isCommitteeVotingAllowed =
  isVotingAllowed . votingCommitteeThresholdInternal def committee
  where
    -- Information about presence of committe is irrelevant for knowing if voting is
    -- allowed or not
    committee = SNothing

votingCommitteeThreshold ::
  ConwayEraPParams era =>
  RatifyState era ->
  GovAction era ->
  StrictMaybe UnitInterval
votingCommitteeThreshold ratifyState =
  toRatifyVotingThreshold . votingCommitteeThresholdInternal pp committee
  where
    committee = ratifyState ^. rsEnactStateL . ensCommitteeL
    pp = ratifyState ^. rsEnactStateL . ensCurPParamsL

votingCommitteeThresholdInternal ::
  ConwayEraPParams era =>
  PParams era ->
  StrictMaybe (Committee era) ->
  GovAction era ->
  VotingThreshold
votingCommitteeThresholdInternal pp committee = \case
  NoConfidence {} -> NoVotingAllowed
  UpdateCommittee {} -> NoVotingAllowed
  NewConstitution {} -> threshold
  HardForkInitiation {} -> threshold
  ParameterChange {} -> threshold
  TreasuryWithdrawals {} -> threshold
  InfoAction {} -> NoVotingThreshold
  where
    threshold =
      case committeeQuorum <$> committee of
        -- if the committee size is smaller than the mnimimum given in pparams,
        -- we treat it as if we had no committe
        SJust t | committeeSize >= minSize -> VotingThreshold t
        _ -> NoVotingThreshold
    minSize = pp ^. ppCommitteeMinSizeL
    committeeSize = fromIntegral $ Map.size $ foldMap' committeeMembers committee

isDRepVotingAllowed ::
  ConwayEraPParams era =>
  GovAction era ->
  Bool
isDRepVotingAllowed =
  isVotingAllowed . votingDRepThresholdInternal pp isElectedCommittee
  where
    -- Information about presence of committe or values in PParams are irrelevant for
    -- knowing if voting is allowed or not:
    pp = emptyPParams
    isElectedCommittee = False

votingDRepThreshold ::
  ConwayEraPParams era =>
  RatifyState era ->
  GovAction era ->
  StrictMaybe UnitInterval
votingDRepThreshold ratifyState =
  toRatifyVotingThreshold . votingDRepThresholdInternal pp isElectedCommittee
  where
    pp = ratifyState ^. rsEnactStateL . ensCurPParamsL
    isElectedCommittee = isSJust $ ratifyState ^. rsEnactStateL . ensCommitteeL

votingDRepThresholdInternal ::
  ConwayEraPParams era =>
  PParams era ->
  Bool ->
  GovAction era ->
  VotingThreshold
votingDRepThresholdInternal pp isElectedCommittee action =
  let DRepVotingThresholds
        { dvtCommitteeNoConfidence
        , dvtCommitteeNormal
        , dvtUpdateToConstitution
        , dvtHardForkInitiation
        , dvtTreasuryWithdrawal
        } = pp ^. ppDRepVotingThresholdsL
   in case action of
        NoConfidence {} -> VotingThreshold dvtCommitteeNoConfidence
        UpdateCommittee {} ->
          VotingThreshold $
            if isElectedCommittee
              then dvtCommitteeNormal
              else dvtCommitteeNoConfidence
        NewConstitution {} -> VotingThreshold dvtUpdateToConstitution
        HardForkInitiation {} -> VotingThreshold dvtHardForkInitiation
        ParameterChange _ ppu -> VotingThreshold $ pparamsUpdateThreshold pp ppu
        TreasuryWithdrawals {} -> VotingThreshold dvtTreasuryWithdrawal
        InfoAction {} -> NoVotingThreshold

-- ===================================================================
-- Lenses for access to (DRepPulsingState era)

newEpochStateDRepPulsingStateL :: ConwayEraGov era => Lens' (NewEpochState era) (DRepPulsingState era)
newEpochStateDRepPulsingStateL = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . drepPulsingStateGovStateL

epochStateDRepPulsingStateL :: ConwayEraGov era => Lens' (EpochState era) (DRepPulsingState era)
epochStateDRepPulsingStateL = esLStateL . lsUTxOStateL . utxosGovStateL . drepPulsingStateGovStateL

-- ===============================================================================
-- Algorithm for computing the DRep stake distrubution, with and without pulsing.
-- ===============================================================================

-- | Given three inputs
--   1) Map (Credential 'Staking c) (DRep c).   The delegation map. Inside the DRepUView of the UMap 'um' from the DState.
--   2) regDreps :: Map (Credential 'DRepRole c) (DRepState c). The map of registered DReps to their state. The first part of the VState.
--   3) stakeDistr :: VMap VB VP (Credential 'Staking c) (CompactForm Coin). The aggregated stake distr extracted from the
--      first component of the IncrementalStake i.e. (IStake credmap _) where credmap is converted to a VMap
--  Compute the Drep distribution of stake(Coin)
--  cost is expected to be O(size of 'stakeDistr' * log (size of 'um') * log (size of 'regDreps'))
--  This is going to be expensive, so we will want to pulse it. Without pulsing, we estimate 3-5 seconds
computeDrepDistr ::
  UMap c ->
  Map (Credential 'DRepRole c) (DRepState c) ->
  Map (Credential 'Staking c) (CompactForm Coin) ->
  Map (DRep c) (CompactForm Coin)
computeDrepDistr um regDreps stakeDistr = Map.foldlWithKey' (accumDRepDistr um regDreps) Map.empty stakeDistr

-- | For each 'stakecred' and coin 'c', check if that credential is delegated to some DRep.
--   If so then add that coin to the aggregated map 'ans', mapping DReps to compact Coin
--   If the DRep is a DRepCredential (rather than AwaysAbstain or AlwaysNoConfidence) then check
--   that the credential is a member of the registered DRep map ('regDreps') before adding it to 'ans'
accumDRepDistr ::
  UMap c ->
  Map (Credential 'DRepRole c) (DRepState c) ->
  Map (DRep c) (CompactForm Coin) ->
  Credential 'Staking c ->
  CompactForm Coin ->
  Map (DRep c) (CompactForm Coin)
accumDRepDistr um regDreps ans stakecred c =
  case UMap.lookup stakecred (DRepUView um) of
    Nothing -> ans
    Just drep@DRepAlwaysAbstain -> Map.insertWith UMap.addCompact drep c ans
    Just drep@DRepAlwaysNoConfidence -> Map.insertWith UMap.addCompact drep c ans
    Just (DRepCredential cred2) | Map.notMember cred2 regDreps -> ans
    Just drep@(DRepCredential _) -> Map.insertWith UMap.addCompact drep c ans

-- | The type of a Pulser which uses 'accumDRepDistr' as its underlying function.
--   'accumDRepDistr' will be partially applied to the components of type (UMap c)
--   and (Map (Credential 'DRepRole c) (DRepState c)) when pulsing. Note that we use two type
--   equality (~) constraints to fix both the monad 'm' and the 'ans' type, to
--   the context where we will use the type as a Pulser. The type DRepPulser must
--   have 'm' and 'ans' as its last two parameters so we can make a Pulsable instance.
--   We will always use this instantiation (DRepPulser era Identity (RatifyState era))
data DRepPulser era (m :: Type -> Type) ans where
  DRepPulser ::
    forall era ans m.
    (ans ~ RatifyState era, m ~ Identity, RunConwayRatify era) =>
    { dpPulseSize :: !Int
    -- ^ How many elements of 'dpBalance' to consume each pulse.
    , dpUMap :: !(UMap (EraCrypto era))
    -- ^ Snapshot containing the mapping of stake credentials to Pools.
    , dpBalance :: !(Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin))
    -- ^ The object we are iterating over. Shrinks with each pulse
    , dpStakeDistr :: !(Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin))
    -- ^ Snapshot of the stake distr (comes from the IncrementalStake)
    , dpStakePoolDistr :: !(PoolDistr (EraCrypto era))
    -- ^ Snapshot of the pool distr
    , dpDRepDistr :: !(Map (DRep (EraCrypto era)) (CompactForm Coin))
    -- ^ The partial result that grows with each pulse. The purpose of the pulsing.
    , dpDRepState :: !(Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
    -- ^ Snapshot of registered DRep credentials
    , dpCurrentEpoch :: !EpochNo
    -- ^ Snapshot of the Epoch this pulser will complete in.
    , dpCommitteeState :: !(CommitteeState era)
    -- ^ Snapshot of the CommitteeState
    , dpEnactState :: !(EnactState era)
    -- ^ Snapshot of the EnactState, Used to build the Env of the RATIFY rule
    , dpProposals :: !(StrictSeq (GovActionState era))
    -- ^ Snap shot of the (Proposals era) isomorphic to (StrictSeq (GovActionState era))
    --   Used to build the Signal of the RATIFY rule
    , dpGlobals :: !Globals
    } ->
    DRepPulser era m ans

instance EraPParams era => Eq (DRepPulser era Identity (RatifyState era)) where
  x == y = finishDRepPulser (DRPulsing x) == finishDRepPulser (DRPulsing y)

instance Pulsable (DRepPulser era) where
  done DRepPulser {dpBalance} = Map.null dpBalance

  current x@(DRepPulser {}) = snd $ finishDRepPulser (DRPulsing x)

  pulseM pulser@(DRepPulser {..})
    | Map.null dpBalance = pure pulser
    | otherwise =
        let !(!steps, !balance') = Map.splitAt dpPulseSize dpBalance
            drep' = Map.foldlWithKey' (accumDRepDistr dpUMap dpDRepState) dpDRepDistr steps
         in pure (pulser {dpBalance = balance', dpDRepDistr = drep'})

  completeM x@(DRepPulser {}) = pure (snd $ finishDRepPulser @era (DRPulsing x))

deriving instance (EraPParams era, Show ans) => Show (DRepPulser era m ans)

instance EraPParams era => NoThunks (DRepPulser era Identity (RatifyState era)) where
  showTypeOf _ = "DRepPulser"
  wNoThunks ctxt drp@(DRepPulser _ _ _ _ _ _ _ _ _ _ _ _) =
    allNoThunks
      [ noThunks ctxt (dpPulseSize drp)
      , noThunks ctxt (dpUMap drp)
      , noThunks ctxt (dpBalance drp)
      , noThunks ctxt (dpStakeDistr drp)
      , noThunks ctxt (dpStakePoolDistr drp)
      , noThunks ctxt (dpDRepDistr drp)
      , noThunks ctxt (dpDRepState drp)
      , noThunks ctxt (dpCurrentEpoch drp)
      , noThunks ctxt (dpCommitteeState drp)
      , noThunks ctxt (dpEnactState drp)
      , noThunks ctxt (dpProposals drp)
      , noThunks ctxt (dpGlobals drp)
      ]

instance EraPParams era => NFData (DRepPulser era Identity (RatifyState era)) where
  rnf (DRepPulser n um bal stake pool drep dstate ep cs es as gs) =
    n `deepseq`
      um `deepseq`
        bal `deepseq`
          stake `deepseq`
            pool `deepseq`
              drep `deepseq`
                dstate `deepseq`
                  ep `deepseq`
                    cs `deepseq`
                      es `deepseq`
                        as `deepseq`
                          rnf gs

class
  ( STS (ConwayRATIFY era)
  , Signal (ConwayRATIFY era) ~ RatifySignal era
  , BaseM (ConwayRATIFY era) ~ Reader Globals
  , Environment (ConwayRATIFY era) ~ RatifyEnv era
  , State (ConwayRATIFY era) ~ RatifyState era
  ) =>
  RunConwayRatify era
  where
  runConwayRatify :: Globals -> RatifyEnv era -> RatifyState era -> RatifySignal era -> RatifyState era
  runConwayRatify globals ratifyEnv ratifyState ratifySig =
    let ratifyResult =
          runReader (applySTS @(ConwayRATIFY era) (TRC (ratifyEnv, ratifyState, ratifySig))) globals
     in case ratifyResult of
          Left ps ->
            error (unlines ("Impossible: RATIFY rule never fails, but it did:" : map show ps))
          Right ratifyState' -> ratifyState'

finishDRepPulser :: DRepPulsingState era -> (PulsingSnapshot era, RatifyState era)
finishDRepPulser (DRComplete snap ratifyState) = (snap, ratifyState)
finishDRepPulser (DRPulsing (DRepPulser {..})) =
  (PulsingSnapshot dpProposals finalDRepDistr dpDRepState, ratifyState')
  where
    !finalDRepDistr = Map.foldlWithKey' (accumDRepDistr dpUMap dpDRepState) dpDRepDistr dpBalance
    !ratifyEnv =
      RatifyEnv
        { reStakeDistr = dpStakeDistr
        , reStakePoolDistr = dpStakePoolDistr
        , reDRepDistr = finalDRepDistr
        , reDRepState = dpDRepState
        , reCurrentEpoch = dpCurrentEpoch
        , reCommitteeState = dpCommitteeState
        }
    !ratifySig = RatifySignal dpProposals
    !ratifyState =
      RatifyState
        { rsRemoved = mempty
        , rsEnacted = mempty
        , rsEnactState = dpEnactState
        , rsDelayed = False
        }
    !ratifyState' = runConwayRatify dpGlobals ratifyEnv ratifyState ratifySig

-- ===========================================================
-- The State which is stored in ConwayGovState
-- ===========================================================

data DRepPulsingState era
  = DRPulsing !(DRepPulser era Identity (RatifyState era))
  | DRComplete
      !(PulsingSnapshot era)
      !(RatifyState era)
  deriving (Generic, NoThunks, NFData)

dormantEpoch :: DRepPulsingState era -> Bool
dormantEpoch (DRPulsing x) = SS.null (dpProposals x)
dormantEpoch (DRComplete b _) = SS.null (psProposals b)

-- | This is potentially an expensive getter. Make sure not to use it in the first 80% of
-- the epoch.
psDRepDistrG ::
  SimpleGetter (DRepPulsingState era) (Map (DRep (EraCrypto era)) (CompactForm Coin))
psDRepDistrG = to get
  where
    get (DRComplete x _) = psDRepDistr x
    get x = psDRepDistr . fst $ finishDRepPulser x

instance EraPParams era => Eq (DRepPulsingState era) where
  x == y = finishDRepPulser x == finishDRepPulser y

instance EraPParams era => Show (DRepPulsingState era) where
  show (DRComplete x m) = "(DRComplete " ++ show x ++ " " ++ show m ++ ")"
  show x = show (uncurry DRComplete (finishDRepPulser x))

instance EraPParams era => EncCBOR (DRepPulsingState era) where
  encCBOR (DRComplete x y) = encode (Rec DRComplete !> To x !> To y)
  encCBOR x@(DRPulsing (DRepPulser {})) = encode (Rec DRComplete !> To snap !> To ratstate)
    where
      (snap, ratstate) = finishDRepPulser x

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (DRepPulsingState era) where
  decShareCBOR _ =
    decode $
      RecD DRComplete
        <! From
        <! From

instance EraPParams era => DecCBOR (DRepPulsingState era) where
  decCBOR = decode (RecD DRComplete <! From <! From)

-- =====================================
-- High level operations of DRepDistr

pulseDRepPulsingState :: DRepPulsingState era -> DRepPulsingState era
pulseDRepPulsingState x@(DRComplete _ _) = x
pulseDRepPulsingState (DRPulsing x@(DRepPulser {})) =
  let x2 = pulse x
   in if done x2
        then uncurry DRComplete (finishDRepPulser (DRPulsing x2))
        else DRPulsing x2

completeDRepPulsingState :: DRepPulsingState era -> DRepPulsingState era
completeDRepPulsingState x@(DRPulsing _) = uncurry DRComplete (finishDRepPulser x)
completeDRepPulsingState x@(DRComplete {}) = x

extractDRepPulsingState :: DRepPulsingState era -> RatifyState era
extractDRepPulsingState x@(DRPulsing _) = snd (finishDRepPulser x)
extractDRepPulsingState (DRComplete _ x) = x

setCompleteDRepPulsingState ::
  GovState era ~ ConwayGovState era =>
  PulsingSnapshot era ->
  RatifyState era ->
  EpochState era ->
  EpochState era
setCompleteDRepPulsingState snapshot ratifyState epochState =
  epochState
    & epochStateGovStateL . cgDRepPulsingStateL
      .~ DRComplete snapshot ratifyState

-- | Refresh the pulser in the EpochState using all the new data that is needed to compute
-- the RatifyState when pulsing completes.
setFreshDRepPulsingState ::
  ( GovState era ~ ConwayGovState era
  , Monad m
  , RunConwayRatify era
  ) =>
  EpochNo ->
  PoolDistr (EraCrypto era) ->
  EpochState era ->
  ReaderT Globals m (EpochState era)
setFreshDRepPulsingState epochNo stakePoolDistr epochState = do
  -- When we are finished with the pulser that was started at the last epoch boundary, we
  -- need to initialize a fresh DRep pulser. We do so by computing the pulse size and
  -- gathering the data, which we will snapshot inside the pulser. We expect approximately
  -- 10*k-many blocks to be produced each epoch, where `k` value is the stability
  -- window. We must ensure for secure operation of the Hard Fork Combinator that we have
  -- the new EnactState available 2 stability windows before the end of the epoch, while
  -- spreading out stake distribution computation throughout the first 8 stability
  -- windows. Therefore, we divide the number of stake credentials by 8*k
  globals <- ask
  let ledgerState = epochState ^. esLStateL
      utxoState = lsUTxOState ledgerState
      stakeDistr = credMap $ utxosStakeDistr utxoState
      certState = lsCertState ledgerState
      dState = certDState certState
      vState = certVState certState
      govState = epochState ^. epochStateGovStateL
      stakeSize = Map.size stakeDistr
      -- Maximum number of blocks we are allowed to roll back
      k = securityParameter globals
      pulseSize = max 1 (ceiling (toInteger stakeSize % (8 * toInteger k)))
      epochState' =
        epochState
          & epochStateGovStateL . cgDRepPulsingStateL
            .~ DRPulsing
              ( DRepPulser
                  { dpPulseSize = pulseSize
                  , dpUMap = dsUnified dState
                  , dpBalance = stakeDistr -- used as the balance of things left to iterate over
                  , dpStakeDistr = stakeDistr -- used as part of the snapshot
                  , dpStakePoolDistr = stakePoolDistr
                  , dpDRepDistr = Map.empty -- The partial result starts as the empty map
                  , dpDRepState = vsDReps vState
                  , dpCurrentEpoch = epochNo
                  , dpCommitteeState = vsCommitteeState vState
                  , dpEnactState =
                      (cgEnactState govState)
                        { ensTreasury = epochState ^. epochStateTreasuryL
                        }
                  , dpProposals = proposalsActions (govState ^. cgProposalsL)
                  , dpGlobals = globals
                  }
              )
  pure epochState'

-- ===================================
-- RatifyEnv

newtype RatifySignal era = RatifySignal (StrictSeq (GovActionState era))
  deriving (Show)

data RatifyEnv era = RatifyEnv
  { reStakeDistr :: !(Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin))
  , reStakePoolDistr :: !(PoolDistr (EraCrypto era))
  , reDRepDistr :: !(Map (DRep (EraCrypto era)) (CompactForm Coin))
  , reDRepState :: !(Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
  , reCurrentEpoch :: !EpochNo
  , reCommitteeState :: !(CommitteeState era)
  }

deriving instance Show (RatifyEnv era)
deriving instance Eq (RatifyEnv era)

instance Default (RatifyEnv era) where
  def = RatifyEnv Map.empty (PoolDistr Map.empty) Map.empty Map.empty (EpochNo 0) def

instance Typeable era => NoThunks (RatifyEnv era) where
  showTypeOf _ = "RatifyEnv"
  wNoThunks ctxt (RatifyEnv stake pool drep dstate ep cs) =
    allNoThunks
      [ noThunks ctxt stake
      , noThunks ctxt pool
      , noThunks ctxt drep
      , noThunks ctxt dstate
      , noThunks ctxt ep
      , noThunks ctxt cs
      ]

instance Era era => NFData (RatifyEnv era) where
  rnf (RatifyEnv stake pool drep dstate ep cs) =
    stake `deepseq`
      pool `deepseq`
        drep `deepseq`
          dstate `deepseq`
            ep `deepseq`
              rnf cs

instance EraPParams era => EncCBOR (RatifyState era) where
  encCBOR (RatifyState es removed enacted delayed) =
    encode
      ( Rec (RatifyState @era)
          !> To es
          !> To removed
          !> To enacted
          !> To delayed
      )

instance EraPParams era => DecCBOR (RatifyState era) where
  decCBOR = decode (RecD RatifyState <! From <! From <! From <! From)

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (RatifyState era) where
  decShareCBOR _ =
    decode $
      RecD RatifyState
        <! From
        <! From
        <! From
        <! From

reDRepDistrL :: Lens' (RatifyEnv era) (Map (DRep (EraCrypto era)) (CompactForm Coin))
reDRepDistrL = lens reDRepDistr (\x y -> x {reDRepDistr = y})
