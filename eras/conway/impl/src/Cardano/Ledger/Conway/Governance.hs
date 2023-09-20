{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Governance (
  EraGov (..),
  GovSnapshots (..),
  insertGovSnapshots,
  EnactState (..),
  RatifyState (..),
  ConwayGovState (..),
  Committee (..),
  GovAction (..),
  GovActionState (..),
  GovActionIx (..),
  GovActionId (..),
  GovActionPurpose (..),
  PrevGovActionIds (..),
  PrevGovActionId (..),
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
  ProposalsSnapshot,
  snapshotInsertGovAction,
  snapshotActions,
  snapshotAddVote,
  snapshotIds,
  snapshotRemoveIds,
  fromGovActionStateSeq,
  isConsistent_,
  -- Lenses
  cgGovSnapshotsL,
  cgEnactStateL,
  cgRatifyStateL,
  cgDRepDistrL,
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
  curGovSnapshotsL,
  prevGovSnapshotsL,
  prevDRepsStateL,
  prevCommitteeStateL,
  gasCommitteeVotesL,
  gasDRepVotesL,
  gasExpiresAfterL,
  gasStakePoolVotesL,
  utxosGovStateL,
  newEpochStateDRepDistrL,
  epochStateDRepDistrL,
  epochStateStakeDistrL,
  epochStateIncrStakeDistrL,
  epochStateRegDrepL,
  epochStateUMapL,
  freshDRepPulser,
) where

import Cardano.Ledger.BaseTypes (
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
import Cardano.Ledger.CertState (CommitteeState, DRepState)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  dvtPPEconomicGroupL,
  dvtPPGovGroupL,
  dvtPPNetworkGroupL,
  dvtPPTechnicalGroupL,
 )
import Cardano.Ledger.Conway.Era (ConwayEra)
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
  gasExpiresAfterL,
  gasStakePoolVotesL,
  govActionIdToText,
  indexedGovProps,
 )
import Cardano.Ledger.Conway.Governance.Snapshots (
  ProposalsSnapshot,
  fromGovActionStateSeq,
  isConsistent_,
  snapshotActions,
  snapshotAddVote,
  snapshotIds,
  snapshotInsertGovAction,
  snapshotRemoveIds,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  DRepVotingThresholds (..),
  PParamGroup (..),
  PoolVotingThresholds (..),
  modifiedGroups,
  ppCommitteeMinSizeL,
  ppDRepVotingThresholdsL,
  ppPoolVotingThresholdsL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRepDistr (DRepDistr (..), extractDRepDistr, startDRepDistr)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  NewEpochState (..),
  epochStateIncrStakeDistrL,
  epochStateRegDrepL,
  epochStateStakeDistrL,
  epochStateUMapL,
  esLStateL,
  lsUTxOStateL,
  nesEsL,
  utxosGovStateL,
 )
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData (..))
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- ============================

data GovSnapshots era = GovSnapshots
  { curGovSnapshots :: !(ProposalsSnapshot era)
  , prevGovSnapshots :: !(ProposalsSnapshot era)
  , prevDRepsState ::
      !( Map
          (Credential 'DRepRole (EraCrypto era))
          (DRepState (EraCrypto era))
       )
  , prevCommitteeState :: !(CommitteeState era)
  }
  deriving (Generic)

instance EraPParams era => ToExpr (GovSnapshots era)

insertGovSnapshots ::
  GovActionState era ->
  GovSnapshots era ->
  GovSnapshots era
insertGovSnapshots v =
  curGovSnapshotsL %~ snapshotInsertGovAction v

curGovSnapshotsL ::
  Lens'
    (GovSnapshots era)
    (ProposalsSnapshot era)
curGovSnapshotsL = lens curGovSnapshots (\x y -> x {curGovSnapshots = y})

prevGovSnapshotsL ::
  Lens'
    (GovSnapshots era)
    (ProposalsSnapshot era)
prevGovSnapshotsL = lens prevGovSnapshots (\x y -> x {prevGovSnapshots = y})

prevDRepsStateL ::
  Lens'
    (GovSnapshots era)
    (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
prevDRepsStateL = lens prevDRepsState (\x y -> x {prevDRepsState = y})

prevCommitteeStateL ::
  Lens'
    (GovSnapshots era)
    (CommitteeState era)
prevCommitteeStateL = lens prevCommitteeState (\x y -> x {prevCommitteeState = y})

deriving instance EraPParams era => Eq (GovSnapshots era)

deriving instance EraPParams era => Show (GovSnapshots era)

toGovSnapshotsPairs :: (KeyValue a, EraPParams era) => GovSnapshots era -> [a]
toGovSnapshotsPairs gas@(GovSnapshots _ _ _ _) =
  let GovSnapshots {..} = gas
   in [ "curGovSnapshots" .= curGovSnapshots
      , "prevGovSnapshots" .= prevGovSnapshots
      , "prevDRepsState" .= prevDRepsState
      , "prevCommitteeState" .= prevCommitteeState
      ]

instance EraPParams era => ToJSON (GovSnapshots era) where
  toJSON = object . toGovSnapshotsPairs
  toEncoding = pairs . mconcat . toGovSnapshotsPairs

instance EraPParams era => NFData (GovSnapshots era)

instance EraPParams era => NoThunks (GovSnapshots era)

instance Default (GovSnapshots era) where
  def = GovSnapshots def def def def

instance EraPParams era => EncCBOR (GovSnapshots era) where
  encCBOR GovSnapshots {..} =
    encode $
      Rec GovSnapshots
        !> To curGovSnapshots
        !> To prevGovSnapshots
        !> To prevDRepsState
        !> To prevCommitteeState

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (GovSnapshots era) where
  decShareCBOR _ =
    decode $
      RecD GovSnapshots
        <! (D decNoShareCBOR)
        <! (D decNoShareCBOR)
        <! From
        <! (D decNoShareCBOR)

instance EraPParams era => ToCBOR (GovSnapshots era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (GovSnapshots era) where
  fromCBOR = fromEraShareCBOR @era

data PrevGovActionIds era = PrevGovActionIds
  { pgaPParamUpdate :: !(StrictMaybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
  -- ^ The last enacted GovActionId for a protocol parameter update
  , pgaHardFork :: !(StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
  -- ^ The last enacted GovActionId for a hard fork
  , pgaCommittee :: !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
  -- ^ The last enacted GovActionId for a committee change or no confidence vote
  , pgaConstitution :: !(StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
  -- ^ The last enacted GovActionId for a new constitution
  }
  deriving (Eq, Show, Generic)

instance NoThunks (PrevGovActionIds era)
instance Era era => NFData (PrevGovActionIds era)
instance Default (PrevGovActionIds era)

instance Era era => DecCBOR (PrevGovActionIds era) where
  decCBOR =
    decode $
      RecD PrevGovActionIds
        <! From
        <! From
        <! From
        <! From

instance Era era => EncCBOR (PrevGovActionIds era) where
  encCBOR PrevGovActionIds {..} =
    encode $
      Rec (PrevGovActionIds @era)
        !> To pgaPParamUpdate
        !> To pgaHardFork
        !> To pgaCommittee
        !> To pgaConstitution

toPrevGovActionIdsPairs :: (KeyValue a, Era era) => PrevGovActionIds era -> [a]
toPrevGovActionIdsPairs pga@(PrevGovActionIds _ _ _ _) =
  let PrevGovActionIds {..} = pga
   in [ "pgaPParamUpdate" .= pgaPParamUpdate
      , "pgaHardFork" .= pgaHardFork
      , "pgaCommittee" .= pgaCommittee
      , "pgaConstitution" .= pgaConstitution
      ]

instance Era era => ToJSON (PrevGovActionIds era) where
  toJSON = object . toPrevGovActionIdsPairs
  toEncoding = pairs . mconcat . toPrevGovActionIdsPairs

instance ToExpr (PrevGovActionIds era)

data EnactState era = EnactState
  { ensCommittee :: !(StrictMaybe (Committee era))
  -- ^ Constitutional Committee
  , ensConstitution :: !(Constitution era)
  -- ^ Constitution
  , ensPParams :: !(PParams era)
  , ensPrevPParams :: !(PParams era)
  , ensTreasury :: !Coin
  , ensWithdrawals :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , ensPrevGovActionIds :: !(PrevGovActionIds era)
  -- ^ Last enacted GovAction Ids
  }
  deriving (Generic)

ensCommitteeL :: Lens' (EnactState era) (StrictMaybe (Committee era))
ensCommitteeL = lens ensCommittee (\x y -> x {ensCommittee = y})

ensConstitutionL :: Lens' (EnactState era) (Constitution era)
ensConstitutionL = lens ensConstitution (\x y -> x {ensConstitution = y})

ensProtVerL :: EraPParams era => Lens' (EnactState era) ProtVer
ensProtVerL = ensCurPParamsL . ppProtocolVersionL

ensCurPParamsL :: Lens' (EnactState era) (PParams era)
ensCurPParamsL = lens ensPParams (\es x -> es {ensPParams = x})

ensPrevPParamsL :: Lens' (EnactState era) (PParams era)
ensPrevPParamsL = lens ensPrevPParams (\es x -> es {ensPrevPParams = x})

ensTreasuryL :: Lens' (EnactState era) Coin
ensTreasuryL = lens ensTreasury $ \es x -> es {ensTreasury = x}

ensWithdrawalsL :: Lens' (EnactState era) (Map (Credential 'Staking (EraCrypto era)) Coin)
ensWithdrawalsL = lens ensWithdrawals $ \es x -> es {ensWithdrawals = x}

ensPrevGovActionIdsL :: Lens' (EnactState era) (PrevGovActionIds era)
ensPrevGovActionIdsL = lens ensPrevGovActionIds (\es x -> es {ensPrevGovActionIds = x})

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

instance ToExpr (PParamsHKD Identity era) => ToExpr (EnactState era)

instance EraPParams era => ToJSON (EnactState era) where
  toJSON = object . toEnactStatePairs
  toEncoding = pairs . mconcat . toEnactStatePairs

toEnactStatePairs :: (KeyValue a, EraPParams era) => EnactState era -> [a]
toEnactStatePairs cg@(EnactState _ _ _ _ _ _ _) =
  let EnactState {..} = cg
   in [ "committee" .= ensCommittee
      , "constitution" .= ensConstitution
      , "pparams" .= ensPParams
      , "prevPParams" .= ensPParams
      , "treasury" .= ensTreasury
      , "withdrawals" .= ensWithdrawals
      , "prevGovActionIds" .= ensPrevGovActionIds
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

instance EraPParams era => EncCBOR (EnactState era) where
  encCBOR EnactState {..} =
    encode $
      Rec EnactState
        !> To ensCommittee
        !> To ensConstitution
        !> To ensPParams
        !> To ensPrevPParams
        !> To ensTreasury
        !> To ensWithdrawals
        !> To ensPrevGovActionIds

instance EraPParams era => ToCBOR (EnactState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (EnactState era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => NFData (EnactState era)

instance EraPParams era => NoThunks (EnactState era)

data RatifyState era = RatifyState
  { rsEnactState :: !(EnactState era)
  , rsRemoved :: !(Set (GovActionId (EraCrypto era)))
  , rsDelayed :: !Bool
  }
  deriving (Generic)

deriving instance EraPParams era => Eq (RatifyState era)

deriving instance EraPParams era => Show (RatifyState era)

rsEnactStateL :: Lens' (RatifyState era) (EnactState era)
rsEnactStateL = lens rsEnactState (\x y -> x {rsEnactState = y})

instance EraPParams era => ToExpr (RatifyState era)

instance EraPParams era => Default (RatifyState era)

instance EraPParams era => NFData (RatifyState era)

instance EraPParams era => NoThunks (RatifyState era)

data ConwayGovState era = ConwayGovState
  { cgGovSnapshots :: !(GovSnapshots era)
  , cgEnactState :: !(EnactState era)
  , cgDRepDistr :: !(DRepDistr (EraCrypto era))
  -- ^ The 'cgDRepDistr' field is a pulser that incrementally computes the stake distribution of the DReps
  --   over the Epoch following the close of voting at end of the previous Epoch. The pulser is created
  --   at the Epoch boundary, but does no work until it is pulsed in the 'NEWEPOCH' rule, whenever the
  --   system is NOT at the epoch boundary.
  }
  deriving (Generic, Eq, Show)

cgDRepDistrL :: Lens' (ConwayGovState era) (DRepDistr (EraCrypto era))
cgDRepDistrL = lens cgDRepDistr (\x y -> x {cgDRepDistr = y})

cgGovSnapshotsL :: Lens' (ConwayGovState era) (GovSnapshots era)
cgGovSnapshotsL = lens cgGovSnapshots (\x y -> x {cgGovSnapshots = y})

cgEnactStateL :: Lens' (ConwayGovState era) (EnactState era)
cgEnactStateL = lens cgEnactState (\x y -> x {cgEnactState = y})

{-# DEPRECATED cgRatifyStateL "Use cgEnactStateL instead" #-}
cgRatifyStateL :: Lens' (ConwayGovState era) (RatifyState era)
cgRatifyStateL =
  lens
    (\ConwayGovState {..} -> RatifyState cgEnactState mempty False)
    (\x RatifyState {..} -> x & cgEnactStateL .~ rsEnactState)

curPParamsConwayGovStateL :: Lens' (ConwayGovState era) (PParams era)
curPParamsConwayGovStateL = cgEnactStateL . ensCurPParamsL

prevPParamsConwayGovStateL :: Lens' (ConwayGovState era) (PParams era)
prevPParamsConwayGovStateL = cgEnactStateL . ensPrevPParamsL

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (ConwayGovState era) where
  decShareCBOR _ =
    decode $
      RecD ConwayGovState
        <! (D decNoShareCBOR)
        <! From
        <! From

instance EraPParams era => DecCBOR (ConwayGovState era) where
  decCBOR = decNoShareCBOR

instance EraPParams era => EncCBOR (ConwayGovState era) where
  encCBOR ConwayGovState {..} =
    encode $
      Rec ConwayGovState
        !> To cgGovSnapshots
        !> To cgEnactState
        !> To cgDRepDistr

instance EraPParams era => ToCBOR (ConwayGovState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (ConwayGovState era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => Default (ConwayGovState era) where
  def = ConwayGovState def def (DRComplete Map.empty)

instance EraPParams era => NFData (ConwayGovState era)

instance EraPParams era => NoThunks (ConwayGovState era)

instance EraPParams era => ToJSON (ConwayGovState era) where
  toJSON = object . toConwayGovPairs
  toEncoding = pairs . mconcat . toConwayGovPairs

instance EraPParams era => ToExpr (ConwayGovState era)

toConwayGovPairs :: (KeyValue a, EraPParams era) => ConwayGovState era -> [a]
toConwayGovPairs cg@(ConwayGovState _ _ _) =
  -- TODO WILL DRepDistr CHANGE THIS?
  let ConwayGovState {..} = cg
   in [ "gov" .= cgGovSnapshots
      , "ratify" .= cgEnactState
      ]

instance EraPParams (ConwayEra c) => EraGov (ConwayEra c) where
  type GovState (ConwayEra c) = ConwayGovState (ConwayEra c)

  getConstitution g = Just $ g ^. cgEnactStateL . ensConstitutionL

  curPParamsGovStateL = curPParamsConwayGovStateL

  prevPParamsGovStateL = prevPParamsConwayGovStateL

  obligationGovState st =
    foldMap' gasDeposit $ snapshotActions (st ^. cgGovSnapshotsL . curGovSnapshotsL)

  getDRepDistr govst = extractDRepDistr (govst ^. drepDistrGovStateL)

class EraGov era => ConwayEraGov era where
  constitutionGovStateL :: Lens' (GovState era) (Constitution era)
  snapshotsGovStateL :: Lens' (GovState era) (GovSnapshots era)
  drepDistrGovStateL :: Lens' (GovState era) (DRepDistr (EraCrypto era))

instance Crypto c => ConwayEraGov (ConwayEra c) where
  constitutionGovStateL = cgEnactStateL . ensConstitutionL
  snapshotsGovStateL = cgGovSnapshotsL
  drepDistrGovStateL = cgDRepDistrL

pparamsUpdateThreshold ::
  forall era.
  ConwayEraPParams era =>
  PParams era ->
  PParamsUpdate era ->
  UnitInterval
pparamsUpdateThreshold pp ppu =
  let thresholdLens = \case
        NetworkGroup -> dvtPPNetworkGroupL
        GovernanceGroup -> dvtPPGovGroupL
        TechnicalGroup -> dvtPPTechnicalGroupL
        EconomicGroup -> dvtPPEconomicGroupL
      lookupGroupThreshold grp =
        pp ^. ppDRepVotingThresholdsL . thresholdLens grp
   in Set.foldr' max minBound $
        Set.map lookupGroupThreshold $
          modifiedGroups @era ppu

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
-- Lenses for access to
-- 1. (DRepDistr (EraCrypto era))
-- 2. The 3 inputs we need to initialize one
--    a. (VMap VB VP (Credential 'Staking (EraCrypto era)) (CompactForm Coin)). Extracted from Incremental
--    b. (Set (Credential 'DRepRole (EraCrypto era))). Registered DReps in the CertState
--    c. (UMap (EraCrypto era)). The unified map in the DState.
--                               We will use the to DRepUView to obtain  (Map (Credential 'Staking c) (DRep c))
--                               to see what stake credentials delegate to which DReps
-- 3. and its completion:  (Map (DRep c) (CompactForm Coin)).  The aggregated voting power of each DRep

newEpochStateDRepDistrL :: ConwayEraGov era => Lens' (NewEpochState era) (DRepDistr (EraCrypto era))
newEpochStateDRepDistrL = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . drepDistrGovStateL

epochStateDRepDistrL :: ConwayEraGov era => Lens' (EpochState era) (DRepDistr (EraCrypto era))
epochStateDRepDistrL = esLStateL . lsUTxOStateL . utxosGovStateL . drepDistrGovStateL

-- | Construct a new (as yet unpulsed) DRepDistr from 3 pieces of the EpochState.
--   1) The unified map (storing the map from staking credentials to DReps).
--   2) The set of registered DReps
--   3) The map aggregating all the stake (coin) for each credential, from the Mark SnapShot.
freshDRepPulser :: Int -> EpochState era -> DRepDistr (EraCrypto era)
freshDRepPulser n es =
  startDRepDistr
    n
    (es ^. epochStateUMapL)
    (es ^. epochStateRegDrepL)
    (es ^. epochStateIncrStakeDistrL)
