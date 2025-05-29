{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Governance.Internal (
  EnactState (..),
  RatifyState (..),
  RatifyEnv (..),
  RatifySignal (..),
  votingStakePoolThreshold,
  votingDRepThreshold,
  votingCommitteeThreshold,
  isStakePoolVotingAllowed,
  isDRepVotingAllowed,
  isCommitteeVotingAllowed,
  reorderActions,
  actionPriority,
  hoistGovRelation,
  withGovActionParent,
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
  rsExpiredL,
  rsEnactedL,
  rsDelayedL,
  epochStateStakeDistrL,
  epochStateRegDrepL,
  epochStateUMapL,
  ratifySignalL,
  reStakePoolDistrL,
  reDRepDistrL,
  reDRepStateL,
  reCurrentEpochL,
  reCommitteeStateL,

  -- * Exported for testing
  pparamsUpdateThreshold,
) where

import Cardano.Ledger.BaseTypes (
  EpochNo (..),
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
  Interns,
  ToCBOR (..),
  decNoShareCBOR,
  decodeMap,
  decodeSeq,
  interns,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance.Procedures
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams (..),
  DRepGroup (..),
  DRepVotingThresholds (..),
  PPGroups (..),
  PoolVotingThresholds (..),
  StakePoolGroup (..),
  dvtPPEconomicGroupL,
  dvtPPGovGroupL,
  dvtPPNetworkGroupL,
  dvtPPTechnicalGroupL,
  ppCommitteeMinSizeL,
  ppDRepVotingThresholdsL,
  ppPoolVotingThresholdsL,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.PoolParams (PoolParams)
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import Cardano.Ledger.Shelley.LedgerState (
  epochStateStakeDistrL,
  epochStateUMapL,
 )
import Cardano.Ledger.UMap
import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (..))
import Data.Foldable (Foldable (..))
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..), allNoThunks)

data EnactState era = EnactState
  { ensCommittee :: !(StrictMaybe (Committee era))
  -- ^ Constitutional Committee
  , ensConstitution :: !(Constitution era)
  -- ^ Constitution
  , ensCurPParams :: !(PParams era)
  , ensPrevPParams :: !(PParams era)
  , ensTreasury :: !Coin
  , ensWithdrawals :: !(Map (Credential 'Staking) Coin)
  , ensPrevGovActionIds :: !(GovRelation StrictMaybe)
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
ensCurPParamsL = lens ensCurPParams (\es x -> es {ensCurPParams = x})

ensPrevPParamsL :: Lens' (EnactState era) (PParams era)
ensPrevPParamsL = lens ensPrevPParams (\es x -> es {ensPrevPParams = x})

ensTreasuryL :: Lens' (EnactState era) Coin
ensTreasuryL = lens ensTreasury $ \es x -> es {ensTreasury = x}

ensWithdrawalsL :: Lens' (EnactState era) (Map (Credential 'Staking) Coin)
ensWithdrawalsL = lens ensWithdrawals $ \es x -> es {ensWithdrawals = x}

ensPrevGovActionIdsL :: Lens' (EnactState era) (GovRelation StrictMaybe)
ensPrevGovActionIdsL = lens ensPrevGovActionIds (\es x -> es {ensPrevGovActionIds = x})

ensPrevPParamUpdateL ::
  Lens' (EnactState era) (StrictMaybe (GovPurposeId 'PParamUpdatePurpose))
ensPrevPParamUpdateL = ensPrevGovActionIdsL . grPParamUpdateL

ensPrevHardForkL ::
  Lens' (EnactState era) (StrictMaybe (GovPurposeId 'HardForkPurpose))
ensPrevHardForkL = ensPrevGovActionIdsL . grHardForkL

ensPrevCommitteeL ::
  Lens' (EnactState era) (StrictMaybe (GovPurposeId 'CommitteePurpose))
ensPrevCommitteeL = ensPrevGovActionIdsL . grCommitteeL

ensPrevConstitutionL ::
  Lens' (EnactState era) (StrictMaybe (GovPurposeId 'ConstitutionPurpose))
ensPrevConstitutionL = ensPrevGovActionIdsL . grConstitutionL

instance EraPParams era => ToJSON (EnactState era) where
  toJSON = object . toEnactStatePairs
  toEncoding = pairs . mconcat . toEnactStatePairs

toEnactStatePairs :: (KeyValue e a, EraPParams era) => EnactState era -> [a]
toEnactStatePairs cg@(EnactState _ _ _ _ _ _ _) =
  let EnactState {..} = cg
   in [ "committee" .= ensCommittee
      , "constitution" .= ensConstitution
      , "curPParams" .= ensCurPParams
      , "prevPParams" .= ensPrevPParams
      , "prevGovActionIds" .= ensPrevGovActionIds
      ]

deriving instance (Era era, Eq (PParams era)) => Eq (EnactState era)

deriving instance (Era era, Show (PParams era)) => Show (EnactState era)

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

instance EraPParams era => DecShareCBOR (EnactState era) where
  type Share (EnactState era) = Interns (Credential 'Staking)
  decShareCBOR is =
    decode $
      RecD EnactState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! D (decodeMap (interns is <$> decCBOR) decCBOR)
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

instance EraPParams era => ToCBOR (EnactState era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (EnactState era) where
  fromCBOR = fromEraCBOR @era

instance EraPParams era => NFData (EnactState era)

instance EraPParams era => NoThunks (EnactState era)

-- ========================================

-- | `RatifyState` stores information about what will happen to the active
-- governance actions at the next epoch boundary.
data RatifyState era = RatifyState
  { rsEnactState :: !(EnactState era)
  -- ^ This is the currently active `EnactState`. It contains all the changes
  -- that were applied to it at the last epoch boundary by all the proposals
  -- that were enacted.
  , -- TODO: switch rsEnacted to StrictSeq for the sake of avoiding
    -- space leaks during ledger state deserialization
    rsEnacted :: !(Seq (GovActionState era))
  -- ^ Governance actions that are going to be enacted at the next epoch
  -- boundary.
  , rsExpired :: !(Set GovActionId)
  -- ^ Governance actions that are going to be removed at the next epoch
  -- boundary, either due to expiring or because they would become invalid
  -- after another governance action gets enacted or expired before it
  , rsDelayed :: !Bool
  -- ^ This flag is set to true if one of the proposals that was ratified at the
  -- last epoch boundary was a delaying action. This means that no other
  -- proposals will be ratified this epoch and each active proposal that has not
  -- become invalid will have its expiry date extended by one epoch.
  --
  -- This flag is reset at each epoch boundary before the `RATIFY` rule gets
  -- called, but it might immediately be set to `True` again after the `RATIFY`
  -- rule has finished execution.
  }
  deriving (Generic)

deriving instance EraPParams era => Eq (RatifyState era)

deriving instance EraPParams era => Show (RatifyState era)

rsEnactStateL :: Lens' (RatifyState era) (EnactState era)
rsEnactStateL = lens rsEnactState (\x y -> x {rsEnactState = y})

rsEnactedL :: Lens' (RatifyState era) (Seq (GovActionState era))
rsEnactedL = lens rsEnacted (\x y -> x {rsEnacted = y})

rsExpiredL :: Lens' (RatifyState era) (Set GovActionId)
rsExpiredL = lens rsExpired (\x y -> x {rsExpired = y})

rsDelayedL :: Lens' (RatifyState era) Bool
rsDelayedL = lens rsDelayed (\x y -> x {rsDelayed = y})

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
      , "enactedGovActions" .= rsEnacted
      , "expiredGovActions" .= rsExpired
      , "ratificationDelayed" .= rsDelayed
      ]

pparamsUpdateThreshold ::
  forall era.
  ConwayEraPParams era =>
  DRepVotingThresholds ->
  PParamsUpdate era ->
  UnitInterval
pparamsUpdateThreshold thresholds ppu =
  let thresholdLens = \case
        NetworkGroup -> dvtPPNetworkGroupL
        GovGroup -> dvtPPGovGroupL
        TechnicalGroup -> dvtPPTechnicalGroupL
        EconomicGroup -> dvtPPEconomicGroupL
      lookupGroupThreshold (PPGroups grp _) =
        thresholds ^. thresholdLens grp
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
        , pvtPPSecurityGroup
        , pvtMotionNoConfidence
        } = pp ^. ppPoolVotingThresholdsL
      isSecurityRelevant (PPGroups _ s) =
        case s of
          SecurityGroup -> True
          NoStakePoolGroup -> False
      paramChangeThreshold ppu
        | any isSecurityRelevant (modifiedPPGroups ppu) =
            VotingThreshold pvtPPSecurityGroup
        | otherwise = NoVotingAllowed
   in case action of
        NoConfidence {} -> VotingThreshold pvtMotionNoConfidence
        UpdateCommittee {} ->
          VotingThreshold $
            if isElectedCommittee
              then pvtCommitteeNormal
              else pvtCommitteeNoConfidence
        NewConstitution {} -> NoVotingAllowed
        HardForkInitiation {} -> VotingThreshold pvtHardForkInitiation
        ParameterChange _ ppu _ -> paramChangeThreshold ppu
        TreasuryWithdrawals {} -> NoVotingAllowed
        InfoAction {} -> NoVotingThreshold

isCommitteeVotingAllowed ::
  ConwayEraPParams era =>
  EpochNo ->
  CommitteeState era ->
  GovAction era ->
  Bool
isCommitteeVotingAllowed currentEpoch committeeState =
  isVotingAllowed
    . votingCommitteeThresholdInternal
      currentEpoch
      def
      committee
      committeeState
  where
    -- Information about presence of committee is irrelevant for knowing if voting is
    -- allowed or not
    committee = SNothing

votingCommitteeThreshold ::
  ConwayEraPParams era =>
  EpochNo ->
  RatifyState era ->
  CommitteeState era ->
  GovAction era ->
  StrictMaybe UnitInterval
votingCommitteeThreshold currentEpoch ratifyState committeeState =
  toRatifyVotingThreshold
    . votingCommitteeThresholdInternal
      currentEpoch
      pp
      committee
      committeeState
  where
    committee = ratifyState ^. rsEnactStateL . ensCommitteeL
    pp = ratifyState ^. rsEnactStateL . ensCurPParamsL

votingCommitteeThresholdInternal ::
  ConwayEraPParams era =>
  EpochNo ->
  PParams era ->
  StrictMaybe (Committee era) ->
  CommitteeState era ->
  GovAction era ->
  VotingThreshold
votingCommitteeThresholdInternal currentEpoch pp committee (CommitteeState hotKeys) = \case
  NoConfidence {} -> NoVotingAllowed
  UpdateCommittee {} -> NoVotingAllowed
  NewConstitution {} -> threshold
  HardForkInitiation {} -> threshold
  ParameterChange {} -> threshold
  TreasuryWithdrawals {} -> threshold
  InfoAction {} -> NoVotingThreshold
  where
    threshold =
      case committeeThreshold <$> committee of
        -- when we are not in a bootstrap phase,
        -- if the committee size is smaller than the minimum given in PParams,
        -- we treat it as if we had no committee
        SJust t
          | HF.bootstrapPhase (pp ^. ppProtocolVersionL)
              || activeCommitteeSize >= minSize ->
              VotingThreshold t
        _ -> NoVotingThreshold
    minSize = pp ^. ppCommitteeMinSizeL
    isActive coldKey validUntil =
      case Map.lookup coldKey hotKeys of
        Just (CommitteeMemberResigned _) -> False
        Just _ -> currentEpoch <= validUntil
        Nothing -> False
    activeCommitteeSize =
      fromIntegral . Map.size . Map.filterWithKey isActive $
        foldMap' committeeMembers committee

isDRepVotingAllowed ::
  ConwayEraPParams era =>
  GovAction era ->
  Bool
isDRepVotingAllowed =
  isVotingAllowed . votingDRepThresholdInternal pp isElectedCommittee
  where
    -- Information about presence of committee or values in PParams are irrelevant for
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
  let thresholds@DRepVotingThresholds
        { dvtCommitteeNoConfidence
        , dvtCommitteeNormal
        , dvtMotionNoConfidence
        , dvtUpdateToConstitution
        , dvtHardForkInitiation
        , dvtTreasuryWithdrawal
        } -- We reset all (except InfoAction) DRep thresholds to 0 during bootstrap phase
          | HF.bootstrapPhase (pp ^. ppProtocolVersionL) = def
          | otherwise = pp ^. ppDRepVotingThresholdsL
   in case action of
        NoConfidence {} -> VotingThreshold dvtMotionNoConfidence
        UpdateCommittee {} ->
          VotingThreshold $
            if isElectedCommittee
              then dvtCommitteeNormal
              else dvtCommitteeNoConfidence
        NewConstitution {} -> VotingThreshold dvtUpdateToConstitution
        HardForkInitiation {} -> VotingThreshold dvtHardForkInitiation
        ParameterChange _ ppu _ -> VotingThreshold $ pparamsUpdateThreshold thresholds ppu
        TreasuryWithdrawals {} -> VotingThreshold dvtTreasuryWithdrawal
        InfoAction {} -> NoVotingThreshold

actionPriority :: GovAction era -> Int
actionPriority NoConfidence {} = 0
actionPriority UpdateCommittee {} = 1
actionPriority NewConstitution {} = 2
actionPriority HardForkInitiation {} = 3
actionPriority ParameterChange {} = 4
actionPriority TreasuryWithdrawals {} = 5
actionPriority InfoAction {} = 6

reorderActions :: SS.StrictSeq (GovActionState era) -> SS.StrictSeq (GovActionState era)
reorderActions = SS.fromList . sortOn (actionPriority . gasAction) . toList

newtype RatifySignal era = RatifySignal {unRatifySignal :: StrictSeq (GovActionState era)}
  deriving (Eq, Show, Generic)

ratifySignalL :: Lens' (RatifySignal era) (StrictSeq (GovActionState era))
ratifySignalL = lens unRatifySignal (\x y -> x {unRatifySignal = y})

instance EraPParams era => NFData (RatifySignal era)

data RatifyEnv era = RatifyEnv
  { reInstantStake :: InstantStake era
  , reStakePoolDistr :: PoolDistr
  , reDRepDistr :: Map DRep (CompactForm Coin)
  , reDRepState :: Map (Credential 'DRepRole) DRepState
  , reCurrentEpoch :: EpochNo
  , reCommitteeState :: CommitteeState era
  , reDelegatees :: Map (Credential 'Staking) DRep
  , rePoolParams :: Map (KeyHash 'StakePool) PoolParams
  }
  deriving (Generic)

instance CanGetInstantStake RatifyEnv

instance CanSetInstantStake RatifyEnv where
  instantStakeL = lens reInstantStake (\x y -> x {reInstantStake = y})

reStakePoolDistrL :: Lens' (RatifyEnv era) PoolDistr
reStakePoolDistrL = lens reStakePoolDistr (\x y -> x {reStakePoolDistr = y})

reDRepDistrL :: Lens' (RatifyEnv era) (Map DRep (CompactForm Coin))
reDRepDistrL = lens reDRepDistr (\x y -> x {reDRepDistr = y})

reDRepStateL ::
  Lens' (RatifyEnv era) (Map (Credential 'DRepRole) DRepState)
reDRepStateL = lens reDRepState (\x y -> x {reDRepState = y})

reCurrentEpochL :: Lens' (RatifyEnv era) EpochNo
reCurrentEpochL = lens reCurrentEpoch (\x y -> x {reCurrentEpoch = y})

reCommitteeStateL :: Lens' (RatifyEnv era) (CommitteeState era)
reCommitteeStateL = lens reCommitteeState (\x y -> x {reCommitteeState = y})

deriving instance Show (InstantStake era) => Show (RatifyEnv era)

deriving instance Eq (InstantStake era) => Eq (RatifyEnv era)

instance Default (InstantStake era) => Default (RatifyEnv era) where
  def =
    RatifyEnv
      def
      (PoolDistr Map.empty mempty)
      Map.empty
      Map.empty
      (EpochNo 0)
      def
      Map.empty
      Map.empty

instance (Typeable era, NoThunks (InstantStake era)) => NoThunks (RatifyEnv era) where
  showTypeOf _ = "RatifyEnv"
  wNoThunks ctxt (RatifyEnv stake pool drep dstate ep cs delegatees poolps) =
    allNoThunks
      [ noThunks ctxt stake
      , noThunks ctxt pool
      , noThunks ctxt drep
      , noThunks ctxt dstate
      , noThunks ctxt ep
      , noThunks ctxt cs
      , noThunks ctxt delegatees
      , noThunks ctxt poolps
      ]

instance (Era era, NFData (InstantStake era)) => NFData (RatifyEnv era) where
  rnf (RatifyEnv stake pool drep dstate ep cs delegatees poolps) =
    stake `deepseq`
      pool `deepseq`
        drep `deepseq`
          dstate `deepseq`
            ep `deepseq`
              cs `deepseq`
                delegatees `deepseq`
                  rnf poolps

instance (Era era, EncCBOR (InstantStake era)) => EncCBOR (RatifyEnv era) where
  encCBOR env@(RatifyEnv _ _ _ _ _ _ _ _) =
    let RatifyEnv {..} = env
     in encode $
          Rec (RatifyEnv @era)
            !> To reInstantStake
            !> To reStakePoolDistr
            !> To reDRepDistr
            !> To reDRepState
            !> To reCurrentEpoch
            !> To reCommitteeState
            !> To reDelegatees
            !> To rePoolParams

instance (Era era, DecCBOR (InstantStake era)) => DecCBOR (RatifyEnv era) where
  decCBOR =
    decode $
      RecD RatifyEnv
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance EraPParams era => EncCBOR (RatifyState era) where
  encCBOR (RatifyState es enacted expired delayed) =
    encode
      ( Rec (RatifyState @era)
          !> To es
          !> To enacted
          !> To expired
          !> To delayed
      )

instance EraPParams era => EncCBOR (RatifySignal era) where
  encCBOR (RatifySignal govActions) = encCBOR govActions

instance EraPParams era => DecCBOR (RatifySignal era) where
  decCBOR = RatifySignal <$> decCBOR

instance EraPParams era => DecCBOR (RatifyState era) where
  decCBOR = decode (RecD RatifyState <! From <! From <! From <! From)

instance EraPParams era => DecShareCBOR (RatifyState era) where
  type
    Share (RatifyState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decShareCBOR is@(cs, _, _, _) =
    decode $
      RecD RatifyState
        <! D (decShareCBOR cs)
        <! D (decodeSeq (decShareCBOR is))
        <! From
        <! From
