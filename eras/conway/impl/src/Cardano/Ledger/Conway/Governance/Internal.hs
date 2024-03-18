{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
  epochStateIncrStakeDistrL,
  epochStateRegDrepL,
  epochStateUMapL,
  reDRepDistrL,

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
import Cardano.Ledger.CertState (CommitteeAuthorization (..), CommitteeState (..))
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
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Shelley.LedgerState (
  epochStateIncrStakeDistrL,
  epochStateRegDrepL,
  epochStateStakeDistrL,
  epochStateUMapL,
 )
import Cardano.Ledger.UMap
import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (..))
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
  , ensWithdrawals :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , ensPrevGovActionIds :: !(GovRelation StrictMaybe era)
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

ensWithdrawalsL :: Lens' (EnactState era) (Map (Credential 'Staking (EraCrypto era)) Coin)
ensWithdrawalsL = lens ensWithdrawals $ \es x -> es {ensWithdrawals = x}

ensPrevGovActionIdsL :: Lens' (EnactState era) (GovRelation StrictMaybe era)
ensPrevGovActionIdsL = lens ensPrevGovActionIds (\es x -> es {ensPrevGovActionIds = x})

ensPrevPParamUpdateL ::
  Lens' (EnactState era) (StrictMaybe (GovPurposeId 'PParamUpdatePurpose era))
ensPrevPParamUpdateL = ensPrevGovActionIdsL . grPParamUpdateL

ensPrevHardForkL ::
  Lens' (EnactState era) (StrictMaybe (GovPurposeId 'HardForkPurpose era))
ensPrevHardForkL = ensPrevGovActionIdsL . grHardForkL

ensPrevCommitteeL ::
  Lens' (EnactState era) (StrictMaybe (GovPurposeId 'CommitteePurpose era))
ensPrevCommitteeL = ensPrevGovActionIdsL . grCommitteeL

ensPrevConstitutionL ::
  Lens' (EnactState era) (StrictMaybe (GovPurposeId 'ConstitutionPurpose era))
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

-- TODO: Implement Sharing: https://github.com/intersectmbo/cardano-ledger/issues/3486
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

data RatifyState era = RatifyState
  { rsEnactState :: !(EnactState era)
  , rsEnacted :: !(Seq (GovActionState era))
  , rsExpired :: !(Set (GovActionId (EraCrypto era)))
  , rsDelayed :: !Bool
  }
  deriving (Generic)

deriving instance EraPParams era => Eq (RatifyState era)

deriving instance EraPParams era => Show (RatifyState era)

rsEnactStateL :: Lens' (RatifyState era) (EnactState era)
rsEnactStateL = lens rsEnactState (\x y -> x {rsEnactState = y})

rsEnactedL :: Lens' (RatifyState era) (Seq (GovActionState era))
rsEnactedL = lens rsEnacted (\x y -> x {rsEnacted = y})

rsExpiredL :: Lens' (RatifyState era) (Set (GovActionId (EraCrypto era)))
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
  PParams era ->
  PParamsUpdate era ->
  UnitInterval
pparamsUpdateThreshold pp ppu =
  let thresholdLens = \case
        NetworkGroup -> dvtPPNetworkGroupL
        GovGroup -> dvtPPGovGroupL
        TechnicalGroup -> dvtPPTechnicalGroupL
        EconomicGroup -> dvtPPEconomicGroupL
      lookupGroupThreshold (PPGroups grp _) =
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
        , pvtPPSecurityGroup
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
        NoConfidence {} -> VotingThreshold pvtCommitteeNoConfidence
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

isCommitteeVotingAllowed :: ConwayEraPParams era => CommitteeState era -> GovAction era -> Bool
isCommitteeVotingAllowed committeeState =
  isVotingAllowed . votingCommitteeThresholdInternal def committee committeeState
  where
    -- Information about presence of committee is irrelevant for knowing if voting is
    -- allowed or not
    committee = SNothing

votingCommitteeThreshold ::
  ConwayEraPParams era =>
  RatifyState era ->
  CommitteeState era ->
  GovAction era ->
  StrictMaybe UnitInterval
votingCommitteeThreshold ratifyState committeeState =
  toRatifyVotingThreshold . votingCommitteeThresholdInternal pp committee committeeState
  where
    committee = ratifyState ^. rsEnactStateL . ensCommitteeL
    pp = ratifyState ^. rsEnactStateL . ensCurPParamsL

votingCommitteeThresholdInternal ::
  ConwayEraPParams era =>
  PParams era ->
  StrictMaybe (Committee era) ->
  CommitteeState era ->
  GovAction era ->
  VotingThreshold
votingCommitteeThresholdInternal pp committee (CommitteeState hotKeys) = \case
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
        -- if the committee size is smaller than the minimum given in PParams,
        -- we treat it as if we had no committee
        SJust t | activeCommitteeSize >= minSize -> VotingThreshold t
        _ -> NoVotingThreshold
    minSize = pp ^. ppCommitteeMinSizeL
    isActive coldKey _validUntil =
      case Map.lookup coldKey hotKeys of
        Just (CommitteeMemberResigned _) -> False
        Just _ -> True -- TODO do we need to check if member's term is expired here?
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
        ParameterChange _ ppu _ -> VotingThreshold $ pparamsUpdateThreshold pp ppu
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

newtype RatifySignal era = RatifySignal (StrictSeq (GovActionState era))
  deriving (Eq, Show, Generic)

data RatifyEnv era = RatifyEnv
  { reStakeDistr :: !(Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin))
  , reStakePoolDistr :: !(PoolDistr (EraCrypto era))
  , reDRepDistr :: !(Map (DRep (EraCrypto era)) (CompactForm Coin))
  , reDRepState :: !(Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
  , reCurrentEpoch :: !EpochNo
  , reCommitteeState :: !(CommitteeState era)
  }
  deriving (Generic)

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
  encCBOR (RatifyState es enacted expired delayed) =
    encode
      ( Rec (RatifyState @era)
          !> To es
          !> To enacted
          !> To expired
          !> To delayed
      )

instance EraPParams era => DecCBOR (RatifyState era) where
  decCBOR = decode (RecD RatifyState <! From <! From <! From <! From)

-- TODO: Implement Sharing: https://github.com/intersectmbo/cardano-ledger/issues/3486
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
