{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Ledger.Api.State.Query.Governance (
  -- * Constitution
  QueryResultConstitution (..),
  toQueryResultConstitution,

  -- * Committee members state
  QueryResultCommitteeMemberState (..),
  QueryResultCommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),

  -- * DRep state
  QueryResultDRepState (..),
  QueryResultDRepStates (..),
  toQueryResultDRepState,

  -- * Queries
  queryGovState,
  queryConstitution,
  queryConstitutionHash,
  queryProposals,
  queryRatifyState,
  queryCommitteeMembersState,
  queryDRepState,
  queryDRepDelegations,
  queryDRepStakeDistr,
  queryRegisteredDRepStakeDistr,
  queryDRepDelegatees,

  -- * For testing
  getNextEpochCommitteeMembers,

  -- * Deprecated
  CommitteeMemberState,
  CommitteeMembersState,

  -- * Internal helpers (shared across Query sub-modules)
  finishedPulserState,
) where

import Cardano.Ledger.BaseTypes (
  KeyValuePairs (..),
  ToKeyValuePairs (..),
  UnitInterval,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeEnumBounded,
  encodeEnum,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin, CompactForm (CompactCoin))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Governance (
  Committee (committeeMembers),
  Constitution (constitutionAnchor, constitutionGuardrailsScriptHash),
  ConwayEraGov (..),
  DRepPulser (..),
  DRepPulsingState (..),
  GovActionId,
  GovActionState (..),
  PulsingSnapshot,
  RatifyState,
  committeeThresholdL,
  ensCommitteeL,
  finishDRepPulser,
  proposalsDeposits,
  psDRepDistr,
  psProposalsL,
  rsEnactStateL,
 )
import Cardano.Ledger.Conway.Rules (updateDormantDRepExpiry)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (credToDRep, dRepToCred)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Aeson (ToJSON (..), (.=))
import Data.Foldable (foldMap')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- | Stable query result for constitution data.
data QueryResultConstitution = QueryResultConstitution
  { qrcAnchor :: !Anchor
  , qrcGuardrailsScript :: !(Maybe ScriptHash)
  }
  deriving (Show, Eq, Ord, Generic)

deriving instance ToJSON QueryResultConstitution

instance NFData QueryResultConstitution

instance NoThunks QueryResultConstitution

instance EncCBOR QueryResultConstitution where
  encCBOR (QueryResultConstitution anchor guardrailsScript) =
    encode $
      Rec QueryResultConstitution
        !> To anchor
        !> To guardrailsScript

instance DecCBOR QueryResultConstitution where
  decCBOR =
    decode $
      RecD QueryResultConstitution
        <! From
        <! From

-- | Convert internal 'Constitution' to stable 'QueryResultConstitution'.
toQueryResultConstitution :: Constitution era -> QueryResultConstitution
toQueryResultConstitution con =
  QueryResultConstitution
    { qrcAnchor = constitutionAnchor con
    , qrcGuardrailsScript = strictMaybeToMaybe (constitutionGuardrailsScriptHash con)
    }

data MemberStatus
  = -- | Votes of this member will count during ratification
    Active
  | Expired
  | -- | This can happen when a hot credential for an unknown cold credential exists.
    -- Such Committee member will be either removed from the state at the next
    -- epoch boundary or enacted as a new member.
    Unrecognized
  deriving (Show, Eq, Enum, Bounded, Generic, Ord, ToJSON)

instance NFData MemberStatus

instance NoThunks MemberStatus

instance EncCBOR MemberStatus where
  encCBOR = encodeEnum

instance DecCBOR MemberStatus where
  decCBOR = decodeEnumBounded

data HotCredAuthStatus
  = MemberAuthorized (Credential HotCommitteeRole)
  | -- | Member enacted, but no hot credential for voting has been registered
    MemberNotAuthorized
  | MemberResigned (Maybe Anchor)
  deriving (Show, Eq, Generic, Ord, ToJSON)

instance NFData HotCredAuthStatus

instance NoThunks HotCredAuthStatus

instance EncCBOR HotCredAuthStatus where
  encCBOR =
    encode . \case
      MemberAuthorized cred -> Sum MemberAuthorized 0 !> To cred
      MemberNotAuthorized -> Sum MemberNotAuthorized 1
      MemberResigned anchor -> Sum MemberResigned 2 !> To anchor

instance DecCBOR HotCredAuthStatus where
  decCBOR =
    decode $ Summands "HotCredAuthStatus" $ \case
      0 -> SumD MemberAuthorized <! From
      1 -> SumD MemberNotAuthorized
      2 -> SumD MemberResigned <! From
      k -> Invalid k

data NextEpochChange
  = --- | Member not enacted yet, but will be at the next epoch
    ToBeEnacted
  | -- | Member will be removed
    ToBeRemoved
  | NoChangeExpected
  | ToBeExpired
  | TermAdjusted EpochNo
  deriving (Show, Eq, Generic, Ord, ToJSON)

instance NFData NextEpochChange

instance NoThunks NextEpochChange

instance EncCBOR NextEpochChange where
  encCBOR =
    encode . \case
      ToBeEnacted -> Sum ToBeEnacted 0
      ToBeRemoved -> Sum ToBeRemoved 1
      NoChangeExpected -> Sum NoChangeExpected 2
      ToBeExpired -> Sum ToBeExpired 3
      TermAdjusted e -> Sum TermAdjusted 4 !> To e

instance DecCBOR NextEpochChange where
  decCBOR =
    decode $ Summands "NextEpochChange" $ \case
      0 -> SumD ToBeEnacted
      1 -> SumD ToBeRemoved
      2 -> SumD NoChangeExpected
      3 -> SumD ToBeExpired
      4 -> SumD TermAdjusted <! From
      k -> Invalid k

-- | Per-member committee state including authorization, member status, and expiration.
data QueryResultCommitteeMemberState = QueryResultCommitteeMemberState
  { qrcmsHotCredAuthStatus :: !HotCredAuthStatus
  , qrcmsStatus :: !MemberStatus
  , qrcmsExpiration :: !(Maybe EpochNo)
  -- ^ Absolute epoch number when the member expires
  , qrcmsNextEpochChange :: !NextEpochChange
  -- ^ Changes to the member at the next epoch
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs QueryResultCommitteeMemberState

deriving instance Ord QueryResultCommitteeMemberState

instance NFData QueryResultCommitteeMemberState

instance NoThunks QueryResultCommitteeMemberState

instance EncCBOR QueryResultCommitteeMemberState where
  encCBOR (QueryResultCommitteeMemberState hotCredAuth status expiration nextEpoch) =
    encode $
      Rec QueryResultCommitteeMemberState
        !> To hotCredAuth
        !> To status
        !> To expiration
        !> To nextEpoch

instance DecCBOR QueryResultCommitteeMemberState where
  decCBOR =
    decode $
      RecD QueryResultCommitteeMemberState
        <! From
        <! From
        <! From
        <! From

instance ToKeyValuePairs QueryResultCommitteeMemberState where
  toKeyValuePairs (QueryResultCommitteeMemberState hotCredAuth status expiration nextEpoch) =
    [ "hotCredsAuthStatus" .= hotCredAuth
    , "status" .= status
    , "expiration" .= expiration
    , "nextEpochChange" .= nextEpoch
    ]

type CommitteeMemberState = QueryResultCommitteeMemberState

{-# DEPRECATED CommitteeMemberState "Use QueryResultCommitteeMemberState instead" #-}

-- | Committee members state with the committee map, threshold, and current epoch.
data QueryResultCommitteeMembersState = QueryResultCommitteeMembersState
  { qrcmsCommittee :: !(Map (Credential ColdCommitteeRole) QueryResultCommitteeMemberState)
  , qrcmsThreshold :: !(Maybe UnitInterval)
  , qrcmsEpochNo :: !EpochNo
  -- ^ Current epoch number. This is necessary to interpret committee member states
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via KeyValuePairs QueryResultCommitteeMembersState

deriving instance Ord QueryResultCommitteeMembersState

instance NFData QueryResultCommitteeMembersState

instance NoThunks QueryResultCommitteeMembersState

instance EncCBOR QueryResultCommitteeMembersState where
  encCBOR (QueryResultCommitteeMembersState committee threshold epoch) =
    encode $
      Rec QueryResultCommitteeMembersState
        !> To committee
        !> To threshold
        !> To epoch

instance DecCBOR QueryResultCommitteeMembersState where
  decCBOR =
    decode $
      RecD QueryResultCommitteeMembersState
        <! From
        <! From
        <! From

instance ToKeyValuePairs QueryResultCommitteeMembersState where
  toKeyValuePairs (QueryResultCommitteeMembersState committee threshold epoch) =
    [ "committee" .= committee
    , "threshold" .= threshold
    , "epoch" .= epoch
    ]

type CommitteeMembersState = QueryResultCommitteeMembersState

{-# DEPRECATED CommitteeMembersState "Use QueryResultCommitteeMembersState instead" #-}

-- | Stable query result for a single DRep's state.
data QueryResultDRepState = QueryResultDRepState
  { qrdrsExpiry :: !EpochNo
  , qrdrsAnchor :: !(Maybe Anchor)
  , qrdrsDeposit :: !Coin
  , qrdrsDelegs :: !(Set (Credential Staking))
  }
  deriving (Show, Eq, Ord, Generic)

deriving instance ToJSON QueryResultDRepState

instance NFData QueryResultDRepState

instance NoThunks QueryResultDRepState

instance EncCBOR QueryResultDRepState where
  encCBOR (QueryResultDRepState expiry anchor deposit delegs) =
    encode $
      Rec QueryResultDRepState
        !> To expiry
        !> To anchor
        !> To deposit
        !> To delegs

instance DecCBOR QueryResultDRepState where
  decCBOR =
    decode $
      RecD QueryResultDRepState
        <! From
        <! From
        <! From
        <! From

-- | Convert internal 'DRepState' to stable 'QueryResultDRepState'.
toQueryResultDRepState :: DRepState -> QueryResultDRepState
toQueryResultDRepState DRepState {..} =
  QueryResultDRepState
    { qrdrsExpiry = drepExpiry
    , qrdrsAnchor = strictMaybeToMaybe drepAnchor
    , qrdrsDeposit = fromCompact drepDeposit
    , qrdrsDelegs = drepDelegs
    }

-- | Stable query result wrapping DRep states for all queried DReps.
newtype QueryResultDRepStates = QueryResultDRepStates
  { qrdrssStates :: Map (Credential DRepRole) QueryResultDRepState
  }
  deriving (Show, Eq, Ord, Generic)

deriving instance ToJSON QueryResultDRepStates

instance NFData QueryResultDRepStates

instance NoThunks QueryResultDRepStates

instance EncCBOR QueryResultDRepStates where
  encCBOR (QueryResultDRepStates states) = encCBOR states

instance DecCBOR QueryResultDRepStates where
  decCBOR = QueryResultDRepStates <$> decCBOR

-- | This query returns all of the state related to governance.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:465
--   answerPureBlockQuery case for GetGovState
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:371
--   queryGovState cardano-api wrapper
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1617
--   CLI invocation
queryGovState :: NewEpochState era -> GovState era
queryGovState nes = nes ^. nesEpochStateL . epochStateGovStateL

-- | Query the constitution.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:463
--   answerPureBlockQuery case for GetConstitution
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:359
--   queryConstitution cardano-api wrapper
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1589
--   CLI invocation
queryConstitution :: ConwayEraGov era => NewEpochState era -> QueryResultConstitution
queryConstitution nes = toQueryResultConstitution $ queryGovState nes ^. constitutionGovStateL

-- | Query the constitution hash.
--
-- Convenience extraction — no corresponding standalone query in @ouroboros-consensus@.
queryConstitutionHash ::
  ConwayEraGov era =>
  NewEpochState era ->
  SafeHash AnchorData
queryConstitutionHash nes =
  anchorDataHash . constitutionAnchor $ queryGovState nes ^. constitutionGovStateL

-- | Query proposals that are considered for ratification.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:479
--   answerPureBlockQuery case for GetProposals
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:495
--   queryProposals cardano-api wrapper
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1045
--   CLI invocation
--
-- Empty 'Set' returns all proposals.
queryProposals ::
  ConwayEraGov era =>
  NewEpochState era ->
  -- | Specify a set of Governance Action IDs to filter the proposals. When this set is
  -- empty, all the proposals considered for ratification will be returned.
  Set GovActionId ->
  Seq (GovActionState era)
queryProposals nes gids
  | null gids = proposals
  -- TODO: Add `filter` to `cardano-strict-containers`
  | otherwise =
      Seq.filter (\GovActionState {..} -> gasId `Set.member` gids) proposals
  where
    proposals = fromStrict $ case nes ^. newEpochStateGovStateL . drepPulsingStateGovStateL of
      DRComplete snap _rs -> snap ^. psProposalsL
      DRPulsing DRepPulser {..} -> dpProposals

-- | Query ratification state.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:481
--   answerPureBlockQuery case for GetRatifyState
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:383
--   queryRatifyState cardano-api wrapper
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1645
--   CLI invocation
queryRatifyState :: ConwayEraGov era => NewEpochState era -> RatifyState era
queryRatifyState = snd . finishedPulserState

-- | Query committee members. Whenever the system is in No Confidence mode this query will
-- return no committee members.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:471
--   answerPureBlockQuery case for GetCommitteeMembersState
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:452
--   queryCommitteeMembersState cardano-api wrapper
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1887
--   CLI invocation
--
-- Empty 'Set' returns all matches (i.e. no filtering).
queryCommitteeMembersState ::
  forall era.
  (ConwayEraGov era, ConwayEraCertState era) =>
  -- | filter by cold credentials (don't filter when empty)
  Set (Credential ColdCommitteeRole) ->
  -- | filter by hot credentials (don't filter when empty)
  Set (Credential HotCommitteeRole) ->
  -- | filter by status (don't filter when empty)
  -- (useful, for discovering, for example, only active members)
  Set MemberStatus ->
  NewEpochState era ->
  QueryResultCommitteeMembersState
queryCommitteeMembersState coldCredsFilter hotCredsFilter statusFilter nes =
  let
    committee = nes ^. nesEpochStateL . epochStateGovStateL . committeeGovStateL
    comMembers = foldMap' committeeMembers committee
    nextComMembers = getNextEpochCommitteeMembers nes
    comStateMembers =
      csCommitteeCreds $
        nes ^. nesEpochStateL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL

    withFilteredColdCreds s
      | Set.null coldCredsFilter = s
      | otherwise = s `Set.intersection` coldCredsFilter

    relevantColdKeys
      | Set.null statusFilter || Set.member Unrecognized statusFilter =
          withFilteredColdCreds $
            Set.unions
              [ Map.keysSet comMembers
              , Map.keysSet comStateMembers
              , Map.keysSet nextComMembers
              ]
      | otherwise = withFilteredColdCreds $ Map.keysSet comMembers

    relevantHotKeys =
      Set.fromList
        [ ck
        | (ck, CommitteeHotCredential hk) <- Map.toList comStateMembers
        , hk `Set.member` hotCredsFilter
        ]

    relevant
      | Set.null hotCredsFilter = relevantColdKeys
      | otherwise = relevantColdKeys `Set.intersection` relevantHotKeys

    cms = Map.mapMaybe id $ Map.fromSet mkMaybeMemberState relevant
    currentEpoch = nes ^. nesELL

    mkMaybeMemberState ::
      Credential ColdCommitteeRole ->
      Maybe QueryResultCommitteeMemberState
    mkMaybeMemberState coldCred = do
      let mbExpiry = Map.lookup coldCred comMembers
      let status = case mbExpiry of
            Nothing -> Unrecognized
            Just expiry
              | currentEpoch > expiry -> Expired
              | otherwise -> Active
      guard (null statusFilter || status `Set.member` statusFilter)
      let hkStatus =
            case Map.lookup coldCred comStateMembers of
              Nothing -> MemberNotAuthorized
              Just (CommitteeMemberResigned anchor) -> MemberResigned (strictMaybeToMaybe anchor)
              Just (CommitteeHotCredential hk) -> MemberAuthorized hk
      pure $ QueryResultCommitteeMemberState hkStatus status mbExpiry (nextEpochChange coldCred)

    nextEpochChange :: Credential ColdCommitteeRole -> NextEpochChange
    nextEpochChange ck
      | not inCurrent && inNext = ToBeEnacted
      | not inNext = ToBeRemoved
      | Just curTerm <- lookupCurrent
      , Just nextTerm <- lookupNext
      , curTerm /= nextTerm
      , -- if the term is adjusted such that it expires in the next epoch,
        -- we set it to ToBeExpired instead of TermAdjusted
        not expiringNext =
          TermAdjusted nextTerm
      | expiringCurrent || expiringNext = ToBeExpired
      | otherwise = NoChangeExpected
      where
        lookupCurrent = Map.lookup ck comMembers
        lookupNext = Map.lookup ck nextComMembers
        inCurrent = isJust lookupCurrent
        inNext = isJust lookupNext
        expiringCurrent = lookupCurrent == Just currentEpoch
        expiringNext = lookupNext == Just currentEpoch
   in
    QueryResultCommitteeMembersState
      { qrcmsCommittee = cms
      , qrcmsThreshold = strictMaybeToMaybe $ (^. committeeThresholdL) <$> committee
      , qrcmsEpochNo = currentEpoch
      }

-- | Get the committee members that will be in effect at the next epoch boundary.
getNextEpochCommitteeMembers ::
  ConwayEraGov era =>
  NewEpochState era ->
  Map (Credential ColdCommitteeRole) EpochNo
getNextEpochCommitteeMembers nes =
  let (_, ratifyState) = finishedPulserState nes
      committee = ratifyState ^. rsEnactStateL . ensCommitteeL
   in foldMap' committeeMembers committee

-- | Query DRep state.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:467
--   answerPureBlockQuery case for GetDRepState
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:407
--   queryDRepState cardano-api wrapper
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1710
--   CLI invocation
--
-- Empty 'Set' returns all DReps.
queryDRepState ::
  ConwayEraCertState era =>
  NewEpochState era ->
  -- | Specify a set of DRep credentials whose state should be returned. When this set is
  -- empty, states for all of the DReps will be returned.
  Set (Credential DRepRole) ->
  QueryResultDRepStates
queryDRepState nes creds =
  QueryResultDRepStates $
    Map.map toQueryResultDRepState $
      if null creds
        then updateDormantDRepExpiry' vState ^. vsDRepsL
        else updateDormantDRepExpiry' vStateFiltered ^. vsDRepsL
  where
    vStateFiltered = vState & vsDRepsL %~ (`Map.restrictKeys` creds)
    vState = nes ^. nesEsL . esLStateL . lsCertStateL . certVStateL
    updateDormantDRepExpiry' = updateDormantDRepExpiry (nes ^. nesELL)

-- | Query the delegators delegated to each DRep, including
-- @AlwaysAbstain@ and @NoConfidence@.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:530
--   answerPureBlockQuery case for GetDRepDelegations
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:529
--   queryDRepDelegations cardano-api wrapper (not publicly exported)
--
-- Empty 'Set' returns all DReps.
queryDRepDelegations ::
  forall era.
  ConwayEraCertState era =>
  NewEpochState era ->
  -- | Specify a set of DReps whose delegations should be returned. When this set is
  -- empty, delegations for all DReps will be returned.
  Set DRep ->
  Map DRep (Set (Credential Staking))
queryDRepDelegations nes dreps =
  case getDRepCreds dreps of
    Just creds ->
      Map.map drepDelegs $
        Map.mapKeys credToDRep ((vState ^. vsDRepsL) `Map.restrictKeys` creds)
    Nothing ->
      -- Whenever predefined `AlwaysAbstain` or `AlwaysNoConfidence` are
      -- requested we are forced to iterate over all accounts and find those
      -- delegations.
      Map.foldlWithKey'
        ( \m cred cas ->
            case cas ^. dRepDelegationAccountStateL of
              Just drep
                | Set.null dreps || drep `Set.member` dreps ->
                    Map.insertWith (<>) drep (Set.singleton cred) m
              _ ->
                m
        )
        Map.empty
        (dState ^. accountsL . accountsMapL)
  where
    dState = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL
    vState = nes ^. nesEsL . esLStateL . lsCertStateL . certVStateL
    -- Find all credentials for requested DReps, but only when we don't care
    -- about predefined DReps
    getDRepCreds ds = do
      guard $ not $ Set.null ds
      Set.fromList <$> traverse dRepToCred (Set.elems ds)

-- | Query DRep stake distribution. Note that this can be an expensive query because there
-- is a chance that current distribution has not been fully computed yet.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:469
--   answerPureBlockQuery case for GetDRepStakeDistr
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:423
--   queryDRepStakeDistribution cardano-api wrapper
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1717
--   CLI invocation
--
-- Empty 'Set' returns all DReps.
queryDRepStakeDistr ::
  ConwayEraGov era =>
  NewEpochState era ->
  -- | Specify DRep Ids whose stake distribution should be returned. When this set is
  -- empty, distributions for all of the DReps will be returned.
  Set DRep ->
  Map DRep Coin
queryDRepStakeDistr nes creds
  | null creds = Map.map fromCompact distr
  | otherwise = Map.map fromCompact $ distr `Map.restrictKeys` creds
  where
    distr = psDRepDistr . fst $ finishedPulserState nes

-- | Query the stake distribution of the registered DReps. This does not
-- include the @AlwaysAbstain@ and @NoConfidence@ DReps.
-- Source: added by commit 18a08e4bb (Lucsanszky, Nov 2024) as a ledger-side query.
--   No consensus BlockQuery constructor exists yet (GetRegisteredDRepStakeDistr was planned but not added).
--
-- Empty 'Set' returns all registered DReps.
queryRegisteredDRepStakeDistr ::
  (ConwayEraGov era, ConwayEraCertState era) =>
  NewEpochState era ->
  -- | Specify DRep Ids whose stake distribution should be returned. When this set is
  -- empty, distributions for all of the registered DReps will be returned.
  Set (Credential DRepRole) ->
  Map (Credential DRepRole) Coin
queryRegisteredDRepStakeDistr nes creds =
  Map.foldlWithKey' computeDistr mempty selectedDReps
  where
    selectedDReps
      | null creds = registeredDReps
      | otherwise = registeredDReps `Map.restrictKeys` creds
    registeredDReps = nes ^. nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
    computeDistr distrAcc dRepCred (DRepState {..}) =
      Map.insert dRepCred (totalDelegations drepDelegs) distrAcc
    totalDelegations =
      fromCompact . foldMap stakeAndDeposits
    instantStake = nes ^. instantStakeL . instantStakeCredentialsL
    proposalDeposits = proposalsDeposits $ nes ^. newEpochStateGovStateL . proposalsGovStateL
    stakeAndDeposits stakeCred =
      fromMaybe (CompactCoin 0) $
        Map.lookup stakeCred instantStake <> Map.lookup stakeCred proposalDeposits

-- | Query the DRep delegatee for each given staking credential.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:457
--   answerPureBlockQuery case for GetDRepStakeDistr (partial — delegatees extracted separately)
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:459
--   queryStakeVoteDelegatees cardano-api wrapper
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1042
--   CLI invocation
--
-- Returns the DRep each credential has delegated to. Credentials with no
-- DRep delegation are omitted from the result. Empty 'Set' returns all.
queryDRepDelegatees ::
  (EraCertState era, ConwayEraAccounts era) =>
  NewEpochState era ->
  Set (Credential Staking) ->
  Map (Credential Staking) DRep
queryDRepDelegatees nes creds =
  let accountsMap = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      selected
        | Set.null creds = accountsMap
        | otherwise = accountsMap `Map.restrictKeys` creds
   in Map.mapMaybe (^. dRepDelegationAccountStateL) selected

-- | Force the DRep pulser to completion and return the resulting snapshot and
-- ratify state. Shared across governance query sub-modules.
finishedPulserState ::
  ConwayEraGov era =>
  NewEpochState era ->
  (PulsingSnapshot era, RatifyState era)
finishedPulserState nes = finishDRepPulser (nes ^. newEpochStateGovStateL . drepPulsingStateGovStateL)
