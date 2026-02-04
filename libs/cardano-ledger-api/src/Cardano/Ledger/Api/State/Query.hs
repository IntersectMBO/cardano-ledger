{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Cardano.Ledger.Api.State.Query (
  -- * @GetFilteredDelegationsAndRewardAccounts@
  queryStakePoolDelegsAndRewards,

  -- * @GetGovState@
  queryGovState,

  -- * @GetConstitution@
  queryConstitution,

  -- * @GetConstitutionHash@
  queryConstitutionHash,

  -- * @GetDRepState@
  queryDRepState,

  -- * @GetDRepDelegations@
  queryDRepDelegations,

  -- * @GetDRepStakeDistr@
  queryDRepStakeDistr,

  -- * @GetRegisteredDRepStakeDistr@
  queryRegisteredDRepStakeDistr,

  -- * @GetSPOStakeDistr@
  querySPOStakeDistr,

  -- * @GetCommitteeMembersState@
  queryCommitteeMembersState,

  -- * @GetChainAccountState@
  queryChainAccountState,
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),

  -- * @GetCurrentPParams@
  queryCurrentPParams,

  -- * @GetFuturePParams@
  queryFuturePParams,

  -- * @GetProposals@
  queryProposals,

  -- * @GetRatifyState@
  queryRatifyState,

  -- * @GetStakePoolDefaultVote@
  queryStakePoolDefaultVote,
  DefaultVote (..),

  -- * @GetPoolState@
  queryPoolParameters,
  queryPoolState,
  QueryPoolStateResult (..),
  mkQueryPoolStateResult,

    -- * @GetStakeSnapshots@
  queryStakeSnapshots,

  -- * For testing
  getNextEpochCommitteeMembers,
) where

import Cardano.Ledger.Api.State.Query.CommitteeMembersState (
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),
 )
import Cardano.Ledger.BaseTypes (EpochNo, Network, strictMaybeToMaybe)
import Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Governance (
  Committee (committeeMembers),
  Constitution (constitutionAnchor),
  ConwayEraGov (..),
  DRepPulser (..),
  DRepPulsingState (..),
  DefaultVote (..),
  GovActionId,
  GovActionState (..),
  PulsingSnapshot,
  RatifyState,
  committeeThresholdL,
  defaultStakePoolVote,
  ensCommitteeL,
  finishDRepPulser,
  proposalsDeposits,
  psDRepDistr,
  psPoolDistr,
  psProposalsL,
  rsEnactStateL,
 )
import Cardano.Ledger.Conway.Rules (updateDormantDRepExpiry)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (credToDRep, dRepToCred)
import Cardano.Ledger.Shelley.LedgerState
import Control.DeepSeq
import Control.Monad (guard)
import Data.Foldable (foldMap')
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import GHC.Generics
import Lens.Micro
import Lens.Micro.Extras (view)

-- | Implementation for @GetFilteredDelegationsAndRewardAccounts@ query.
queryStakePoolDelegsAndRewards ::
  EraCertState era =>
  NewEpochState era ->
  Set (Credential Staking) ->
  ( Map (Credential Staking) (KeyHash StakePool)
  , Map (Credential Staking) Coin
  )
queryStakePoolDelegsAndRewards nes creds =
  let accountsMap = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      accountsMapFiltered = accountsMap `Map.restrictKeys` creds
   in ( Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMapFiltered
      , Map.map (fromCompact . (^. balanceAccountStateL)) accountsMapFiltered
      )

queryConstitution :: ConwayEraGov era => NewEpochState era -> Constitution era
queryConstitution = (^. constitutionGovStateL) . queryGovState

queryConstitutionHash ::
  ConwayEraGov era =>
  NewEpochState era ->
  SafeHash AnchorData
queryConstitutionHash nes =
  anchorDataHash . constitutionAnchor $ queryConstitution nes

-- | This query returns all of the state related to governance
queryGovState :: NewEpochState era -> GovState era
queryGovState nes = nes ^. nesEpochStateL . epochStateGovStateL

-- | Query DRep state.
queryDRepState ::
  ConwayEraCertState era =>
  NewEpochState era ->
  -- | Specify a set of DRep credentials whose state should be returned. When this set is
  -- empty, states for all of the DReps will be returned.
  Set (Credential DRepRole) ->
  Map (Credential DRepRole) DRepState
queryDRepState nes creds
  | null creds = updateDormantDRepExpiry' vState ^. vsDRepsL
  | otherwise = updateDormantDRepExpiry' vStateFiltered ^. vsDRepsL
  where
    vStateFiltered = vState & vsDRepsL %~ (`Map.restrictKeys` creds)
    vState = nes ^. nesEsL . esLStateL . lsCertStateL . certVStateL
    updateDormantDRepExpiry' = updateDormantDRepExpiry (nes ^. nesELL)

-- | Query the delegators delegated to each DRep, including
-- @AlwaysAbstain@ and @NoConfidence@.
queryDRepDelegations ::
  forall era.
  ConwayEraCertState era =>
  NewEpochState era ->
  -- | Specify a set of DReps whose state should be returned. When this set is
  -- empty, states for all of the DReps will be returned.
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

-- | Query pool stake distribution.
querySPOStakeDistr ::
  ConwayEraGov era =>
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  -- | Specify pool key hashes whose stake distribution should be returned. When this set is
  -- empty, distributions for all of the pools will be returned.
  Map (KeyHash StakePool) Coin
querySPOStakeDistr nes keys
  | null keys = Map.map fromCompact distr
  | otherwise = Map.map fromCompact $ distr `Map.restrictKeys` keys
  where
    distr = psPoolDistr . fst $ finishedPulserState nes

-- | Query committee members. Whenever the system is in No Confidence mode this query will
-- return `Nothing`.
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
  CommitteeMembersState
queryCommitteeMembersState coldCredsFilter hotCredsFilter statusFilter nes =
  let
    committee = queryGovState nes ^. committeeGovStateL
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
      Maybe CommitteeMemberState
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
      pure $ CommitteeMemberState hkStatus status mbExpiry (nextEpochChange coldCred)

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
    CommitteeMembersState
      { csCommittee = cms
      , csThreshold = strictMaybeToMaybe $ (^. committeeThresholdL) <$> committee
      , csEpochNo = currentEpoch
      }

queryChainAccountState ::
  NewEpochState era ->
  ChainAccountState
queryChainAccountState = view chainAccountStateL

getNextEpochCommitteeMembers ::
  ConwayEraGov era =>
  NewEpochState era ->
  Map (Credential ColdCommitteeRole) EpochNo
getNextEpochCommitteeMembers nes =
  let ratifyState = queryRatifyState nes
      committee = ratifyState ^. rsEnactStateL . ensCommitteeL
   in foldMap' committeeMembers committee

-- | This is a simple lookup into the state for the values of current protocol
-- parameters. These values can change on the epoch boundary. Use `queryFuturePParams` to
-- see if we are aware of any upcoming changes.
queryCurrentPParams :: EraGov era => NewEpochState era -> PParams era
queryCurrentPParams nes = queryGovState nes ^. curPParamsGovStateL

-- | This query will return values for protocol parameters that are likely to be adopted
-- at the next epoch boundary. It is only when we passed 2 stability windows before the
-- end of the epoch that users can rely on this query to produce stable results.
queryFuturePParams :: EraGov era => NewEpochState era -> Maybe (PParams era)
queryFuturePParams nes =
  case queryGovState nes ^. futurePParamsGovStateL of
    NoPParamsUpdate -> Nothing
    PotentialPParamsUpdate mpp -> mpp
    DefinitePParamsUpdate pp -> Just pp

-- | Query proposals that are considered for ratification.
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
queryRatifyState :: ConwayEraGov era => NewEpochState era -> RatifyState era
queryRatifyState = snd . finishedPulserState

finishedPulserState ::
  ConwayEraGov era =>
  NewEpochState era ->
  (PulsingSnapshot era, RatifyState era)
finishedPulserState nes = finishDRepPulser (nes ^. newEpochStateGovStateL . drepPulsingStateGovStateL)

-- | Query a stake pool's account address delegatee which determines the pool's default vote
-- in absence of an explicit vote. Note that this is different from the delegatee determined
-- by the credential of the stake pool itself.
queryStakePoolDefaultVote ::
  (EraCertState era, ConwayEraAccounts era) =>
  NewEpochState era ->
  -- | Specify the key hash of the pool whose default vote should be returned.
  KeyHash StakePool ->
  DefaultVote
queryStakePoolDefaultVote nes poolId =
  defaultStakePoolVote poolId (nes ^. nesEsL . epochStateStakePoolsL) $
    nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL

-- | Used only for the `queryPoolState` query. This resembles the older way of
-- representing StakePoolState in Ledger.
data QueryPoolStateResult = QueryPoolStateResult
  { qpsrStakePoolParams :: !(Map (KeyHash StakePool) StakePoolParams)
  , qpsrFutureStakePoolParams :: !(Map (KeyHash StakePool) StakePoolParams)
  , qpsrRetiring :: !(Map (KeyHash StakePool) EpochNo)
  , qpsrDeposits :: !(Map (KeyHash StakePool) Coin)
  }
  deriving (Show, Eq)

instance EncCBOR QueryPoolStateResult where
  encCBOR (QueryPoolStateResult a b c d) =
    encodeListLen 4 <> encCBOR a <> encCBOR b <> encCBOR c <> encCBOR d

instance DecCBOR QueryPoolStateResult where
  decCBOR = decodeRecordNamed "QueryPoolStateResult" (const 4) $ do
    qpsrStakePoolParams <- decCBOR
    qpsrFutureStakePoolParams <- decCBOR
    qpsrRetiring <- decCBOR
    qpsrDeposits <- decCBOR
    pure
      QueryPoolStateResult {qpsrStakePoolParams, qpsrFutureStakePoolParams, qpsrRetiring, qpsrDeposits}

mkQueryPoolStateResult ::
  (forall x. Map.Map (KeyHash StakePool) x -> Map.Map (KeyHash StakePool) x) ->
  PState era ->
  Network ->
  QueryPoolStateResult
mkQueryPoolStateResult f ps network =
  QueryPoolStateResult
    { qpsrStakePoolParams =
        Map.mapWithKey (`stakePoolStateToStakePoolParams` network) restrictedStakePools
    , qpsrFutureStakePoolParams = f $ psFutureStakePoolParams ps
    , qpsrRetiring = f $ psRetiring ps
    , qpsrDeposits = Map.map (fromCompact . spsDeposit) restrictedStakePools
    }
  where
    restrictedStakePools = f $ psStakePools ps

-- | Query the QueryPoolStateResult. This is slightly different from the internal
-- representation used by Ledger and is intended to resemble how the internal
-- representation used to be.
queryPoolState ::
  EraCertState era =>
  NewEpochState era -> Maybe (Set (KeyHash StakePool)) -> Network -> QueryPoolStateResult
queryPoolState nes mPoolKeys network =
  let pstate = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL
      f :: forall x. Map.Map (KeyHash StakePool) x -> Map.Map (KeyHash StakePool) x
      f = case mPoolKeys of
        Nothing -> id
        Just keys -> (`Map.restrictKeys` keys)
   in mkQueryPoolStateResult f pstate network

-- | Query the current StakePoolParams.
queryPoolParameters ::
  EraCertState era =>
  Network ->
  NewEpochState era ->
  Set (KeyHash StakePool) ->
  Map (KeyHash StakePool) StakePoolParams
queryPoolParameters network nes poolKeys =
  let pools = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
   in Map.mapWithKey (`stakePoolStateToStakePoolParams` network) $ Map.restrictKeys pools poolKeys

-- | The stake snapshot returns information about the mark, set, go ledger snapshots for a pool,
-- plus the total active stake for each snapshot that can be used in a 'sigma' calculation.
--
-- Each snapshot is taken at the end of a different era. The go snapshot is the current one and
-- was taken two epochs earlier, set was taken one epoch ago, and mark was taken immediately
-- before the start of the current epoch.
data StakeSnapshot = StakeSnapshot
  { ssMarkPool :: !Coin
  , ssSetPool :: !Coin
  , ssGoPool :: !Coin
  }
  deriving (Eq, Show, Generic)

instance NFData StakeSnapshot

instance ToCBOR StakeSnapshot where
  toCBOR
    StakeSnapshot
      { ssMarkPool
      , ssSetPool
      , ssGoPool
      } =
      Plain.encodeListLen 3
        <> toCBOR ssMarkPool
        <> toCBOR ssSetPool
        <> toCBOR ssGoPool

instance FromCBOR StakeSnapshot where
  fromCBOR = do
    Plain.enforceSize "StakeSnapshot" 3
    StakeSnapshot
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

data StakeSnapshots = StakeSnapshots
  { ssStakeSnapshots :: !(Map (KeyHash StakePool) StakeSnapshot)
  , ssMarkTotal :: !Coin
  , ssSetTotal :: !Coin
  , ssGoTotal :: !Coin
  }
  deriving (Eq, Show, Generic)

instance NFData StakeSnapshots

instance ToCBOR StakeSnapshots where
  toCBOR
    StakeSnapshots
      { ssStakeSnapshots
      , ssMarkTotal
      , ssSetTotal
      , ssGoTotal
      } =
      Plain.encodeListLen 4
        <> toCBOR ssStakeSnapshots
        <> toCBOR ssMarkTotal
        <> toCBOR ssSetTotal
        <> toCBOR ssGoTotal

instance FromCBOR StakeSnapshots where
  fromCBOR = do
    Plain.enforceSize "StakeSnapshots" 4
    StakeSnapshots
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

queryStakeSnapshots ::
  NewEpochState era ->
  Maybe (Set (KeyHash StakePool)) ->
  StakeSnapshots
queryStakeSnapshots nes mPoolIds =
  let SnapShots
        { ssStakeMark
        , ssStakeSet
        , ssStakeGo
        } = esSnapshots $ nesEs nes

      totalMarkByPoolId :: Map (KeyHash StakePool) Coin
      totalMarkByPoolId = sumStakePerPool (ssDelegations ssStakeMark) (ssStake ssStakeMark)

      totalSetByPoolId :: Map (KeyHash StakePool) Coin
      totalSetByPoolId = sumStakePerPool (ssDelegations ssStakeSet) (ssStake ssStakeSet)

      totalGoByPoolId :: Map (KeyHash StakePool) Coin
      totalGoByPoolId = sumStakePerPool (ssDelegations ssStakeGo) (ssStake ssStakeGo)

      getPoolStakes :: Set (KeyHash StakePool) -> Map (KeyHash StakePool) StakeSnapshot
      getPoolStakes poolIds = Map.fromSet mkStakeSnapshot poolIds
        where
          mkStakeSnapshot poolId =
            StakeSnapshot
              { ssMarkPool = Map.findWithDefault mempty poolId totalMarkByPoolId
              , ssSetPool = Map.findWithDefault mempty poolId totalSetByPoolId
              , ssGoPool = Map.findWithDefault mempty poolId totalGoByPoolId
              }

      getAllStake :: SnapShot -> Coin
      getAllStake (SnapShot stake _ _ _ _) = VMap.foldMap fromCompact (unStake stake)
   in case mPoolIds of
        Nothing ->
          let poolIds =
                Set.fromList $
                  mconcat
                    [ VMap.elems (ssDelegations ssStakeMark)
                    , VMap.elems (ssDelegations ssStakeSet)
                    , VMap.elems (ssDelegations ssStakeGo)
                    ]
           in StakeSnapshots
                { ssStakeSnapshots = getPoolStakes poolIds
                , ssMarkTotal = getAllStake ssStakeMark
                , ssSetTotal = getAllStake ssStakeSet
                , ssGoTotal = getAllStake ssStakeGo
                }
        Just poolIds ->
          StakeSnapshots
            { ssStakeSnapshots = getPoolStakes poolIds
            , ssMarkTotal = getAllStake ssStakeMark
            , ssSetTotal = getAllStake ssStakeSet
            , ssGoTotal = getAllStake ssStakeGo
            }
