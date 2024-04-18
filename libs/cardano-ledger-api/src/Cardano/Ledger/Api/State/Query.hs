{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Api.State.Query (
  -- * @GetFilteredDelegationsAndRewardAccounts@
  filterStakePoolDelegsAndRewards,
  queryStakePoolDelegsAndRewards,

  -- * @GetGovState@
  queryGovState,

  -- * @GetConstitution@
  queryConstitution,

  -- * @GetConstitutionHash@
  queryConstitutionHash,

  -- * @GetDRepState@
  queryDRepState,

  -- * @GetDRepStakeDistr@
  queryDRepStakeDistr,

  -- * @GetCommitteeState@
  queryCommitteeState,

  -- * @GetCommitteeMembersState@
  queryCommitteeMembersState,

  -- * @GetAccountState@
  queryAccountState,
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),

  -- * @GetCurrentPParams@
  queryCurrentPParams,

  -- * @GetFuturePParams@
  queryFuturePParams,

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
import Cardano.Ledger.BaseTypes (EpochNo (EpochNo), binOpEpochNo, strictMaybeToMaybe)
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Governance (
  Committee (committeeMembers),
  Constitution (constitutionAnchor),
  ConwayEraGov (..),
  committeeThresholdL,
  ensCommitteeL,
  finishDRepPulser,
  psDRepDistr,
  rsEnactStateL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (drepExpiryL)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Governance (EraGov (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UMap (
  StakeCredentials (scRewards, scSPools),
  UMap,
  domRestrictedStakeCredentials,
 )
import Control.Monad (guard)
import Data.Foldable (foldMap')
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (<&>), (^.))
import Lens.Micro.Extras (view)

-- | Filter out stake pool delegations and rewards for a set of stake credentials
filterStakePoolDelegsAndRewards ::
  UMap c ->
  Set (Credential 'Staking c) ->
  (Map (Credential 'Staking c) (KeyHash 'StakePool c), Map (Credential 'Staking c) Coin)
filterStakePoolDelegsAndRewards umap creds =
  (scSPools stakeCredentials, scRewards stakeCredentials)
  where
    stakeCredentials = domRestrictedStakeCredentials creds umap

-- | Uses `filterStakePoolDelegsAndRewards` to get the same information from the `NewEpochState`
--
-- Implementation for @GetFilteredDelegationsAndRewardAccounts@ query.
queryStakePoolDelegsAndRewards ::
  NewEpochState era ->
  Set (Credential 'Staking (EraCrypto era)) ->
  ( Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
  , Map (Credential 'Staking (EraCrypto era)) Coin
  )
queryStakePoolDelegsAndRewards nes = filterStakePoolDelegsAndRewards (dsUnified (getDState nes))

getDState :: NewEpochState era -> DState era
getDState = certDState . lsCertState . esLState . nesEs

queryConstitution :: ConwayEraGov era => NewEpochState era -> Constitution era
queryConstitution = (^. constitutionGovStateL) . queryGovState

queryConstitutionHash ::
  ConwayEraGov era =>
  NewEpochState era ->
  SafeHash (EraCrypto era) AnchorData
queryConstitutionHash nes =
  anchorDataHash . constitutionAnchor $ queryConstitution nes

-- | This query returns all of the state related to governance
queryGovState :: NewEpochState era -> GovState era
queryGovState nes = nes ^. nesEpochStateL . esLStateL . lsUTxOStateL . utxosGovStateL

-- | Query DRep state.
queryDRepState ::
  NewEpochState era ->
  -- | Specify a set of DRep credentials whose state should be returned. When this set is
  -- empty, states for all of the DReps will be returned.
  Set (Credential 'DRepRole (EraCrypto era)) ->
  Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era))
queryDRepState nes creds
  | null creds = drepsState
  | otherwise =
      drepsState `Map.restrictKeys` creds
        & if numDormantEpochs == EpochNo 0
          then id
          else (<&> drepExpiryL %~ binOpEpochNo (+) numDormantEpochs)
  where
    drepsState = vsDReps $ certVState $ lsCertState $ esLState $ nesEs nes
    numDormantEpochs = vsNumDormantEpochs $ certVState $ lsCertState $ esLState $ nesEs nes

-- | Query DRep stake distribution. Note that this can be an expensive query because there
-- is a chance that current distribution has not been fully computed yet.
queryDRepStakeDistr ::
  ConwayEraGov era =>
  NewEpochState era ->
  -- | Specify DRep Ids whose stake distribution should be returned. When this set is
  -- empty, distributions for all of the DReps will be returned.
  Set (DRep (EraCrypto era)) ->
  Map (DRep (EraCrypto era)) Coin
queryDRepStakeDistr nes creds
  | null creds = Map.map fromCompact distr
  | otherwise = Map.map fromCompact $ distr `Map.restrictKeys` creds
  where
    distr = psDRepDistr . fst $ finishDRepPulser (nes ^. newEpochStateGovStateL . drepPulsingStateGovStateL)

-- | Query committee members
queryCommitteeState :: NewEpochState era -> CommitteeState era
queryCommitteeState nes =
  vsCommitteeState $ certVState $ lsCertState $ esLState $ nesEs nes
{-# DEPRECATED queryCommitteeState "In favor of `queryCommitteeMembersState`" #-}

-- | Query committee members. Whenever the system is in No Confidence mode this query will
-- return `Nothing`.
queryCommitteeMembersState ::
  forall era.
  ConwayEraGov era =>
  -- | filter by cold credentials (don't filter when empty)
  Set (Credential 'ColdCommitteeRole (EraCrypto era)) ->
  -- | filter by hot credentials (don't filter when empty)
  Set (Credential 'HotCommitteeRole (EraCrypto era)) ->
  -- | filter by status (don't filter when empty)
  -- (useful, for discovering, for example, only active members)
  Set MemberStatus ->
  NewEpochState era ->
  CommitteeMembersState (EraCrypto era)
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
      Credential 'ColdCommitteeRole (EraCrypto era) ->
      Maybe (CommitteeMemberState (EraCrypto era))
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

    nextEpochChange :: Credential 'ColdCommitteeRole (EraCrypto era) -> NextEpochChange
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

queryAccountState ::
  NewEpochState era ->
  AccountState
queryAccountState = view $ nesEsL . esAccountStateL

getNextEpochCommitteeMembers ::
  ConwayEraGov era =>
  NewEpochState era ->
  Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo
getNextEpochCommitteeMembers nes =
  let ratifyState = snd . finishDRepPulser $ queryGovState nes ^. drepPulsingStateGovStateL
      committee = ratifyState ^. rsEnactStateL . ensCommitteeL
   in foldMap' committeeMembers committee

-- | This is a simple lookup into the state for the values of current protocol
-- parameters. These values can change on the epoch boundary. Use `queryFuturePParams` to
-- see if we are aware of any upcoming changes.
queryCurrentPParams :: EraGov era => NewEpochState era -> PParams era
queryCurrentPParams nes = queryGovState nes ^. curPParamsGovStateL

-- | This query will return values for protocol parameters that will be after the next
-- epoch boundary, but only when there was a protocol parameter update initiated in this
-- epoch. Otherwise it will return `Nothing`.
--
-- Semantics of this query are such that it will produce reliable results and efficiently
-- only two stability windows before the end of the epoch.
queryFuturePParams :: EraGov era => NewEpochState era -> Maybe (PParams era)
queryFuturePParams nes = queryGovState nes ^. futurePParamsGovStateG
