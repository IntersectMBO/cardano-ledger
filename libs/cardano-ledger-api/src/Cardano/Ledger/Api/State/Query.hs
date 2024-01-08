{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
) where

import Cardano.Ledger.Allegra.Core (Constitution (constitutionAnchor))
import Cardano.Ledger.Api.State.Query.CommitteeMembersState (
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),
 )
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (drepExpiryL)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Governance (
  EraGov (
    GovState,
    getCommitteeMembers,
    getConstitution,
    getDRepDistr,
    getNextEpochCommitteeMembers
  ),
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UMap (
  StakeCredentials (scRewards, scSPools),
  UMap,
  domRestrictedStakeCredentials,
 )
import Control.Monad (guard)
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

queryConstitution :: EraGov era => NewEpochState era -> Maybe (Constitution era)
queryConstitution = getConstitution . queryGovState

queryConstitutionHash ::
  EraGov era =>
  NewEpochState era ->
  Maybe (SafeHash (EraCrypto era) AnchorData)
queryConstitutionHash nes =
  anchorDataHash . constitutionAnchor <$> queryConstitution nes

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
        & if numDormantEpochs == 0
          then id
          else (<&> drepExpiryL %~ (+ numDormantEpochs))
  where
    drepsState = vsDReps $ certVState $ lsCertState $ esLState $ nesEs nes
    numDormantEpochs = vsNumDormantEpochs $ certVState $ lsCertState $ esLState $ nesEs nes

-- | Query DRep stake distribution. Note that this can be an expensive query because there
-- is a chance that current distribution has not been fully computed yet.
queryDRepStakeDistr ::
  EraGov era =>
  NewEpochState era ->
  -- | Specify DRep Ids whose stake distribution should be returned. When this set is
  -- empty, distributions for all of the DReps will be returned.
  Set (DRep (EraCrypto era)) ->
  Map (DRep (EraCrypto era)) Coin
queryDRepStakeDistr nes creds
  | null creds = Map.map fromCompact distr
  | otherwise = Map.map fromCompact $ distr `Map.restrictKeys` creds
  where
    distr = getDRepDistr (nes ^. newEpochStateGovStateL)

-- | Query committee members
queryCommitteeState :: NewEpochState era -> CommitteeState era
queryCommitteeState nes =
  vsCommitteeState $ certVState $ lsCertState $ esLState $ nesEs nes
{-# DEPRECATED queryCommitteeState "In favor of `queryCommitteeMembersState`" #-}

-- | Query committee members. Whenever the system is in No Confidence mode this query will
-- return `Nothing`.
queryCommitteeMembersState ::
  forall era.
  EraGov era =>
  -- | filter by cold credentials (don't filter when empty)
  Set (Credential 'ColdCommitteeRole (EraCrypto era)) ->
  -- | filter by hot credentials (don't filter when empty)
  Set (Credential 'HotCommitteeRole (EraCrypto era)) ->
  -- | filter by status (don't filter when empty)
  -- (useful, for discovering, for example, only active members)
  Set MemberStatus ->
  NewEpochState era ->
  Maybe (CommitteeMembersState (EraCrypto era))
queryCommitteeMembersState coldCredsFilter hotCredsFilter statusFilter nes = do
  (comMembers, comQuorum) <- getCommitteeMembers (queryGovState nes)
  let nextComMembers =
        maybe Map.empty fst (getNextEpochCommitteeMembers (queryGovState nes))
  let comStateMembers =
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
        Map.keysSet $
          Map.filter (maybe False (`Set.member` hotCredsFilter)) comStateMembers

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
                Just Nothing -> MemberResigned
                Just (Just hk) -> MemberAuthorized hk
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

  pure
    CommitteeMembersState
      { csCommittee = cms
      , csQuorum = comQuorum
      , csEpochNo = currentEpoch
      }

queryAccountState ::
  NewEpochState era ->
  AccountState
queryAccountState = view $ nesEsL . esAccountStateL
