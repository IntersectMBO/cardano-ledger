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

  -- * @GetCommitteeState@
  queryCommitteeMembersState,
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
import Cardano.Ledger.DRepDistr (drepExpiryL)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Governance (
  EraGov (
    GovState,
    getCommitteeMembers,
    getConstitution,
    getDRepDistr
  ),
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UMap (
  StakeCredentials (scRewards, scSPools),
  UMap,
  domRestrictedStakeCredentials,
 )
import Cardano.Slotting.Slot (EpochNo)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (<&>), (^.))

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

-- | Query committee members
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
  CommitteeMembersState (EraCrypto era)
queryCommitteeMembersState coldCredsFilter hotCredsFilter statusFilter nes =
  let comStateMembers =
        csCommitteeCreds $
          nes ^. nesEpochStateL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
      comMembers = getCommitteeMembers (queryGovState nes)

      filteredComMembers
        | null coldCredsFilter = comMembers
        | otherwise = comMembers `Map.restrictKeys` coldCredsFilter

      -- Start from the committee and lookup the hot credential in the Committee State.
      -- This will miss 'Unrecognized' members (since we're iterating over the "recognized" ones).
      recognized :: [(Credential 'ColdCommitteeRole (EraCrypto era), CommitteeMemberState (EraCrypto era))]
      recognized =
        Map.assocs filteredComMembers >>= \(ck, expirationEpoch) ->
          let mbHk = Map.lookup ck comStateMembers
              mbCommitteeMemberState = do
                hkStatus <- hotCredStatus mbHk
                status <- memberStatus expirationEpoch
                let cms =
                      CommitteeMemberState
                        hkStatus
                        status
                        (Just expirationEpoch)
                        nextEpochChange
                pure (ck, cms)
           in maybeToList mbCommitteeMemberState

      unrecognizedMembers = Map.difference comStateMembers comMembers
      filteredUnrecognizedMembers
        | null coldCredsFilter = unrecognizedMembers
        | otherwise = Map.restrictKeys unrecognizedMembers coldCredsFilter

      -- Check for Unrecognized members, by starting from the CommitteeState
      unrecognized :: [(Credential 'ColdCommitteeRole (EraCrypto era), CommitteeMemberState (EraCrypto era))]
      unrecognized
        -- we only look for Unrecognized members when it is contained in the filter, or when there is no filtering
        | Set.null statusFilter || Set.member Unrecognized statusFilter =
            unrecognized' filteredUnrecognizedMembers
        | otherwise = []

      unrecognized' ::
        Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))) ->
        [(Credential 'ColdCommitteeRole (EraCrypto era), CommitteeMemberState (EraCrypto era))]
      unrecognized' members =
        Map.assocs members >>= \(ck, mbHk) ->
          let mbCommitteeMemberState = do
                hkStatus <- hotCredStatus (Just mbHk)
                let cms =
                      CommitteeMemberState
                        hkStatus
                        Unrecognized
                        Nothing
                        nextEpochChange
                pure (ck, cms)
           in maybeToList mbCommitteeMemberState
   in CommitteeMembersState
        { csCommittee = Map.fromList (recognized ++ unrecognized)
        , csEpochNo = nes ^. nesELL - 1
        }
  where
    hotCredStatus ::
      Maybe (Maybe (Credential 'HotCommitteeRole (EraCrypto era))) ->
      Maybe (HotCredAuthStatus (EraCrypto era))
    hotCredStatus Nothing = Just MemberNotAuthorized
    hotCredStatus (Just Nothing) = Just MemberResigned
    hotCredStatus (Just (Just hk)) = filtered hotCredsFilter hk MemberAuthorized

    memberStatus :: EpochNo -> Maybe MemberStatus
    memberStatus expiry =
      let status =
            if (nes ^. nesELL - 1) > expiry
              then Expired
              else Active
       in filtered statusFilter status id

    -- TODO: implement this
    nextEpochChange :: NextEpochChange
    nextEpochChange = NoChangeExpected

    filtered :: Ord a => Set a -> a -> (a -> b) -> Maybe b
    filtered fl x f
      | Set.null fl = Just (f x)
      | Set.member x fl = Just (f x)
      | otherwise = Nothing
