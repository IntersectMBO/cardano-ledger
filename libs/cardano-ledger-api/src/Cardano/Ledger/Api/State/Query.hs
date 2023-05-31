{-# LANGUAGE DataKinds #-}

module Cardano.Ledger.Api.State.Query (
  -- * @GetFilteredDelegationsAndRewardAccounts@
  filterStakePoolDelegsAndRewards,
  queryStakePoolDelegsAndRewards,
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UMap (
  StakeCredentials (scRewards, scSPools),
  UMap,
  domRestrictedStakeCredentials,
 )
import Data.Map (Map)
import Data.Set (Set)

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
