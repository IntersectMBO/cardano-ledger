{-# LANGUAGE DataKinds #-}

module Test.Cardano.Ledger.Api.State.Query (
  -- * Old versions of queries

  --
  -- These are useful for testing and benchmarking
  getFilteredDelegationsAndRewardAccounts,
)
where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.UMap (UMap, UView (SPoolUView), domRestrictedMap, rewardMap)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

-- | This is an old implementation for @GetFilteredDelegationsAndRewardAccounts@ query
getFilteredDelegationsAndRewardAccounts ::
  UMap c ->
  Set (Credential 'Staking c) ->
  (Map (Credential 'Staking c) (KeyHash 'StakePool c), Map (Credential 'Staking c) Coin)
getFilteredDelegationsAndRewardAccounts umap creds =
  (filteredDelegations, filteredRwdAcnts)
  where
    filteredDelegations = domRestrictedMap creds $ SPoolUView umap
    filteredRwdAcnts = rewardMap umap `Map.restrictKeys` creds
