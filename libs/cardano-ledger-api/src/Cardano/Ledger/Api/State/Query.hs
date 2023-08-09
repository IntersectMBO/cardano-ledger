{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.Ledger.Api.State.Query (
  -- * @GetFilteredDelegationsAndRewardAccounts@
  filterStakePoolDelegsAndRewards,
  queryStakePoolDelegsAndRewards,

  -- * @GetConstitutionHash@
  queryConstitutionHash,
) where

import Cardano.Ledger.Allegra.Core (Constitution (constitutionAnchor))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Governance (Anchor (..), AnchorData, EraGov (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UMap (
  StakeCredentials (scRewards, scSPools),
  UMap,
  domRestrictedStakeCredentials,
 )
import Data.Map (Map)
import Data.Set (Set)
import Lens.Micro

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
queryConstitution nes =
  getConstitution (nes ^. nesEpochStateL . esLStateL . lsUTxOStateL . utxosGovStateL)

queryConstitutionHash ::
  EraGov era =>
  NewEpochState era ->
  Maybe (SafeHash (EraCrypto era) (AnchorData (EraCrypto era)))
queryConstitutionHash nes =
  anchorDataHash . constitutionAnchor <$> queryConstitution nes
