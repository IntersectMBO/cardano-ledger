{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- ===========================================================================
-- There are three parts to IncrementalStaking.
-- 1) The incremental part, where we keep track of each update to the UTxO
--    adding Inputs and deleting consumed Outputs. Done in the Utxo rules.
-- 2) Finalizing and aggregating by stake credential to create a Snapshot.
--    done in the Snap rules.
-- 3) Applying the RewardUpdate, to the Rewards component of the UMap.
--    done in the NewEpoch rules.

module Cardano.Ledger.Shelley.LedgerState.IncrementalStake (
  applyRUpd,
  applyRUpdFiltered,
  smartUTxOState,
  filterAllRewards,
  FilteredRewards (..),
)
where

import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.CertState (
  DState (..),
  EraCertState (..),
  dsUnifiedL,
  rewards,
 )
import Cardano.Ledger.Coin (Coin (..), addDeltaCoin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.Shelley.RewardUpdate (RewardUpdate (..), PoolRewards)
import Cardano.Ledger.Shelley.Rewards (
  aggregateCompactRewards,
  aggregateRewards,
  filterRewards,
 )
import Cardano.Ledger.State
import Cardano.Ledger.UMap (member)
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Lens.Micro

-- ================================================

-- | A valid (or self-consistent) UTxOState{utxosUtxo, utxosDeposited , utxosFees  , utxosPpups , utxosStakeDistr}
--   maintains an invariant between the utxosUtxo and utxosStakeDistr fields. the utxosStakeDistr field is
--   the aggregation of Coin over the StakeReferences in the UTxO. It can be computed by a pure
--   function from the _utxo field. In some situations, mostly unit or example tests, or when
--   initializing a small UTxO, we want to create a UTxOState that computes the utxosStakeDistr from
--   the utxosUtxo. This is aways safe to do, but if the utxosUtxo field is big, this can be very expensive,
--   which defeats the purpose of memoizing the utxosStakeDistr field. So use of this function should be
--   restricted to tests and initializations, where the invariant should be maintained.
--
--   TO IncrementalStake
smartUTxOState ::
  EraStake era =>
  PParams era ->
  UTxO era ->
  Coin ->
  Coin ->
  GovState era ->
  Coin ->
  UTxOState era
smartUTxOState _pp utxo c1 c2 st =
  UTxOState
    utxo
    c1
    c2
    st
    (addInstantStake utxo mempty)

-- =====================================================
-- Part 3 Apply a reward update, in NewEpoch rule
-- =====================================================

-- | Apply a RewardUpdate to the EpochState. Does several things
--   1) Adds reward coins to Rewards component of the UMap field of the DState,
--      for actively delegated Stake
--   2) Adds to the Treasury of the AccountState for non-actively delegated stake
--   3) Adds fees to the UTxOState
applyRUpd ::
  (EraGov era, EraCertState era) =>
  RewardUpdate ->
  EpochState era ->
  EpochState era
applyRUpd ru es =
  let (!es', _) = applyRUpdFiltered ru es
   in es'

-- TO IncrementalStake
applyRUpdFiltered ::
  (EraGov era, EraCertState era) =>
  RewardUpdate ->
  EpochState era ->
  (EpochState era, FilteredRewards era)
applyRUpdFiltered
  ru
  es@(EpochState as ls ss _nm) = (epochStateAns, filteredRewards)
    where
      !epochStateAns =
        EpochState as' ls' ss nm'
          & curPParamsEpochStateL .~ es ^. curPParamsEpochStateL
          & prevPParamsEpochStateL .~ es ^. prevPParamsEpochStateL
      dpState = lsCertState ls
      dState = dpState ^. certDStateL
      prevPParams = es ^. prevPParamsEpochStateL
      prevProVer = prevPParams ^. ppProtocolVersionL
      filteredRewards@FilteredRewards
        { frRegistered
        , frTotalUnregistered
        } = filterAllRewards' (rs ru) prevProVer dState
      -- Note: domain filteredRewards is a subset of domain (rewards dstate)
      registeredAggregated = aggregateCompactRewards prevProVer frRegistered
      -- Note: domain registeredAggregated is a subset of domain (rewards dstate)
      as' =
        as
          { asTreasury = addDeltaCoin (asTreasury as) (deltaT ru) <> frTotalUnregistered
          , asReserves = addDeltaCoin (asReserves as) (deltaR ru)
          }
      ls' =
        ls
          & lsUTxOStateL . utxosFeesL %~ (`addDeltaCoin` deltaF ru)
          & lsCertStateL . certDStateL . dsUnifiedL .~ (rewards dState UM.∪+ registeredAggregated)
      nm' = nonMyopic ru

data FilteredRewards era = FilteredRewards
  { -- Only the first component is strict on purpose. The others are lazy because in most instances
    -- they are never used, so this keeps them from being evaluated.

    frRegistered :: !(Map (Credential 'Staking) (Set Reward))
  -- ^ These are registered, in the current Unified map of the CertState
  , frShelleyIgnored :: Map (Credential 'Staking) (Set Reward)
  -- ^ These are registered, but ignored in the ShelleyEra because of backward
  --   compatibility in non-Shelley Eras, this field will be Map.empty
  , frUnregistered :: Set (Credential 'Staking)
  -- ^ These are NOT registered in the current Unified map of the CertState
  , frTotalUnregistered :: Coin
  -- ^ Total Coin of the unregistered rewards. These will end up in the Treasury or Reserves.
  }

instance NFData (FilteredRewards era) where
  rnf (FilteredRewards a b c d) = a `deepseq` b `deepseq` c `deepseq` rnf d

-- | Return aggregated information from a reward mapping from the previous Epoch.
--   Breaks the mapping into several parts captured by the 'Filtered' data type.
--   Note that the 'registered' field of the returned (FilteredRewards) is a Map
--   whose domain is always a subset of the Rewards View of the Unified Map in the DState of the EpochState.
--   'prevPParams' is the ProtocolParams of the previous Epoch
--   'rs' is the rewards mapping of the RewardUpdate from that previous Epoch
filterAllRewards' ::
  Map (Credential 'Staking) PoolRewards ->
  ProtVer ->
  DState era ->
  FilteredRewards era
filterAllRewards' rs protVer dState =
  FilteredRewards registered shelleyIgnored unregistered totalUnregistered
  where
    (regRU, unregRU) = Map.partitionWithKey (\k _ -> member k (rewards dState)) rs
    -- Partition on memebership in the rewards view of the unified map of DState
    -- Note that only registered rewards appear in 'regRU' because of this 'member' check.
    totalUnregistered = fold $ aggregateRewards protVer unregRU
    unregistered = Map.keysSet unregRU
    (registered, shelleyIgnored) = filterRewards protVer regRU

filterAllRewards ::
  (EraGov era, EraCertState era) =>
  Map (Credential 'Staking) (Set Reward) ->
  EpochState era ->
  FilteredRewards era
filterAllRewards mp epochstate = filterAllRewards' mp prevPP dState
  where
    prevPP = epochstate ^. prevPParamsEpochStateL . ppProtocolVersionL
    dState = epochstate ^. esLStateL . lsCertStateL . certDStateL
