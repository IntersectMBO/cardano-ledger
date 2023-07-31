{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
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
  updateStakeDistribution,
  incrementalStakeDistr,
  applyRUpd,
  applyRUpdFiltered,
  smartUTxOState,
  filterAllRewards,
  FilteredRewards (..),
)
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.CertState (
  CertState (..),
  DState (..),
  PState (..),
  delegations,
  rewards,
 )
import Cardano.Ledger.Coin (
  Coin (..),
  addDeltaCoin,
 )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (
  Credential (..),
  StakeReference (StakeRefBase, StakeRefPtr),
 )
import Cardano.Ledger.EpochBoundary (
  SnapShot (..),
  Stake (..),
 )
import Cardano.Ledger.Keys (
  KeyRole (..),
 )
import Cardano.Ledger.Shelley.Governance (EraGovernance (GovernanceState))
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.Shelley.RewardUpdate (RewardUpdate (..))
import Cardano.Ledger.Shelley.Rewards (
  aggregateCompactRewards,
  aggregateRewards,
  filterRewards,
 )
import Cardano.Ledger.UMap (
  UMElem,
  UMap (..),
  compactCoinOrError,
  member,
 )
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (
  UTxO (..),
 )
import Control.DeepSeq (NFData (rnf), deepseq)
import Control.Exception (assert)
import Data.Foldable (fold)
import Data.Group (invert)
import Data.Map.Internal.Debug as Map (valid)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.VMap as VMap
import Lens.Micro

-- =======================================================================
-- Part 1, Incrementally updating the IncrementalStake in Utxo rule
-- =======================================================================

-- | Incrementally add the inserts 'utxoAdd' and the deletes 'utxoDel' to the IncrementalStake.
updateStakeDistribution ::
  EraTxOut era =>
  PParams era ->
  IncrementalStake (EraCrypto era) ->
  UTxO era ->
  UTxO era ->
  IncrementalStake (EraCrypto era)
updateStakeDistribution pp incStake0 utxoDel utxoAdd = incStake2
  where
    incStake1 = incrementalAggregateUtxoCoinByCredential pp id utxoAdd incStake0
    incStake2 = incrementalAggregateUtxoCoinByCredential pp invert utxoDel incStake1

-- | Incrementally sum up all the Coin, for each staking Credential, in the outputs of the UTxO, and
--   "add" them to the IncrementalStake. "add" has different meaning depending on if we are inserting
--   or deleting the UtxO entries. For inserts (mode is the identity: id) and for deletes (mode is invert).
--   Never store a (Coin 0) balance, since these do not occur in the non-incremental style that
--   works directly from the whole UTxO.
--   This function has a non-incremental analog 'aggregateUtxoCoinByCredential' . In this incremental
--   version we expect the size of the UTxO to be fairly small. I.e the number of inputs and outputs
--   in a transaction, which is aways < 4096, not millions, and very often < 10).
incrementalAggregateUtxoCoinByCredential ::
  forall era.
  EraTxOut era =>
  PParams era ->
  (Coin -> Coin) ->
  UTxO era ->
  IncrementalStake (EraCrypto era) ->
  IncrementalStake (EraCrypto era)
incrementalAggregateUtxoCoinByCredential pp mode (UTxO u) initial =
  Map.foldl' accum initial u
  where
    keepOrDelete new Nothing =
      case mode new of
        Coin 0 -> Nothing
        final -> Just final
    keepOrDelete new (Just old) =
      case mode new <> old of
        Coin 0 -> Nothing
        final -> Just final
    ignorePtrs = HardForks.forgoPointerAddressResolution (pp ^. ppProtocolVersionL)
    accum ans@(IStake stake ptrs) out =
      let c = out ^. coinTxOutL
       in case out ^. addrTxOutL of
            Addr _ _ (StakeRefPtr p) ->
              if ignorePtrs
                then ans
                else IStake stake (Map.alter (keepOrDelete c) p ptrs)
            Addr _ _ (StakeRefBase hk) -> IStake (Map.alter (keepOrDelete c) hk stake) ptrs
            _other -> ans

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
  EraTxOut era =>
  PParams era ->
  UTxO era ->
  Coin ->
  Coin ->
  GovernanceState era ->
  UTxOState era
smartUTxOState pp utxo c1 c2 st =
  UTxOState
    utxo
    c1
    c2
    st
    (updateStakeDistribution pp mempty mempty utxo)

-- =======================================================================
-- Part 2. Compute a Snapshot using the IncrementalStake in Snap rule
-- =======================================================================

-- | This computes a Snapshot using IncrementalStake (which is an
--   aggregate of the current UTxO) and UMap (which tracks Coin,
--   SPoolUView, and Ptrs simultaneously).  Note that logically:
--   1) IncrementalStake = (credStake, ptrStake)
--   2) UMap = (rewards, activeDelegs, ptrmap :: Map ptr cred)
--
--   Using this scheme the logic can do 3 things in one go, without touching the UTxO.
--   1) Resolve Pointers
--   2) Throw away things not actively delegated
--   3) Add up the coin
--
--   The Stake distribution function (Map cred coin) (the first component of a SnapShot)
--   is defined by this SetAlgebra expression:
--   (dom activeDelegs) ◁ (aggregate+ (credStake ∪ ptrStake ∪ rewards))
--
--   We can apply meaning preserving operations to get equivalent expressions
--
--   (dom activeDelegs) ◁ (aggregate+ (credStake ∪ ptrStake ∪ rewards))
--   aggregate+ (dom activeDelegs ◁ (credStake ∪ ptrStake ∪ rewards))
--   aggregate+ ((dom activeDelegs ◁ credStake) ∪ (dom activeDelegs ◁ ptrStake) ∪ (dom activeDelegs ◁ rewards))
--
--   We will compute this in several steps
--   step1 = (dom activeDelegs ◁ credStake) ∪ (dom activeDelegs ◁ ptrStake)
--   step2 =  aggregate (dom activeDelegs ◁ rewards) step1
--   This function has a non-incremental analog, 'stakeDistr', mosty used in tests, which does use the UTxO.
incrementalStakeDistr ::
  forall era.
  EraPParams era =>
  PParams era ->
  IncrementalStake (EraCrypto era) ->
  DState era ->
  PState era ->
  SnapShot (EraCrypto era)
incrementalStakeDistr pp (IStake credStake ptrStake) ds ps =
  SnapShot
    (Stake $ VMap.fromMap (compactCoinOrError <$> step2))
    delegs_
    (VMap.fromMap poolParams)
  where
    UMap triplesMap ptrsMap = dsUnified ds
    PState {psStakePoolParams = poolParams} = ps
    delegs_ = UM.unUnifyToVMap (delegations ds)
    -- A credential is active, only if it is being delegated
    activeCreds = Map.filterWithKey (\k _ -> VMap.member k delegs_) credStake
    ignorePtrs = HardForks.forgoPointerAddressResolution (pp ^. ppProtocolVersionL)
    -- pre Conway: (dom activeDelegs ◁ credStake) ∪ (dom activeDelegs ◁ ptrStake)
    -- afterwards we forgo ptr resolution: (dom activeDelegs ◁ credStake)
    step1 =
      if ignorePtrs
        then activeCreds
        else -- Resolve inserts and delets which were indexed by ptrs, by looking them up in the ptrsMap
        -- and combining the result of the lookup with teh ordinary stake, keeping only the active credentials
          Map.foldlWithKey' addResolvedPointer activeCreds ptrStake
    step2 = aggregateActiveStake triplesMap step1
    addResolvedPointer ans ptr coin =
      case Map.lookup ptr ptrsMap of -- map of ptrs to credentials
        Just cred | VMap.member cred delegs_ -> Map.insertWith (<>) cred coin ans
        _ -> ans

-- | Aggregate active stake by merging two maps. The triple map from the
--   UMap, and the IncrementalStake. Only keep the active stake. Active can
--   be determined if there is a (SJust deleg) in the Tuple.  This is step2 =
--   aggregate (dom activeDelegs ◁ rewards) step1
aggregateActiveStake :: Ord k => Map k (UMElem c) -> Map k Coin -> Map k Coin
aggregateActiveStake m1 m2 = assert (Map.valid m) m
  where
    m =
      Map.mergeWithKey
        -- How to merge the ranges of the two maps where they have a common key. Below
        -- 'coin1' and 'coin2' have the same key, '_k', and the stake is active if the delegation is SJust
        (\_k trip coin2 -> extractAndAdd coin2 <$> UM.umElemRDActive trip)
        -- what to do when a key appears just in 'tripmap', we only add the coin if the key is active
        (Map.mapMaybe (\trip -> fromCompact . UM.rdReward <$> UM.umElemRDActive trip))
        -- what to do when a key is only in 'incremental', keep everything, because at
        -- the call site of aggregateActiveStake, the arg 'incremental' is filtered by
        -- 'resolveActiveIncrementalPtrs' which guarantees that only active stake is included.
        id
        m1
        m2
    extractAndAdd :: Coin -> UM.RDPair -> Coin
    extractAndAdd coin (UM.RDPair rew _dep) = coin <> fromCompact rew

-- =====================================================
-- Part 3 Apply a reward update, in NewEpoch rule
-- =====================================================

-- | Apply a RewardUpdate to the EpochState. Does several things
--   1) Adds reward coins to Rewards component of the UMap field of the DState,
--      for actively delegated Stake
--   2) Adds to the Treasury of the AccountState for non-actively delegated stake
--   3) Adds fees to the UTxOState
applyRUpd ::
  EraGovernance era =>
  RewardUpdate (EraCrypto era) ->
  EpochState era ->
  EpochState era
applyRUpd ru es =
  let (!es', _) = applyRUpdFiltered ru es
   in es'

-- TO IncrementalStake
applyRUpdFiltered ::
  EraGovernance era =>
  RewardUpdate (EraCrypto era) ->
  EpochState era ->
  (EpochState era, FilteredRewards era)
applyRUpdFiltered
  ru
  es@(EpochState as ss ls _nm) = (epochStateAns, filteredRewards)
    where
      !epochStateAns =
        EpochState as' ss ls' nm'
          & curPParamsEpochStateL .~ es ^. curPParamsEpochStateL
          & prevPParamsEpochStateL .~ es ^. prevPParamsEpochStateL
      utxoState_ = lsUTxOState ls
      dpState = lsCertState ls
      dState = certDState dpState
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
          { lsUTxOState =
              utxoState_ {utxosFees = utxosFees utxoState_ `addDeltaCoin` deltaF ru}
          , lsCertState =
              dpState
                { certDState =
                    dState
                      { dsUnified = rewards dState UM.∪+ registeredAggregated
                      }
                }
          }
      nm' = nonMyopic ru

data FilteredRewards era = FilteredRewards
  { -- Only the first component is strict on purpose. The others are lazy because in most instances
    -- they are never used, so this keeps them from being evaluated.

    frRegistered :: !(Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))))
  -- ^ These are registered, in the current Unified map of the CertState
  , frShelleyIgnored :: Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era)))
  -- ^ These are registered, but ignored in the ShelleyEra because of backward
  --   compatibility in non-Shelley Eras, this field will be Map.empty
  , frUnregistered :: Set (Credential 'Staking (EraCrypto era))
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
  Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))) ->
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
  EraGovernance era =>
  Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))) ->
  EpochState era ->
  FilteredRewards era
filterAllRewards mp epochstate = filterAllRewards' mp prevPP dState
  where
    prevPP = epochstate ^. prevPParamsEpochStateL . ppProtocolVersionL
    dState = (certDState . lsCertState . esLState) epochstate
