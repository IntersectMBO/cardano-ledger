{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- ===========================================================================
-- There are three parts to IncrementalStaking.
-- 1) The incremental part, where we keep track of each update to the UTxO
--    adding Inputs and deleting consumed Outputs. Done in the Utxo rules.
-- 2) Finalizing and aggregating by stake credential to create a Snapshot.
--    done in the Snap rules.
-- 3) Applying the RewardUpdate, to the Rewards component of the UnifiedMap.
--    done in the NewEpoch rules.

module Cardano.Ledger.Shelley.LedgerState.IncrementalStake
  ( updateStakeDistribution,
    incrementalStakeDistr,
    applyRUpd,
    applyRUpdFiltered,
    smartUTxOState,
    filterAllRewards,
    FilteredRewards (..),
  )
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes
  ( ProtVer (..),
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    addDeltaCoin,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
  ( Credential (..),
    StakeReference (StakeRefBase, StakeRefPtr),
  )
import Cardano.Ledger.EpochBoundary
  ( SnapShot (..),
    Stake (..),
  )
import Cardano.Ledger.Keys
  ( KeyRole (..),
  )
import Cardano.Ledger.Shelley.LedgerState.DPState
  ( DPState (..),
    DState (..),
    PState (..),
    delegations,
    rewards,
  )
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.Shelley.RewardUpdate (RewardUpdate (..))
import Cardano.Ledger.Shelley.Rewards (aggregateRewards, filterRewards)
import Cardano.Ledger.Shelley.TxBody
  ( Ptr (..),
  )
import Cardano.Ledger.UTxO
  ( UTxO (..),
  )
import Cardano.Ledger.UnifiedMap
  ( Triple,
    UMap (..),
  )
import Control.DeepSeq (NFData (rnf), deepseq)
import Control.SetAlgebra (dom, eval, (∈))
import Control.State.Transition (STS (State))
import Data.Foldable (fold)
import Data.Group (invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.UMap as UM
import qualified Data.VMap as VMap
import GHC.Records (HasField (..))
import Lens.Micro

-- =======================================================================
-- Part 1, Incrementally updating the IncrementalStake in Utxo rule
-- =======================================================================

-- | Incrementally add the inserts 'utxoAdd' and the deletes 'utxoDel' to the IncrementalStake.
updateStakeDistribution ::
  EraTxOut era =>
  IncrementalStake (EraCrypto era) ->
  UTxO era ->
  UTxO era ->
  IncrementalStake (EraCrypto era)
updateStakeDistribution incStake0 utxoDel utxoAdd = incStake2
  where
    incStake1 = incrementalAggregateUtxoCoinByCredential id utxoAdd incStake0
    incStake2 = incrementalAggregateUtxoCoinByCredential invert utxoDel incStake1

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
  (Coin -> Coin) ->
  UTxO era ->
  IncrementalStake (EraCrypto era) ->
  IncrementalStake (EraCrypto era)
incrementalAggregateUtxoCoinByCredential mode (UTxO u) initial =
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
    accum ans@(IStake stake ptrs) out =
      let c = out ^. coinTxOutL
       in case out ^. addrTxOutL of
            Addr _ _ (StakeRefPtr p) -> IStake stake (Map.alter (keepOrDelete c) p ptrs)
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
  UTxO era ->
  Coin ->
  Coin ->
  State (EraRule "PPUP" era) ->
  UTxOState era
smartUTxOState utxo c1 c2 st =
  UTxOState
    utxo
    c1
    c2
    st
    (updateStakeDistribution mempty mempty utxo)

-- =======================================================================
-- Part 2. Compute a Snapshot using the IncrementalStake in Snap rule
-- =======================================================================

-- | This computes a Snapshot using IncrementalStake (which is an
--   aggregate of the current UTxO) and UnifiedMap (which tracks Coin,
--   Delegations, and Ptrs simultaneously).  Note that logically:
--   1) IncrementalStake = (credStake, ptrStake)
--   2) UnifiedMap = (rewards, activeDelegs, ptrmap :: Map ptr cred)
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
  forall c.
  IncrementalStake c ->
  DState c ->
  PState c ->
  SnapShot c
incrementalStakeDistr incstake ds ps =
  SnapShot
    (Stake $ VMap.fromMap (compactCoinOrError <$> step2))
    delegs_
    (VMap.fromMap poolParams)
  where
    UnifiedMap tripmap ptrmap = dsUnified ds
    PState poolParams _ _ = ps
    delegs_ = UM.viewToVMap (delegations ds)
    -- A credential is active, only if it is being delegated
    step1 = resolveActiveIncrementalPtrs (`VMap.member` delegs_) ptrmap incstake
    step2 = aggregateActiveStake tripmap step1

-- | Resolve inserts and deletes which were indexed by Ptrs, by looking them
--   up in 'ptrs' and combining the result of the lookup with the ordinary stake.
--   keep ony the active credentials.
--   This is  step1 = (dom activeDelegs ◁ credStake) ∪ (dom activeDelegs ◁ ptrStake)
resolveActiveIncrementalPtrs ::
  (Credential 'Staking c -> Bool) ->
  Map Ptr (Credential 'Staking c) ->
  IncrementalStake c ->
  Map (Credential 'Staking c) Coin
resolveActiveIncrementalPtrs isActive ptrMap_ (IStake credStake ptrStake) =
  Map.foldlWithKey' accum step1A ptrStake -- step1A  ∪ (dom activeDelegs ◁ ptrStake)
  where
    -- (dom activeDelegs ◁ credStake)
    step1A = Map.filterWithKey (\k _ -> isActive k) credStake
    accum ans ptr coin =
      case Map.lookup ptr ptrMap_ of -- Map ptrs to Credentials
        Nothing -> ans
        Just cred ->
          if isActive cred
            then Map.insertWith (<>) cred coin ans
            else ans

-- | Aggregate active stake by merging two maps. The triple map from the
--   UnifiedMap, and the IncrementalStake Only keep the active stake. Active can
--   be determined if there is a (SJust deleg) in the Triple.  This is step2 =
--   aggregate (dom activeDelegs ◁ rewards) step1
aggregateActiveStake :: Ord k => Map k (Triple c) -> Map k Coin -> Map k Coin
aggregateActiveStake =
  Map.mergeWithKey
    -- How to merge the ranges of the two maps where they have a common key. Below
    -- 'coin1' and 'coin2' have the same key, '_k', and the stake is active if the delegation is SJust
    (\_k trip coin2 -> (<> coin2) <$> UM.tripRewardActiveDelegation trip)
    -- what to do when a key appears just in 'tripmap', we only add the coin if the key is active
    (Map.mapMaybe UM.tripRewardActiveDelegation)
    -- what to do when a key is only in 'incremental', keep everything, because at
    -- the call site of aggregateActiveStake, the arg 'incremental' is filtered by
    -- 'resolveActiveIncrementalPtrs' which guarantees that only active stake is included.
    id

compactCoinOrError :: Coin -> CompactForm Coin
compactCoinOrError c =
  case toCompact c of
    Nothing -> error $ "Invalid ADA value in staking: " <> show c
    Just compactCoin -> compactCoin

-- =====================================================
-- Part 3 Apply a reward update, in NewEpoch rule
-- =====================================================

-- | Apply a RewardUpdate to the EpochState. Does several things
--   1) Adds reward coins to Rewards component of the UnifiedMap field of the DState,
--      for actively delegated Stake
--   2) Adds to the Treasury of the AccountState for non-actively delegated stake
--   3) Adds fees to the UTxOState
applyRUpd ::
  ( HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  RewardUpdate (EraCrypto era) ->
  EpochState era ->
  EpochState era
applyRUpd ru es =
  let (!es', _) = applyRUpdFiltered ru es
   in es'

-- TO IncrementalStake
applyRUpdFiltered ::
  ( HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  RewardUpdate (EraCrypto era) ->
  EpochState era ->
  (EpochState era, FilteredRewards era)
applyRUpdFiltered
  ru
  es@(EpochState as ss ls pr pp _nm) =
    (epochStateAns, filteredRewards)
    where
      utxoState_ = lsUTxOState ls
      delegState = lsDPState ls
      !epochStateAns = EpochState as' ss ls' pr pp nm'
      dState = dpsDState delegState
      filteredRewards@FilteredRewards {frRegistered, frTotalUnregistered} = filterAllRewards (rs ru) es
      registeredAggregated = aggregateRewards pp frRegistered
      as' =
        as
          { _treasury = addDeltaCoin (_treasury as) (deltaT ru) <> frTotalUnregistered,
            _reserves = addDeltaCoin (_reserves as) (deltaR ru)
          }
      ls' =
        ls
          { lsUTxOState =
              utxoState_ {utxosFees = utxosFees utxoState_ `addDeltaCoin` deltaF ru},
            lsDPState =
              delegState
                { dpsDState =
                    dState
                      { _unified = rewards dState UM.∪+ registeredAggregated
                      }
                }
          }
      nm' = nonMyopic ru

data FilteredRewards era = FilteredRewards
  { -- Only the first component is strict on purpose. The others are lazy because in most instances
    -- they are never used, so this keeps them from being evaluated.

    -- | These are registered, but in the ShelleyEra they are ignored because of backward compatibility
    --  in other Eras, this field will be the Map.empty
    frRegistered :: !(Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era)))),
    frShelleyIgnored :: Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))),
    frUnregistered :: Set (Credential 'Staking (EraCrypto era)),
    frTotalUnregistered :: Coin
  }

instance NFData (FilteredRewards era) where
  rnf (FilteredRewards a b c d) = a `deepseq` b `deepseq` c `deepseq` rnf d

-- | Return aggregated information from a reward mapping from the previous Epoch
--   Breaks the mapping into several parts captured by the 'Filtered' data type.
filterAllRewards ::
  ( HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))) ->
  EpochState era ->
  FilteredRewards era
filterAllRewards rs' (EpochState _as _ss ls pr _pp _nm) =
  -- pr is the ProtocolParams of the previous Epoch
  -- rs' is the rewards mapping of the RewardUpdate from that previous Epoch
  FilteredRewards registered shelleyIgnored unregistered totalUnregistered
  where
    delegState = lsDPState ls
    dState = dpsDState delegState
    (regRU, unregRU) =
      Map.partitionWithKey
        (\k _ -> eval (k ∈ dom (rewards dState))) -- The rewards view of the unified map of DState
        rs'
    totalUnregistered = fold $ aggregateRewards pr unregRU
    unregistered = Map.keysSet unregRU
    (registered, shelleyIgnored) = filterRewards pr regRU
