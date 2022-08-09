{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Shelley.LedgerState.IncrementalStake
  ( updateStakeDistribution,
    incrementalStakeDistr,
    applyRUpd,
    applyRUpd',
    smartUTxOState,
    filterAllRewards,
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
import Cardano.Ledger.Keys
  ( KeyRole (..),
  )
import Cardano.Ledger.Shelley.EpochBoundary
  ( SnapShot (..),
    Stake (..),
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
import Cardano.Ledger.Shelley.Rewards
  ( Reward (..),
    aggregateRewards,
    filterRewards,
  )
import Cardano.Ledger.Shelley.TxBody
  ( Ptr (..),
  )
import Cardano.Ledger.Shelley.UTxO
  ( UTxO (..),
  )
import Cardano.Ledger.UnifiedMap
  ( Triple,
    UMap (..),
  )
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

-- | Incrementally add the inserts 'utxoAdd' and the deletes 'utxoDel' to the IncrementalStake.
updateStakeDistribution ::
  EraTxOut era =>
  IncrementalStake (Crypto era) ->
  UTxO era ->
  UTxO era ->
  IncrementalStake (Crypto era)
updateStakeDistribution incStake0 utxoDel utxoAdd = incStake2
  where
    incStake1 = incrementalAggregateUtxoCoinByCredential id utxoAdd incStake0
    incStake2 = incrementalAggregateUtxoCoinByCredential invert utxoDel incStake1

-- | Incrementally sum up all the Coin for each staking Credential, use different 'mode' operations
--   for UTxO that are inserts (id) and UTxO that are deletes (invert). Never store a (Coin 0) balance,
--   since these do not occur in the non-incremental style that works directly from the whole UTxO.
--   This function has a non-incremental analog 'aggregateUtxoCoinByCredential' . In this incremental
--   version we expect the size of the UTxO to be fairly small. I.e the number of inputs and outputs
--   in a transaction, which is aways < 4096, not millions, and very often < 10).
incrementalAggregateUtxoCoinByCredential ::
  forall era.
  EraTxOut era =>
  (Coin -> Coin) ->
  UTxO era ->
  IncrementalStake (Crypto era) ->
  IncrementalStake (Crypto era)
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

filterAllRewards ::
  ( HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))) ->
  EpochState era ->
  ( Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))),
    Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))),
    Set (Credential 'Staking (Crypto era)),
    Coin
  )
filterAllRewards rs' (EpochState _as _ss ls pr _pp _nm) =
  (registered, eraIgnored, unregistered, totalUnregistered)
  where
    delegState = lsDPState ls
    dState = dpsDState delegState
    (regRU, unregRU) =
      Map.partitionWithKey
        (\k _ -> eval (k ∈ dom (rewards dState)))
        rs'
    totalUnregistered = fold $ aggregateRewards pr unregRU
    unregistered = Map.keysSet unregRU
    (registered, eraIgnored) = filterRewards pr regRU

-- | Aggregate active stake by merging two maps. The triple map from the
--   UnifiedMap, and the IncrementalStake Only keep the active stake. Active can
--   be determined if there is a (SJust deleg) in the Triple.  This is step2 =
--   aggregate (dom activeDelegs ◁ rewards) step1
--
--   TO IncrementalStake
aggregateActiveStake :: Ord k => Map k (Triple crypto) -> Map k Coin -> Map k Coin
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

-- ================================================

-- | A valid (or self-consistent) UTxOState{_utxo, _deposited, _fees, _ppups, _stakeDistro}
--   maintains an invariant between the _utxo and _stakeDistro fields. the _stakeDistro field is
--   the aggregation of Coin over the StakeReferences in the UTxO. It can be computed by a pure
--   function from the _utxo field. In some situations, mostly unit or example tests, or when
--   initializing a small UTxO, we want to create a UTxOState that computes the _stakeDistro from
--   the _utxo. This is aways safe to do, but if the _utxo field is big, this can be very expensive,
--   which defeats the purpose of memoizing the _stakeDistro field. So use of this function should be
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

-- ==============================

-- | Apply a reward update
--
-- TO IncrementalStake
applyRUpd ::
  ( HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  RewardUpdate (Crypto era) ->
  EpochState era ->
  EpochState era
applyRUpd ru es =
  let (es', _, _, _) = applyRUpd' ru es
   in es'

-- TO IncrementalStake
applyRUpd' ::
  ( HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  RewardUpdate (Crypto era) ->
  EpochState era ->
  ( EpochState era,
    Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))),
    Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))),
    Set (Credential 'Staking (Crypto era))
  )
applyRUpd'
  ru
  es@(EpochState as ss ls pr pp _nm) =
    (EpochState as' ss ls' pr pp nm', registered, eraIgnored, unregistered)
    where
      utxoState_ = lsUTxOState ls
      delegState = lsDPState ls
      dState = dpsDState delegState
      (registered, eraIgnored, unregistered, totalUnregistered) =
        filterAllRewards (rs ru) es
      registeredAggregated = aggregateRewards pp registered
      as' =
        as
          { _treasury = addDeltaCoin (_treasury as) (deltaT ru) <> totalUnregistered,
            _reserves = addDeltaCoin (_reserves as) (deltaR ru)
          }
      ls' =
        ls
          { lsUTxOState =
              utxoState_ {_fees = _fees utxoState_ `addDeltaCoin` deltaF ru},
            lsDPState =
              delegState
                { dpsDState =
                    dState
                      { _unified = rewards dState UM.∪+ registeredAggregated
                      }
                }
          }
      nm' = nonMyopic ru

-- | Compute the current state distribution by using the IncrementalStake,

-- | This computes the stake distribution using IncrementalStake (which is an
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
  forall crypto.
  IncrementalStake crypto ->
  DState crypto ->
  PState crypto ->
  SnapShot crypto
incrementalStakeDistr incstake ds ps =
  SnapShot
    (Stake $ VMap.fromMap (compactCoinOrError <$> step2))
    delegs_
    (VMap.fromMap poolParams)
  where
    UnifiedMap tripmap ptrmap = _unified ds
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
  (Credential 'Staking crypto -> Bool) ->
  Map Ptr (Credential 'Staking crypto) ->
  IncrementalStake crypto ->
  Map (Credential 'Staking crypto) Coin
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

compactCoinOrError :: Coin -> CompactForm Coin
compactCoinOrError c =
  case toCompact c of
    Nothing -> error $ "Invalid ADA value in staking: " <> show c
    Just compactCoin -> compactCoin
