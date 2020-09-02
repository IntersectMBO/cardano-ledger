{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Shelley.Spec.Ledger.Rules.TestPoolreap
  ( constantSumPots,
    nonNegativeDeposits,
    removedAfterPoolreap,
  )
where

import Control.Iterate.SetAlgebra (dom, eval, setSingleton, (∩), (⊆), (▷))
import Control.State.Transition.Trace
  ( SourceSignalTarget,
    signal,
    source,
    target,
    pattern SourceSignalTarget,
  )
import Data.Foldable (fold)
import qualified Data.Set as Set (Set, null)
import Shelley.Spec.Ledger.API (POOLREAP)
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Shelley.Spec.Ledger.LedgerState
  ( _deposited,
    _fees,
    _pParams,
    _reserves,
    _rewards,
    _treasury,
    _utxo,
    pattern AccountState,
    pattern DState,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.STS.PoolReap
  ( prAcnt,
    prDState,
    prPState,
    prUTxOSt,
    pattern PoolreapState,
  )
import Shelley.Spec.Ledger.UTxO (balance)
import Test.QuickCheck (Property, conjoin)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Rules.TestPool (getRetiring)

-----------------------------
-- Properties for POOLREAP --
-----------------------------

-- | Check that after a POOLREAP certificate transition the pool is removed from
-- the stake pool and retiring maps.
removedAfterPoolreap ::
  [SourceSignalTarget (POOLREAP C)] ->
  Property
removedAfterPoolreap tr =
  conjoin $
    map poolRemoved tr
  where
    poolRemoved
      ( SourceSignalTarget
          { source = PoolreapState {prPState = p},
            signal = e,
            target = PoolreapState {prPState = p'}
          }
        ) =
        let stp = _pParams p
            stp' = _pParams p'
            retiring = getRetiring p
            retiring' = getRetiring p'
            retire :: Set.Set (KeyHash 'StakePool C) -- This declaration needed to disambiguate 'eval'
            retire = eval (dom (retiring ▷ setSingleton e))
         in eval (retire ⊆ dom stp)
              && Set.null (eval (retire ∩ dom stp'))
              && Set.null (eval (retire ∩ dom retiring'))

-- | Check that deposits are always non-negative
nonNegativeDeposits ::
  [SourceSignalTarget (POOLREAP C)] ->
  Property
nonNegativeDeposits tr =
  conjoin $
    map
      ( \PoolreapState
           { prUTxOSt =
               UTxOState {_deposited = deposit}
           } -> deposit >= mempty
      )
      (map source tr)

-- | Check that the sum of circulation, deposits, fees, treasury, rewards and
-- reserves is constant.
constantSumPots ::
  [SourceSignalTarget (POOLREAP C)] ->
  Property
constantSumPots tr =
  conjoin $
    map potsSumEqual tr
  where
    potsSumEqual
      ( SourceSignalTarget
          { source =
              PoolreapState
                { prUTxOSt =
                    UTxOState
                      { _utxo = u,
                        _deposited = d,
                        _fees = fees
                      },
                  prAcnt =
                    AccountState
                      { _treasury = treasury,
                        _reserves = reserves
                      },
                  prDState = DState {_rewards = rewards}
                },
            target =
              PoolreapState
                { prUTxOSt =
                    UTxOState
                      { _utxo = u',
                        _deposited = d',
                        _fees = fees'
                      },
                  prAcnt =
                    AccountState
                      { _treasury = treasury',
                        _reserves = reserves'
                      },
                  prDState = DState {_rewards = rewards'}
                }
          }
        ) =
        ( balance u
            <> d
            <> fees
            <> treasury
            <> reserves
            <> fold rewards
        )
          == ( balance u'
                 <> d'
                 <> fees'
                 <> treasury'
                 <> reserves'
                 <> fold rewards'
             )
