{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestPoolreap
  ( constantSumPots
  , nonNegativeDeposits
  , removedAfterPoolreap)
where

import qualified Data.Set as Set (intersection, isSubsetOf, null, singleton)

import           Test.QuickCheck (Property, conjoin)

import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     signal, source, target)

import           Coin (pattern Coin)
import           ConcreteCryptoTypes (POOLREAP)
import           LedgerState (pattern AccountState, pattern DState, pattern UTxOState, _deposited,
                     _fees, _reserves, _rewards, _treasury, _utxo)
import           STS.PoolReap (pattern PoolreapState, prAcnt, prDState, prPState, prUTxOSt)
import           TxData (pattern StakePools)
import           UTxO (balance)

import           Ledger.Core (dom, (▷))
import           Rules.TestPool (getRetiring, getStPools)

-----------------------------
-- Properties for POOLREAP --
-----------------------------

-- | Check that after a POOLREAP certificate transition the pool is removed from
-- the stake pool and retiring maps.
removedAfterPoolreap
  :: [SourceSignalTarget POOLREAP]
  -> Property
removedAfterPoolreap tr =
  conjoin $
    map poolRemoved tr

  where poolRemoved (SourceSignalTarget
                      { source = PoolreapState { prPState = p }
                      , signal = e
                      , target = PoolreapState { prPState = p' }}) =
          let StakePools stp  = getStPools p
              StakePools stp' = getStPools p'
              retiring        = getRetiring p
              retiring'       = getRetiring p'
              retire          = dom $ retiring ▷ Set.singleton e in

          (retire `Set.isSubsetOf` dom stp)
          && Set.null (retire `Set.intersection` dom stp')
          && Set.null (retire `Set.intersection` dom retiring')


-- | Check that deposits are always non-negative
nonNegativeDeposits
  :: [SourceSignalTarget POOLREAP]
  -> Property
nonNegativeDeposits tr =
  conjoin $
    map (\PoolreapState
           { prUTxOSt =
                UTxOState { _deposited = deposit} } -> deposit >= 0)
        (map source tr)

-- | Check that the sum of circulation, deposits, fees, treasury, rewards and
-- reserves is constant.
constantSumPots
  :: [SourceSignalTarget POOLREAP]
  -> Property
constantSumPots tr =
  conjoin $
    map potsSumEqual tr

  where potsSumEqual (SourceSignalTarget
                      { source = PoolreapState
                                 { prUTxOSt = UTxOState {
                                       _utxo = u
                                     , _deposited = d
                                     , _fees = fees }
                                 , prAcnt = AccountState { _treasury = treasury
                                                         , _reserves = reserves }
                                 , prDState = DState { _rewards = rewards}}
                      , target = PoolreapState
                                 { prUTxOSt = UTxOState {
                                       _utxo = u'
                                     , _deposited = d'
                                     , _fees = fees' }
                                 , prAcnt = AccountState { _treasury = treasury'
                                                         , _reserves = reserves' }
                                 , prDState = DState { _rewards = rewards'}}}) =
          (balance u + d + fees + treasury + reserves + foldl (+) (Coin 0) rewards) ==
          (balance u' + d' + fees' + treasury' + reserves' + foldl (+) (Coin 0) rewards')
