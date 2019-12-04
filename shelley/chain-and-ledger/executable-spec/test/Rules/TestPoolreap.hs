{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestPoolreap where

import qualified Data.Set as Set (intersection, isSubsetOf, null, singleton)
import           Data.Word (Word64)

import           Hedgehog (Property, forAll, property, withTests)

import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (pattern NewestFirst, pattern SourceSignalTarget,
                     signal, source, sourceSignalTargets, target, traceStates)

import           Coin (pattern Coin)
import           LedgerState (pattern AccountState, pattern DState, pattern UTxOState, _deposited,
                     _fees, _reserves, _rewards, _treasury, _utxo)
import           MockTypes (POOLREAP)
import           STS.PoolReap (pattern PoolreapState, prAcnt, prDState, prPState, prUTxOSt)
import           TxData (pattern StakePools)
import           UTxO (balance)

import           Ledger.Core (dom, (▷))
import           Rules.TestPool (getRetiring, getStPools)
import           Test.Utils (assertAll, testGlobals)


------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

-----------------------------
-- Properties for POOLREAP --
-----------------------------

-- | Check that after a POOLREAP certificate transition the pool is removed from
-- the stake pool and retiring maps.
removedAfterPoolreap :: Property
removedAfterPoolreap = withTests (fromIntegral numberOfTests) . property $ do
  tr <- fmap sourceSignalTargets
        $ forAll
        $ trace @POOLREAP testGlobals traceLen `ofLengthAtLeast` 1

  assertAll poolRemoved tr

  where poolRemoved (SourceSignalTarget
                      { source = PoolreapState { prPState = p }
                      , signal = e
                      , target = PoolreapState { prPState = p' }}) =
          let StakePools stp  = getStPools p
              StakePools stp' = getStPools p'
              retiring        = getRetiring p
              retiring'       = getRetiring p'
              retire          = dom $ retiring ▷ Set.singleton e in
             (not . null) retire
          && (retire `Set.isSubsetOf` dom stp)
          && Set.null (retire `Set.intersection` dom stp')
          && Set.null (retire `Set.intersection` dom retiring')


-- | Check that deposits are always non-negative
nonNegativeDeposits :: Property
nonNegativeDeposits = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll $ trace @POOLREAP testGlobals traceLen
  let states = traceStates NewestFirst t

  assertAll (\PoolreapState
              { prUTxOSt =
                  UTxOState { _deposited = deposit} } -> deposit >= 0) states


-- | Check that the sum of circulation, deposits, fees, treasury, rewards and
-- reserves is constant.
constantSumPots :: Property
constantSumPots = withTests (fromIntegral numberOfTests) . property $ do
  tr <- fmap sourceSignalTargets
        $ forAll
        $ trace @POOLREAP testGlobals traceLen `ofLengthAtLeast` 1

  assertAll potsSumEqual tr

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
