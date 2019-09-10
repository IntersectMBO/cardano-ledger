{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestPoolreap where

import           Control.Monad (when)
import qualified Data.Set as Set (intersection, isSubsetOf, null, singleton)

import           Hedgehog (Property, forAll, property, withTests, (===))

import           Control.State.Transition.Generator (trace)
import           Control.State.Transition.Trace (sourceSignalTargets, traceLength)

import           MockTypes (POOLREAP)
import           STS.PoolReap (PoolreapState (..))
import           TxData (pattern StakePools)

import           Rules.TestPool (getRetiring, getStPools)

import           Ledger.Core (dom, (▷))

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Int
numberOfTests = 300

traceLen :: Int
traceLen = 100

-----------------------------
-- Properties for POOLREAP --
-----------------------------

-- | Check that after a POOLREAP certificate transition the pool is removed from
-- the stake pool and retiring maps.
removedAfterPoolreap :: Property
removedAfterPoolreap = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @POOLREAP $ fromIntegral traceLen)
  let
    n :: Integer
    n = fromIntegral $ traceLength t
    tr = sourceSignalTargets t

  when (n > 1) $
    [] === filter (not . poolRemoved) tr

  where poolRemoved (PoolreapState _ _ _ p, e, PoolreapState _ _ _ p') =
          let StakePools stp  = getStPools p
              StakePools stp' = getStPools p'
              retiring        = getRetiring p
              retiring'       = getRetiring p'
              retire          = dom $ retiring ▷ Set.singleton e in
             (not . null) retire
          && (retire `Set.isSubsetOf` dom stp)
          && Set.null (retire `Set.intersection` dom stp')
          && Set.null (retire `Set.intersection` dom retiring')
