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
import           Control.State.Transition.Trace (pattern SourceSignalTarget, signal, source,
                     sourceSignalTargets, target)

import           MockTypes (POOLREAP)
import           STS.PoolReap (PoolreapState (..))
import           TxData (pattern StakePools)

import           Ledger.Core (dom, (▷))
import           Rules.TestPool (getRetiring, getStPools)
import           Test.Utils (assertAll)


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
        $ trace @POOLREAP traceLen `ofLengthAtLeast` 1

  assertAll poolRemoved tr

  where poolRemoved (SourceSignalTarget
                      { source = PoolreapState _ _ _ p
                      , signal = e
                      , target = PoolreapState _ _ _ p'}) =
          let StakePools stp  = getStPools p
              StakePools stp' = getStPools p'
              retiring        = getRetiring p
              retiring'       = getRetiring p'
              retire          = dom $ retiring ▷ Set.singleton e in
             (not . null) retire
          && (retire `Set.isSubsetOf` dom stp)
          && Set.null (retire `Set.intersection` dom stp')
          && Set.null (retire `Set.intersection` dom retiring')
