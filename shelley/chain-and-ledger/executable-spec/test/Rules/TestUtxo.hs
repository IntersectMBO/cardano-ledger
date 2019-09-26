{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestUtxo where

import           Data.Word (Word64)

import           Hedgehog (Property, forAll, property, withTests)

import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (pattern SourceSignalTarget, source,
                     sourceSignalTargets, target)

import           LedgerState (pattern UTxOState)
import           MockTypes (UTXO)

import           Test.Utils (assertAll)

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

--------------------------
-- Properties for UTXOW --
--------------------------

-- | Property that checks that the fees are non-decreasing
feesNonDecreasing :: Property
feesNonDecreasing = withTests (fromIntegral numberOfTests) . property $ do
  tr <- fmap sourceSignalTargets $ forAll (trace @UTXO traceLen `ofLengthAtLeast` 1)

  assertAll feesDoNotIncrease tr

  where feesDoNotIncrease (SourceSignalTarget
                            { source = UTxOState _ _ fees _
                            , target = UTxOState _ _ fees' _}) =
          fees <= fees'
