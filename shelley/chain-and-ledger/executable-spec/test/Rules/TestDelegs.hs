{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestDelegs where

import           Data.Word (Word64)

import           Hedgehog (Property, TestLimit, forAll, property, withTests)

import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (pattern SourceSignalTarget, source,
                     sourceSignalTargets, target, _traceEnv)

import           Coin (pattern Coin)
import           LedgerState (_dstate, _rewards)
import           MockTypes (DELEGS)
import           STS.Delegs (pattern DelegsEnv)
import           TxData (_body, _wdrls)

import           Test.Utils (assertAll, testGlobals)

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: TestLimit
numberOfTests = 300

traceLen :: Word64
traceLen = 100

---------------------------
-- Properties for DELEGS --
---------------------------

-- | Check that the rewards pot decreases by the sum of withdrawals in the
-- transaction.
rewardsDecreasesByWithdrawals :: Property
rewardsDecreasesByWithdrawals = withTests numberOfTests . property $ do
  t <- forAll $ trace @DELEGS testGlobals traceLen `ofLengthAtLeast` 1

  let DelegsEnv _ _ _ tx _ = _traceEnv t
      tr = sourceSignalTargets t

  assertAll (rewardsPotdecreases $ _body tx) tr

  where rewardsPotdecreases tx (SourceSignalTarget
                              { source = d
                              , target = d'}) =
          let wdrls = _wdrls tx
              rewards  = (_rewards . _dstate) d
              rewards' = (_rewards . _dstate) d'
              rewardsSum = foldl (+) (Coin 0) rewards
              rewardsSum' = foldl (+) (Coin 0) rewards'
              wdrlSum = foldl (+) (Coin 0) wdrls
          in
             rewardsSum >= rewardsSum'
          && wdrlSum >= Coin 0
          && rewardsSum == wdrlSum + rewardsSum'
