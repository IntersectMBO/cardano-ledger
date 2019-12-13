{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestDelegs where

import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     source, target)

import           Test.QuickCheck (Property, conjoin)

import           Coin (pattern Coin)
import           LedgerState (_dstate, _rewards)
import           MockTypes (DELEGS, Wdrl)

---------------------------
-- Properties for DELEGS --
---------------------------

-- | Check that the rewards pot decreases by the sum of withdrawals in the
-- transaction.
rewardsDecreasesByWithdrawals
  :: [(Wdrl, SourceSignalTarget DELEGS)]
  -> Property
rewardsDecreasesByWithdrawals tr =
  conjoin $
    map (uncurry rewardsPotdecreases) tr
  where
    rewardsPotdecreases wdrls SourceSignalTarget
                                { source = d
                                , target = d'} =
      let rewards  = (_rewards . _dstate) d
          rewards' = (_rewards . _dstate) d'
          rewardsSum = foldl (+) (Coin 0) rewards
          rewardsSum' = foldl (+) (Coin 0) rewards'
          wdrlSum = foldl (+) (Coin 0) wdrls
      in
         rewardsSum >= rewardsSum'
      && wdrlSum >= Coin 0
      && rewardsSum == wdrlSum + rewardsSum'
