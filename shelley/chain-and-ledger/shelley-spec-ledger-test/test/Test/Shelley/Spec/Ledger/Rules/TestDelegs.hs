{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Shelley.Spec.Ledger.Rules.TestDelegs where

import Control.State.Transition.Trace
  ( SourceSignalTarget,
    source,
    target,
    pattern SourceSignalTarget,
  )
import Data.List (foldl')
import Shelley.Spec.Ledger.API (DELEGS)
import Shelley.Spec.Ledger.Coin (pattern Coin)
import Shelley.Spec.Ledger.LedgerState (_dstate, _rewards)
import qualified Shelley.Spec.Ledger.TxBody as T
import Test.QuickCheck (Property, conjoin)

---------------------------
-- Properties for DELEGS --
---------------------------

-- | Check that the rewards pot decreases by the sum of withdrawals in the
-- transaction.
rewardsDecreasesByWithdrawals :: forall era.
  [(T.Wdrl era, SourceSignalTarget (DELEGS era))] ->
  Property
rewardsDecreasesByWithdrawals tr =
  conjoin $
    map (uncurry rewardsPotdecreases) tr
  where
    rewardsPotdecreases
      (T.Wdrl wdrls)
      SourceSignalTarget
        { source = d,
          target = d'
        } =
        let rewards = (_rewards . _dstate) d
            rewards' = (_rewards . _dstate) d'
            rewardsSum = foldl' (<>) (Coin 0) rewards
            rewardsSum' = foldl' (<>) (Coin 0) rewards'
            wdrlSum = foldl' (<>) (Coin 0) wdrls
         in rewardsSum >= rewardsSum'
              && wdrlSum >= Coin 0
              && rewardsSum == wdrlSum <> rewardsSum'
