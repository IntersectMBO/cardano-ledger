{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Shelley.Spec.Ledger.Rules.TestUtxo
  ( feesNonDecreasing,
  )
where

import Control.State.Transition.Trace
  ( SourceSignalTarget,
    source,
    target,
    pattern SourceSignalTarget,
  )
import Shelley.Spec.Ledger.API (UTXO)
import Shelley.Spec.Ledger.LedgerState (_fees, pattern UTxOState)
import Test.QuickCheck (Property, conjoin)

--------------------------
-- Properties for UTXOW --
--------------------------

-- | Property that checks that the fees are non-decreasing
feesNonDecreasing :: forall era.
  [SourceSignalTarget (UTXO era)] ->
  Property
feesNonDecreasing ssts =
  conjoin $
    map feesDoNotIncrease ssts
  where
    feesDoNotIncrease
      SourceSignalTarget
        { source = UTxOState {_fees = fees},
          target = UTxOState {_fees = fees'}
        } =
        fees <= fees'
