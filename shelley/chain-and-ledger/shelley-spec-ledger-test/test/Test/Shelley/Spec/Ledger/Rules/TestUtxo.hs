{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Shelley.Spec.Ledger.Rules.TestUtxo
  ( feesNonDecreasing,
    potsSumIncreaseWdrls,
  )
where

import Cardano.Ledger.Era (Era)
import Control.State.Transition.Trace
  ( SourceSignalTarget,
    signal,
    source,
    target,
    pattern SourceSignalTarget,
  )
import Data.Foldable (fold)
import Shelley.Spec.Ledger.API (UTXO)
import Shelley.Spec.Ledger.Coin (pattern Coin)
import Shelley.Spec.Ledger.LedgerState (_deposited, _fees, _utxo, pattern UTxOState)
import Shelley.Spec.Ledger.Tx (_body, pattern Tx)
import Shelley.Spec.Ledger.TxData (Wdrl (..), _wdrls)
import Shelley.Spec.Ledger.UTxO (balance)
import Test.QuickCheck (Property, conjoin)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)

--------------------------
-- Properties for UTXOW --
--------------------------

-- | Property that checks that the fees are non-decreasing
feesNonDecreasing ::
  [SourceSignalTarget (UTXO C)] ->
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

-- | Property that checks that the sum of the pots circulation, deposits and
-- fees increases by the sum of withdrawals of a transaction.
potsSumIncreaseWdrls ::
  Era era =>
  [SourceSignalTarget (UTXO era)] ->
  Property
potsSumIncreaseWdrls ssts =
  conjoin $
    map potsIncreaseWithWdrlsSum ssts
  where
    potsIncreaseWithWdrlsSum
      SourceSignalTarget
        { source =
            UTxOState
              { _utxo = u,
                _deposited = d,
                _fees = fees
              },
          target =
            UTxOState
              { _utxo = u',
                _deposited = d',
                _fees = fees'
              },
          signal = Tx {_body = txbody}
        } =
        let circulation = balance u
            circulation' = balance u'
            withdrawals = fold $ unWdrl $ _wdrls txbody
         in withdrawals >= Coin 0
              && circulation' <> d' <> fees' == circulation <> d <> fees <> withdrawals
