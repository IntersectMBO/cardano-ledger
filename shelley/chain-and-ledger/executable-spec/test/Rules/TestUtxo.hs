{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestUtxo
  ( feesNonDecreasing
  , potsSumIncreaseWdrls)
where

import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     signal, source, target)
import           Test.QuickCheck (Property, conjoin)

import           Coin (pattern Coin)
import           Tx (pattern Tx, _body)
import           TxData (Wdrl (..), _wdrls)
import           UTxO (balance)

import           ConcreteCryptoTypes (UTXO)
import           LedgerState (pattern UTxOState, _deposited, _fees, _utxo)

--------------------------
-- Properties for UTXOW --
--------------------------

-- | Property that checks that the fees are non-decreasing
feesNonDecreasing
  :: [SourceSignalTarget UTXO]
  -> Property
feesNonDecreasing ssts =
  conjoin $
    map feesDoNotIncrease ssts

  where
    feesDoNotIncrease SourceSignalTarget
                        { source = UTxOState { _fees = fees }
                        , target = UTxOState { _fees = fees' }} =
      fees <= fees'

-- | Property that checks that the sum of the pots circulation, deposits and
-- fees increases by the sum of withdrawals of a transaction.
potsSumIncreaseWdrls
  :: [SourceSignalTarget UTXO]
  -> Property
potsSumIncreaseWdrls ssts =
  conjoin $
    map potsIncreaseWithWdrlsSum ssts

  where
    potsIncreaseWithWdrlsSum SourceSignalTarget
                               { source = UTxOState { _utxo = u
                                                    , _deposited = d
                                                    , _fees = fees}
                               , target = UTxOState { _utxo = u'
                                                    , _deposited = d'
                                                    , _fees = fees'}
                               , signal = Tx { _body = txbody }} =
      let circulation  = balance u
          circulation' = balance u'
          withdrawals  = foldl (+) (Coin 0) $ unWdrl $ _wdrls txbody
      in
         withdrawals >= Coin 0
      && circulation' + d' + fees' == circulation + d + fees + withdrawals
