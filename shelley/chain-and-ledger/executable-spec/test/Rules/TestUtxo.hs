{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestUtxo where

import           Data.Word (Word64)

import           Hedgehog (Property, forAll, property, withTests)

import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (pattern SourceSignalTarget, signal, source,
                     sourceSignalTargets, target)

import           Coin (pattern Coin)
import           TxData (pattern Tx, _body, _wdrls)
import           UTxO (balance)

import           LedgerState (pattern UTxOState, _deposited, _fees, _utxo)
import           MockTypes (UTXO)

import           Test.Utils (assertAll, testGlobals)

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
  tr <- fmap sourceSignalTargets
        $ forAll (trace @UTXO testGlobals traceLen `ofLengthAtLeast` 1)

  assertAll feesDoNotIncrease tr

  where feesDoNotIncrease (SourceSignalTarget
                            { source = UTxOState { _fees = fees }
                            , target = UTxOState { _fees = fees' }}) =
          fees <= fees'

-- | Property that checks that the sum of the pots circulation, deposits and
-- fees increases by the sum of withdrawals of a transaction.
potsSumIncreaseWdrls :: Property
potsSumIncreaseWdrls = withTests (fromIntegral numberOfTests) . property $ do
  tr <-
    fmap sourceSignalTargets
      $ forAll (trace @UTXO testGlobals traceLen `ofLengthAtLeast` 1)

  assertAll potsIncreaseWithWdrlsSum tr

  where potsIncreaseWithWdrlsSum (SourceSignalTarget
                                   { source = UTxOState { _utxo = u
                                                        , _deposited = d
                                                        , _fees = fees}
                                   , target = UTxOState { _utxo = u'
                                                        , _deposited = d'
                                                        , _fees = fees'}
                                   , signal = Tx { _body = txbody }}) =
          let circulation  = balance u
              circulation' = balance u'
              withdrawals  = foldl (+) (Coin 0) $ _wdrls txbody
          in
             withdrawals >= Coin 0
          && circulation' + d' + fees' == circulation + d + fees + withdrawals
