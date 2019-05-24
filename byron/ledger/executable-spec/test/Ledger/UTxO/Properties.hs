{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ledger.UTxO.Properties where

import Hedgehog (Property, PropertyT, (===), forAll, property, classify, success, withTests)

import Control.State.Transition.Generator (trace)
import Control.State.Transition.Trace (firstAndLastState, traceLength, Trace)

import Cardano.Ledger.Spec.STS.UTXO (reserves, utxo)
import Cardano.Ledger.Spec.STS.UTXOW (UTXOW)
import Ledger.UTxO (balance, TxId)


-- | Check that the money is constant in the system.
moneyIsConstant :: Property
moneyIsConstant = property $ do
  (st0, st) <- firstAndLastState <$> forAll (trace @(UTXOW TxId) 100)
  reserves st0 + balance (utxo st0) === reserves st + balance (utxo st)

-- | Classify the traces.
prop_classifyTraces :: Property
prop_classifyTraces = withTests 100 . property $ do
  tr <- forAll (trace @(UTXOW TxId) 500)
  classifyTraces tr
  success

classifyTraces :: Trace s -> PropertyT IO ()
classifyTraces tr = do
  classify "empty"      $ traceLength tr == 0
  classify "singleton"  $ traceLength tr == 1
  classify "[2, 100)"   $   2 <= traceLength tr && traceLength tr < 100
  classify "[100, 400)" $ 100 <= traceLength tr && traceLength tr < 400
  classify "[400, 500)" $ 400 <= traceLength tr && traceLength tr < 500
  classify "500"        $ traceLength tr == 500
