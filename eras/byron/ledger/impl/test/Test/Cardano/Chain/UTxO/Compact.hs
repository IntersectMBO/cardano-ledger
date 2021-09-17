{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.UTxO.Compact
  ( tests,
  )
where

import Cardano.Chain.UTxO
  ( fromCompactTxId,
    fromCompactTxIn,
    fromCompactTxOut,
    toCompactTxId,
    toCompactTxIn,
    toCompactTxOut,
  )
import Cardano.Prelude
import Hedgehog (MonadTest, assert, forAll, property, tripping)
import Test.Cardano.Chain.UTxO.Gen (genTxId, genTxIn, genTxOut)
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS, withTestsTS)

--------------------------------------------------------------------------------
-- Compact TxIn
--------------------------------------------------------------------------------

ts_roundTripCompactTxIn :: TSProperty
ts_roundTripCompactTxIn =
  eachOfTS 1000 genTxIn (trippingCompact toCompactTxIn fromCompactTxIn)

ts_prop_heapWordsSavingsCompactTxIn :: TSProperty
ts_prop_heapWordsSavingsCompactTxIn = withTestsTS 1000 $
  property $ do
    txIn <- forAll genTxIn
    let compactTxIn = toCompactTxIn txIn
    assert $ heapWords compactTxIn < heapWords txIn

--------------------------------------------------------------------------------
-- Compact TxId
--------------------------------------------------------------------------------

ts_roundTripCompactTxId :: TSProperty
ts_roundTripCompactTxId =
  eachOfTS 1000 genTxId (trippingCompact toCompactTxId fromCompactTxId)

ts_prop_heapWordsSavingsCompactTxId :: TSProperty
ts_prop_heapWordsSavingsCompactTxId = withTestsTS 1000 $
  property $ do
    txId <- forAll genTxId
    let compactTxId = toCompactTxId txId
    assert $ heapWords compactTxId < heapWords txId

--------------------------------------------------------------------------------
-- Compact TxOut
--------------------------------------------------------------------------------

ts_roundTripCompactTxOut :: TSProperty
ts_roundTripCompactTxOut =
  eachOfTS 1000 genTxOut (trippingCompact toCompactTxOut fromCompactTxOut)

ts_prop_heapWordsSavingsCompactTxOut :: TSProperty
ts_prop_heapWordsSavingsCompactTxOut = withTestsTS 1000 $
  property $ do
    txOut <- forAll genTxOut
    let compactTxOut = toCompactTxOut txOut
    assert $ heapWords compactTxOut < heapWords txOut

-------------------------------------------------------------------------------
-- Tripping util
-------------------------------------------------------------------------------

trippingCompact ::
  (HasCallStack, MonadTest m, Show a, Show b, Eq a) =>
  (a -> b) ->
  (b -> a) ->
  a ->
  m ()
trippingCompact toCompact fromCompact x =
  tripping x toCompact (Identity . fromCompact)

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [$$discoverPropArg, $$discoverRoundTripArg]
