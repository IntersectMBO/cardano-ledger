{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Txp.Compact
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude
  ( discoverPropArg
  , discoverRoundTripArg
  )

import Hedgehog
  ( MonadTest
  , assert
  , property
  , forAll
  , property
  , tripping
  )
import qualified Hedgehog as H

import Cardano.Chain.Txp
  ( fromCompactTxId
  , toCompactTxId
  , fromCompactTxIn
  , toCompactTxIn
  , fromCompactTxOut
  , toCompactTxOut
  )

import Test.Cardano.Chain.Txp.Gen
  ( genTxId
  , genTxIn
  , genTxOut
  )
import Test.Options
  (TestScenario, TSProperty, eachOfTS, withTestsTS)

--------------------------------------------------------------------------------
-- Compact TxIn
--------------------------------------------------------------------------------

ts_roundTripCompactTxIn :: TSProperty
ts_roundTripCompactTxIn =
  eachOfTS 1000 genTxIn (trippingCompact toCompactTxIn fromCompactTxIn)

ts_prop_heapWordsSavingsCompactTxIn :: TSProperty
ts_prop_heapWordsSavingsCompactTxIn = withTestsTS 1000 $ property $ do
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
ts_prop_heapWordsSavingsCompactTxId = withTestsTS 1000 $ property $ do
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
ts_prop_heapWordsSavingsCompactTxOut = withTestsTS 1000 $ property $ do
  txOut <- forAll genTxOut
  let compactTxOut = toCompactTxOut txOut
  assert $ heapWords compactTxOut < heapWords txOut

-------------------------------------------------------------------------------
-- Tripping util
-------------------------------------------------------------------------------

trippingCompact
  :: (HasCallStack, MonadTest m, Show a, Show b, Eq a)
  => (a -> b) -> (b -> a) -> a -> m ()
trippingCompact toCompact fromCompact x =
  tripping x toCompact (Identity . fromCompact)

-------------------------------------------------------------------------------
-- Main test export
-------------------------------------------------------------------------------

tests :: TestScenario -> IO Bool
tests ts =
  (&&) <$> H.checkParallel (($$discoverPropArg :: TestScenario -> H.Group) ts)
       <*> H.checkParallel (($$discoverRoundTripArg :: TestScenario -> H.Group) ts)
