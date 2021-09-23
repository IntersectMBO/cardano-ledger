{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Cardano.Chain.Common.Compact
  ( tests,
  )
where

import Cardano.Chain.Common (fromCompactAddress, toCompactAddress)
import Cardano.Prelude
import Hedgehog (MonadTest, assert, forAll, property, tripping)
import Test.Cardano.Chain.Common.Gen (genAddress)
import Test.Cardano.Prelude (discoverPropArg, discoverRoundTripArg)
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS, withTestsTS)

--------------------------------------------------------------------------------
-- Compact Address
--------------------------------------------------------------------------------

ts_roundTripCompactAddress :: TSProperty
ts_roundTripCompactAddress =
  eachOfTS 1000 genAddress (trippingCompact toCompactAddress fromCompactAddress)

ts_prop_heapWordsSavingsCompactAddress :: TSProperty
ts_prop_heapWordsSavingsCompactAddress = withTestsTS 1000 $
  property $ do
    addr <- forAll genAddress
    let compactAddr = toCompactAddress addr
    assert $ heapWords compactAddr < heapWords addr

--------------------------------------------------------------------------------
-- Tripping util
--------------------------------------------------------------------------------

trippingCompact ::
  (HasCallStack, MonadTest m, Show a, Show b, Eq a) =>
  (a -> b) ->
  (b -> a) ->
  a ->
  m ()
trippingCompact toCompact fromCompact x =
  tripping x toCompact (Identity . fromCompact)

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [$$discoverPropArg, $$discoverRoundTripArg]
