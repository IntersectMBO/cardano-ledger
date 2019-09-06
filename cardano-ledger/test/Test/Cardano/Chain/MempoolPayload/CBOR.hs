{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Cardano.Chain.MempoolPayload.CBOR
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestCBORAnnotated, roundTripsCBORAnnotatedShow)
import Test.Cardano.Chain.MempoolPayload.Example
    ( exampleMempoolPayload
    , exampleMempoolPayload1
    , exampleMempoolPayload2
    , exampleMempoolPayload3
    )
import Test.Cardano.Chain.MempoolPayload.Gen (genMempoolPayload)
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- MempoolPayload
--------------------------------------------------------------------------------

goldenMempoolPayload :: Property
goldenMempoolPayload = goldenTestCBORAnnotated
  exampleMempoolPayload
  "test/golden/cbor/mempoolpayload/MempoolPayload"

goldenMempoolPayloadFilledIn :: Property
goldenMempoolPayloadFilledIn = goldenTestCBORAnnotated
  exampleMempoolPayload
  "test/golden/cbor/mempoolpayload/MempoolPayload"

goldenMempoolPayload1 :: Property
goldenMempoolPayload1 = goldenTestCBORAnnotated
  exampleMempoolPayload1
  "test/golden/cbor/mempoolpayload/MempoolPayload1"

goldenMempoolPayload1FilledIn :: Property
goldenMempoolPayload1FilledIn = goldenTestCBORAnnotated
  exampleMempoolPayload1
  "test/golden/cbor/mempoolpayload/MempoolPayload1"

goldenMempoolPayload2 :: Property
goldenMempoolPayload2 = goldenTestCBORAnnotated
  exampleMempoolPayload2
  "test/golden/cbor/mempoolpayload/MempoolPayload2"

goldenMempoolPayload2FilledIn :: Property
goldenMempoolPayload2FilledIn = goldenTestCBORAnnotated
  exampleMempoolPayload2
  "test/golden/cbor/mempoolpayload/MempoolPayload2"

goldenMempoolPayload3 :: Property
goldenMempoolPayload3 = goldenTestCBORAnnotated
  exampleMempoolPayload3
  "test/golden/cbor/mempoolpayload/MempoolPayload3"

goldenMempoolPayload3FilledIn :: Property
goldenMempoolPayload3FilledIn = goldenTestCBORAnnotated
  exampleMempoolPayload3
  "test/golden/cbor/mempoolpayload/MempoolPayload3"

ts_roundTripMempoolPayload :: TSProperty
ts_roundTripMempoolPayload =
  eachOfTS 200 (feedPM genMempoolPayload) roundTripsCBORAnnotatedShow

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
