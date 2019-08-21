{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.MempoolPayload.CBOR
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestCBOR, roundTripsCBORShow)
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
goldenMempoolPayload = goldenTestCBOR
  exampleMempoolPayload
  "test/golden/cbor/mempoolpayload/MempoolPayload"

goldenMempoolPayload1 :: Property
goldenMempoolPayload1 = goldenTestCBOR
  exampleMempoolPayload1
  "test/golden/cbor/mempoolpayload/MempoolPayload1"

goldenMempoolPayload2 :: Property
goldenMempoolPayload2 = goldenTestCBOR
  exampleMempoolPayload2
  "test/golden/cbor/mempoolpayload/MempoolPayload2"

goldenMempoolPayload3 :: Property
goldenMempoolPayload3 = goldenTestCBOR
  exampleMempoolPayload3
  "test/golden/cbor/mempoolpayload/MempoolPayload3"

ts_roundTripMempoolPayload :: TSProperty
ts_roundTripMempoolPayload =
  eachOfTS 200 (feedPM genMempoolPayload) roundTripsCBORShow

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
