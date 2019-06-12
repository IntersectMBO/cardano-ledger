{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Slotting.CBOR
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)

import Cardano.Chain.Slotting
  (EpochSlots(..), SlotNumber, LocalSlotIndex(..))

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestCBOR, roundTripsCBORBuildable)
import Test.Cardano.Chain.Slotting.Example
  (exampleEpochIndex, exampleSlotId)
import Test.Cardano.Chain.Slotting.Gen
  ( feedPMEpochSlots
  , genEpochIndex
  , genSlotNumber
  , genSlotId
  , genLocalSlotIndex
  , genEpochSlots
  )
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- EpochIndex
--------------------------------------------------------------------------------
golden_EpochIndex :: Property
golden_EpochIndex =
  goldenTestCBOR exampleEpochIndex "test/golden/cbor/slotting/EpochIndex"

ts_roundTripEpochIndexCBOR :: TSProperty
ts_roundTripEpochIndexCBOR = eachOfTS 1000 genEpochIndex roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- SlotNumber
--------------------------------------------------------------------------------
golden_SlotNumber :: Property
golden_SlotNumber = goldenTestCBOR fsi "test/golden/cbor/slotting/SlotNumber"
  where fsi = 5001 :: SlotNumber

ts_roundTripSlotNumberCBOR :: TSProperty
ts_roundTripSlotNumberCBOR = eachOfTS 1000 genSlotNumber roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- LocalSlotIndex
--------------------------------------------------------------------------------
golden_LocalSlotIndex :: Property
golden_LocalSlotIndex =
  goldenTestCBOR lsi "test/golden/cbor/slotting/LocalSlotIndex"
 where
  lsi = UnsafeLocalSlotIndex 52

ts_roundTripLocalSlotIndexCBOR :: TSProperty
ts_roundTripLocalSlotIndexCBOR = eachOfTS 1000 gen roundTripsCBORBuildable
 where
  gen = feedPMEpochSlots (\_pm es -> genLocalSlotIndex es)

--------------------------------------------------------------------------------
-- EpochSlots
--------------------------------------------------------------------------------
golden_EpochSlots :: Property
golden_EpochSlots = goldenTestCBOR sc "test/golden/cbor/slotting/EpochSlots"
  where sc = EpochSlots 474747

ts_roundTripEpochSlotsCBOR :: TSProperty
ts_roundTripEpochSlotsCBOR = eachOfTS 1000 genEpochSlots roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- SlotId
--------------------------------------------------------------------------------
golden_SlotId :: Property
golden_SlotId = goldenTestCBOR (exampleSlotId (EpochSlots 777))
                               "test/golden/cbor/slotting/SlotId"

ts_roundTripSlotIdCBOR :: TSProperty
ts_roundTripSlotIdCBOR = eachOfTS 1000 gen roundTripsCBORBuildable
 where
  gen = feedPMEpochSlots (\_pm es -> genSlotId es)

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
