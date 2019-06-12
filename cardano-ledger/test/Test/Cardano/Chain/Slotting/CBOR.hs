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
  (exampleEpochNumber, exampleEpochAndSlotCount)
import Test.Cardano.Chain.Slotting.Gen
  ( feedPMEpochSlots
  , genEpochNumber
  , genSlotNumber
  , genEpochAndSlotCount
  , genLocalSlotIndex
  , genEpochSlots
  )
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- EpochNumber
--------------------------------------------------------------------------------
golden_EpochNumber :: Property
golden_EpochNumber =
  goldenTestCBOR exampleEpochNumber "test/golden/cbor/slotting/EpochNumber"

ts_roundTripEpochNumberCBOR :: TSProperty
ts_roundTripEpochNumberCBOR = eachOfTS 1000 genEpochNumber roundTripsCBORBuildable

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
-- EpochAndSlotCount
--------------------------------------------------------------------------------
golden_EpochAndSlotCount :: Property
golden_EpochAndSlotCount = goldenTestCBOR
  exampleEpochAndSlotCount
  "test/golden/cbor/slotting/EpochAndSlotCount"

ts_roundTripEpochAndSlotCountCBOR :: TSProperty
ts_roundTripEpochAndSlotCountCBOR = eachOfTS 1000 gen roundTripsCBORBuildable
 where
  gen = feedPMEpochSlots (\_pm es -> genEpochAndSlotCount es)

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
