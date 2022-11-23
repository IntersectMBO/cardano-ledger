{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Slotting.CBOR
  ( tests,
  )
where

import Cardano.Chain.Slotting (EpochSlots (..), SlotNumber)
import Cardano.Prelude
import GetDataFileName ((<:<))
import Hedgehog (Property)
import Test.Cardano.Chain.Slotting.Example
  ( exampleEpochAndSlotCount,
    exampleEpochNumber,
  )
import Test.Cardano.Chain.Slotting.Gen
  ( feedPMEpochSlots,
    genEpochAndSlotCount,
    genEpochNumber,
    genEpochSlots,
    genSlotNumber,
  )
import Test.Cardano.Ledger.Binary.Vintage.Helpers.GoldenRoundTrip
  ( goldenTestCBOR,
    roundTripsCBORBuildable,
  )
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)

--------------------------------------------------------------------------------
-- EpochNumber
--------------------------------------------------------------------------------
golden_EpochNumber :: Property
golden_EpochNumber =
  goldenTestCBOR exampleEpochNumber <:< "golden/cbor/slotting/EpochNumber"

ts_roundTripEpochNumberCBOR :: TSProperty
ts_roundTripEpochNumberCBOR = eachOfTS 1000 genEpochNumber roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- SlotNumber
--------------------------------------------------------------------------------
golden_SlotNumber :: Property
golden_SlotNumber = goldenTestCBOR fsi <:< "golden/cbor/slotting/SlotNumber"
  where
    fsi = 5001 :: SlotNumber

ts_roundTripSlotNumberCBOR :: TSProperty
ts_roundTripSlotNumberCBOR = eachOfTS 1000 genSlotNumber roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- EpochSlots
--------------------------------------------------------------------------------
golden_EpochSlots :: Property
golden_EpochSlots = goldenTestCBOR sc <:< "golden/cbor/slotting/EpochSlots"
  where
    sc = EpochSlots 474747

ts_roundTripEpochSlotsCBOR :: TSProperty
ts_roundTripEpochSlotsCBOR = eachOfTS 1000 genEpochSlots roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- EpochAndSlotCount
--------------------------------------------------------------------------------
golden_EpochAndSlotCount :: Property
golden_EpochAndSlotCount =
  goldenTestCBOR
    exampleEpochAndSlotCount
    <:< "golden/cbor/slotting/EpochAndSlotCount"

ts_roundTripEpochAndSlotCountCBOR :: TSProperty
ts_roundTripEpochAndSlotCountCBOR = eachOfTS 1000 gen roundTripsCBORBuildable
  where
    gen = feedPMEpochSlots (\_pm es -> genEpochAndSlotCount es)

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
