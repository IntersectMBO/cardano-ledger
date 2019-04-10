{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Slotting.CBOR
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog
  (Group, Property)
import qualified Hedgehog as H

import Cardano.Chain.Slotting
  (EpochSlots(..), FlatSlotId, LocalSlotIndex(..))

import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestCBOR, roundTripsCBORBuildable)
import Test.Cardano.Chain.Slotting.Example
  (exampleEpochIndex, exampleSlotId)
import Test.Cardano.Chain.Slotting.Gen
  ( feedPMEpochSlots
  , genEpochIndex
  , genFlatSlotId
  , genSlotId
  , genLocalSlotIndex
  , genEpochSlots
  )
import Test.Options (TestScenario, TSProperty, eachOfTS)


--------------------------------------------------------------------------------
-- EpochIndex
--------------------------------------------------------------------------------
golden_EpochIndex :: Property
golden_EpochIndex =
  goldenTestCBOR exampleEpochIndex "test/golden/bi/slotting/EpochIndex"

ts_roundTripEpochIndexBi :: TSProperty
ts_roundTripEpochIndexBi = eachOfTS 1000 genEpochIndex roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- FlatSlotId
--------------------------------------------------------------------------------
golden_FlatSlotId :: Property
golden_FlatSlotId = goldenTestCBOR fsi "test/golden/bi/slotting/FlatSlotId"
  where fsi = 5001 :: FlatSlotId

ts_roundTripFlatSlotIdBi :: TSProperty
ts_roundTripFlatSlotIdBi = eachOfTS 1000 genFlatSlotId roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- LocalSlotIndex
--------------------------------------------------------------------------------
golden_LocalSlotIndex :: Property
golden_LocalSlotIndex =
  goldenTestCBOR lsi "test/golden/bi/slotting/LocalSlotIndex"
 where
  lsi = UnsafeLocalSlotIndex 52

ts_roundTripLocalSlotIndexBi :: TSProperty
ts_roundTripLocalSlotIndexBi = eachOfTS 1000 gen roundTripsCBORBuildable
 where
  gen = feedPMEpochSlots (\_pm es -> genLocalSlotIndex es)

--------------------------------------------------------------------------------
-- EpochSlots
--------------------------------------------------------------------------------
golden_EpochSlots :: Property
golden_EpochSlots = goldenTestCBOR sc "test/golden/bi/slotting/EpochSlots"
  where sc = EpochSlots 474747

ts_roundTripEpochSlotsBi :: TSProperty
ts_roundTripEpochSlotsBi = eachOfTS 1000 genEpochSlots roundTripsCBORBuildable

--------------------------------------------------------------------------------
-- SlotId
--------------------------------------------------------------------------------
golden_SlotId :: Property
golden_SlotId = goldenTestCBOR (exampleSlotId (EpochSlots 777))
                               "test/golden/bi/slotting/SlotId"

ts_roundTripSlotIdBi :: TSProperty
ts_roundTripSlotIdBi = eachOfTS 1000 gen roundTripsCBORBuildable
 where
  gen = feedPMEpochSlots (\_pm es -> genSlotId es)

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------
tests :: TestScenario -> IO Bool
tests ts =
  (&&) <$> H.checkSequential $$discoverGolden
       <*> H.checkParallel (($$discoverRoundTripArg :: TestScenario -> Group) ts)
