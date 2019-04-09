{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Slotting.Json
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)
import qualified Hedgehog as H

import Cardano.Chain.Slotting
  (EpochSlots(..), FlatSlotId (..), LocalSlotIndex (..))

import Test.Cardano.Chain.Slotting.Example
  (exampleEpochIndex)
import Test.Cardano.Chain.Slotting.Gen
  ( genEpochIndex
  , genFlatSlotId
  , genLocalSlotIndex
  , genEpochSlots
  , feedPMEpochSlots
  )
import Test.Options (TestScenario, TSProperty, eachOfTS)


--------------------------------------------------------------------------------
-- EpochIndex
--------------------------------------------------------------------------------

golden_EpochIndex :: Property
golden_EpochIndex = goldenTestJSON exampleEpochIndex "test/golden/json/slotting/EpochIndex"

ts_roundTripEpochIndex :: TSProperty
ts_roundTripEpochIndex = eachOfTS 1000 genEpochIndex roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- FlatSlotId
--------------------------------------------------------------------------------

golden_FlatSlotId :: Property
golden_FlatSlotId = goldenTestJSON fsi "test/golden/json/slotting/FlatSlotId"
  where fsi = 5001 :: FlatSlotId

ts_roundTripFlatSlotId :: TSProperty
ts_roundTripFlatSlotId = eachOfTS 1000 genFlatSlotId roundTripsAesonShow

--------------------------------------------------------------------------------
-- LocalSlotIndex
--------------------------------------------------------------------------------

golden_LocalSlotIndex :: Property
golden_LocalSlotIndex = goldenTestJSON lsi "test/golden/json/slotting/LocalSlotIndex"
  where lsi = UnsafeLocalSlotIndex 52

ts_roundTripLocalSlotIndex :: TSProperty
ts_roundTripLocalSlotIndex = eachOfTS 1000 gen roundTripsAesonBuildable
 where
  gen = feedPMEpochSlots (\_pm es -> genLocalSlotIndex es)

--------------------------------------------------------------------------------
-- EpochSlots
--------------------------------------------------------------------------------

golden_EpochSlots :: Property
golden_EpochSlots = goldenTestJSON es "test/golden/json/slotting/EpochSlots"
  where es = EpochSlots 77

ts_roundTripEpochSlots :: TSProperty
ts_roundTripEpochSlots = eachOfTS 1000 genEpochSlots roundTripsAesonShow


--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------
tests :: TestScenario -> IO Bool
tests ts =
  (&&) <$> H.checkSequential $$discoverGolden
       <*> H.checkParallel (($$discoverRoundTripArg :: TestScenario -> H.Group) ts)
