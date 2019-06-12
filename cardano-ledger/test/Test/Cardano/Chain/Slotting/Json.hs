{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Slotting.Json
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)

import Cardano.Chain.Slotting
  (EpochSlots(..), SlotNumber (..), LocalSlotIndex (..))

import Test.Cardano.Chain.Slotting.Example
  (exampleEpochNumber)
import Test.Cardano.Chain.Slotting.Gen
  ( genEpochNumber
  , genSlotNumber
  , genLocalSlotIndex
  , genEpochSlots
  , feedPMEpochSlots
  )
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- EpochNumber
--------------------------------------------------------------------------------

golden_EpochNumber :: Property
golden_EpochNumber = goldenTestJSON exampleEpochNumber "test/golden/json/slotting/EpochNumber"

ts_roundTripEpochNumber :: TSProperty
ts_roundTripEpochNumber = eachOfTS 1000 genEpochNumber roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SlotNumber
--------------------------------------------------------------------------------

golden_SlotNumber :: Property
golden_SlotNumber = goldenTestJSON fsi "test/golden/json/slotting/SlotNumber"
  where fsi = 5001 :: SlotNumber

ts_roundTripSlotNumber :: TSProperty
ts_roundTripSlotNumber = eachOfTS 1000 genSlotNumber roundTripsAesonShow

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

tests :: TSGroup
tests = concatTSGroups [const $$discoverGolden, $$discoverRoundTripArg]
