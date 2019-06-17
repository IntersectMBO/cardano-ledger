{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Slotting.Json
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)

import Cardano.Chain.Slotting (EpochSlots(..))

import Test.Cardano.Chain.Slotting.Example (exampleEpochNumber)
import Test.Cardano.Chain.Slotting.Gen
  ( genEpochNumber
  , genEpochSlots
  )
import Test.Options (TSGroup, TSProperty, concatTSGroups, eachOfTS)


--------------------------------------------------------------------------------
-- EpochNumber
--------------------------------------------------------------------------------

golden_EpochNumber :: Property
golden_EpochNumber =
  goldenTestJSON exampleEpochNumber "test/golden/json/slotting/EpochNumber"

ts_roundTripEpochNumber :: TSProperty
ts_roundTripEpochNumber = eachOfTS 1000 genEpochNumber roundTripsAesonBuildable

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
