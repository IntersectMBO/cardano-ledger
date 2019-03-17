{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Slotting.Example
  ( exampleEpochIndex
  , exampleSlotId
  , exampleSlottingData
  )
where

import Cardano.Prelude

import qualified Data.Map.Strict as M

import Cardano.Chain.Slotting
  ( EpochIndex(..)
  , EpochSlottingData(..)
  , EpochSlots(..)
  , SlotId(..)
  , SlottingData
  , mkLocalSlotIndex
  , mkSlottingData
  )


exampleEpochIndex :: EpochIndex
exampleEpochIndex = EpochIndex 14

exampleEpochSlottingData :: EpochSlottingData
exampleEpochSlottingData =
  EpochSlottingData {esdSlotDuration = 100e-6, esdStartDiff = 100e-6}

exampleSlotId :: SlotId
exampleSlotId = SlotId (EpochIndex 11) lsi
  where Right lsi = mkLocalSlotIndex (EpochSlots 50) 47

exampleSlottingData :: SlottingData
exampleSlottingData = slottingData
 where
  Right slottingData =
    mkSlottingData
      $   M.fromList
      $   (,)
      <$> [0 .. 9]
      <*> pure exampleEpochSlottingData
