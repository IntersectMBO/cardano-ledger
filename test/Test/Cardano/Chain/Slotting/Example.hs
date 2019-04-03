{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Slotting.Example
  ( exampleEpochIndex
  , exampleSlotId
  , exampleFlatSlotId
  )
where

import Cardano.Prelude

import Cardano.Chain.Slotting
  ( EpochIndex(..)
  , EpochSlots(..)
  , SlotId(..)
  , FlatSlotId
  , mkLocalSlotIndex
  , flattenSlotId
  )


exampleEpochIndex :: EpochIndex
exampleEpochIndex = EpochIndex 14

exampleFlatSlotId :: EpochSlots -> FlatSlotId
exampleFlatSlotId es =
  flattenSlotId es (exampleSlotId es)

exampleSlotId :: EpochSlots -> SlotId
exampleSlotId es = SlotId (EpochIndex 11) lsi
  where Right lsi = mkLocalSlotIndex es 47
