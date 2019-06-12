{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Slotting.Example
  ( exampleEpochIndex
  , exampleSlotId
  , exampleSlotNumber
  )
where

import Cardano.Prelude

import Cardano.Chain.Slotting
  ( EpochIndex(..)
  , EpochSlots(..)
  , SlotId(..)
  , SlotNumber
  , mkLocalSlotIndex
  , flattenSlotId
  )


exampleEpochIndex :: EpochIndex
exampleEpochIndex = EpochIndex 14

exampleSlotNumber :: EpochSlots -> SlotNumber
exampleSlotNumber es =
  flattenSlotId es (exampleSlotId es)

exampleSlotId :: EpochSlots -> SlotId
exampleSlotId es = SlotId (EpochIndex 11) lsi
  where Right lsi = mkLocalSlotIndex es 47
