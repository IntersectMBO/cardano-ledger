{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Slotting.Example
  ( exampleEpochNumber
  , exampleSlotId
  , exampleSlotNumber
  )
where

import Cardano.Prelude

import Cardano.Chain.Slotting
  ( EpochNumber(..)
  , EpochSlots(..)
  , SlotId(..)
  , SlotNumber
  , mkLocalSlotIndex
  , flattenSlotId
  )


exampleEpochNumber :: EpochNumber
exampleEpochNumber = EpochNumber 14

exampleSlotNumber :: EpochSlots -> SlotNumber
exampleSlotNumber es =
  flattenSlotId es (exampleSlotId es)

exampleSlotId :: EpochSlots -> SlotId
exampleSlotId es = SlotId (EpochNumber 11) lsi
  where Right lsi = mkLocalSlotIndex es 47
