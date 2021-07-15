module Test.Cardano.Chain.Slotting.Example
  ( exampleEpochNumber,
    exampleEpochAndSlotCount,
    exampleSlotNumber,
  )
where

import Cardano.Chain.Slotting
  ( EpochAndSlotCount (..),
    EpochNumber (..),
    EpochSlots (..),
    SlotCount (..),
    SlotNumber,
    toSlotNumber,
  )

exampleEpochNumber :: EpochNumber
exampleEpochNumber = EpochNumber 14

exampleSlotNumber :: EpochSlots -> SlotNumber
exampleSlotNumber es = toSlotNumber es exampleEpochAndSlotCount

exampleEpochAndSlotCount :: EpochAndSlotCount
exampleEpochAndSlotCount = EpochAndSlotCount (EpochNumber 11) (SlotCount 47)
