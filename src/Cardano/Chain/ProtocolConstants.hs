-- | Constants derived from security parameter
--
--   TODO: Find a better home for these

module Cardano.Chain.ProtocolConstants
  ( kSlotSecurityParam
  , kChainQualityThreshold
  , kEpochSlots
  )
where

import Cardano.Prelude

import Cardano.Chain.Common.BlockCount (BlockCount(..))
import Cardano.Chain.Slotting.EpochSlots (EpochSlots(..))
import Cardano.Chain.Slotting.SlotCount (SlotCount(..))


-- | Security parameter expressed in number of slots. It uses chain quality
--   property. It's basically @blkSecurityParam / chainQualityThreshold@.
kSlotSecurityParam :: BlockCount -> SlotCount
kSlotSecurityParam = SlotCount . (*) 2 . unBlockCount

-- | Minimal chain quality (number of blocks divided by number of
--   slots) necessary for security of the system.
kChainQualityThreshold :: Fractional f => BlockCount -> f
kChainQualityThreshold k = realToFrac (unBlockCount k)
  / fromIntegral (unSlotCount $ kSlotSecurityParam k)

-- | Number of slots inside one epoch
kEpochSlots :: BlockCount -> EpochSlots
kEpochSlots = EpochSlots . (*) 10 . unBlockCount
