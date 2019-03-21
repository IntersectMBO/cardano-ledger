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


-- | Security parameter expressed in number of slots. It uses chain quality
--   property. It's basically @blkSecurityParam / chainQualityThreshold@.
kSlotSecurityParam :: BlockCount -> EpochSlots
kSlotSecurityParam = EpochSlots . (*) 2 . getBlockCount

-- | Minimal chain quality (number of blocks divided by number of
--   slots) necessary for security of the system.
kChainQualityThreshold :: Fractional f => BlockCount -> f
kChainQualityThreshold k =
  realToFrac k / fromIntegral (unEpochSlots $ kSlotSecurityParam k)

-- | Number of slots inside one epoch
kEpochSlots :: BlockCount -> EpochSlots
kEpochSlots = EpochSlots . (*) 10 . getBlockCount

