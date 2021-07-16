-- | Constants derived from security parameter
--
--   TODO: Find a better home for these
module Cardano.Chain.ProtocolConstants
  ( kSlotSecurityParam,
    kUpdateStabilityParam,
    kChainQualityThreshold,
    kEpochSlots,
  )
where

import Cardano.Chain.Common.BlockCount (BlockCount (..))
import Cardano.Chain.Slotting.EpochSlots (EpochSlots (..))
import Cardano.Chain.Slotting.SlotCount (SlotCount (..))
import Cardano.Prelude

-- | Security parameter expressed in number of slots. It uses chain quality
--   property. It's basically @blkSecurityParam / chainQualityThreshold@.
kSlotSecurityParam :: BlockCount -> SlotCount
kSlotSecurityParam = SlotCount . (*) 2 . unBlockCount

-- | Update stability parameter expressed in number of slots. This is the time
--   between an protocol version update receiving its final endorsement and
--   being accepted, and is set to double the security param.
--
--   This extra safety margin is required because an update in the protocol
--   version may trigger a hard fork, which can change "era"-level parameters
--   such as slot length and the number of slots per epoch. As such, the
--   consensus layer wishes to always have a margin between such an update being
--   _certain to happen_ and it actually happening.
--
--   For full details, you can see
--   https://github.com/input-output-hk/cardano-ledger-specs/issues/1288
kUpdateStabilityParam :: BlockCount -> SlotCount
kUpdateStabilityParam = SlotCount . (*) 4 . unBlockCount

-- | Minimal chain quality (number of blocks divided by number of
--   slots) necessary for security of the system.
kChainQualityThreshold :: Fractional f => BlockCount -> f
kChainQualityThreshold k =
  realToFrac (unBlockCount k)
    / fromIntegral (unSlotCount $ kSlotSecurityParam k)

-- | Number of slots inside one epoch
kEpochSlots :: BlockCount -> EpochSlots
kEpochSlots = EpochSlots . (*) 10 . unBlockCount
