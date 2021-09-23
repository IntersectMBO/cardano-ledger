-- | Ledger global parameters.
module Byron.Spec.Ledger.GlobalParams
  ( epochFirstSlot,
    lovelaceCap,
    slotsPerEpoch,
    slotsPerEpochToK,
    c,
  )
where

import Byron.Spec.Ledger.Core (BlockCount (BlockCount), Epoch (..), Slot (..), lovelaceCap)
import Data.Word (Word64)

-- | Given the chain stability parameter, often referred to as @k@, which is
-- expressed in an amount of blocks, return the number of slots contained in an
-- epoch.
slotsPerEpoch :: Integral n => BlockCount -> n
slotsPerEpoch (BlockCount bc) = fromIntegral $ bc * 10

-- | The inverse of 'slotsPerEpoch': given a number of slots per-epoch, return
-- the chain stability parameter @k@.
slotsPerEpochToK :: (Integral n) => n -> BlockCount
slotsPerEpochToK n = BlockCount $ floor $ (fromIntegral n :: Double) / 10

-- | Given the chain stability parameter, calculate the first slot in a given
-- epoch.
epochFirstSlot :: BlockCount -> Epoch -> Slot
epochFirstSlot bc (Epoch epochs) = Slot $ epochs * slotsPerEpoch bc

-- | Factor used to bound the concrete size by the abstract size.
--
-- This constant should satisfy that given an elaboration function 'elaborate'
-- which elaborates abstract values intro concrete ones, for each abstract data
-- value 'a' we have:
--
-- > size (elaborate a) <= c * abstractSize a
--
-- TODO: we need to investigate what this factor is, and probably use different
-- factors for different data types (update, UTxO transactions, etc).
c :: Word64
c = 4096
