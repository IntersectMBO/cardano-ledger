{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Chain.Slotting.EpochSlots (
  EpochSlots (..),
  WithEpochSlots (..),
  epochFirstSlot,
)
where

import Cardano.Chain.Slotting.EpochNumber
import Cardano.Chain.Slotting.SlotNumber
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Prelude
import Data.Data (Data)
import Formatting.Buildable (Buildable)
import NoThunks.Class (NoThunks (..))

-- | The number of slots per epoch.
newtype EpochSlots = EpochSlots
  { unEpochSlots :: Word64
  }
  deriving (Data, Eq, Ord, Read, Show, Buildable, Generic, NoThunks)

instance EncCBOR EpochSlots where
  encCBOR = encCBOR . unEpochSlots

instance DecCBOR EpochSlots where
  decCBOR = EpochSlots <$> decCBOR

-- | Data with an accompanying slots per epoch context.
data WithEpochSlots a = WithEpochSlots
  { epochSlots :: EpochSlots
  , unWithEpochSlots :: a
  }
  deriving (Show, Eq)

-- | Calculate the first slot in an epoch.
--
-- Note that this function will give an undetermined result if Byron is not the
-- first and only era - a more robust method should use 'EpochInfo' from
-- cardano-slotting.
epochFirstSlot :: EpochSlots -> EpochNumber -> SlotNumber
epochFirstSlot (EpochSlots n) (EpochNumber k) = SlotNumber $ n * k
