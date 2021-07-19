{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Chain.Slotting.SlotCount
  ( SlotCount (..),
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Prelude
import Formatting.Buildable (Buildable)

-- | A number of slots
newtype SlotCount = SlotCount
  { unSlotCount :: Word64
  }
  deriving stock (Read, Show, Generic)
  deriving newtype (Eq, Ord, Buildable, ToCBOR, FromCBOR)
  deriving anyclass (NFData)
