{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Chain.Slotting.SlotCount
  ( SlotCount(..)
  )
where

import Cardano.Prelude

import Formatting.Buildable (Buildable)


-- | A number of slots
newtype SlotCount = SlotCount
  { unSlotCount :: Word64
  } deriving (Eq, Ord, Read, Show, Buildable, Generic)
