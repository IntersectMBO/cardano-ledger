{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Cardano.Chain.Slotting.EpochSlots
  ( EpochSlots(..)
  , WithEpochSlots (..)
  )
where

import Cardano.Prelude

import Data.Data (Data)
import Formatting.Buildable (Buildable)

import Cardano.Binary (FromCBOR(..), ToCBOR(..))

-- | The number of slots per epoch.
newtype EpochSlots = EpochSlots
  { unEpochSlots :: Word64
  } deriving (Data, Eq, Ord, Read, Show, Buildable, Generic)

instance ToCBOR EpochSlots where
  toCBOR = toCBOR . unEpochSlots

instance FromCBOR EpochSlots where
  fromCBOR = EpochSlots <$> fromCBOR

-- | Data with an accompanying slots per epoch context.
data WithEpochSlots a = WithEpochSlots
  { epochSlots       :: EpochSlots
  , unWithEpochSlots :: a
  }
  deriving (Show, Eq)
