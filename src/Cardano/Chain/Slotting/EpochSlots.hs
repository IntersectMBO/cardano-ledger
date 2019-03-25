{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Cardano.Chain.Slotting.EpochSlots
  ( EpochSlots(..)
  )
where

import Cardano.Prelude

import Data.Aeson (ToJSON(..))
import Data.Data (Data)
import Formatting.Buildable (Buildable)

import Cardano.Binary.Class (Bi(..))

-- | The number of slots per epoch.
newtype EpochSlots = EpochSlots
  { unEpochSlots :: Word64
  } deriving (Data, Eq, Ord, Read, Show, Buildable, Generic)

instance Bi EpochSlots where
  encode = encode . unEpochSlots
  decode = EpochSlots <$> decode

deriving instance ToJSON EpochSlots
