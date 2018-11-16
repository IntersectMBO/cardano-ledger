{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Cardano.Chain.Slotting.SlotCount
  ( SlotCount(..)
  )
where

import Cardano.Prelude

import Data.Aeson (ToJSON(..))
import Formatting.Buildable (Buildable)

import Cardano.Binary.Class (Bi(..))


newtype SlotCount = SlotCount
    { getSlotCount :: Word64
    } deriving ( Eq
              , Ord
              , Num
              , Real
              , Integral
              , Enum
              , Read
              , Show
              , Buildable
              , Generic
              , NFData
              )

instance Bi SlotCount where
  encode = encode . getSlotCount
  decode = SlotCount <$> decode

deriving instance ToJSON SlotCount
