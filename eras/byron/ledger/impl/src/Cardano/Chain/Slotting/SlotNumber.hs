{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Chain.Slotting.SlotNumber
  ( SlotNumber (..),
    addSlotCount,
    -- deprecated
    subSlotCount,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Chain.Slotting.SlotCount (SlotCount (..))
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Formatting (bprint, int)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))

-- | 'SlotNumber' is an absolute slot number from the beginning of time
--
--   'SlotNumber' is held in a 'Word64'. Assuming a slot every 20 seconds, 'Word64'
--   is sufficient for slot indices for 10^13 years.
newtype SlotNumber = SlotNumber
  { unSlotNumber :: Word64
  }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Num)
  deriving anyclass (NFData, NoThunks)

-- Used for debugging purposes only
instance Aeson.ToJSON SlotNumber

instance ToCBOR SlotNumber where
  toCBOR = toCBOR . unSlotNumber
  encodedSizeExpr size = encodedSizeExpr size . fmap unSlotNumber

instance FromCBOR SlotNumber where
  fromCBOR = SlotNumber <$> fromCBOR

instance Monad m => ToJSON m SlotNumber where
  toJSON = toJSON . unSlotNumber

instance MonadError SchemaError m => FromJSON m SlotNumber where
  fromJSON val = do
    number <- fromJSON val
    pure $ SlotNumber number

instance B.Buildable SlotNumber where
  build s = bprint int (unSlotNumber s)

-- | Increase a 'SlotNumber' by 'SlotCount'
addSlotCount :: SlotCount -> SlotNumber -> SlotNumber
addSlotCount (SlotCount a) (SlotNumber b)
  | a <= maxBound - b = SlotNumber $ a + b
  | otherwise = SlotNumber maxBound

-- | Decrease a 'SlotNumber' by 'SlotCount', going no lower than 0
{-# DEPRECATED subSlotCount "this function is dangerous and can usually be replaced by addSlotCount" #-}
subSlotCount :: SlotCount -> SlotNumber -> SlotNumber
subSlotCount (SlotCount a) (SlotNumber b) =
  if a > b then SlotNumber 0 else SlotNumber (b - a)
