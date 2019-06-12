{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Slotting.SlotNumber
  ( SlotNumber(..)
  , addSlotNumber
  , subSlotNumber
  , twice
  )
where

import Cardano.Prelude

import Formatting (bprint, int)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary (FromCBOR(..), ToCBOR(..))
import Cardano.Chain.Common.BlockCount (BlockCount, unBlockCount)
import Cardano.Chain.Slotting.SlotCount (SlotCount(..))


-- | 'SlotNumber' is an absolute slot number from the beginning of time
--
--   'SlotNumber' is held in a 'Word64'. Assuming a slot every 20 seconds, 'Word64'
--   is sufficient for slot indices for 10^13 years.
newtype SlotNumber = SlotNumber
  { unSlotNumber :: Word64
  } deriving (Eq, Generic, Ord, Show)
    deriving newtype Num
    deriving anyclass NFData

instance ToCBOR SlotNumber where
  toCBOR = toCBOR . unSlotNumber

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
addSlotNumber :: SlotCount -> SlotNumber -> SlotNumber
addSlotNumber (SlotCount a) (SlotNumber b) = SlotNumber $ a + b

-- | Decrease a 'SlotNumber' by 'SlotCount', going no lower than 0
subSlotNumber :: SlotCount -> SlotNumber -> SlotNumber
subSlotNumber (SlotCount a) (SlotNumber b) =
  if a > b then SlotNumber 0 else SlotNumber (b - a)

-- | Compute the number of slots after which a block becomes stable as @2 * k@,
-- where @k@ is the chain security parameter, which is expressed in number of
-- blocks.
--
twice :: BlockCount -> SlotNumber
twice k = SlotNumber (2 * unBlockCount k)
