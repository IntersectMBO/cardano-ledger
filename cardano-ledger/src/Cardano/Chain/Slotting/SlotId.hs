{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Cardano.Chain.Slotting.SlotId
  ( SlotId(..)
  , slotIdF
  , slotIdSucc
  , slotIdPred
  , SlotNumber(..)
  , flattenSlotId
  , unflattenSlotId
  , addSlotNumber
  , subSlotNumber
  , slotNumberEpoch
  , crucialSlot
  , twice
  )
where

import Cardano.Prelude

import qualified Data.Aeson as Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Formatting (Format, bprint, build, int, ords, sformat)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary
  (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)
import Cardano.Chain.Common.BlockCount (BlockCount, unBlockCount)
import Cardano.Chain.ProtocolConstants (kEpochSlots, kSlotSecurityParam)
import Cardano.Chain.Slotting.EpochIndex (EpochIndex(..))
import Cardano.Chain.Slotting.EpochSlots (EpochSlots(..))
import Cardano.Chain.Slotting.LocalSlotIndex
  ( LocalSlotIndex(..)
  , localSlotIndexMinBound
  , mkLocalSlotIndex
  , unLocalSlotIndex
  )
import Cardano.Chain.Slotting.SlotCount (SlotCount(..))


-- | Slot is identified by index of epoch and index of slot in
--   this epoch. This is a global index, an index to a global
--   slot position.
data SlotId = SlotId
  { siEpoch :: !EpochIndex
  , siSlot  :: !LocalSlotIndex
  } deriving (Show, Eq, Ord, Generic)
    deriving anyclass NFData

instance B.Buildable SlotId where
  build si = bprint
    (ords . " slot of " . ords . " epoch")
    (unLocalSlotIndex $ siSlot si)
    (getEpochIndex $ siEpoch si)

instance ToCBOR SlotId where
  toCBOR si = encodeListLen 2 <> toCBOR (siEpoch si) <> toCBOR (siSlot si)

instance FromCBOR SlotId where
  fromCBOR = do
    enforceSize "SlotId" 2
    SlotId <$> fromCBOR <*> fromCBOR

deriveJSON defaultOptions ''SlotId

slotIdToEnum :: EpochSlots -> SlotNumber -> SlotId
slotIdToEnum = unflattenSlotId

slotIdFromEnum :: EpochSlots -> SlotId -> Word64
slotIdFromEnum sc sId = unSlotNumber $ flattenSlotId sc sId

slotIdSucc :: EpochSlots -> SlotId -> SlotId
slotIdSucc sc sId =
  slotIdToEnum sc . SlotNumber . (1 +) $ slotIdFromEnum sc sId

slotIdPred :: EpochSlots -> SlotId -> SlotId
slotIdPred epochSlots sId =
  slotIdToEnum epochSlots . SlotNumber . subtract 1 $ slotIdFromEnum
    epochSlots
    sId

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | SlotNumber is a flat version of SlotId
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

instance Aeson.FromJSON SlotNumber where
  parseJSON v = do
    c <- Aeson.parseJSON v
    pure $ SlotNumber c

instance Aeson.ToJSON SlotNumber where
  toJSON = Aeson.toJSON . unSlotNumber

instance B.Buildable SlotNumber where
  build (unSlotNumber -> x) = bprint
    int
    x

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
-- 'SlotNumber' is held in a 'Word64'. Assuming a slot every 20 seconds, 'Word64'
-- is sufficient for slot indices for 10^13 years.
flattenSlotId :: EpochSlots -> SlotId -> SlotNumber
flattenSlotId es si = SlotNumber $ pastSlots + lsi
 where
  lsi :: Word64
  lsi = fromIntegral . unLocalSlotIndex $ siSlot si
  pastSlots :: Word64
  pastSlots = unSlotNumber (flattenEpochIndex es $ siEpoch si)

-- | Flattens 'EpochIndex' into a single number
flattenEpochIndex :: EpochSlots -> EpochIndex -> SlotNumber
flattenEpochIndex es (EpochIndex i) = SlotNumber $ i * unEpochSlots es

-- | Construct a 'SlotId' from a flattened variant, using a given 'EpochSlots'
--   modulus
unflattenSlotId :: EpochSlots -> SlotNumber -> SlotId
unflattenSlotId (EpochSlots n) (SlotNumber fsId)
  | n == 0    =  panic $  "'unflattenSlotId': The number of slots-per-epoch "
                       <> "passed to this function must be positive"
  | otherwise =
    SlotId
      { siEpoch = EpochIndex epoch
      , siSlot  = UnsafeLocalSlotIndex slotCount
      }
 where
  -- `slot` accounts for the `LocalSlotIndex`
  (epoch, slot) = fsId `divMod` n

  slotCount :: Word16
  slotCount =
    if fromIntegral (maxBound :: Word16) < slot
    then panic $ "unflattenSlotId: The slot count exceeded its maximum bound. \n"
               <> "Input slot number: " <> show fsId <> ". \n"
               <> "Input slots-per-epoch: " <> show n <> "."
    else fromIntegral slot

-- | Increase a 'SlotNumber' by 'SlotCount'
addSlotNumber :: SlotCount -> SlotNumber -> SlotNumber
addSlotNumber (SlotCount a) (SlotNumber b) = SlotNumber $ a + b

-- | Decrease a 'SlotNumber' by 'SlotCount', going no lower than 0
subSlotNumber :: SlotCount -> SlotNumber -> SlotNumber
subSlotNumber (SlotCount a) (SlotNumber b) =
  if a > b then SlotNumber 0 else SlotNumber (b - a)

slotNumberEpoch :: EpochSlots -> SlotNumber -> EpochIndex
slotNumberEpoch epochSlots slot = siEpoch $ unflattenSlotId epochSlots slot

-- | Slot such that at the beginning of epoch blocks with SlotId ≤ to this slot
--   are stable
crucialSlot :: BlockCount -> EpochIndex -> SlotId
crucialSlot _ 0        = SlotId {siEpoch = 0, siSlot = localSlotIndexMinBound}
crucialSlot k epochIdx = SlotId {siEpoch = epochIdx - 1, siSlot = slot}
 where
  epochSlots = kEpochSlots k
  idx :: Word16
  idx =
    fromIntegral
      $ unEpochSlots epochSlots
      - unSlotCount (kSlotSecurityParam k)
      - 1
  slot = case mkLocalSlotIndex epochSlots idx of
    Left err ->
      panic $ sformat ("The impossible happened in crucialSlot: " . build) err
    Right lsi -> lsi

-- | Compute the number of slots after which a block becomes stable as @2 * k@,
-- where @k@ is the chain security parameter, which is expressed in number of
-- blocks.
--
twice :: BlockCount -> SlotNumber
twice k = SlotNumber (2 * unBlockCount k)
