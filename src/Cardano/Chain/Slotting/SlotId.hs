{-# LANGUAGE DeriveGeneric              #-}
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
  , siEpochL
  , siSlotL
  , slotIdF
  , slotIdSucc
  , slotIdPred
  , FlatSlotId(..)
  , flattenSlotId
  , unflattenSlotId
  , addSlotNumber
  , subSlotNumber
  , slotNumberEpoch
  , crucialSlot
  )
where

import Cardano.Prelude

import Control.Lens (makeLensesFor)
import qualified Data.Aeson as Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Formatting (Format, bprint, build, int, ords, sformat)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)
import Cardano.Chain.Common.BlockCount (BlockCount)
import Cardano.Chain.ProtocolConstants (kEpochSlots, kSlotSecurityParam)
import Cardano.Chain.Slotting.EpochIndex (EpochIndex(..))
import Cardano.Chain.Slotting.LocalSlotIndex
  ( LocalSlotIndex(..)
  , unLocalSlotIndex
  , localSlotIndexMinBound
  , mkLocalSlotIndex
  )
import Cardano.Chain.Slotting.EpochSlots (EpochSlots(..))


-- | Slot is identified by index of epoch and index of slot in
--   this epoch. This is a global index, an index to a global
--   slot position.
data SlotId = SlotId
  { siEpoch :: !EpochIndex
  , siSlot  :: !LocalSlotIndex
  } deriving (Show, Eq, Ord, Generic)

instance B.Buildable SlotId where
  build si = bprint
    (ords . " slot of " . ords . " epoch")
    (unLocalSlotIndex $ siSlot si)
    (getEpochIndex $ siEpoch si)

instance NFData SlotId

instance Bi SlotId where
  encode si = encodeListLen 2 <> encode (siEpoch si) <> encode (siSlot si)

  decode = do
    enforceSize "SlotId" 2
    SlotId <$> decode <*> decode

deriveJSON defaultOptions ''SlotId

makeLensesFor
    [ ("siEpoch", "siEpochL")
    , ("siSlot" , "siSlotL")
    ]
    ''SlotId

slotIdToEnum :: EpochSlots -> FlatSlotId -> SlotId
slotIdToEnum = unflattenSlotId

slotIdFromEnum :: EpochSlots -> SlotId -> Word64
slotIdFromEnum sc sId = unFlatSlotId $ flattenSlotId sc sId

slotIdSucc :: EpochSlots -> SlotId -> SlotId
slotIdSucc sc sId =
  slotIdToEnum sc . FlatSlotId . (1 +) $ slotIdFromEnum sc sId

slotIdPred :: EpochSlots -> SlotId -> SlotId
slotIdPred epochSlots sId =
  slotIdToEnum epochSlots . FlatSlotId . subtract 1 $ slotIdFromEnum
    epochSlots
    sId

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
newtype FlatSlotId = FlatSlotId
      { unFlatSlotId :: Word64
      } deriving (Eq, Generic, Num, Ord, Show)

instance Bi FlatSlotId where
  encode = encode . unFlatSlotId
  decode = FlatSlotId <$> decode

instance Monad m => ToJSON m FlatSlotId where
  toJSON = toJSON . unFlatSlotId

instance MonadError SchemaError m => FromJSON m FlatSlotId where
  fromJSON val = do
    number <- fromJSON val
    pure $ FlatSlotId number

instance Aeson.FromJSON FlatSlotId where
  parseJSON v = do
    c <- Aeson.parseJSON v
    pure $ FlatSlotId c

instance Aeson.ToJSON FlatSlotId where
  toJSON = Aeson.toJSON . unFlatSlotId

instance B.Buildable FlatSlotId where
  build (unFlatSlotId -> x) = bprint
    int
    x

instance NFData FlatSlotId

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
-- 'FlatSlotId' is held in a 'Word64'. Assuming a slot every 20 seconds, 'Word64'
-- is sufficient for slot indices for 10^13 years.
flattenSlotId :: EpochSlots -> SlotId -> FlatSlotId
flattenSlotId es si = FlatSlotId $ pastSlots + lsi
 where
  lsi :: Word64
  lsi = fromIntegral . unLocalSlotIndex $ siSlot si
  pastSlots :: Word64
  pastSlots = unFlatSlotId (flattenEpochIndex es $ siEpoch si)

-- | Flattens 'EpochIndex' into a single number
flattenEpochIndex :: EpochSlots -> EpochIndex -> FlatSlotId
flattenEpochIndex es (EpochIndex i) = FlatSlotId $ i * unEpochSlots es

-- | Construct a 'SlotId' from a flattened variant, using a given 'EpochSlots'
--   modulus
unflattenSlotId :: EpochSlots -> FlatSlotId -> SlotId
unflattenSlotId es (FlatSlotId fsId) = SlotId
  { siEpoch = EpochIndex epoch
  , siSlot  = lsi
  }
 where
  -- `slot` accounts for the `LocalSlotIndex`
  (epoch, slot) = fsId `divMod` unEpochSlots es
  lsi           = case mkLocalSlotIndex es (fromIntegral slot) of
    Left err -> panic
      $ sformat ("The impossible happened in unflattenSlotId: " . build) err
    Right lsi' -> lsi'

-- | Increase a 'FlatSlotId' by 'EpochSlots'
addSlotNumber :: EpochSlots -> FlatSlotId -> FlatSlotId
addSlotNumber (EpochSlots a) (FlatSlotId b) = FlatSlotId $ a + b

-- | Decrease a 'FlatSlotId' by 'EpochSlots', going no lower than 0
subSlotNumber :: EpochSlots -> FlatSlotId -> FlatSlotId
subSlotNumber (EpochSlots a) (FlatSlotId b) =
  if a > b then FlatSlotId 0 else FlatSlotId (b - a)

slotNumberEpoch :: EpochSlots -> FlatSlotId -> EpochIndex
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
      - unEpochSlots (kSlotSecurityParam k)
      - 1
  slot = case mkLocalSlotIndex epochSlots idx of
    Left err ->
      panic $ sformat ("The impossible happened in crucialSlot: " . build) err
    Right lsi -> lsi
