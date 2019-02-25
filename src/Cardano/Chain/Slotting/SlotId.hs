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
  , SlotIdError(..)
  , siEpochL
  , siSlotL
  , slotIdF
  , slotIdToEnum
  , slotIdFromEnum
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
import Formatting (Format, bprint, build, int, ords, sformat, shown)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromJSON(..), ToJSON(..))

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)
import Cardano.Chain.Common.BlockCount (BlockCount)
import Cardano.Chain.ProtocolConstants (kEpochSlots, kSlotSecurityParam)
import Cardano.Chain.Slotting.EpochIndex (EpochIndex(..))
import Cardano.Chain.Slotting.LocalSlotIndex
  (LocalSlotIndex(..), getSlotIndex, localSlotIndexMinBound, mkLocalSlotIndex)
import Cardano.Chain.Slotting.SlotCount (SlotCount(..))


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
    (getSlotIndex $ siSlot si)
    (getEpochIndex $ siEpoch si)

instance B.Buildable SlotIdError where
  build (SlotIdOverflow sId sc) = bprint
    ("SlotId: "
    . shown
    . " and SlotCount: "
    . shown
    . " exceeds the maximum boundary when flattened."
    )
    sId
    sc

data SlotIdError
  = SlotIdOverflow SlotId SlotCount deriving (Eq, Show)

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

slotIdToEnum :: SlotCount -> FlatSlotId -> SlotId
slotIdToEnum = unflattenSlotId

slotIdFromEnum :: SlotCount -> SlotId -> Either SlotIdError Int
slotIdFromEnum sc sId = fromIntegral . getFlatSlotId <$> flattenSlotId sc sId

slotIdSucc :: SlotCount -> SlotId -> Either SlotIdError SlotId
slotIdSucc sc sId =
  slotIdToEnum sc . FlatSlotId . fromIntegral . (1 +) <$> slotIdFromEnum sc sId

slotIdPred :: SlotCount -> SlotId -> Either SlotIdError SlotId
slotIdPred epochSlots sId =
  slotIdToEnum epochSlots
    .   FlatSlotId
    .   fromIntegral
    .   subtract 1
    <$> slotIdFromEnum epochSlots sId

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
newtype FlatSlotId = FlatSlotId
      { getFlatSlotId :: Word64
      } deriving (Eq, Generic, Num, Ord, Show)

instance Bi FlatSlotId where
  encode = encode . getFlatSlotId
  decode = FlatSlotId <$> decode

instance Monad m => ToJSON m FlatSlotId where
  toJSON = toJSON . getFlatSlotId

instance MonadError SchemaError m => FromJSON m FlatSlotId where
  fromJSON val = do
    number <- fromJSON val
    pure $ FlatSlotId number

instance Aeson.FromJSON FlatSlotId where
  parseJSON v = do
    c <- Aeson.parseJSON v
    pure $ FlatSlotId c

instance Aeson.ToJSON FlatSlotId where
  toJSON = Aeson.toJSON . getFlatSlotId

instance B.Buildable FlatSlotId where
  build (getFlatSlotId -> x) = bprint
    int
    x

instance NFData FlatSlotId

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
flattenSlotId :: SlotCount -> SlotId -> Either SlotIdError FlatSlotId
flattenSlotId sc si
  | flattened > fromIntegral (maxBound :: Word64) = Left $ SlotIdOverflow si sc
  | otherwise = Right . FlatSlotId $ fromIntegral flattened
 where
  lsi :: Integer
  lsi = fromIntegral . getSlotIndex $ siSlot si
  pastSlots :: Integer
  pastSlots =
    fromIntegral . getFlatSlotId $ flattenEpochIndex sc (siEpoch si)
  flattened :: Integer
  flattened = pastSlots + lsi

-- | Flattens 'EpochIndex' into a single number
flattenEpochIndex :: SlotCount -> EpochIndex -> FlatSlotId
flattenEpochIndex epochSlots (EpochIndex i) =
  FlatSlotId $ fromIntegral (fromIntegral i * epochSlots)

-- | Construct a 'SlotId' from a flattened variant, using a given 'SlotCount'
--   modulus
unflattenSlotId :: SlotCount -> FlatSlotId -> SlotId
unflattenSlotId es (FlatSlotId fsId) = SlotId
  { siEpoch = EpochIndex epoch
  , siSlot  = lsi
  }
 where
  -- `slot` accounts for the `LocalSlotIndex`
  (epoch, slot) = fsId `divMod` fromIntegral es
  lsi           = case mkLocalSlotIndex es (fromIntegral slot) of
    Left err -> panic
      $ sformat ("The impossible happened in unflattenSlotId: " . build) err
    Right lsi' -> lsi'

-- | Increase a 'FlatSlotId' by 'SlotCount'
addSlotNumber :: SlotCount -> FlatSlotId -> FlatSlotId
addSlotNumber a (FlatSlotId b) = FlatSlotId $ fromIntegral a + b

-- | Decrease a 'FlatSlotId' by 'SlotCount', going no lower than 0
subSlotNumber :: SlotCount -> FlatSlotId -> FlatSlotId
subSlotNumber (SlotCount a) (FlatSlotId b) =
  if a > b then FlatSlotId 0 else FlatSlotId (b - a)

slotNumberEpoch :: SlotCount -> FlatSlotId -> EpochIndex
slotNumberEpoch epochSlots slot = siEpoch $ unflattenSlotId epochSlots slot

-- | Slot such that at the beginning of epoch blocks with SlotId ≤ to this slot
--   are stable
crucialSlot :: BlockCount -> EpochIndex -> SlotId
crucialSlot _ 0        = SlotId {siEpoch = 0, siSlot = localSlotIndexMinBound}
crucialSlot k epochIdx = SlotId {siEpoch = epochIdx - 1, siSlot = slot}
 where
  epochSlots = kEpochSlots k
  idx :: Word16
  idx  = fromIntegral $ epochSlots - kSlotSecurityParam k - 1
  slot = case mkLocalSlotIndex epochSlots idx of
    Left err ->
      panic $ sformat ("The impossible happened in crucialSlot: " . build) err
    Right lsi -> lsi
