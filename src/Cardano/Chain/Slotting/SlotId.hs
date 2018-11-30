{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Chain.Slotting.SlotId
  ( SlotId(..)
  , siEpochL
  , siSlotL
  , slotIdF
  , slotIdToEnum
  , slotIdFromEnum
  , slotIdSucc
  , slotIdPred
  , FlatSlotId
  , flatSlotId
  , flattenSlotId
  , flattenEpochIndex
  , unflattenSlotId
  , addSlotNumber
  , subSlotNumber
  , slotNumberEpoch
  , crucialSlot
  )
where

import Cardano.Prelude

import Control.Lens (Iso', iso, makeLensesFor)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Formatting (Format, bprint, build, ords, sformat)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)
import Cardano.Chain.Common.BlockCount (BlockCount)
import Cardano.Chain.ProtocolConstants (kEpochSlots, kSlotSecurityParam)
import Cardano.Chain.Slotting.EpochIndex (EpochIndex(..))
import Cardano.Chain.Slotting.LocalSlotIndex
  (LocalSlotIndex, getSlotIndex, localSlotIndexMinBound, mkLocalSlotIndex)
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

slotIdToEnum :: SlotCount -> Int -> SlotId
slotIdToEnum epochSlots = unflattenSlotId epochSlots . fromIntegral

slotIdFromEnum :: SlotCount -> SlotId -> Int
slotIdFromEnum epochSlots = fromIntegral . flattenSlotId epochSlots

slotIdSucc :: SlotCount -> SlotId -> SlotId
slotIdSucc epochSlots =
  slotIdToEnum epochSlots . (+ 1) . slotIdFromEnum epochSlots

slotIdPred :: SlotCount -> SlotId -> SlotId
slotIdPred epochSlots =
  slotIdToEnum epochSlots . subtract 1 . slotIdFromEnum epochSlots

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
flattenSlotId :: SlotCount -> SlotId -> FlatSlotId
flattenSlotId es si =
  fromIntegral $ fromIntegral (siEpoch si) * es + fromIntegral
    (getSlotIndex $ siSlot si)

-- | Flattens 'EpochIndex' into a single number
flattenEpochIndex :: SlotCount -> EpochIndex -> FlatSlotId
flattenEpochIndex epochSlots (EpochIndex i) =
  fromIntegral (fromIntegral i * epochSlots)

-- | Construct a 'SlotId' from a flattened variant, using a given 'SlotCount'
--   modulus
unflattenSlotId :: SlotCount -> FlatSlotId -> SlotId
unflattenSlotId es n = SlotId {siEpoch = fromIntegral epoch, siSlot = lsi}
 where
  (epoch, slot) = n `divMod` fromIntegral es
  lsi           = case mkLocalSlotIndex es (fromIntegral slot) of
    Left err -> panic
      $ sformat ("The impossible happened in unflattenSlotId: " . build) err
    Right lsi' -> lsi'

flatSlotId :: SlotCount -> Iso' SlotId FlatSlotId
flatSlotId epochSlots =
  iso (flattenSlotId epochSlots) (unflattenSlotId epochSlots)

-- | Increase a 'FlatSlotId' by 'SlotCount'
addSlotNumber :: SlotCount -> FlatSlotId -> FlatSlotId
addSlotNumber a b = fromIntegral a + b

-- | Decrease a 'FlatSlotId' by 'SlotCount', going no lower than 0
subSlotNumber :: SlotCount -> FlatSlotId -> FlatSlotId
subSlotNumber (SlotCount a) b = if a > b then 0 else b - a

slotNumberEpoch :: SlotCount -> FlatSlotId -> EpochIndex
slotNumberEpoch epochSlots slot = siEpoch $ unflattenSlotId epochSlots slot

-- | Slot such that at the beginning of epoch blocks with SlotId ≤- this slot
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
