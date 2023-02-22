{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Chain.Slotting.EpochAndSlotCount (
  EpochAndSlotCount (..),
  toSlotNumber,
  fromSlotNumber,
  slotNumberEpoch,
)
where

import Cardano.Chain.Slotting.EpochNumber (EpochNumber (..))
import Cardano.Chain.Slotting.EpochSlots (EpochSlots (..))
import Cardano.Chain.Slotting.SlotCount (SlotCount (..))
import Cardano.Chain.Slotting.SlotNumber (SlotNumber (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import Formatting (bprint, ords)
import qualified Formatting.Buildable as B

-- | 'EpochAndSlotCount' identifies a slot by its 'EpochNumber' and the number of
--   slots into the epoch that it sits
data EpochAndSlotCount = EpochAndSlotCount
  { epochNo :: !EpochNumber
  , slotCount :: !SlotCount
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance B.Buildable EpochAndSlotCount where
  build eas =
    bprint
      (ords . " slot of " . ords . " epoch")
      (unSlotCount $ slotCount eas)
      (getEpochNumber $ epochNo eas)

instance ToCBOR EpochAndSlotCount where
  toCBOR = toByronCBOR

instance FromCBOR EpochAndSlotCount where
  fromCBOR = fromByronCBOR

instance EncCBOR EpochAndSlotCount where
  encCBOR eas = encodeListLen 2 <> encCBOR (epochNo eas) <> encCBOR (slotCount eas)
  encodedSizeExpr f eas =
    1
      + encodedSizeExpr f (epochNo <$> eas)
      + encodedSizeExpr f (slotCount <$> eas)

instance DecCBOR EpochAndSlotCount where
  decCBOR = do
    enforceSize "EpochAndSlotCount" 2
    EpochAndSlotCount <$> decCBOR <*> decCBOR

-- | Flatten 'EpochAndSlotCount' into a single absolute 'SlotNumber'
toSlotNumber :: EpochSlots -> EpochAndSlotCount -> SlotNumber
toSlotNumber es eas = SlotNumber $ pastSlots + slots
  where
    slots :: Word64
    slots = unSlotCount $ slotCount eas
    pastSlots :: Word64
    pastSlots = unSlotNumber (flattenEpochNumber es $ epochNo eas)

-- | Flattens 'EpochNumber' into a single number
flattenEpochNumber :: EpochSlots -> EpochNumber -> SlotNumber
flattenEpochNumber es (EpochNumber i) = SlotNumber $ i * unEpochSlots es

-- | Construct a 'EpochAndSlotCount' from a 'SlotNumber', using a given 'EpochSlots'
fromSlotNumber :: EpochSlots -> SlotNumber -> EpochAndSlotCount
fromSlotNumber (EpochSlots n) (SlotNumber fsId)
  | n == 0 =
      panic $
        "'unflattenEpochAndSlotCount': The number of slots-per-epoch "
          <> "passed to this function must be positive"
  | otherwise =
      EpochAndSlotCount
        { epochNo = EpochNumber epoch
        , slotCount = SlotCount slot
        }
  where
    (epoch, slot) = fsId `divMod` n

slotNumberEpoch :: EpochSlots -> SlotNumber -> EpochNumber
slotNumberEpoch epochSlots slot = epochNo $ fromSlotNumber epochSlots slot
