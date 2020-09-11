{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Shelley.Spec.Ledger.OverlaySchedule
  ( -- * Overlay schedule
    isOverlaySlot,
    classifyOverlaySlot,
    lookupInOverlaySchedule,

    -- * OBftSlot
    OBftSlot (..),

    -- * Testing
    overlaySlots,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    TokenType (TypeNull),
    decodeNull,
    encodeNull,
    peekTokenType,
  )
import Cardano.Ledger.Era (Era)
import Cardano.Prelude (NFData, NoUnexpectedThunks)
import Cardano.Slotting.Slot
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Keys
  ( KeyHash (..),
    KeyRole (..),
  )
import Shelley.Spec.Ledger.Slot

data OBftSlot era
  = NonActiveSlot
  | ActiveSlot !(KeyHash 'Genesis era)
  deriving (Show, Eq, Ord, Generic)

instance
  Era era =>
  ToCBOR (OBftSlot era)
  where
  toCBOR NonActiveSlot = encodeNull
  toCBOR (ActiveSlot k) = toCBOR k

instance
  Era era =>
  FromCBOR (OBftSlot era)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure NonActiveSlot
      _ -> ActiveSlot <$> fromCBOR

instance NoUnexpectedThunks (OBftSlot era)

instance NFData (OBftSlot era)

isOverlaySlot ::
  SlotNo -> -- starting slot
  UnitInterval -> -- decentralization parameter
  SlotNo -> -- slot to check
  Bool
isOverlaySlot firstSlotNo dval slot = step s < step (s + 1)
  where
    s = fromIntegral $ slot -* firstSlotNo
    d = unitIntervalToRational dval
    step :: Rational -> Integer
    step x = ceiling (x * d)

classifyOverlaySlot ::
  SlotNo -> -- first slot of the epoch
  [KeyHash 'Genesis era] -> -- genesis Nodes
  UnitInterval -> -- decentralization parameter
  ActiveSlotCoeff -> -- active slot coefficent
  SlotNo -> -- overlay slot to classify
  OBftSlot era
classifyOverlaySlot firstSlotNo gkeys dval ascValue slot =
  if isActive
    then
      let genesisIdx = (position `div` ascInv) `mod` (fromIntegral $ length gkeys)
       in gkeys `getAtIndex` genesisIdx
    else NonActiveSlot
  where
    d = unitIntervalToRational dval
    position = ceiling (fromIntegral (slot -* firstSlotNo) * d)
    isActive = position `mod` ascInv == 0
    getAtIndex ls i = if i < length ls then ActiveSlot (ls !! i) else NonActiveSlot
    ascInv = floor (1 / (unitIntervalToRational . activeSlotVal $ ascValue))

lookupInOverlaySchedule ::
  SlotNo -> -- first slot of the epoch
  [KeyHash 'Genesis era] -> -- genesis Nodes
  UnitInterval -> -- decentralization parameter
  ActiveSlotCoeff -> -- active slot coefficent
  SlotNo -> -- slot to lookup
  Maybe (OBftSlot era)
lookupInOverlaySchedule firstSlotNo gkeys dval ascValue slot =
  if isOverlaySlot firstSlotNo dval slot
    then Just $ classifyOverlaySlot firstSlotNo gkeys dval ascValue slot
    else Nothing

-- | Return the list of overlaySlots for a given epoch.
-- Note that this linear in the size of the epoch, and should probably
-- only be used for testing.
-- If something more performant is needed, we could probably use
-- [start + floor(x/d) | x <- [0 .. (spe -1)], floor(x/d) < spe]
-- but we would need to make sure that this is equivalent.
overlaySlots ::
  SlotNo -> -- starting slot
  UnitInterval -> -- decentralization parameter
  EpochSize ->
  [SlotNo]
overlaySlots start d (EpochSize spe) =
  [SlotNo x | x <- [unSlotNo start .. end], isOverlaySlot start d (SlotNo x)]
  where
    end = unSlotNo start + spe - 1
