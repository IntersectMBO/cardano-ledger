{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Shelley.Spec.Ledger.OverlaySchedule
  ( -- * Overlay schedule
    OverlaySchedule,
    compactOverlaySchedule,
    decompactOverlaySchedule,
    emptyOverlaySchedule,
    isOverlaySlot,
    lookupInOverlaySchedule,
    overlaySchedule,
    overlayScheduleHelper,
    overlayScheduleIsEmpty,
    overlayScheduleToMap,

    -- * OBftSlot
    OBftSlot (..),
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
import Control.Monad.Trans.Reader (asks)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Keys
  ( KeyHash (..),
    KeyRole (..),
  )
import Shelley.Spec.Ledger.PParams (PParams, _d)
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

newtype OverlaySchedule era = OverlaySchedule (Map SlotNo (OBftSlot era))
  deriving stock (Show, Eq)
  deriving newtype (NoUnexpectedThunks, NFData)

emptyOverlaySchedule :: OverlaySchedule era
emptyOverlaySchedule = OverlaySchedule Map.empty

lookupInOverlaySchedule ::
  SlotNo ->
  OverlaySchedule era ->
  Maybe (OBftSlot era)
lookupInOverlaySchedule slot (OverlaySchedule oSched) = Map.lookup slot oSched

overlayScheduleIsEmpty :: OverlaySchedule era -> Bool
overlayScheduleIsEmpty (OverlaySchedule oSched) = Map.null oSched

-- | Overlay schedule
-- This is just a very simple round-robin, evenly spaced schedule.
overlaySchedule ::
  EpochNo ->
  Set (KeyHash 'Genesis era) ->
  PParams era ->
  ShelleyBase (OverlaySchedule era)
overlaySchedule e gkeys pp = do
  ei <- asks epochInfo
  slotsPerEpoch <- epochInfoSize ei e
  firstSlotNo <- epochInfoFirst ei e
  asc <- asks activeSlotCoeff
  pure $ overlayScheduleHelper slotsPerEpoch firstSlotNo gkeys (_d pp) asc

overlayScheduleHelper ::
  EpochSize ->
  -- | First slot of the epoch
  SlotNo ->
  Set (KeyHash 'Genesis era) ->
  -- | Decentralization parameter @d@
  UnitInterval ->
  ActiveSlotCoeff ->
  OverlaySchedule era
overlayScheduleHelper slotsPerEpoch firstSlotNo gkeys d asc
  | dval == 0 =
    OverlaySchedule $ Map.empty
  | otherwise =
    OverlaySchedule $ Map.union active inactive
  where
    dval = intervalValue d
    numActive = dval * fromIntegral slotsPerEpoch
    dInv = 1 / dval
    ascValue = (intervalValue . activeSlotVal) asc
    toRelativeSlotNo x = (Duration . floor) (dInv * fromInteger x)
    toSlotNo x = firstSlotNo +* toRelativeSlotNo x
    genesisSlots = [toSlotNo x | x <- [0 .. (floor numActive - 1)]]
    numInactivePerActive = floor (1 / ascValue) - 1
    activitySchedule = cycle (True : replicate numInactivePerActive False)
    unassignedSched = zip activitySchedule genesisSlots
    genesisCycle = if Set.null gkeys then [] else cycle (Set.toList gkeys)
    active =
      Map.fromList $
        fmap
          (\(gk, (_, s)) -> (s, ActiveSlot gk))
          (zip genesisCycle (filter fst unassignedSched))
    inactive =
      Map.fromList $
        fmap
          (\x -> (snd x, NonActiveSlot))
          (filter (not . fst) unassignedSched)

overlayScheduleToMap :: OverlaySchedule era -> Map SlotNo (OBftSlot era)
overlayScheduleToMap (OverlaySchedule oSched) = oSched

-- | Convert the overlay schedule to a representation that is more compact
-- when serialised to a bytestring, but less efficient for lookups.
--
-- Each genesis key hash will only be stored once, instead of each time it is
-- assigned to a slot.
compactOverlaySchedule ::
  OverlaySchedule era ->
  Map (OBftSlot era) (NonEmpty SlotNo)
compactOverlaySchedule (OverlaySchedule oSched) =
  Map.foldrWithKey'
    ( \slot obftSlot ->
        Map.insertWith (<>) obftSlot (pure slot)
    )
    Map.empty
    oSched

-- | Inverse of 'compactOverlaySchedule'
decompactOverlaySchedule ::
  Map (OBftSlot era) (NonEmpty SlotNo) ->
  OverlaySchedule era
decompactOverlaySchedule compact =
  OverlaySchedule $
    Map.fromList
      [ (slot, obftSlot)
        | (obftSlot, slots) <- Map.toList compact,
          slot <- NonEmpty.toList slots
      ]

instance Era era => ToCBOR (OverlaySchedule era) where
  toCBOR = toCBOR . compactOverlaySchedule

instance Era era => FromCBOR (OverlaySchedule era) where
  fromCBOR = decompactOverlaySchedule <$> fromCBOR

isOverlaySlot :: SlotNo -> OverlaySchedule c -> Bool
isOverlaySlot slot (OverlaySchedule oslots) = Map.member slot oslots
