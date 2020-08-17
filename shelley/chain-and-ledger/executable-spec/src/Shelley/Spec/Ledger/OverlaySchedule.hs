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
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Keys
  ( KeyHash (..),
    KeyRole (..),
  )
import Shelley.Spec.Ledger.PParams (PParams, _d)
import Shelley.Spec.Ledger.Slot

data OBftSlot crypto
  = NonActiveSlot
  | ActiveSlot !(KeyHash 'Genesis crypto)
  deriving (Show, Eq, Ord, Generic)

instance
  Crypto crypto =>
  ToCBOR (OBftSlot crypto)
  where
  toCBOR NonActiveSlot = encodeNull
  toCBOR (ActiveSlot k) = toCBOR k

instance
  Crypto crypto =>
  FromCBOR (OBftSlot crypto)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure NonActiveSlot
      _ -> ActiveSlot <$> fromCBOR

instance NoUnexpectedThunks (OBftSlot crypto)

instance NFData (OBftSlot crypto)

newtype OverlaySchedule crypto = OverlaySchedule (Map SlotNo (OBftSlot crypto))
  deriving stock (Show, Eq)
  deriving newtype (NoUnexpectedThunks, NFData)

emptyOverlaySchedule :: OverlaySchedule crypto
emptyOverlaySchedule = OverlaySchedule Map.empty

lookupInOverlaySchedule ::
  SlotNo ->
  OverlaySchedule crypto ->
  Maybe (OBftSlot crypto)
lookupInOverlaySchedule slot (OverlaySchedule oSched) = Map.lookup slot oSched

overlayScheduleIsEmpty :: OverlaySchedule crypto -> Bool
overlayScheduleIsEmpty (OverlaySchedule oSched) = Map.null oSched

-- | Overlay schedule
-- This is just a very simple round-robin, evenly spaced schedule.
overlaySchedule ::
  EpochNo ->
  Set (KeyHash 'Genesis crypto) ->
  PParams ->
  ShelleyBase (OverlaySchedule crypto)
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
  Set (KeyHash 'Genesis crypto) ->
  -- | Decentralization parameter @d@
  UnitInterval ->
  ActiveSlotCoeff ->
  OverlaySchedule crypto
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

overlayScheduleToMap :: OverlaySchedule crypto -> Map SlotNo (OBftSlot crypto)
overlayScheduleToMap (OverlaySchedule oSched) = oSched

-- | Convert the overlay schedule to a representation that is more compact
-- when serialised to a bytestring, but less efficient for lookups.
--
-- Each genesis key hash will only be stored once, instead of each time it is
-- assigned to a slot.
compactOverlaySchedule ::
  OverlaySchedule crypto ->
  Map (OBftSlot crypto) (NonEmpty SlotNo)
compactOverlaySchedule (OverlaySchedule oSched) =
  Map.foldrWithKey'
    ( \slot obftSlot ->
        Map.insertWith (<>) obftSlot (pure slot)
    )
    Map.empty
    oSched

-- | Inverse of 'compactOverlaySchedule'
decompactOverlaySchedule ::
  Map (OBftSlot crypto) (NonEmpty SlotNo) ->
  OverlaySchedule crypto
decompactOverlaySchedule compact =
  OverlaySchedule $
    Map.fromList
      [ (slot, obftSlot)
        | (obftSlot, slots) <- Map.toList compact,
          slot <- NonEmpty.toList slots
      ]

instance Crypto crypto => ToCBOR (OverlaySchedule crypto) where
  toCBOR = toCBOR . compactOverlaySchedule

instance Crypto crypto => FromCBOR (OverlaySchedule crypto) where
  fromCBOR = decompactOverlaySchedule <$> fromCBOR

isOverlaySlot :: SlotNo -> OverlaySchedule c -> Bool
isOverlaySlot slot (OverlaySchedule oslots) = Map.member slot oslots
