{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.LegacyOverlay
  ( legacyOverlayTest,
  )
where

import Cardano.Slotting.Slot
import Cardano.Ledger.Era (Era)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Proxy
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Set (Set)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Keys
  ( KeyHash (..),
    KeyRole (..),
  )
import Shelley.Spec.Ledger.OverlaySchedule (OBftSlot (..), classifyOverlaySlot, overlaySlots)
import Shelley.Spec.Ledger.Slot
import Test.Shelley.Spec.Ledger.Examples.Federation (genDelegs)
import Test.Shelley.Spec.Ledger.Utils
import Test.Tasty.QuickCheck

legacyOverlay ::
  EpochSize ->
  SlotNo ->
  -- | First slot of the epoch
  Set (KeyHash 'Genesis era) ->
  UnitInterval ->
  -- | Decentralization parameter @d@
  ActiveSlotCoeff ->
  Map SlotNo (OBftSlot era)
legacyOverlay slotsPerEpoch firstSlotNo gkeys d asc
  | dval == 0 = Map.empty
  | otherwise = Map.union active inactive
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

mainnetEpochSize :: EpochSize
mainnetEpochSize = EpochSize 432000

makeConcreteOverlay ::
  SlotNo -> -- first slot of the epoch
  Set (KeyHash 'Genesis era) -> -- genesis Nodes
  UnitInterval -> -- decentralization parameter
  ActiveSlotCoeff -> -- active slot coefficent
  EpochSize -> -- slots per epoch
  Map SlotNo (OBftSlot era)
makeConcreteOverlay start gkeys dval asc spe =
  Map.fromList $
    map
      (\s -> (s, classifyOverlaySlot start gkeys dval asc s))
      (overlaySlots start dval spe)

legacyOverlayTest :: forall era. Era era => Proxy era -> Property
legacyOverlayTest _proxy = property $ do
  d <- choose (0, 100)
  e <- choose (0, 100)
  let dval = unsafeMkUnitInterval (d % 100)
      asc = mkActiveSlotCoeff . unsafeMkUnitInterval $ 1 % 20
      EpochSize spe = mainnetEpochSize
      start = SlotNo $ e * spe
      os =
        legacyOverlay
          mainnetEpochSize
          start
          (Map.keysSet (genDelegs @era))
          dval
          asc
  pure $ os === makeConcreteOverlay start (Map.keysSet (genDelegs @era)) dval asc mainnetEpochSize
