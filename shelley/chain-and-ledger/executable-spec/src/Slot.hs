{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Slot
  ( SlotNo(..)
  , Duration(..)
  , (-*)
  , (+*)
  , (*-)
  , EpochNo(..)
  , EpochSize(..)
  , EpochInfo
  -- conversion between Byron / Shelley
  , slotByronToShelley
  , slotShelleyToByron
  -- Block number
  , BlockNo(..)
  , epochInfoEpoch
  , epochInfoFirst
  , epochInfoSize
  )
where

import           BaseTypes                      ( ShelleyBase )
import           Data.Functor.Identity          ( Identity )
import           Data.Word                      ( Word64 )
import           Cardano.Prelude                ( NoUnexpectedThunks(..) )
import           Cardano.Slotting.Block         ( BlockNo(..) )
import           Cardano.Slotting.Slot          ( SlotNo(..), EpochNo(..), EpochSize(..) )
import           Cardano.Slotting.EpochInfo     ( EpochInfo )
import qualified Cardano.Slotting.EpochInfo as EI
import           Control.Monad.Trans (lift)
import qualified Ledger.Core                   as Byron
                                                ( Slot(..) )

newtype Duration = Duration Word64
  deriving (Show, Eq, Ord, NoUnexpectedThunks, Num, Integral, Real, Enum)

instance Semigroup Duration where
  (Duration x) <> (Duration y) = Duration $ x + y

instance Monoid Duration where
  mempty  = Duration 0
  mappend = (<>)

(-*) :: SlotNo -> SlotNo -> Duration
(SlotNo s) -* (SlotNo t) = Duration (if s > t then s - t else t - s)

(+*) :: SlotNo -> Duration -> SlotNo
(SlotNo s) +* (Duration d) = SlotNo (s + d)

-- | Subtract a duration from a slot
(*-) :: SlotNo -> Duration -> SlotNo
(SlotNo s) *- (Duration d) = SlotNo (if s > d then s - d else 0)

-- | Convert `SlotNo` data from Byron to Shelley.
slotByronToShelley :: Byron.Slot -> SlotNo
slotByronToShelley (Byron.Slot s) = SlotNo s

slotShelleyToByron :: SlotNo -> Byron.Slot
slotShelleyToByron (SlotNo s) = Byron.Slot s

epochInfoEpoch :: EpochInfo Identity -> SlotNo -> ShelleyBase EpochNo
epochInfoEpoch ei = lift . EI.epochInfoEpoch ei

epochInfoFirst :: EpochInfo Identity -> EpochNo -> ShelleyBase SlotNo
epochInfoFirst ei = lift . EI.epochInfoFirst ei

epochInfoSize :: EpochInfo Identity -> EpochNo -> ShelleyBase EpochSize
epochInfoSize ei = lift . EI.epochInfoSize ei
