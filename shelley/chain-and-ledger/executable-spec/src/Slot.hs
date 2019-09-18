{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Slot
  ( Slot(..)
  , Duration(..)
  , (-*)
  , (+*)
  , (*-)
  , Epoch(..)
  -- conversion functions
  , slotFromEpoch
  , epochFromSlot
  , slotsPerEpoch
  , firstSlot
  -- conversion between Byron / Shelley
  , slotByronToShelley
  , slotShelleyToByron
  -- Block number
  , BlockNo(..)
  )
where

import           Data.Word                      ( Word64 )
import           Numeric.Natural                ( Natural )
import           GHC.Generics                   ( Generic )
import           Cardano.Binary                 (ToCBOR(..))
import           Cardano.Prelude                ( NoUnexpectedThunks(..) )

import qualified Ledger.Core                   as Byron
                                                ( Slot(..) )

-- |A Slot
newtype Slot = Slot Natural
  deriving (Show, Eq, Ord, NoUnexpectedThunks, Num)

instance ToCBOR Slot where
  toCBOR (Slot s) = toCBOR (fromInteger (toInteger s) :: Word64)

instance Semigroup Slot where
  (Slot x) <> (Slot y) = Slot $ x + y

instance Monoid Slot where
  mempty  = Slot 0
  mappend = (<>)

newtype Duration = Duration Natural
  deriving (Show, Eq, Ord, NoUnexpectedThunks, Num, Integral, Real, Enum)

instance Semigroup Duration where
  (Duration x) <> (Duration y) = Duration $ x + y

instance Monoid Duration where
  mempty  = Duration 0
  mappend = (<>)

(-*) :: Slot -> Slot -> Duration
(Slot s) -* (Slot t) = Duration (if s > t then s - t else t - s)

(+*) :: Slot -> Duration -> Slot
(Slot s) +* (Duration d) = Slot (s + d)

-- | Subtract a duration from a slot
(*-) :: Slot -> Duration -> Slot
(Slot s) *- (Duration d) = Slot (if s > d then s - d else 0)

-- |An Epoch
newtype Epoch = Epoch Natural
  deriving (Show, Eq, NoUnexpectedThunks, Ord, ToCBOR)

instance Semigroup Epoch where
  (Epoch x) <> (Epoch y) = Epoch $ x + y

instance Monoid Epoch where
  mempty  = Epoch 0
  mappend = (<>)

slotFromEpoch :: Epoch -> Slot
slotFromEpoch (Epoch n) = Slot $ slotsPerEpoch * n

epochFromSlot :: Slot -> Epoch
epochFromSlot (Slot n) = Epoch $ n `div` slotsPerEpoch

firstSlot :: Epoch -> Slot
firstSlot = slotFromEpoch

-- | Hard coded global constant for number of slots per epoch
slotsPerEpoch :: Natural
slotsPerEpoch = 100

-- | Convert `Slot` data from Byron to Shelley, there should be a check that
-- Shelley slots fit into `Word64` used in Byron.
slotByronToShelley :: Byron.Slot -> Slot
slotByronToShelley (Byron.Slot s) = Slot $ fromIntegral s

slotShelleyToByron :: Slot -> Byron.Slot
slotShelleyToByron (Slot s) = Byron.Slot $ fromIntegral s

newtype BlockNo = BlockNo { unBlockNo :: Word64 }
  deriving stock   (Show, Eq, Ord, Generic)
  deriving newtype (Enum, Bounded, Num, NoUnexpectedThunks)
