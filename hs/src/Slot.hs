{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Slot
  ( Slot(..)
  , Duration(..)
  , (-*), (+*)
  , Epoch(..)
  -- conversion functions
  , slotFromEpoch
  , epochFromSlot
  ) where

import           Data.Monoid             (Sum(..))
import           Numeric.Natural         (Natural)

-- |A Slot
newtype Slot = Slot Natural
  deriving (Show, Eq, Ord, Num)
  deriving (Semigroup, Monoid) via (Sum Natural)

newtype Duration = Duration Natural
  deriving (Show, Eq, Ord, Num, Integral, Real, Enum)
  deriving (Semigroup, Monoid) via (Sum Natural)

(-*) :: Slot -> Slot -> Duration
(Slot s) -* (Slot t) = Duration (if s > t then s - t else t - s)

(+*) :: Slot -> Duration -> Slot
(Slot s) +* (Duration d) = Slot (s + d)

-- |An Epoch
newtype Epoch = Epoch Natural
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

-- | Required to convert from / to Slot / Epoch
slotsPerEpoch :: Natural
slotsPerEpoch = 100

slotFromEpoch :: Epoch -> Slot
slotFromEpoch (Epoch n) = Slot $ slotsPerEpoch * n

epochFromSlot :: Slot -> Epoch
epochFromSlot (Slot n) = Epoch $ n `rem` slotsPerEpoch
