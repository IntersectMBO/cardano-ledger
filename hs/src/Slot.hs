{-# LANGUAGE DerivingVia #-}

module Slot
  ( Slot(..)
  , Epoch(..)
  ) where

import           Data.Monoid             (Sum(..))
import           Numeric.Natural         (Natural)

-- |A Slot
newtype Slot = Slot Natural
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

-- |An Epoch
newtype Epoch = Epoch Natural
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

