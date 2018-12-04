{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Lovelace
    (
     Lovelace(..)
    , splitLovelace
    ) where

import           Data.Monoid (Sum(..))
import           Numeric.Natural (Natural)

-- |The amount of value held by a transaction output.
newtype Lovelace = Lovelace Natural
  deriving (Show, Eq, Ord, Num, Integral, Real, Enum)
  deriving (Semigroup, Monoid) via (Sum Natural)

splitLovelace :: Lovelace -> Natural -> (Lovelace, Lovelace)
splitLovelace (Lovelace n) 0 = (Lovelace 0, Lovelace n)
splitLovelace (Lovelace n) m = (Lovelace $ n `div` m, Lovelace $ n `rem` m)
