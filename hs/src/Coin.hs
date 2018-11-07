{-# LANGUAGE DerivingVia #-}

module Coin
    (
     Coin(..)
    , splitCoin
    ) where

import           Data.Monoid (Sum(..))
import           Numeric.Natural (Natural)

-- |The amount of value held by a transaction output.
newtype Coin = Coin Natural
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

splitCoin :: Coin -> Natural -> (Coin, Coin)
splitCoin (Coin n) 0 = (Coin 0, Coin n)
splitCoin (Coin n) m = (Coin $ n `div` m, Coin $ n `rem` m)
