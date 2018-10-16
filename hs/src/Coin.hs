module Coin
    (
     Coin(..)
    ) where

import           Numeric.Natural       (Natural)

-- |The amount of value held by a transaction output.
newtype Coin = Coin Natural deriving (Show, Eq, Ord)

instance Semigroup Coin where
  (Coin a) <> (Coin b) = Coin (a + b)

instance Monoid Coin where
  mempty = Coin 0
  mappend = (<>)
