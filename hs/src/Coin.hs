module Coin
    (
     Coin(..)
    , splitCoin
    ) where

import           Numeric.Natural (Natural)

-- |The amount of value held by a transaction output.
newtype Coin = Coin Natural deriving (Show, Eq, Ord)

instance Semigroup Coin where
  (Coin a) <> (Coin b) = Coin (a + b)

instance Monoid Coin where
  mempty = Coin 0
  mappend = (<>)

splitCoin :: Coin -> Natural -> (Coin, Coin)
splitCoin (Coin n) 0 = (Coin 0, Coin n)
splitCoin (Coin n) m = (Coin $ n `div` m, Coin $ n `rem` m)
