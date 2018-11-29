module PrtlConsts
    (
     PrtlConsts(..)
    ) where

import Coin (Coin(..))

data PrtlConsts =
  PrtlConsts
  { -- |The linear factor for the minimum fee calculation
    minfeeA :: Coin
    -- |The constant factor for the minimum fee calculation
  , minfeeB :: Coin
  } deriving (Show, Eq)
