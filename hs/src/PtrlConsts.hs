module PtrlConsts
    (
     PtrlConsts(..)
    ) where

import Coin (Coin(..))

data PtrlConsts =
  PtrlConsts
  { -- |The linear factor for the minimum fee calculation
    minfeeA :: Coin
    -- |The constant factor for the minimum fee calculation
  , minfeeB :: Coin
  } deriving (Show, Eq)
