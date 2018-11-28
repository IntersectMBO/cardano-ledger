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
    -- |The amount of a key registration deposit
  , keyDeposit :: Coin
    -- |The amount of a pool registration deposit
  , poolDeposit :: Coin
    -- |The minimum percent refund guarantee
  , minRefund :: Float
    -- |The deposit decay rate
  , decayRate :: Float
  } deriving (Show, Eq)
