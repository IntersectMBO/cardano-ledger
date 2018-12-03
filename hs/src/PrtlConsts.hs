module PrtlConsts
    (
     PrtlConsts(..)
    ) where

import           Data.Ratio (Rational)

import           Coin (Coin(..))

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
  , minRefund :: Rational
    -- |The deposit decay rate
  , decayRate :: Rational
  } deriving (Show, Eq)
