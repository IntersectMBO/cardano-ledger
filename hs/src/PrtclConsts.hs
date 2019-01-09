{-# LANGUAGE TemplateHaskell #-}

module PrtclConsts
  ( PrtclConsts(..)
    -- lenses
  , minfeeA
  , minfeeB
  , keyDeposit
  , poolDeposit
  , minRefund
  , decayRate
  , movingAvgWeight
  , movingAvgExp
  , poolConsts
  ) where

import           Data.Ratio      (Ratio, Rational)
import           Numeric.Natural (Natural)

import           Coin            (Coin (..))

import           Lens.Micro.TH   (makeLenses)

data PrtclConsts = PrtclConsts
  { -- |The linear factor for the minimum fee calculation
    _minfeeA         :: Natural
    -- |The constant factor for the minimum fee calculation
  , _minfeeB         :: Natural
    -- |The amount of a key registration deposit
  , _keyDeposit      :: Coin
    -- |The amount of a pool registration deposit
  , _poolDeposit     :: Coin
    -- |The minimum percent refund guarantee
  , _minRefund       :: Rational
    -- |The deposit decay rate
  , _decayRate       :: Rational
    -- |Moving average weight.
  , _movingAvgWeight :: Ratio Natural
    -- |Moving average exponent.
  , _movingAvgExp    :: Ratio Natural
    -- |Pool constants
  , _poolConsts      :: (Ratio Natural, Natural)
  } deriving (Show, Eq)

makeLenses ''PrtclConsts
