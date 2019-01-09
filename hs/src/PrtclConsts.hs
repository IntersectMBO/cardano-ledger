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
  , poolMinRefund
  , poolDecayRate
  ) where

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
  , _movingAvgWeight :: Rational
    -- |Moving average exponent.
  , _movingAvgExp    :: Rational
    -- |Pool constants
  , _poolConsts      :: (Rational, Natural)
    -- | The minimum percent pool refund
  , _poolMinRefund   :: Rational
    -- | Decay rate for pool deposits
  , _poolDecayRate   :: Rational
  } deriving (Show, Eq)

makeLenses ''PrtclConsts
