{-# LANGUAGE TemplateHaskell #-}

module PParams
  ( PParams(..)
    -- lenses
  , minfeeA
  , minfeeB
  , keyDeposit
  , poolDeposit
  , minRefund
  , decayRate
  , movingAvgWeight
  , eMax
  , movingAvgExp
  , poolConsts
  , poolMinRefund
  , poolDecayRate
  , rho
  , tau
  , intervalValue
  , interval0
  , interval1
  , emptyPParams
  , maxBHSize
  , maxBBSize
  , activeSlotCoeff
  ) where

import           Numeric.Natural (Natural)

import           BaseTypes
import           Coin            (Coin (..))
import           Slot            (Epoch(..))

import           Lens.Micro.TH   (makeLenses)

data PParams = PParams
  { -- |The linear factor for the minimum fee calculation
    _minfeeA         :: Integer
    -- |The constant factor for the minimum fee calculation
  , _minfeeB         :: Integer
    -- |The amount of a key registration deposit
  , _keyDeposit      :: Coin
    -- |The amount of a pool registration deposit
  , _poolDeposit     :: Coin
    -- |The minimum percent refund guarantee
  , _minRefund       :: UnitInterval
    -- |The deposit decay rate
  , _decayRate       :: Rational
    -- |Moving average weight.
  , _movingAvgWeight :: UnitInterval
    -- |Moving average exponent.
  , _movingAvgExp    :: Rational
    -- | epoch bound on pool retirement
  , _eMax            :: Epoch
    -- |Pool constants
  , _poolConsts      :: (Rational, Natural)
    -- | The minimum percent pool refund
  , _poolMinRefund   :: UnitInterval
    -- | Decay rate for pool deposits
  , _poolDecayRate   :: Rational
    -- | Account transition parameter
  , _rho             :: UnitInterval
  , _tau             :: UnitInterval
    -- | Maximal block header size
  , _maxBHSize       :: Integer
    -- | Maximal block body size
  , _maxBBSize       :: Integer
    -- | Active slot coefficient
  , _activeSlotCoeff :: UnitInterval
  } deriving (Show, Eq)

makeLenses ''PParams

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: PParams
emptyPParams =
    PParams 0 0 (Coin 0) (Coin 0) interval0 0 interval0 0 (Epoch 0) (0, 0) interval0 0 interval0 interval0 0 0 interval0
