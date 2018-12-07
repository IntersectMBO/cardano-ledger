module PrtlConsts
    (
     PrtlConsts(..)
    ) where

import           Data.Ratio      (Rational)
import           Numeric.Natural (Natural)

import           Coin            (Coin (..))

data PrtlConsts =
  PrtlConsts
  { -- |The linear factor for the minimum fee calculation
    _minfeeA     :: Natural
    -- |The constant factor for the minimum fee calculation
  , _minfeeB     :: Natural
    -- |The amount of a key registration deposit
  , _keyDeposit  :: Coin
    -- |The amount of a pool registration deposit
  , _poolDeposit :: Coin
    -- |The minimum percent refund guarantee
  , _minRefund   :: Rational
    -- |The deposit decay rate
  , _decayRate   :: Rational
  } deriving (Show, Eq)
