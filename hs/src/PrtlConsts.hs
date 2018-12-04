module PrtlConsts
    (
     PrtlConsts(..)
    ) where

import           Data.Ratio      (Rational)
import           Numeric.Natural (Natural)

import           Lovelace        (Lovelace (..))

data PrtlConsts =
  PrtlConsts
  { -- |The linear factor for the minimum fee calculation
    minfeeA     :: Natural
    -- |The constant factor for the minimum fee calculation
  , minfeeB     :: Natural
    -- |The amount of a key registration deposit
  , keyDeposit  :: Lovelace
    -- |The amount of a pool registration deposit
  , poolDeposit :: Lovelace
    -- |The minimum percent refund guarantee
  , minRefund   :: Rational
    -- |The deposit decay rate
  , decayRate   :: Rational
  } deriving (Show, Eq)
