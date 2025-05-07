{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Core.Rational (
  module Test.Cardano.Ledger.Core.Rational,
  unsafeBoundRational,
) where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  NonNegativeInterval,
  PositiveInterval,
  PositiveUnitInterval,
  UnitInterval,
 )
import Cardano.Ledger.Core (unsafeBoundRational)
import qualified Data.Ratio
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)

-- | polymorphic rationals that agree with the Show instances of UnitInterval
-- and friends.
class IsRatio r where
  (%!) :: HasCallStack => Integer -> Integer -> r
  default (%!) :: (HasCallStack, Typeable r, BoundedRational r) => Integer -> Integer -> r
  n %! d = unsafeBoundRational $ n Data.Ratio.% d

instance IsRatio UnitInterval

instance IsRatio Rational where
  (%!) = (Data.Ratio.%)

instance IsRatio NonNegativeInterval

instance IsRatio PositiveInterval

instance IsRatio PositiveUnitInterval
