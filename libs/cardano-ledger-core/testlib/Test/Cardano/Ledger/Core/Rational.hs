{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Core.Rational where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  NonNegativeInterval,
  PositiveInterval,
  PositiveUnitInterval,
  UnitInterval,
  boundRational,
 )
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Ratio
import Data.Typeable (Typeable, typeRep)
import GHC.Stack (HasCallStack)

unsafeBoundRational :: forall r. (HasCallStack, Typeable r, BoundedRational r) => Rational -> r
unsafeBoundRational x = fromMaybe (error errMessage) $ boundRational x
  where
    errMessage = show (typeRep (Proxy :: Proxy r)) <> " is out of bounds: " <> show x

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
