{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Rational where

import Cardano.Ledger.BaseTypes
  ( BoundedRational,
    NonNegativeInterval,
    UnitInterval,
    boundRational,
  )
import Data.Proxy (Proxy (..))
import qualified Data.Ratio
import Data.Typeable (Typeable, typeRep)
import GHC.Stack (HasCallStack)

unsafeFromRational :: forall r. (HasCallStack, Typeable r, BoundedRational r) => String -> Rational -> r
unsafeFromRational clue x = maybe (error $ "unsafeFromRational@" <> show (typeRep (Proxy :: Proxy r)) <> " " <> clue <> " out of bounds:" <> show x) id $ boundRational x

-- | polymorphic rationals that agree with the Show instances of UnitInterval
-- and friends.
class IsRatio r where
  (%) :: HasCallStack => Integer -> Integer -> r

instance IsRatio UnitInterval where
  n % d = unsafeFromRational "IsRatio.%" $ n Data.Ratio.% d

instance IsRatio Rational where
  (%) = (Data.Ratio.%)

instance IsRatio NonNegativeInterval where
  n % d = unsafeFromRational "IsRatio.%" $ n Data.Ratio.% d
