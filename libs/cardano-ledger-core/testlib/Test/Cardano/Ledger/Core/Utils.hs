module Test.Cardano.Ledger.Core.Utils
  ( unsafeBoundRational,
  )
where

import Cardano.Ledger.BaseTypes
import Data.Maybe (fromMaybe)
import GHC.Stack

-- | Convert to a bounded rational type why throwing an error on failure
unsafeBoundRational :: (HasCallStack, BoundedRational r) => Rational -> r
unsafeBoundRational r =
  fromMaybe (error $ "Could not convert from Rational: " ++ show r) $ boundRational r
