module Test.Cardano.Ledger.Core.Utils (
  unsafeBoundRational,
  epsilonMaybeEq,
  Fail (..),
  runFailError,
)
where

import Cardano.Ledger.Address (Fail (..))
import Cardano.Ledger.BaseTypes (BoundedRational (boundRational))
import Data.Maybe (fromMaybe)
import Test.Cardano.Ledger.Common

runFailError :: HasCallStack => Fail a -> a
runFailError = either error id . runFail

-- | Convert to a bounded rational type why throwing an error on failure
unsafeBoundRational :: (HasCallStack, BoundedRational r) => Rational -> r
unsafeBoundRational r =
  fromMaybe (error $ "Could not convert from Rational: " ++ show r) $ boundRational r

-- | This test for equality takes into account magnitude of the arguments
epsilonMaybeEq ::
  -- | Epsilon, a maximum tolerated error. Sign is ignored.
  Rational ->
  -- | Expected result.
  Rational ->
  -- | Tested value.
  Rational ->
  Property
epsilonMaybeEq epsilon x y =
  counterexample
    ( concat
        [show x, " /= ", show y, " (Tolerance: ", show diff, " > ", show n, ")"]
    )
    (classify True "Exactly" (x === y) .||. diff <= n)
  where
    (absx, absy) = (abs x, abs y)
    n = epsilon * (1 + max absx absy)
    diff = abs (y - x)
