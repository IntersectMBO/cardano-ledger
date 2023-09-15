module Test.Cardano.Ledger.Core.Utils (
  unsafeBoundRational,
  epsilonMaybeEq,
)
where

import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Rational (unsafeBoundRational)

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
