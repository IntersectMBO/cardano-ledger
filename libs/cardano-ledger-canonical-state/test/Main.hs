module Main (
  main,
) where

import qualified Test.Cardano.Ledger.CanonicalState.Spec
import Test.Cardano.Ledger.Common

main :: IO ()
main = ledgerTestMain $ do
  Test.Cardano.Ledger.CanonicalState.Spec.spec
