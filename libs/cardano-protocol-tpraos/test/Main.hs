module Main where

import qualified Test.Cardano.Protocol.Binary.CddlSpec as Cddl
import Test.Cardano.Ledger.Common

main :: IO ()
main =
  ledgerTestMain $
    describe "TPraos" $ do
      Cddl.spec
