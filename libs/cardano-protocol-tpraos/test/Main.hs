module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Protocol.Binary.CddlSpec as Cddl

main :: IO ()
main =
  ledgerTestMain $
    describe "TPraos" $ do
      Cddl.spec
