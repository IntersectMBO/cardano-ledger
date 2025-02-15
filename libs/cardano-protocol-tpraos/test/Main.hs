module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Protocol.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Protocol.Binary.RoundTrip as RoundTrip

main :: IO ()
main =
  ledgerTestMain $
    describe "TPraos" $ do
      Cddl.spec
      RoundTrip.spec
