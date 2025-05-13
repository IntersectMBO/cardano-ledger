module Test.Cardano.Ledger.Babbage.Imp.PlutusSpec (spec) where

import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: SpecWith (ImpInit (LedgerSpec era))
spec = describe "Plutus" $ do
  pure ()
