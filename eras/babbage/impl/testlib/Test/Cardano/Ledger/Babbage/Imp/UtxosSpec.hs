module Test.Cardano.Ledger.Babbage.Imp.UtxosSpec (spec) where

import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $ do
  describe "Plutus V1 with references" $ do
    it "fails with a reference script" $ do
      const $ pendingWith "not implemented yet"

    it "fails with a reference input" $ do
      const $ pendingWith "not implemented yet"
