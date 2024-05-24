{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Valid (spec) where

import Test.Cardano.Ledger.Alonzo.ImpTest (ImpTestState)
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  SpecWith (ImpTestState era)
spec = do
  describe "Valid transactions" $ do
    it "Validating SPEND script" $ do
      const $ pendingWith "not implemented yet"
    it "Not validating SPEND script" $ do
      const $ pendingWith "not implemented yet"
    it "Validating CERT script" $ do
      const $ pendingWith "not implemented yet"
    it "Not validating CERT script" $ do
      const $ pendingWith "not implemented yet"
    it "Validating WITHDRAWAL script" $ do
      const $ pendingWith "not implemented yet"
    it "Not validating WITHDRAWAL script" $ do
      const $ pendingWith "not implemented yet"
    it "Validating MINT script" $ do
      const $ pendingWith "not implemented yet"
    it "Not validating MINT script" $ do
      const $ pendingWith "not implemented yet"
    it "Validating scripts everywhere" $ do
      const $ pendingWith "not implemented yet"
    it "Acceptable supplimentary datum" $ do
      const $ pendingWith "not implemented yet"
    it "Multiple identical certificates" $ do
      const $ pendingWith "not implemented yet"
    it "Non-script output with datum" $ do
      const $ pendingWith "not implemented yet"
