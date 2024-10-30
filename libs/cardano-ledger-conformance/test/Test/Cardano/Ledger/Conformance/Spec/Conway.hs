{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Conway (spec) where

import Cardano.Ledger.Conway (ConwayEra)
import Test.Cardano.Ledger.Conformance (conformsToImpl, inputsGenerateWithin)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway ()
import Test.Cardano.Ledger.Conformance.ExecSpecRule.MiniTrace qualified as MiniTrace
import Test.Cardano.Ledger.Conformance.Imp qualified as Imp (spec)
import Test.Cardano.Ledger.Conformance.Imp.Ratify qualified as RatifyImp
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.ImpTest ()
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = do
  describe "MiniTrace" MiniTrace.spec
  describe "Generators" $ do
    inputsGenerateWithin @ConwayFn @"GOV" @ConwayEra 60_000_000
    inputsGenerateWithin @ConwayFn @"ENACT" @ConwayEra 60_000_000
  describe "Conformance" $ do
    describe "Ticks transition graph" $ do
      prop "ENACT" $ conformsToImpl @"ENACT" @ConwayFn @ConwayEra
      prop "RATIFY" $ conformsToImpl @"RATIFY" @ConwayFn @ConwayEra
      xprop "EPOCH" $ conformsToImpl @"EPOCH" @ConwayFn @ConwayEra
      xprop "NEWEPOCH" $ conformsToImpl @"NEWEPOCH" @ConwayFn @ConwayEra
    describe "Blocks transition graph" $ do
      prop "DELEG" $ conformsToImpl @"DELEG" @ConwayFn @Conway
      prop "GOVCERT" $ conformsToImpl @"GOVCERT" @ConwayFn @Conway
      prop "POOL" $ conformsToImpl @"POOL" @ConwayFn @Conway
      -- prop "CERT" $ conformsToImpl @"CERT" @ConwayFn @Conway
      -- prop "CERTS" $ conformsToImpl @"CERTS" @ConwayFn @Conway
      prop "GOV" $ conformsToImpl @"GOV" @ConwayFn @Conway
      prop "UTXO" $ conformsToImpl @"UTXO" @ConwayFn @Conway
      xprop "UTXOW" $ conformsToImpl @"UTXOW" @ConwayFn @Conway
      xprop "LEDGER" $ conformsToImpl @"LEDGER" @ConwayFn @Conway
      xprop "LEDGERS" $ conformsToImpl @"LEDGERS" @ConwayFn @Conway
    describe "ImpTests" $ do
      RatifyImp.spec
      Imp.spec
