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
      prop "DELEG" $ conformsToImpl @"DELEG" @ConwayFn @ConwayEra
      prop "GOVCERT" $ conformsToImpl @"GOVCERT" @ConwayFn @ConwayEra
      prop "POOL" $ conformsToImpl @"POOL" @ConwayFn @ConwayEra
      prop "CERT" $ conformsToImpl @"CERT" @ConwayFn @ConwayEra
      prop "CERTS" $ conformsToImpl @"CERTS" @ConwayFn @ConwayEra
      prop "GOV" $ conformsToImpl @"GOV" @ConwayFn @ConwayEra
      -- UTXO is disabled due to: https://github.com/IntersectMBO/cardano-ledger/issues/4876
      xprop "UTXO" $ conformsToImpl @"UTXO" @ConwayFn @ConwayEra
      xprop "UTXOW" $ conformsToImpl @"UTXOW" @ConwayFn @ConwayEra
      xprop "LEDGER" $ conformsToImpl @"LEDGER" @ConwayFn @ConwayEra
      xprop "LEDGERS" $ conformsToImpl @"LEDGERS" @ConwayFn @ConwayEra
    describe "ImpTests" $ do
      RatifyImp.spec
      Imp.spec
