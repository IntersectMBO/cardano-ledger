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
import Test.Cardano.Ledger.Conway.ImpTest ()
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = do
  describe "MiniTrace" MiniTrace.spec
  describe "Generators" $ do
    inputsGenerateWithin @"GOV" @ConwayEra 60_000_000
    inputsGenerateWithin @"ENACT" @ConwayEra 60_000_000
  describe "Conformance" $ do
    describe "Ticks transition graph" $ do
      prop "ENACT" $ conformsToImpl @"ENACT" @ConwayEra
      prop "RATIFY" $ conformsToImpl @"RATIFY" @ConwayEra
      xprop "EPOCH" $ conformsToImpl @"EPOCH" @ConwayEra
      xprop "NEWEPOCH" $ conformsToImpl @"NEWEPOCH" @ConwayEra
    describe "Blocks transition graph" $ do
      prop "DELEG" $ conformsToImpl @"DELEG" @ConwayEra
      prop "GOVCERT" $ conformsToImpl @"GOVCERT" @ConwayEra
      prop "POOL" $ conformsToImpl @"POOL" @ConwayEra
      prop "CERT" $ conformsToImpl @"CERT" @ConwayEra
      xprop "CERTS" $ conformsToImpl @"CERTS" @ConwayEra
      prop "GOV" $ conformsToImpl @"GOV" @ConwayEra
      -- UTXO is disabled due to: https://github.com/IntersectMBO/cardano-ledger/issues/4876
      xprop "UTXO" $ conformsToImpl @"UTXO" @ConwayEra
      xprop "UTXOW" $ conformsToImpl @"UTXOW" @ConwayEra
      xprop "LEDGER" $ conformsToImpl @"LEDGER" @ConwayEra
      xprop "LEDGERS" $ conformsToImpl @"LEDGERS" @ConwayEra
    describe "ImpTests" $ do
      RatifyImp.spec
      Imp.spec
