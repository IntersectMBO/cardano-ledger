{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Conway (spec) where

import Cardano.Ledger.Conway (Conway)
import Test.Cardano.Ledger.Conformance (conformsToImpl, inputsGenerateWithin)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway ()
import qualified Test.Cardano.Ledger.Conformance.ExecSpecRule.MiniTrace as MiniTrace
import qualified Test.Cardano.Ledger.Conformance.Imp.Ratify as RatifyImp
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.ImpTest ()
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = do
  describe "MiniTrace" MiniTrace.spec
  describe "Generators" $ do
    inputsGenerateWithin @ConwayFn @"GOV" @Conway 60_000_000
    inputsGenerateWithin @ConwayFn @"ENACT" @Conway 60_000_000
  describe "Conformance" $ do
    describe "Ticks transition graph" $ do
      prop "ENACT" $ conformsToImpl @"ENACT" @ConwayFn @Conway
      prop "RATIFY" $ conformsToImpl @"RATIFY" @ConwayFn @Conway
      xprop "EPOCH" $ conformsToImpl @"EPOCH" @ConwayFn @Conway
      xprop "NEWEPOCH" $ conformsToImpl @"NEWEPOCH" @ConwayFn @Conway
    describe "Blocks transition graph" $ do
      prop "DELEG" $ conformsToImpl @"DELEG" @ConwayFn @Conway
      prop "GOVCERT" $ conformsToImpl @"GOVCERT" @ConwayFn @Conway
      prop "POOL" $ conformsToImpl @"POOL" @ConwayFn @Conway
      prop "CERT" $ conformsToImpl @"CERT" @ConwayFn @Conway
      prop "CERTS" $ conformsToImpl @"CERTS" @ConwayFn @Conway
      prop "GOV" $ conformsToImpl @"GOV" @ConwayFn @Conway
      prop "UTXO" $ conformsToImpl @"UTXO" @ConwayFn @Conway
    describe "ImpTests" $ do
      RatifyImp.spec
