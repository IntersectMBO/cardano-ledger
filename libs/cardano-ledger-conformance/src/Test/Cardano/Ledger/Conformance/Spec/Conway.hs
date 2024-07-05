{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Conway (spec) where

import Cardano.Ledger.Conway (Conway)
import qualified Constrained as CV2
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), conformsToImpl, generatesWithin)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.ImpTest ()
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = do
  describe "Generators" $ do
    let
      genEnv = do
        ctx <- genExecContext @ConwayFn @"GOV" @Conway
        CV2.genFromSpec $ environmentSpec @ConwayFn @"GOV" @Conway ctx
      genSt = do
        ctx <- genExecContext @ConwayFn @"GOV" @Conway
        env <- genEnv
        CV2.genFromSpec $ stateSpec @ConwayFn @"GOV" @Conway ctx env
      genSig = do
        ctx <- genExecContext @ConwayFn @"GOV" @Conway
        env <- genEnv
        st <- genSt
        CV2.genFromSpec $ signalSpec @ConwayFn @"GOV" @Conway ctx env st
    genEnv `generatesWithin` 3_000_000
    genSt `generatesWithin` 40_000_000
    genSig `generatesWithin` 60_000_000
  describe "Conformance" $ do
    describe "Ticks transition graph" $ do
      xprop "ENACT" $ conformsToImpl @"ENACT" @ConwayFn @Conway
      xprop "RATIFY" $ conformsToImpl @"RATIFY" @ConwayFn @Conway
      xprop "EPOCH" $ conformsToImpl @"EPOCH" @ConwayFn @Conway
      xprop "NEWEPOCH" $ conformsToImpl @"NEWEPOCH" @ConwayFn @Conway
    describe "Blocks transition graph" $ do
      prop "DELEG" $ conformsToImpl @"DELEG" @ConwayFn @Conway
      xprop "GOVCERT" $ conformsToImpl @"GOVCERT" @ConwayFn @Conway
      prop "POOL" $ conformsToImpl @"POOL" @ConwayFn @Conway
      prop "CERT" $ conformsToImpl @"CERT" @ConwayFn @Conway
      xprop "CERTS" $ conformsToImpl @"CERTS" @ConwayFn @Conway
      prop "GOV" $ conformsToImpl @"GOV" @ConwayFn @Conway
      xprop "UTXO" $ conformsToImpl @"UTXO" @ConwayFn @Conway
