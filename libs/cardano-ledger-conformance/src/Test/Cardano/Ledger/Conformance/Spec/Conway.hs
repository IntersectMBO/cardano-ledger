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
spec = describe "Conway conformance tests" $ do
  xprop "UTXO" $ conformsToImpl @"UTXO" @ConwayFn @Conway
  prop "GOV" $ conformsToImpl @"GOV" @ConwayFn @Conway
  prop "CERT" $ conformsToImpl @"CERT" @ConwayFn @Conway
  xprop "RATIFY" $ conformsToImpl @"RATIFY" @ConwayFn @Conway
  prop "GOVCERT" $ conformsToImpl @"GOVCERT" @ConwayFn @Conway
  xprop "ENACT" $ conformsToImpl @"ENACT" @ConwayFn @Conway
  describe "Generators" $ do
    let
      genEnv = do
        ctx <- genExecContext @ConwayFn @"GOV" @Conway
        CV2.genFromSpec_ $ environmentSpec @ConwayFn @"GOV" @Conway ctx
      genSt = do
        ctx <- genExecContext @ConwayFn @"GOV" @Conway
        env <- genEnv
        CV2.genFromSpec_ $ stateSpec @ConwayFn @"GOV" @Conway ctx env
      genSig = do
        ctx <- genExecContext @ConwayFn @"GOV" @Conway
        env <- genEnv
        st <- genSt
        CV2.genFromSpec_ $ signalSpec @ConwayFn @"GOV" @Conway ctx env st
    genEnv `generatesWithin` 3_000_000
    genSt `generatesWithin` 40_000_000
    genSig `generatesWithin` 60_000_000
