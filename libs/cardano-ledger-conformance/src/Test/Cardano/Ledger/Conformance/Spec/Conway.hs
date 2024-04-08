{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Conway (spec) where

import Cardano.Ledger.Conway (Conway)
import Test.Cardano.Ledger.Conformance (conformsToImpl)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.ImpTest (withImpState)
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = describe "Conway conformance tests" $ do
  withImpState @Conway $ do
    xit "UTXO" . replicateM_ 100 $ conformsToImpl @"UTXO" @ConwayFn
    xit "GOV" . replicateM_ 100 $ conformsToImpl @"GOV" @ConwayFn
