{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Conformance (spec) where

import Cardano.Ledger.Conway (Conway)
import Test.Cardano.Ledger.Conformance (conformsToImpl)
import Test.Cardano.Ledger.Conway.Conformance.ExecutableSpecRule ()
import Test.Cardano.Ledger.Conway.Constrained.Instances (ConwayFn)
import Test.Cardano.Ledger.Conway.ImpTest (withImpState)
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = describe "Conway conformance tests" $ do
  withImpState @Conway $ do
    xit "UTXO" . replicateM_ 100 $ conformsToImpl @"UTXO" @ConwayFn
    xit "GOV" . replicateM_ 100 $ conformsToImpl @"GOV" @ConwayFn
