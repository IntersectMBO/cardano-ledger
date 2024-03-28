{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Test.Cardano.Ledger.Conway.Conformance (spec) where
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Conformance (conformsToImpl)
import Cardano.Ledger.Conway (Conway)
import Test.Cardano.Ledger.Conway.ImpTest (withImpState)
import Test.Cardano.Ledger.Conway.Constrained.Instances (ConwayFn)
import Test.Cardano.Ledger.Conway.Conformance.ExecutableSpecRule ()

spec :: Spec
spec = describe "Conway conformance tests" $ do
  withImpState @Conway $ do
    --it "UTXO" . replicateM_ 100 $ conformsToImpl @"UTXO" @ConwayFn
    it "GOV" . replicateM_ 100 $ conformsToImpl @"GOV" @ConwayFn
