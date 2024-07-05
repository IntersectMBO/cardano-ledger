{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (Conway)
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Conway.Binary.Regression as Regression
import qualified Test.Cardano.Ledger.Conway.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Conway.CommitteeRatifySpec as CommitteeRatify
import qualified Test.Cardano.Ledger.Conway.DRepRatifySpec as DRepRatify
import qualified Test.Cardano.Ledger.Conway.GenesisSpec as Genesis
import qualified Test.Cardano.Ledger.Conway.GovActionReorderSpec as GovActionReorder
import qualified Test.Cardano.Ledger.Conway.Imp as ConwayImp
import Test.Cardano.Ledger.Conway.Plutus.PlutusSpec as PlutusSpec
import qualified Test.Cardano.Ledger.Conway.Proposals as Proposals
import qualified Test.Cardano.Ledger.Conway.Spec as Spec
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)

main :: IO ()
main =
  ledgerTestMain $
    describe "Conway" $ do
      Spec.spec
      Proposals.spec
      Binary.spec
      Cddl.spec
      DRepRatify.spec
      CommitteeRatify.spec
      Genesis.spec
      GovActionReorder.spec
      roundTripJsonEraSpec @Conway
      describe "Imp" $ do
        ConwayImp.spec @Conway
      describe "CostModels" $ do
        CostModelsSpec.spec @Conway
      describe "TxWits" $ do
        TxWitsSpec.spec @Conway
      describe "Plutus" $ do
        PlutusSpec.spec
      Regression.spec @Conway
