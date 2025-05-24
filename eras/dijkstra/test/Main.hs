{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules ()
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Conway.Binary.Regression as Regression
import qualified Test.Cardano.Ledger.Conway.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Conway.CommitteeRatifySpec as CommitteeRatify
import qualified Test.Cardano.Ledger.Conway.DRepRatifySpec as DRepRatify
import qualified Test.Cardano.Ledger.Conway.GenesisSpec as Genesis
import Test.Cardano.Ledger.Conway.GoldenSpec as Golden
import qualified Test.Cardano.Ledger.Conway.GovActionReorderSpec as GovActionReorder
import qualified Test.Cardano.Ledger.Conway.Imp as Imp
import Test.Cardano.Ledger.Conway.Plutus.PlutusSpec as PlutusSpec
import qualified Test.Cardano.Ledger.Conway.Proposals as Proposals
import qualified Test.Cardano.Ledger.Conway.SPORatifySpec as SPORatifySpec
import qualified Test.Cardano.Ledger.Conway.TxInfoSpec as TxInfo
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.ImpTest ()

main :: IO ()
main =
  ledgerTestMain $
    describe "Dijkstra" $ do
      Golden.spec
      Proposals.spec
      Binary.spec
      Cddl.spec @DijkstraEra
      DRepRatify.spec
      CommitteeRatify.spec
      SPORatifySpec.spec
      Genesis.spec
      GovActionReorder.spec
      roundTripJsonEraSpec @DijkstraEra
      describe "Imp" $
        Imp.spec @DijkstraEra
      describe "CostModels" $ do
        CostModelsSpec.spec @DijkstraEra
      describe "TxWits" $ do
        TxWitsSpec.spec @DijkstraEra
      describe "Plutus" $ do
        PlutusSpec.spec
      Regression.spec @DijkstraEra
      TxInfo.spec
