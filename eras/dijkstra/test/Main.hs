{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Dijkstra (DijkstraEra)

main :: IO ()
main =
  ledgerTestMain $
    describe "Dijkstra" $ do
      GoldenTranslation.spec
      Golden.spec
      Spec.spec
      Proposals.spec
      Binary.spec
      Cddl.spec
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

