{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.ImpTest () where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Governance ()
import Cardano.Ledger.Dijkstra.Translation ()
import Cardano.Ledger.Dijkstra.TxBody ()
import Cardano.Ledger.Plutus (SLanguage (..))
import Test.Cardano.Ledger.Conway.ImpTest
import Cardano.Ledger.Conway.Rules ()

instance ShelleyEraImp DijkstraEra where
  initGenesis = pure ()

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = babbageFixupTx
  expectTxSuccess = impBabbageExpectTxSuccess

instance MaryEraImp DijkstraEra

instance AlonzoEraImp DijkstraEra where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3

instance ConwayEraImp DijkstraEra
