{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Dijkstra (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Dijkstra ()
import Test.Cardano.Ledger.Conformance.Imp.Core
import Test.Cardano.Ledger.Conway.Imp.BbodySpec qualified as ConwayBBODY
import Test.Cardano.Ledger.Conway.Imp.CertsSpec qualified as ConwayCERTS
import Test.Cardano.Ledger.Conway.Imp.DelegSpec qualified as ConwayDELEG
import Test.Cardano.Ledger.Conway.Imp.EnactSpec qualified as ConwayENACT
import Test.Cardano.Ledger.Conway.Imp.EpochSpec qualified as ConwayEPOCH
import Test.Cardano.Ledger.Conway.Imp.GovCertSpec qualified as ConwayGOVCERT
import Test.Cardano.Ledger.Conway.Imp.GovSpec qualified as ConwayGOV
import Test.Cardano.Ledger.Conway.Imp.LedgerSpec qualified as ConwayLEDGER
import Test.Cardano.Ledger.Conway.Imp.RatifySpec qualified as ConwayRATIFY
import Test.Cardano.Ledger.Conway.Imp.UtxoSpec qualified as ConwayUTXO
import Test.Cardano.Ledger.Conway.Imp.UtxosSpec qualified as ConwayUTXOS
import Test.Cardano.Ledger.Conway.Imp.UtxowSpec qualified as ConwayUTXOW
import Test.Cardano.Ledger.Dijkstra.Imp.CertSpec qualified as CERT
import Test.Cardano.Ledger.Dijkstra.Imp.CertsSpec qualified as CERTS
import Test.Cardano.Ledger.Dijkstra.Imp.LedgerSpec qualified as LEDGER
import Test.Cardano.Ledger.Dijkstra.Imp.UtxoSpec qualified as UTXO
import Test.Cardano.Ledger.Dijkstra.Imp.UtxowSpec qualified as UTXOW
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common hiding (Args)

spec :: Spec
spec = do
  describe "Imp" $ do
    withImpInit @(LedgerSpec DijkstraEra) $
      modifyImpInitProtVer @DijkstraEra (natVersion @12) $
        modifyImpInitPostSubmitTxHook submitTxConformanceHook $ do
          modifyImpInitPostEpochBoundaryHook epochBoundaryConformanceHook $ do
            ConwayBBODY.spec
            CERT.spec
            xdescribe "disabled" ConwayCERTS.spec
            CERTS.spec
            ConwayDELEG.spec
            ConwayENACT.spec
            ConwayEPOCH.spec
            ConwayGOV.spec
            ConwayGOVCERT.spec
            ConwayLEDGER.spec
            LEDGER.spec
            ConwayRATIFY.spec
            ConwayUTXO.spec
            UTXO.spec
            ConwayUTXOW.spec
            UTXOW.spec
            xdescribe "disabled" ConwayUTXOS.spec
