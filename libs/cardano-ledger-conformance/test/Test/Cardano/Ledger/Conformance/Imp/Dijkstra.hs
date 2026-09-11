{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Dijkstra (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec qualified as AlonzoUTXO
import Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec qualified as AlonzoUTXOS
import Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec qualified as AlonzoUTXOW
import Test.Cardano.Ledger.Babbage.Imp.UtxoSpec qualified as BabbageUTXO
import Test.Cardano.Ledger.Babbage.Imp.UtxosSpec qualified as BabbageUTXOS
import Test.Cardano.Ledger.Babbage.Imp.UtxowSpec qualified as BabbageUTXOW
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
import Test.Cardano.Ledger.Conway.Imp.SnapSpec qualified as ConwaySNAP
import Test.Cardano.Ledger.Conway.Imp.UtxoSpec qualified as ConwayUTXO
import Test.Cardano.Ledger.Conway.Imp.UtxosSpec qualified as ConwayUTXOS
import Test.Cardano.Ledger.Conway.Imp.UtxowSpec qualified as ConwayUTXOW
import Test.Cardano.Ledger.Dijkstra.Imp.CertSpec qualified as CERT
import Test.Cardano.Ledger.Dijkstra.Imp.EntitiesSpec qualified as ENTITIES
import Test.Cardano.Ledger.Dijkstra.Imp.LedgerSpec qualified as LEDGER
import Test.Cardano.Ledger.Dijkstra.Imp.PoolSpec qualified as POOL
import Test.Cardano.Ledger.Dijkstra.Imp.SubUtxowSpec qualified as SUBUTXOW
import Test.Cardano.Ledger.Dijkstra.Imp.UtxoSpec qualified as UTXO
import Test.Cardano.Ledger.Dijkstra.Imp.UtxowSpec qualified as UTXOW
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common hiding (Args)
import Test.Cardano.Ledger.Mary.Imp.UtxoSpec qualified as MaryUTXO
import Test.Cardano.Ledger.Shelley.Imp.DelegSpec qualified as ShelleyDELEG
import Test.Cardano.Ledger.Shelley.Imp.EpochSpec qualified as ShelleyEPOCH
import Test.Cardano.Ledger.Shelley.Imp.LedgerSpec qualified as ShelleyLEDGER
import Test.Cardano.Ledger.Shelley.Imp.PoolSpec qualified as ShelleyPOOL
import Test.Cardano.Ledger.Shelley.Imp.UtxoSpec qualified as ShelleyUTXO
import Test.Cardano.Ledger.Shelley.Imp.UtxowSpec qualified as ShelleyUTXOW

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

            ENTITIES.spec

            ShelleyDELEG.spec
            ConwayDELEG.spec

            ConwayENACT.spec

            ShelleyEPOCH.spec
            ConwayEPOCH.spec

            ConwayGOV.spec

            ConwayGOVCERT.spec

            ShelleyLEDGER.spec
            ConwayLEDGER.spec
            LEDGER.spec

            ShelleyPOOL.spec
            POOL.spec

            ConwayRATIFY.spec

            ConwaySNAP.spec

            ShelleyUTXO.spec
            MaryUTXO.spec
            AlonzoUTXO.spec
            BabbageUTXO.spec
            ConwayUTXO.spec
            UTXO.spec

            ShelleyUTXOW.spec
            AlonzoUTXOW.spec
            BabbageUTXOW.spec
            ConwayUTXOW.spec
            UTXOW.spec
            SUBUTXOW.spec

            AlonzoUTXOS.spec
            BabbageUTXOS.spec
            ConwayUTXOS.spec
