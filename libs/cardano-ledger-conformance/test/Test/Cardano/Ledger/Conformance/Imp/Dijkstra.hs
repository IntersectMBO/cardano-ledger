{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Dijkstra (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Dijkstra ()
import Test.Cardano.Ledger.Conformance.Imp.Core
import Test.Cardano.Ledger.Conway.Imp.BbodySpec qualified as ConwayBbody
import Test.Cardano.Ledger.Conway.Imp.CertsSpec qualified as ConwayCerts
import Test.Cardano.Ledger.Conway.Imp.DelegSpec qualified as ConwayDeleg
import Test.Cardano.Ledger.Conway.Imp.EnactSpec qualified as ConwayEnact
import Test.Cardano.Ledger.Conway.Imp.EpochSpec qualified as ConwayEpoch
import Test.Cardano.Ledger.Conway.Imp.GovCertSpec qualified as ConwayGovCert
import Test.Cardano.Ledger.Conway.Imp.GovSpec qualified as ConwayGov
import Test.Cardano.Ledger.Conway.Imp.LedgerSpec qualified as ConwayLedger
import Test.Cardano.Ledger.Conway.Imp.RatifySpec qualified as ConwayRatify
import Test.Cardano.Ledger.Conway.Imp.UtxoSpec qualified as ConwayUtxo
import Test.Cardano.Ledger.Conway.Imp.UtxosSpec qualified as ConwayUtxos
import Test.Cardano.Ledger.Conway.Imp.UtxowSpec qualified as ConwayUtxow
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
            LEDGER.spec
            CERT.spec
            CERTS.spec
            UTXOW.spec
            UTXO.spec
            describe "Conway" $ do
              ConwayBbody.spec
              xdescribe "disabled" ConwayCerts.spec
              ConwayDeleg.spec
              ConwayEnact.spec
              ConwayEpoch.spec
              ConwayGov.spec
              ConwayGovCert.spec
              ConwayLedger.spec
              ConwayRatify.spec
              ConwayUtxo.spec
              xdescribe "disabled" ConwayUtxos.spec
              ConwayUtxow.spec
