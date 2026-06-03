{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Dijkstra (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Dijkstra ()
import Test.Cardano.Ledger.Conformance.Imp.Core
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
      modifyImpInitProtVer @DijkstraEra (natVersion @11) $
        modifyImpInitPostSubmitTxHook submitTxConformanceHook $ do
          modifyImpInitPostEpochBoundaryHook epochBoundaryConformanceHook $ do
            describe "LEDGER" LEDGER.spec
            describe "CERT" CERT.spec
            xdescribe "CERTS" CERTS.spec
            xdescribe "UTXOW" UTXOW.spec
            xdescribe "UTXO" UTXO.spec
