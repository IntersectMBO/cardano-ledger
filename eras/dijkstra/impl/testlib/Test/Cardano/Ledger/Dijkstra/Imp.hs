{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Imp where

import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Shelley.Rules
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp as ConwayImp
import qualified Test.Cardano.Ledger.Dijkstra.Imp.CertSpec as Cert
import qualified Test.Cardano.Ledger.Dijkstra.Imp.CertsSpec as Certs
import qualified Test.Cardano.Ledger.Dijkstra.Imp.LedgerSpec as Ledger
import qualified Test.Cardano.Ledger.Dijkstra.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Dijkstra.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Dijkstra.ImpTest

spec ::
  ( DijkstraEraImp era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  ConwayImp.spec era
  describe "DijkstraEra Onwards" $ withImpInitEachEraVersion era $ do
    Ledger.spec
    Cert.spec
    Certs.spec
    Utxow.spec
    Utxo.spec
