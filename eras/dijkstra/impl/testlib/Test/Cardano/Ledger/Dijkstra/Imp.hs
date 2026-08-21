{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Imp where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition (Event)
import Data.Default (Default)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp as ConwayImp
import qualified Test.Cardano.Ledger.Dijkstra.Imp.CertSpec as CERT
import qualified Test.Cardano.Ledger.Dijkstra.Imp.EntitiesSpec as ENTITIES
import qualified Test.Cardano.Ledger.Dijkstra.Imp.LedgerSpec as LEDGER
import qualified Test.Cardano.Ledger.Dijkstra.Imp.PoolSpec as POOL
import qualified Test.Cardano.Ledger.Dijkstra.Imp.UtxoSpec as UTXO
import qualified Test.Cardano.Ledger.Dijkstra.Imp.UtxowSpec as UTXOW
import Test.Cardano.Ledger.Dijkstra.ImpTest

spec ::
  ( DijkstraEraImp era
  , Event (EraRule "EPOCH" era) ~ Conway.ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ Conway.ConwayNewEpochEvent era
  , Event (EraRule "HARDFORK" era) ~ Conway.ConwayHardForkEvent era
  , Event (EraRule "RUPD" era) ~ Shelley.RupdEvent
  , Default (LevelTxInfo TopTx era)
  ) =>
  proxy era ->
  Spec
spec era = do
  ConwayImp.spec era
  describe "DijkstraEra Onwards" $ withImpInitEachEraVersion era $ do
    LEDGER.spec
    CERT.spec
    ENTITIES.spec
    POOL.spec
    UTXOW.spec
    UTXO.spec
