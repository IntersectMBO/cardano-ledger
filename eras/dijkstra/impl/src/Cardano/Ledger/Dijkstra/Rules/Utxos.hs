{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxos () where

import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra.Core (
  EraRuleEvent,
  EraRuleFailure,
  InjectRuleEvent (..),
  InjectRuleFailure (..),
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)

type instance EraRuleFailure "UTXOS" DijkstraEra = Conway.ConwayUtxosPredFailure DijkstraEra

type instance EraRuleEvent "UTXOS" DijkstraEra = Conway.ConwayUtxosEvent DijkstraEra

instance InjectRuleFailure "UTXOS" Conway.ConwayUtxosPredFailure DijkstraEra

instance InjectRuleEvent "UTXOS" Conway.ConwayUtxosEvent DijkstraEra

instance InjectRuleFailure "UTXOS" Alonzo.AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = Conway.alonzoToConwayUtxosPredFailure

instance InjectRuleEvent "UTXOS" Alonzo.AlonzoUtxosEvent DijkstraEra where
  injectEvent = Conway.alonzoToConwayUtxosEvent
