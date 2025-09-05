{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules () where

import Cardano.Ledger.Conway.Rules (ConwayEpochEvent, ConwayNewEpochEvent (..))
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, InjectRuleEvent (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Bbody ()
import Cardano.Ledger.Dijkstra.Rules.Cert ()
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.Deleg ()
import Cardano.Ledger.Dijkstra.Rules.Gov ()
import Cardano.Ledger.Dijkstra.Rules.GovCert ()
import Cardano.Ledger.Dijkstra.Rules.Ledger ()
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Dijkstra.Rules.Pool ()
import Cardano.Ledger.Dijkstra.Rules.Utxo ()
import Cardano.Ledger.Dijkstra.Rules.Utxos ()
import Cardano.Ledger.Dijkstra.Rules.Utxow ()
import Cardano.Ledger.Shelley.Rules (ShelleyTickEvent (..))

type instance EraRuleEvent "TICK" DijkstraEra = ShelleyTickEvent DijkstraEra

instance InjectRuleEvent "TICK" ConwayEpochEvent DijkstraEra where
  injectEvent = TickNewEpochEvent . EpochEvent
