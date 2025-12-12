{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules (
  module Cardano.Ledger.Dijkstra.Rules.Bbody,
  module Cardano.Ledger.Dijkstra.Rules.Gov,
  module Cardano.Ledger.Dijkstra.Rules.GovCert,
  module Cardano.Ledger.Dijkstra.Rules.Ledger,
  module Cardano.Ledger.Dijkstra.Rules.Mempool,
  module Cardano.Ledger.Dijkstra.Rules.Utxo,
  module Cardano.Ledger.Dijkstra.Rules.Utxow,
  module Control.State.Transition.Extended,
) where

import Cardano.Ledger.Conway.Rules (
  ConwayEpochEvent (..),
  ConwayHardForkEvent,
  ConwayNewEpochEvent (..),
 )
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, InjectRuleEvent (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Bbody
import Cardano.Ledger.Dijkstra.Rules.Cert ()
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.Deleg ()
import Cardano.Ledger.Dijkstra.Rules.Gov
import Cardano.Ledger.Dijkstra.Rules.GovCert
import Cardano.Ledger.Dijkstra.Rules.Ledger
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Dijkstra.Rules.Mempool
import Cardano.Ledger.Dijkstra.Rules.Pool ()
import Cardano.Ledger.Dijkstra.Rules.Utxo
import Cardano.Ledger.Dijkstra.Rules.Utxos ()
import Cardano.Ledger.Dijkstra.Rules.Utxow
import Cardano.Ledger.Shelley.Rules (ShelleyTickEvent (..))
import Control.State.Transition.Extended (STS (PredicateFailure))

type instance EraRuleEvent "TICK" DijkstraEra = ShelleyTickEvent DijkstraEra

type instance EraRuleEvent "NEWEPOCH" DijkstraEra = ConwayNewEpochEvent DijkstraEra

type instance EraRuleEvent "EPOCH" DijkstraEra = ConwayEpochEvent DijkstraEra

type instance EraRuleEvent "HARDFORK" DijkstraEra = ConwayHardForkEvent DijkstraEra

instance InjectRuleEvent "TICK" ConwayNewEpochEvent DijkstraEra where
  injectEvent = TickNewEpochEvent

instance InjectRuleEvent "TICK" ConwayEpochEvent DijkstraEra where
  injectEvent = TickNewEpochEvent . injectEvent

instance InjectRuleEvent "TICK" ConwayHardForkEvent DijkstraEra where
  injectEvent = TickNewEpochEvent . injectEvent

instance InjectRuleEvent "NEWEPOCH" ConwayEpochEvent DijkstraEra where
  injectEvent = EpochEvent

instance InjectRuleEvent "NEWEPOCH" ConwayHardForkEvent DijkstraEra where
  injectEvent = EpochEvent . injectEvent

instance InjectRuleEvent "EPOCH" ConwayHardForkEvent DijkstraEra where
  injectEvent = HardForkEvent
