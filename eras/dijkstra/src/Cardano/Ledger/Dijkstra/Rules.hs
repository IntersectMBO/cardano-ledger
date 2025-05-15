{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules (
  module Cardano.Ledger.Dijkstra.Rules.Bbody,
  module Cardano.Ledger.Dijkstra.Rules.Cert,
  module Cardano.Ledger.Dijkstra.Rules.Deleg,
  module Cardano.Ledger.Dijkstra.Rules.GovCert,
  module Cardano.Ledger.Dijkstra.Rules.Certs,
  module Cardano.Ledger.Dijkstra.Rules.Enact,
  module Cardano.Ledger.Dijkstra.Rules.Epoch,
  module Cardano.Ledger.Dijkstra.Rules.HardFork,
  module Cardano.Ledger.Dijkstra.Rules.Ledger,
  module Cardano.Ledger.Dijkstra.Rules.Mempool,
  module Cardano.Ledger.Dijkstra.Rules.NewEpoch,
  module Cardano.Ledger.Dijkstra.Rules.Tickf,
  module Cardano.Ledger.Dijkstra.Rules.Ratify,
  module Cardano.Ledger.Dijkstra.Rules.Gov,
  module Cardano.Ledger.Dijkstra.Rules.Utxo,
  module Cardano.Ledger.Dijkstra.Rules.Utxos,
  module Cardano.Ledger.Dijkstra.Rules.Utxow,
)
where

import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, InjectRuleEvent (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Bbody
import Cardano.Ledger.Dijkstra.Rules.Cert
import Cardano.Ledger.Dijkstra.Rules.Certs
import Cardano.Ledger.Dijkstra.Rules.Deleg
import Cardano.Ledger.Dijkstra.Rules.Enact
import Cardano.Ledger.Dijkstra.Rules.Epoch
import Cardano.Ledger.Dijkstra.Rules.Gov
import Cardano.Ledger.Dijkstra.Rules.GovCert
import Cardano.Ledger.Dijkstra.Rules.HardFork
import Cardano.Ledger.Dijkstra.Rules.Ledger
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Dijkstra.Rules.Mempool
import Cardano.Ledger.Dijkstra.Rules.NewEpoch
import Cardano.Ledger.Dijkstra.Rules.Pool ()
import Cardano.Ledger.Dijkstra.Rules.Ratify
import Cardano.Ledger.Dijkstra.Rules.Tickf
import Cardano.Ledger.Dijkstra.Rules.Utxo
import Cardano.Ledger.Dijkstra.Rules.Utxos
import Cardano.Ledger.Dijkstra.Rules.Utxow
import Cardano.Ledger.Shelley.Rules (ShelleyTickEvent (..))

type instance EraRuleEvent "TICK" DijkstraEra = ShelleyTickEvent DijkstraEra

instance InjectRuleEvent "TICK" DijkstraEpochEvent DijkstraEra where
  injectEvent = TickNewEpochEvent . EpochEvent
