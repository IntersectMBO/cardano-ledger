{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules (
  module Cardano.Ledger.Dijkstra.Rules.Bbody,
  module Cardano.Ledger.Dijkstra.Rules.Entities,
  module Cardano.Ledger.Dijkstra.Rules.Gov,
  module Cardano.Ledger.Dijkstra.Rules.GovCert,
  module Cardano.Ledger.Dijkstra.Rules.Ledger,
  module Cardano.Ledger.Dijkstra.Rules.Mempool,
  module Cardano.Ledger.Dijkstra.Rules.SubCert,
  module Cardano.Ledger.Dijkstra.Rules.SubCerts,
  module Cardano.Ledger.Dijkstra.Rules.SubDeleg,
  module Cardano.Ledger.Dijkstra.Rules.SubEntities,
  module Cardano.Ledger.Dijkstra.Rules.SubGovCert,
  module Cardano.Ledger.Dijkstra.Rules.SubGov,
  module Cardano.Ledger.Dijkstra.Rules.SubLedger,
  module Cardano.Ledger.Dijkstra.Rules.SubLedgers,
  module Cardano.Ledger.Dijkstra.Rules.SubPool,
  module Cardano.Ledger.Dijkstra.Rules.SubUtxo,
  module Cardano.Ledger.Dijkstra.Rules.SubUtxow,
  module Cardano.Ledger.Dijkstra.Rules.Utxo,
  module Cardano.Ledger.Dijkstra.Rules.Utxow,
  module Control.State.Transition.Extended,
) where

import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, InjectRuleEvent (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Bbody
import Cardano.Ledger.Dijkstra.Rules.Cert ()
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.Deleg ()
import Cardano.Ledger.Dijkstra.Rules.Entities
import Cardano.Ledger.Dijkstra.Rules.Gov
import Cardano.Ledger.Dijkstra.Rules.GovCert
import Cardano.Ledger.Dijkstra.Rules.Ledger
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Dijkstra.Rules.Mempool
import Cardano.Ledger.Dijkstra.Rules.Pool ()
import Cardano.Ledger.Dijkstra.Rules.SubCert
import Cardano.Ledger.Dijkstra.Rules.SubCerts
import Cardano.Ledger.Dijkstra.Rules.SubDeleg
import Cardano.Ledger.Dijkstra.Rules.SubEntities
import Cardano.Ledger.Dijkstra.Rules.SubGov
import Cardano.Ledger.Dijkstra.Rules.SubGovCert
import Cardano.Ledger.Dijkstra.Rules.SubLedger
import Cardano.Ledger.Dijkstra.Rules.SubLedgers
import Cardano.Ledger.Dijkstra.Rules.SubPool
import Cardano.Ledger.Dijkstra.Rules.SubUtxo
import Cardano.Ledger.Dijkstra.Rules.SubUtxow
import Cardano.Ledger.Dijkstra.Rules.Utxo
import Cardano.Ledger.Dijkstra.Rules.Utxos ()
import Cardano.Ledger.Dijkstra.Rules.Utxow
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition.Extended (STS (PredicateFailure))

type instance EraRuleEvent "TICK" DijkstraEra = Shelley.ShelleyTickEvent DijkstraEra

type instance EraRuleEvent "NEWEPOCH" DijkstraEra = Conway.ConwayNewEpochEvent DijkstraEra

type instance EraRuleEvent "EPOCH" DijkstraEra = Conway.ConwayEpochEvent DijkstraEra

type instance EraRuleEvent "HARDFORK" DijkstraEra = Conway.ConwayHardForkEvent DijkstraEra

instance InjectRuleEvent "TICK" Conway.ConwayNewEpochEvent DijkstraEra where
  injectEvent = Shelley.TickNewEpochEvent

instance InjectRuleEvent "TICK" Conway.ConwayEpochEvent DijkstraEra where
  injectEvent = Shelley.TickNewEpochEvent . injectEvent

instance InjectRuleEvent "TICK" Conway.ConwayHardForkEvent DijkstraEra where
  injectEvent = Shelley.TickNewEpochEvent . injectEvent

instance InjectRuleEvent "NEWEPOCH" Conway.ConwayEpochEvent DijkstraEra where
  injectEvent = Conway.EpochEvent

instance InjectRuleEvent "NEWEPOCH" Conway.ConwayHardForkEvent DijkstraEra where
  injectEvent = Conway.EpochEvent . injectEvent

instance InjectRuleEvent "EPOCH" Conway.ConwayHardForkEvent DijkstraEra where
  injectEvent = Conway.HardForkEvent
