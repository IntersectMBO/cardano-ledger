{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules (
  module Cardano.Ledger.Conway.Rules.Bbody,
  module Cardano.Ledger.Conway.Rules.Cert,
  module Cardano.Ledger.Conway.Rules.Deleg,
  module Cardano.Ledger.Conway.Rules.GovCert,
  module Cardano.Ledger.Conway.Rules.Certs,
  module Cardano.Ledger.Conway.Rules.Enact,
  module Cardano.Ledger.Conway.Rules.Epoch,
  module Cardano.Ledger.Conway.Rules.Ledger,
  module Cardano.Ledger.Conway.Rules.Mempool,
  module Cardano.Ledger.Conway.Rules.NewEpoch,
  module Cardano.Ledger.Conway.Rules.Tickf,
  module Cardano.Ledger.Conway.Rules.Ratify,
  module Cardano.Ledger.Conway.Rules.Gov,
  module Cardano.Ledger.Conway.Rules.Utxo,
  module Cardano.Ledger.Conway.Rules.Utxos,
  module Cardano.Ledger.Conway.Rules.Utxow,
)
where

import Cardano.Ledger.Conway.Core (EraRuleEvent, InjectRuleEvent (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Rules.Bbody
import Cardano.Ledger.Conway.Rules.Cert
import Cardano.Ledger.Conway.Rules.Certs
import Cardano.Ledger.Conway.Rules.Deleg
import Cardano.Ledger.Conway.Rules.Enact
import Cardano.Ledger.Conway.Rules.Epoch
import Cardano.Ledger.Conway.Rules.Gov
import Cardano.Ledger.Conway.Rules.GovCert
import Cardano.Ledger.Conway.Rules.Ledger
import Cardano.Ledger.Conway.Rules.Ledgers ()
import Cardano.Ledger.Conway.Rules.Mempool
import Cardano.Ledger.Conway.Rules.NewEpoch
import Cardano.Ledger.Conway.Rules.Pool ()
import Cardano.Ledger.Conway.Rules.Ratify
import Cardano.Ledger.Conway.Rules.Tickf
import Cardano.Ledger.Conway.Rules.Utxo
import Cardano.Ledger.Conway.Rules.Utxos
import Cardano.Ledger.Conway.Rules.Utxow
import Cardano.Ledger.Shelley.Rules (ShelleyTickEvent (..))

type instance EraRuleEvent "TICK" (ConwayEra c) = ShelleyTickEvent (ConwayEra c)

instance InjectRuleEvent "TICK" ConwayEpochEvent (ConwayEra c) where
  injectEvent = TickNewEpochEvent . EpochEvent
