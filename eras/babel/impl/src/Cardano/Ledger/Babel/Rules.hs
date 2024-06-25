{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules (
  module Cardano.Ledger.Babel.Rules.Ledger,
  module Cardano.Ledger.Babel.Rules.Ledgers,
  module Cardano.Ledger.Babel.Rules.Utxo,
  module Cardano.Ledger.Babel.Rules.Utxos,
  module Cardano.Ledger.Babel.Rules.Utxow,
  module Cardano.Ledger.Babel.Rules.Zone,
  module Cardano.Ledger.Babel.Rules.Zones,
)
where

import Cardano.Ledger.Babel.Core (EraRuleEvent, InjectRuleEvent (..))
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Rules.Bbody ()
import Cardano.Ledger.Babel.Rules.Ledger
import Cardano.Ledger.Babel.Rules.Ledgers
import Cardano.Ledger.Babel.Rules.Pool ()
import Cardano.Ledger.Babel.Rules.Utxo
import Cardano.Ledger.Babel.Rules.Utxos
import Cardano.Ledger.Babel.Rules.Utxow
import Cardano.Ledger.Babel.Rules.Zone
import Cardano.Ledger.Babel.Rules.Zones
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Shelley.Rules (ShelleyTickEvent (..))

type instance EraRuleEvent "TICK" (BabelEra c) = ShelleyTickEvent (BabelEra c)

instance InjectRuleEvent "TICK" ConwayEpochEvent (BabelEra c) where
  injectEvent = TickNewEpochEvent . EpochEvent
