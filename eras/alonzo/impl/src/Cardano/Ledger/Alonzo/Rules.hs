{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Rules (
  module Cardano.Ledger.Alonzo.Rules.Bbody,
  module Cardano.Ledger.Alonzo.Rules.Ledger,
  module Cardano.Ledger.Alonzo.Rules.Utxo,
  module Cardano.Ledger.Alonzo.Rules.Utxos,
  module Cardano.Ledger.Alonzo.Rules.Utxow,
)
where

import Cardano.Ledger.Alonzo.Core (EraRuleEvent)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Bbody
import Cardano.Ledger.Alonzo.Rules.Deleg ()
import Cardano.Ledger.Alonzo.Rules.Delegs ()
import Cardano.Ledger.Alonzo.Rules.Delpl ()
import Cardano.Ledger.Alonzo.Rules.Ledger
import Cardano.Ledger.Alonzo.Rules.Ledgers ()
import Cardano.Ledger.Alonzo.Rules.Pool ()
import Cardano.Ledger.Alonzo.Rules.Ppup ()
import Cardano.Ledger.Alonzo.Rules.Utxo
import Cardano.Ledger.Alonzo.Rules.Utxos
import Cardano.Ledger.Alonzo.Rules.Utxow
import Cardano.Ledger.Shelley.Rules (ShelleyLedgerEvent, ShelleyTickEvent)

type instance EraRuleEvent "LEDGER" (AlonzoEra c) = ShelleyLedgerEvent (AlonzoEra c)
type instance EraRuleEvent "TICK" (AlonzoEra c) = ShelleyTickEvent (AlonzoEra c)
