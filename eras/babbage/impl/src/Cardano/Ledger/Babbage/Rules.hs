{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules (
  module Cardano.Ledger.Babbage.Rules.Ledger,
  module Cardano.Ledger.Babbage.Rules.Utxo,
  module Cardano.Ledger.Babbage.Rules.Utxos,
  module Cardano.Ledger.Babbage.Rules.Utxow,
)
where

import Cardano.Ledger.Babbage.Core (EraRuleEvent)
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules.Bbody ()
import Cardano.Ledger.Babbage.Rules.Deleg ()
import Cardano.Ledger.Babbage.Rules.Delegs ()
import Cardano.Ledger.Babbage.Rules.Delpl ()
import Cardano.Ledger.Babbage.Rules.Ledger
import Cardano.Ledger.Babbage.Rules.Ledgers ()
import Cardano.Ledger.Babbage.Rules.Pool ()
import Cardano.Ledger.Babbage.Rules.Ppup ()
import Cardano.Ledger.Babbage.Rules.Utxo
import Cardano.Ledger.Babbage.Rules.Utxos
import Cardano.Ledger.Babbage.Rules.Utxow
import Cardano.Ledger.Shelley.Rules (PpupEvent, ShelleyLedgerEvent, ShelleyTickEvent)

type instance EraRuleEvent "TICK" (BabbageEra c) = ShelleyTickEvent (BabbageEra c)

type instance EraRuleEvent "LEDGER" (BabbageEra c) = ShelleyLedgerEvent (BabbageEra c)

type instance EraRuleEvent "PPUP" (BabbageEra c) = PpupEvent (BabbageEra c)
