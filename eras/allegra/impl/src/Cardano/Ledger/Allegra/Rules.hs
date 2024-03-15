{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Allegra.Rules (
  module Cardano.Ledger.Allegra.Rules.Utxo,
  module Cardano.Ledger.Allegra.Rules.Utxow,
)
where

import Cardano.Ledger.Allegra.Core (EraRuleEvent)
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Bbody ()
import Cardano.Ledger.Allegra.Rules.Deleg ()
import Cardano.Ledger.Allegra.Rules.Delegs ()
import Cardano.Ledger.Allegra.Rules.Delpl ()
import Cardano.Ledger.Allegra.Rules.Ledger ()
import Cardano.Ledger.Allegra.Rules.Ledgers ()
import Cardano.Ledger.Allegra.Rules.Pool ()
import Cardano.Ledger.Allegra.Rules.Ppup ()
import Cardano.Ledger.Allegra.Rules.Utxo
import Cardano.Ledger.Allegra.Rules.Utxow
import Cardano.Ledger.Shelley.Rules (ShelleyTickEvent)

type instance EraRuleEvent "TICK" (AllegraEra c) = ShelleyTickEvent (AllegraEra c)
