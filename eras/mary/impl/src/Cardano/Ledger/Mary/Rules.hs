{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules () where

import Cardano.Ledger.Mary.Core (EraRuleEvent)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Bbody ()
import Cardano.Ledger.Mary.Rules.Deleg ()
import Cardano.Ledger.Mary.Rules.Delegs ()
import Cardano.Ledger.Mary.Rules.Delpl ()
import Cardano.Ledger.Mary.Rules.Ledger ()
import Cardano.Ledger.Mary.Rules.Ledgers ()
import Cardano.Ledger.Mary.Rules.Pool ()
import Cardano.Ledger.Mary.Rules.Ppup ()
import Cardano.Ledger.Mary.Rules.Utxo ()
import Cardano.Ledger.Mary.Rules.Utxow ()
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleEvent "TICK" MaryEra = Shelley.ShelleyTickEvent MaryEra
