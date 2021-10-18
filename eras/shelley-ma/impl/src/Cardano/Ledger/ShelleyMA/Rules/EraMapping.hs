{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.ShelleyMA.Rules.EraMapping () where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Bbody as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Epoch as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Mir as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Newpp as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Rupd as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Snap as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Tick as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Upec as Shelley
import Cardano.Ledger.ShelleyMA (ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Rules.Utxo (UTXO)
import Cardano.Ledger.ShelleyMA.Rules.Utxow (UTXOW)

-- These rules are all inherited from Shelley

type instance Core.EraRule "BBODY" (ShelleyMAEra ma c) = Shelley.BBODY (ShelleyMAEra ma c)

type instance Core.EraRule "DELEG" (ShelleyMAEra ma c) = Shelley.DELEG (ShelleyMAEra ma c)

type instance Core.EraRule "DELEGS" (ShelleyMAEra ma c) = Shelley.DELEGS (ShelleyMAEra ma c)

type instance Core.EraRule "DELPL" (ShelleyMAEra ma c) = Shelley.DELPL (ShelleyMAEra ma c)

type instance Core.EraRule "EPOCH" (ShelleyMAEra ma c) = Shelley.EPOCH (ShelleyMAEra ma c)

type instance Core.EraRule "LEDGER" (ShelleyMAEra ma c) = Shelley.LEDGER (ShelleyMAEra ma c)

type instance Core.EraRule "LEDGERS" (ShelleyMAEra ma c) = Shelley.LEDGERS (ShelleyMAEra ma c)

type instance Core.EraRule "MIR" (ShelleyMAEra ma c) = Shelley.MIR (ShelleyMAEra ma c)

type instance Core.EraRule "NEWEPOCH" (ShelleyMAEra ma c) = Shelley.NEWEPOCH (ShelleyMAEra ma c)

type instance Core.EraRule "NEWPP" (ShelleyMAEra ma c) = Shelley.NEWPP (ShelleyMAEra ma c)

type instance Core.EraRule "POOL" (ShelleyMAEra ma c) = Shelley.POOL (ShelleyMAEra ma c)

type instance Core.EraRule "POOLREAP" (ShelleyMAEra ma c) = Shelley.POOLREAP (ShelleyMAEra ma c)

type instance Core.EraRule "PPUP" (ShelleyMAEra ma c) = Shelley.PPUP (ShelleyMAEra ma c)

type instance Core.EraRule "RUPD" (ShelleyMAEra ma c) = Shelley.RUPD (ShelleyMAEra ma c)

type instance Core.EraRule "SNAP" (ShelleyMAEra ma c) = Shelley.SNAP (ShelleyMAEra ma c)

type instance Core.EraRule "TICK" (ShelleyMAEra ma c) = Shelley.TICK (ShelleyMAEra ma c)

type instance Core.EraRule "TICKF" (ShelleyMAEra ma c) = Shelley.TICKF (ShelleyMAEra ma c)

type instance Core.EraRule "TICKN" (ShelleyMAEra _ma _c) = Shelley.TICKN

type instance Core.EraRule "UPEC" (ShelleyMAEra ma c) = Shelley.UPEC (ShelleyMAEra ma c)

-- These rules are defined anew in the ShelleyMA era(s)

type instance Core.EraRule "UTXO" (ShelleyMAEra ma c) = UTXO (ShelleyMAEra ma c)

type instance Core.EraRule "UTXOW" (ShelleyMAEra ma c) = UTXOW (ShelleyMAEra ma c)
