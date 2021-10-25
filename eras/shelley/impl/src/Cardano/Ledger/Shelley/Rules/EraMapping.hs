{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides the mapping from conceptual rule names to concrete
-- rules for the Shelley era.
module Cardano.Ledger.Shelley.Rules.EraMapping () where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Rules.Bbody (BBODY)
import Cardano.Ledger.Shelley.Rules.Deleg (DELEG)
import Cardano.Ledger.Shelley.Rules.Delegs (DELEGS)
import Cardano.Ledger.Shelley.Rules.Delpl (DELPL)
import Cardano.Ledger.Shelley.Rules.Epoch (EPOCH)
import Cardano.Ledger.Shelley.Rules.Ledger (LEDGER)
import Cardano.Ledger.Shelley.Rules.Ledgers (LEDGERS)
import Cardano.Ledger.Shelley.Rules.Mir (MIR)
import Cardano.Ledger.Shelley.Rules.NewEpoch (NEWEPOCH)
import Cardano.Ledger.Shelley.Rules.Newpp (NEWPP)
import Cardano.Ledger.Shelley.Rules.Pool (POOL)
import Cardano.Ledger.Shelley.Rules.PoolReap (POOLREAP)
import Cardano.Ledger.Shelley.Rules.Ppup (PPUP)
import Cardano.Ledger.Shelley.Rules.Rupd (RUPD)
import Cardano.Ledger.Shelley.Rules.Snap (SNAP)
import Cardano.Ledger.Shelley.Rules.Tick (TICK, TICKF)
import Cardano.Ledger.Shelley.Rules.Upec (UPEC)
import Cardano.Ledger.Shelley.Rules.Utxo (UTXO)
import Cardano.Ledger.Shelley.Rules.Utxow (UTXOW)

type instance Core.EraRule "BBODY" (ShelleyEra c) = BBODY (ShelleyEra c)

type instance Core.EraRule "DELEG" (ShelleyEra c) = DELEG (ShelleyEra c)

type instance Core.EraRule "DELEGS" (ShelleyEra c) = DELEGS (ShelleyEra c)

type instance Core.EraRule "DELPL" (ShelleyEra c) = DELPL (ShelleyEra c)

type instance Core.EraRule "EPOCH" (ShelleyEra c) = EPOCH (ShelleyEra c)

type instance Core.EraRule "LEDGER" (ShelleyEra c) = LEDGER (ShelleyEra c)

type instance Core.EraRule "LEDGERS" (ShelleyEra c) = LEDGERS (ShelleyEra c)

type instance Core.EraRule "MIR" (ShelleyEra c) = MIR (ShelleyEra c)

type instance Core.EraRule "NEWEPOCH" (ShelleyEra c) = NEWEPOCH (ShelleyEra c)

type instance Core.EraRule "NEWPP" (ShelleyEra c) = NEWPP (ShelleyEra c)

type instance Core.EraRule "POOL" (ShelleyEra c) = POOL (ShelleyEra c)

type instance Core.EraRule "POOLREAP" (ShelleyEra c) = POOLREAP (ShelleyEra c)

type instance Core.EraRule "PPUP" (ShelleyEra c) = PPUP (ShelleyEra c)

type instance Core.EraRule "RUPD" (ShelleyEra c) = RUPD (ShelleyEra c)

type instance Core.EraRule "SNAP" (ShelleyEra c) = SNAP (ShelleyEra c)

type instance Core.EraRule "TICK" (ShelleyEra c) = TICK (ShelleyEra c)

type instance Core.EraRule "TICKF" (ShelleyEra c) = TICKF (ShelleyEra c)

type instance Core.EraRule "UPEC" (ShelleyEra c) = UPEC (ShelleyEra c)

type instance Core.EraRule "UTXO" (ShelleyEra c) = UTXO (ShelleyEra c)

type instance Core.EraRule "UTXOW" (ShelleyEra c) = UTXOW (ShelleyEra c)
