{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides the mapping from conceptual rule names to concrete
-- rules for the Shelley era.
module Shelley.Spec.Ledger.STS.EraMapping () where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Shelley.Spec.Ledger.STS.Bbody (BBODY)
import Shelley.Spec.Ledger.STS.Deleg (DELEG)
import Shelley.Spec.Ledger.STS.Delegs (DELEGS)
import Shelley.Spec.Ledger.STS.Delpl (DELPL)
import Shelley.Spec.Ledger.STS.Epoch (EPOCH)
import Shelley.Spec.Ledger.STS.Ledger (LEDGER)
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS)
import Shelley.Spec.Ledger.STS.Mir (MIR)
import Shelley.Spec.Ledger.STS.NewEpoch (NEWEPOCH)
import Shelley.Spec.Ledger.STS.Newpp (NEWPP)
import Shelley.Spec.Ledger.STS.Ocert (OCERT)
import Shelley.Spec.Ledger.STS.Overlay (OVERLAY)
import Shelley.Spec.Ledger.STS.Pool (POOL)
import Shelley.Spec.Ledger.STS.PoolReap (POOLREAP)
import Shelley.Spec.Ledger.STS.Ppup (PPUP)
import Shelley.Spec.Ledger.STS.Rupd (RUPD)
import Shelley.Spec.Ledger.STS.Snap (SNAP)
import Shelley.Spec.Ledger.STS.Tick (TICK, TICKF)
import Shelley.Spec.Ledger.STS.Tickn (TICKN)
import Shelley.Spec.Ledger.STS.Upec (UPEC)
import Shelley.Spec.Ledger.STS.Utxo (UTXO)
import Shelley.Spec.Ledger.STS.Utxow (UTXOW)

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

type instance Core.EraRule "OCERT" (ShelleyEra c) = OCERT (ShelleyEra c)

type instance Core.EraRule "OVERLAY" (ShelleyEra c) = OVERLAY (ShelleyEra c)

type instance Core.EraRule "POOL" (ShelleyEra c) = POOL (ShelleyEra c)

type instance Core.EraRule "POOLREAP" (ShelleyEra c) = POOLREAP (ShelleyEra c)

type instance Core.EraRule "PPUP" (ShelleyEra c) = PPUP (ShelleyEra c)

type instance Core.EraRule "RUPD" (ShelleyEra c) = RUPD (ShelleyEra c)

type instance Core.EraRule "SNAP" (ShelleyEra c) = SNAP (ShelleyEra c)

type instance Core.EraRule "TICK" (ShelleyEra c) = TICK (ShelleyEra c)

type instance Core.EraRule "TICKF" (ShelleyEra c) = TICKF (ShelleyEra c)

type instance Core.EraRule "TICKN" (ShelleyEra c) = TICKN

type instance Core.EraRule "UPEC" (ShelleyEra c) = UPEC (ShelleyEra c)

type instance Core.EraRule "UTXO" (ShelleyEra c) = UTXO (ShelleyEra c)

type instance Core.EraRule "UTXOW" (ShelleyEra c) = UTXOW (ShelleyEra c)
