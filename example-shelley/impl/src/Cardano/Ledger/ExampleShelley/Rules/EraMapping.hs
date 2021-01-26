{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.ExampleShelley.Rules.EraMapping () where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.ExampleShelley (ExampleShelleyEra)
import Cardano.Ledger.ExampleShelley.Rules.Utxo (UTXO)
import Cardano.Ledger.ExampleShelley.Rules.Utxow (UTXOW)
import qualified Shelley.Spec.Ledger.API as Shelley
import qualified Shelley.Spec.Ledger.STS.Bbody as Shelley
import qualified Shelley.Spec.Ledger.STS.Epoch as Shelley
import qualified Shelley.Spec.Ledger.STS.Mir as Shelley
import qualified Shelley.Spec.Ledger.STS.Newpp as Shelley
import qualified Shelley.Spec.Ledger.STS.Ocert as Shelley
import qualified Shelley.Spec.Ledger.STS.Overlay as Shelley
import qualified Shelley.Spec.Ledger.STS.Rupd as Shelley
import qualified Shelley.Spec.Ledger.STS.Snap as Shelley
import qualified Shelley.Spec.Ledger.STS.Tick as Shelley
import qualified Shelley.Spec.Ledger.STS.Upec as Shelley

-- These rules are all inherited from Shelley

type instance Core.EraRule "BBODY" (ExampleShelleyEra c) = Shelley.BBODY (ExampleShelleyEra c)

type instance Core.EraRule "DELEG" (ExampleShelleyEra c) = Shelley.DELEG (ExampleShelleyEra c)

type instance Core.EraRule "DELEGS" (ExampleShelleyEra c) = Shelley.DELEGS (ExampleShelleyEra c)

type instance Core.EraRule "DELPL" (ExampleShelleyEra c) = Shelley.DELPL (ExampleShelleyEra c)

type instance Core.EraRule "EPOCH" (ExampleShelleyEra c) = Shelley.EPOCH (ExampleShelleyEra c)

type instance Core.EraRule "LEDGER" (ExampleShelleyEra c) = Shelley.LEDGER (ExampleShelleyEra c)

type instance Core.EraRule "LEDGERS" (ExampleShelleyEra c) = Shelley.LEDGERS (ExampleShelleyEra c)

type instance Core.EraRule "MIR" (ExampleShelleyEra c) = Shelley.MIR (ExampleShelleyEra c)

type instance Core.EraRule "NEWEPOCH" (ExampleShelleyEra c) = Shelley.NEWEPOCH (ExampleShelleyEra c)

type instance Core.EraRule "NEWPP" (ExampleShelleyEra c) = Shelley.NEWPP (ExampleShelleyEra c)

type instance Core.EraRule "OCERT" (ExampleShelleyEra c) = Shelley.OCERT (ExampleShelleyEra c)

type instance Core.EraRule "OVERLAY" (ExampleShelleyEra c) = Shelley.OVERLAY (ExampleShelleyEra c)

type instance Core.EraRule "POOL" (ExampleShelleyEra c) = Shelley.POOL (ExampleShelleyEra c)

type instance Core.EraRule "POOLREAP" (ExampleShelleyEra c) = Shelley.POOLREAP (ExampleShelleyEra c)

type instance Core.EraRule "PPUP" (ExampleShelleyEra c) = Shelley.PPUP (ExampleShelleyEra c)

type instance Core.EraRule "RUPD" (ExampleShelleyEra c) = Shelley.RUPD (ExampleShelleyEra c)

type instance Core.EraRule "SNAP" (ExampleShelleyEra c) = Shelley.SNAP (ExampleShelleyEra c)

type instance Core.EraRule "TICK" (ExampleShelleyEra c) = Shelley.TICK (ExampleShelleyEra c)

type instance Core.EraRule "TICKF" (ExampleShelleyEra c) = Shelley.TICKF (ExampleShelleyEra c)

type instance Core.EraRule "TICKN" (ExampleShelley _c) = Shelley.TICKN

type instance Core.EraRule "UPEC" (ExampleShelleyEra c) = Shelley.UPEC (ExampleShelleyEra c)

-- These rules are defined anew in the ShelleyMA era(s)

type instance Core.EraRule "UTXO" (ExampleShelleyEra c) = UTXO (ExampleShelleyEra c)

type instance Core.EraRule "UTXOW" (ExampleShelleyEra c) = UTXOW (ExampleShelleyEra c)
