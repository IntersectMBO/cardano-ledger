{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides the mapping from conceptual rule names to concrete
-- rules for the Shelley era.
module Cardano.Ledger.Shelley.Rules.EraMapping () where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Rules.Bbody (ShelleyBBODY)
import Cardano.Ledger.Shelley.Rules.Deleg (ShelleyDELEG)
import Cardano.Ledger.Shelley.Rules.Delegs (ShelleyDELEGS)
import Cardano.Ledger.Shelley.Rules.Delpl (ShelleyDELPL)
import Cardano.Ledger.Shelley.Rules.Epoch (ShelleyEPOCH)
import Cardano.Ledger.Shelley.Rules.Ledger (ShelleyLEDGER)
import Cardano.Ledger.Shelley.Rules.Ledgers (ShelleyLEDGERS)
import Cardano.Ledger.Shelley.Rules.Mir (ShelleyMIR)
import Cardano.Ledger.Shelley.Rules.NewEpoch (ShelleyNEWEPOCH)
import Cardano.Ledger.Shelley.Rules.Newpp (ShelleyNEWPP)
import Cardano.Ledger.Shelley.Rules.Pool (ShelleyPOOL)
import Cardano.Ledger.Shelley.Rules.PoolReap (ShelleyPOOLREAP)
import Cardano.Ledger.Shelley.Rules.Ppup (ShelleyPPUP)
import Cardano.Ledger.Shelley.Rules.Rupd (ShelleyRUPD)
import Cardano.Ledger.Shelley.Rules.Snap (ShelleySNAP)
import Cardano.Ledger.Shelley.Rules.Tick (ShelleyTICK, ShelleyTICKF)
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUPEC)
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUTXO)
import Cardano.Ledger.Shelley.Rules.Utxow (ShelleyUTXOW)

type instance EraRule "BBODY" (ShelleyEra c) = ShelleyBBODY (ShelleyEra c)

type instance EraRule "DELEG" (ShelleyEra c) = ShelleyDELEG (ShelleyEra c)

type instance EraRule "DELEGS" (ShelleyEra c) = ShelleyDELEGS (ShelleyEra c)

type instance EraRule "DELPL" (ShelleyEra c) = ShelleyDELPL (ShelleyEra c)

type instance EraRule "EPOCH" (ShelleyEra c) = ShelleyEPOCH (ShelleyEra c)

type instance EraRule "LEDGER" (ShelleyEra c) = ShelleyLEDGER (ShelleyEra c)

type instance EraRule "LEDGERS" (ShelleyEra c) = ShelleyLEDGERS (ShelleyEra c)

type instance EraRule "MIR" (ShelleyEra c) = ShelleyMIR (ShelleyEra c)

type instance EraRule "NEWEPOCH" (ShelleyEra c) = ShelleyNEWEPOCH (ShelleyEra c)

type instance EraRule "NEWPP" (ShelleyEra c) = ShelleyNEWPP (ShelleyEra c)

type instance EraRule "POOL" (ShelleyEra c) = ShelleyPOOL (ShelleyEra c)

type instance EraRule "POOLREAP" (ShelleyEra c) = ShelleyPOOLREAP (ShelleyEra c)

type instance EraRule "PPUP" (ShelleyEra c) = ShelleyPPUP (ShelleyEra c)

type instance EraRule "RUPD" (ShelleyEra c) = ShelleyRUPD (ShelleyEra c)

type instance EraRule "SNAP" (ShelleyEra c) = ShelleySNAP (ShelleyEra c)

type instance EraRule "TICK" (ShelleyEra c) = ShelleyTICK (ShelleyEra c)

type instance EraRule "TICKF" (ShelleyEra c) = ShelleyTICKF (ShelleyEra c)

type instance EraRule "UPEC" (ShelleyEra c) = ShelleyUPEC (ShelleyEra c)

type instance EraRule "UTXO" (ShelleyEra c) = ShelleyUTXO (ShelleyEra c)

type instance EraRule "UTXOW" (ShelleyEra c) = ShelleyUTXOW (ShelleyEra c)
