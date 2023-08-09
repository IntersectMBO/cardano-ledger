{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Era (
  AlonzoEra,
  AlonzoUTXO,
  AlonzoUTXOS,
  AlonzoUTXOW,
  AlonzoBBODY,
  AlonzoLEDGER,
)
where

import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary (MaryEra, MaryValue)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Rules

-- =====================================================

-- | The Alonzo era
data AlonzoEra c

instance Crypto c => Era (AlonzoEra c) where
  type EraCrypto (AlonzoEra c) = c
  type PreviousEra (AlonzoEra c) = MaryEra c
  type ProtVerLow (AlonzoEra c) = 5
  type ProtVerHigh (AlonzoEra c) = 6

  eraName = "Alonzo"

type instance Value (AlonzoEra c) = MaryValue c

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- These rules are new or changed in Alonzo

data AlonzoUTXOS era

type instance EraRule "UTXOS" (AlonzoEra c) = AlonzoUTXOS (AlonzoEra c)

data AlonzoUTXO era

type instance EraRule "UTXO" (AlonzoEra c) = AlonzoUTXO (AlonzoEra c)

data AlonzoUTXOW era

type instance EraRule "UTXOW" (AlonzoEra c) = AlonzoUTXOW (AlonzoEra c)

data AlonzoLEDGER era

type instance EraRule "LEDGER" (AlonzoEra c) = AlonzoLEDGER (AlonzoEra c)

data AlonzoBBODY era

type instance EraRule "BBODY" (AlonzoEra c) = AlonzoBBODY (AlonzoEra c)

-- Rules inherited from Shelley

type instance EraRule "DELEG" (AlonzoEra c) = ShelleyDELEG (AlonzoEra c)

type instance EraRule "DELEGS" (AlonzoEra c) = ShelleyDELEGS (AlonzoEra c)

type instance EraRule "DELPL" (AlonzoEra c) = ShelleyDELPL (AlonzoEra c)

type instance EraRule "EPOCH" (AlonzoEra c) = ShelleyEPOCH (AlonzoEra c)

type instance EraRule "LEDGERS" (AlonzoEra c) = ShelleyLEDGERS (AlonzoEra c)

type instance EraRule "MIR" (AlonzoEra c) = ShelleyMIR (AlonzoEra c)

type instance EraRule "NEWEPOCH" (AlonzoEra c) = ShelleyNEWEPOCH (AlonzoEra c)

type instance EraRule "NEWPP" (AlonzoEra c) = ShelleyNEWPP (AlonzoEra c)

type instance EraRule "POOL" (AlonzoEra c) = ShelleyPOOL (AlonzoEra c)

type instance EraRule "POOLREAP" (AlonzoEra c) = ShelleyPOOLREAP (AlonzoEra c)

type instance EraRule "PPUP" (AlonzoEra c) = ShelleyPPUP (AlonzoEra c)

type instance EraRule "RUPD" (AlonzoEra c) = ShelleyRUPD (AlonzoEra c)

type instance EraRule "SNAP" (AlonzoEra c) = ShelleySNAP (AlonzoEra c)

type instance EraRule "TICK" (AlonzoEra c) = ShelleyTICK (AlonzoEra c)

type instance EraRule "TICKF" (AlonzoEra c) = ShelleyTICKF (AlonzoEra c)

type instance EraRule "UPEC" (AlonzoEra c) = ShelleyUPEC (AlonzoEra c)
