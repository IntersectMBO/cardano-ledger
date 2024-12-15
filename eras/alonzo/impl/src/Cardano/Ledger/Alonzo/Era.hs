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

import Cardano.Ledger.Mary (MaryEra, MaryValue)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Rules

-- =====================================================

-- | The Alonzo era
data AlonzoEra

instance Era AlonzoEra where
  type PreviousEra AlonzoEra = MaryEra
  type ProtVerLow AlonzoEra = 5
  type ProtVerHigh AlonzoEra = 6

  eraName = "Alonzo"

type instance Value AlonzoEra = MaryValue

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- These rules are new or changed in Alonzo

data AlonzoUTXOS era

type instance EraRule "UTXOS" AlonzoEra = AlonzoUTXOS AlonzoEra

data AlonzoUTXO era

type instance EraRule "UTXO" AlonzoEra = AlonzoUTXO AlonzoEra

data AlonzoUTXOW era

type instance EraRule "UTXOW" AlonzoEra = AlonzoUTXOW AlonzoEra

data AlonzoLEDGER era

type instance EraRule "LEDGER" AlonzoEra = AlonzoLEDGER AlonzoEra

data AlonzoBBODY era

type instance EraRule "BBODY" AlonzoEra = AlonzoBBODY AlonzoEra

-- Rules inherited from Shelley

type instance EraRule "DELEG" AlonzoEra = ShelleyDELEG AlonzoEra

type instance EraRule "DELEGS" AlonzoEra = ShelleyDELEGS AlonzoEra

type instance EraRule "DELPL" AlonzoEra = ShelleyDELPL AlonzoEra

type instance EraRule "EPOCH" AlonzoEra = ShelleyEPOCH AlonzoEra

type instance EraRule "LEDGERS" AlonzoEra = ShelleyLEDGERS AlonzoEra

type instance EraRule "MIR" AlonzoEra = ShelleyMIR AlonzoEra

type instance EraRule "NEWEPOCH" AlonzoEra = ShelleyNEWEPOCH AlonzoEra

type instance EraRule "NEWPP" AlonzoEra = ShelleyNEWPP AlonzoEra

type instance EraRule "POOL" AlonzoEra = ShelleyPOOL AlonzoEra

type instance EraRule "POOLREAP" AlonzoEra = ShelleyPOOLREAP AlonzoEra

type instance EraRule "PPUP" AlonzoEra = ShelleyPPUP AlonzoEra

type instance EraRule "RUPD" AlonzoEra = ShelleyRUPD AlonzoEra

type instance EraRule "SNAP" AlonzoEra = ShelleySNAP AlonzoEra

type instance EraRule "TICK" AlonzoEra = ShelleyTICK AlonzoEra

type instance EraRule "TICKF" AlonzoEra = ShelleyTICKF AlonzoEra

type instance EraRule "UPEC" AlonzoEra = ShelleyUPEC AlonzoEra
