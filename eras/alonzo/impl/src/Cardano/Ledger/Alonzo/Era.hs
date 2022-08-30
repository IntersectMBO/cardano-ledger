{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Era
  ( AlonzoEra,
    AlonzoUTXO,
    AlonzoUTXOS,
    AlonzoUTXOW,
    AlonzoBBODY,
    AlonzoLEDGER,
  )
where

import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Rules

-- =====================================================

-- | The Alonzo era
data AlonzoEra c

instance CC.Crypto c => Era (AlonzoEra c) where
  type EraCrypto (AlonzoEra c) = c
  type ProtVerLow (AlonzoEra c) = 5
  type ProtVerHigh (AlonzoEra c) = 6

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

type instance EraRule "DELEG" (AlonzoEra c) = API.ShelleyDELEG (AlonzoEra c)

type instance EraRule "DELEGS" (AlonzoEra c) = API.ShelleyDELEGS (AlonzoEra c)

type instance EraRule "DELPL" (AlonzoEra c) = API.ShelleyDELPL (AlonzoEra c)

type instance EraRule "EPOCH" (AlonzoEra c) = ShelleyEPOCH (AlonzoEra c)

type instance EraRule "LEDGERS" (AlonzoEra c) = API.ShelleyLEDGERS (AlonzoEra c)

type instance EraRule "MIR" (AlonzoEra c) = ShelleyMIR (AlonzoEra c)

type instance EraRule "NEWEPOCH" (AlonzoEra c) = API.ShelleyNEWEPOCH (AlonzoEra c)

type instance EraRule "NEWPP" (AlonzoEra c) = ShelleyNEWPP (AlonzoEra c)

type instance EraRule "POOL" (AlonzoEra c) = API.ShelleyPOOL (AlonzoEra c)

type instance EraRule "POOLREAP" (AlonzoEra c) = API.ShelleyPOOLREAP (AlonzoEra c)

type instance EraRule "PPUP" (AlonzoEra c) = API.ShelleyPPUP (AlonzoEra c)

type instance EraRule "RUPD" (AlonzoEra c) = ShelleyRUPD (AlonzoEra c)

type instance EraRule "SNAP" (AlonzoEra c) = ShelleySNAP (AlonzoEra c)

type instance EraRule "TICK" (AlonzoEra c) = ShelleyTICK (AlonzoEra c)

type instance EraRule "TICKF" (AlonzoEra c) = ShelleyTICKF (AlonzoEra c)

type instance EraRule "UPEC" (AlonzoEra c) = ShelleyUPEC (AlonzoEra c)
