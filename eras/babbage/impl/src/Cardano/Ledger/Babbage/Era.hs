{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Babbage.Era (
  BabbageEra,
  BabbageUTXO,
  BabbageUTXOS,
  BabbageUTXOW,
  BabbageLEDGER,
)
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Rules (
  ShelleyEPOCH,
  ShelleyMIR,
  ShelleyNEWPP,
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
  ShelleyTICKF,
  ShelleyUPEC,
 )

-- =====================================================

-- | The Babbage era
data BabbageEra c

instance Crypto c => Era (BabbageEra c) where
  type PreviousEra (BabbageEra c) = AlonzoEra c
  type EraCrypto (BabbageEra c) = c
  type ProtVerLow (BabbageEra c) = 7
  type ProtVerHigh (BabbageEra c) = 8

  eraName = "Babbage"

type instance Value (BabbageEra c) = MaryValue c

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data BabbageUTXOS era

type instance EraRule "UTXOS" (BabbageEra c) = BabbageUTXOS (BabbageEra c)

data BabbageUTXO era

type instance EraRule "UTXO" (BabbageEra c) = BabbageUTXO (BabbageEra c)

data BabbageUTXOW era

type instance EraRule "UTXOW" (BabbageEra c) = BabbageUTXOW (BabbageEra c)

data BabbageLEDGER c

type instance EraRule "LEDGER" (BabbageEra c) = BabbageLEDGER (BabbageEra c)

-- Rules inherited from Alonzo

type instance EraRule "BBODY" (BabbageEra c) = AlonzoBBODY (BabbageEra c)

-- Rules inherited from Shelley

type instance EraRule "DELEG" (BabbageEra c) = API.ShelleyDELEG (BabbageEra c)

type instance EraRule "DELEGS" (BabbageEra c) = API.ShelleyDELEGS (BabbageEra c)

type instance EraRule "DELPL" (BabbageEra c) = API.ShelleyDELPL (BabbageEra c)

type instance EraRule "EPOCH" (BabbageEra c) = ShelleyEPOCH (BabbageEra c)

type instance EraRule "LEDGERS" (BabbageEra c) = API.ShelleyLEDGERS (BabbageEra c)

type instance EraRule "MIR" (BabbageEra c) = ShelleyMIR (BabbageEra c)

type instance EraRule "NEWEPOCH" (BabbageEra c) = API.ShelleyNEWEPOCH (BabbageEra c)

type instance EraRule "NEWPP" (BabbageEra c) = ShelleyNEWPP (BabbageEra c)

type instance EraRule "POOL" (BabbageEra c) = API.ShelleyPOOL (BabbageEra c)

type instance EraRule "POOLREAP" (BabbageEra c) = API.ShelleyPOOLREAP (BabbageEra c)

type instance EraRule "PPUP" (BabbageEra c) = API.ShelleyPPUP (BabbageEra c)

type instance EraRule "RUPD" (BabbageEra c) = ShelleyRUPD (BabbageEra c)

type instance EraRule "SNAP" (BabbageEra c) = ShelleySNAP (BabbageEra c)

type instance EraRule "TICK" (BabbageEra c) = ShelleyTICK (BabbageEra c)

type instance EraRule "TICKF" (BabbageEra c) = ShelleyTICKF (BabbageEra c)

type instance EraRule "UPEC" (BabbageEra c) = ShelleyUPEC (BabbageEra c)

-- =================================================
