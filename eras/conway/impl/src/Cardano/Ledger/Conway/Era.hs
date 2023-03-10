{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Conway.Era (
  ConwayEra,
  ConwayCERT,
  ConwayDELEGS,
  ConwayTALLY,
  ConwayNEWEPOCH,
  ConwayEPOCH,
  ConwayENACT,
  ConwayUTXOS,
  ConwayTICKF,
  ConwayLEDGER,
  ConwayRATIFY,
) where

import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Rules (BabbageUTXO, BabbageUTXOW)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Rules (
  ShelleyMIR,
  ShelleyNEWPP,
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
 )

-- =====================================================

-- | The Conway era
data ConwayEra c

instance Crypto c => Era (ConwayEra c) where
  type PreviousEra (ConwayEra c) = BabbageEra c
  type EraCrypto (ConwayEra c) = c
  type ProtVerLow (ConwayEra c) = 9

type instance Value (ConwayEra c) = MaryValue c

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data ConwayTALLY era

type instance EraRule "TALLY" (ConwayEra c) = ConwayTALLY (ConwayEra c)

data ConwayNEWEPOCH era

type instance EraRule "NEWEPOCH" (ConwayEra c) = ConwayNEWEPOCH (ConwayEra c)

data ConwayEPOCH era

type instance EraRule "EPOCH" (ConwayEra c) = ConwayEPOCH (ConwayEra c)

data ConwayENACT era

type instance EraRule "ENACT" (ConwayEra c) = ConwayENACT (ConwayEra c)

data ConwayUTXOS era

type instance EraRule "UTXOS" (ConwayEra c) = ConwayUTXOS (ConwayEra c)

data ConwayLEDGER era

type instance EraRule "LEDGER" (ConwayEra c) = ConwayLEDGER (ConwayEra c)

data ConwayTICKF era

type instance EraRule "TICKF" (ConwayEra c) = ConwayTICKF (ConwayEra c)

data ConwayRATIFY era

type instance EraRule "RATIFY" (ConwayEra c) = ConwayRATIFY (ConwayEra c)

data ConwayDELEGS era

type instance EraRule "DELEGS" (ConwayEra c) = ConwayDELEGS (ConwayEra c)

data ConwayCERT era

type instance EraRule "CERT" (ConwayEra c) = ConwayCERT (ConwayEra c)

-- Rules inherited from Babbage

type instance EraRule "UTXO" (ConwayEra c) = BabbageUTXO (ConwayEra c)

type instance EraRule "UTXOW" (ConwayEra c) = BabbageUTXOW (ConwayEra c)

-- Rules inherited from Alonzo

type instance EraRule "BBODY" (ConwayEra c) = AlonzoBBODY (ConwayEra c)

-- Rules inherited from Shelley

type instance EraRule "DELEG" (ConwayEra c) = API.ShelleyDELEG (ConwayEra c)

type instance EraRule "LEDGERS" (ConwayEra c) = API.ShelleyLEDGERS (ConwayEra c)

type instance EraRule "MIR" (ConwayEra c) = ShelleyMIR (ConwayEra c)

type instance EraRule "NEWPP" (ConwayEra c) = ShelleyNEWPP (ConwayEra c)

type instance EraRule "POOL" (ConwayEra c) = API.ShelleyPOOL (ConwayEra c)

type instance EraRule "POOLREAP" (ConwayEra c) = API.ShelleyPOOLREAP (ConwayEra c)

type instance EraRule "RUPD" (ConwayEra c) = ShelleyRUPD (ConwayEra c)

type instance EraRule "SNAP" (ConwayEra c) = ShelleySNAP (ConwayEra c)

type instance EraRule "TICK" (ConwayEra c) = ShelleyTICK (ConwayEra c)

type instance EraRule "TICKF" (ConwayEra c) = ConwayTICKF (ConwayEra c)

-- =================================================
