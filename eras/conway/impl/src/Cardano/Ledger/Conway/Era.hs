{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Conway.Era (
  ConwayEra,
  ConwayTALLY,
  ConwayNEWEPOCH,
  ConwayEPOCH,
  ConwayENACTMENT,
  ConwayUTXOS,
  ConwayLEDGER,
) where

import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Rules (BabbageUTXO, BabbageUTXOW)
import Cardano.Ledger.Conway.Governance (ConwayTallyState)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Core (EraTallyState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyMIR,
  ShelleyNEWPP,
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
  ShelleyTICKF,
 )

-- =====================================================

-- | The Conway era
data ConwayEra c

instance CC.Crypto c => Era (ConwayEra c) where
  type PreviousEra (ConwayEra c) = BabbageEra c
  type EraCrypto (ConwayEra c) = c
  type ProtVerLow (ConwayEra c) = 9

type instance Value (ConwayEra c) = MaryValue c

instance EraTallyState (ConwayEra c) where
  type TallyState (ConwayEra c) = ConwayTallyState (ConwayEra c)

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data ConwayTALLY era

type instance EraRule "TALLY" (ConwayEra c) = ConwayTALLY (ConwayEra c)

data ConwayNEWEPOCH era

type instance EraRule "NEWEPOCH" (ConwayEra c) = ConwayNEWEPOCH (ConwayEra c)

data ConwayEPOCH era

type instance EraRule "EPOCH" (ConwayEra c) = ConwayEPOCH (ConwayEra c)

data ConwayENACTMENT era

type instance EraRule "ENACTMENT" (ConwayEra c) = ConwayENACTMENT (ConwayEra c)

data ConwayUTXOS era

type instance EraRule "UTXOS" (ConwayEra c) = ConwayUTXOS (ConwayEra c)

data ConwayLEDGER era

type instance EraRule "LEDGER" (ConwayEra c) = ConwayLEDGER (ConwayEra c)

-- Rules inherited from Babbage

type instance EraRule "UTXO" (ConwayEra c) = BabbageUTXO (ConwayEra c)

type instance EraRule "UTXOW" (ConwayEra c) = BabbageUTXOW (ConwayEra c)

-- Rules inherited from Alonzo

type instance EraRule "BBODY" (ConwayEra c) = AlonzoBBODY (ConwayEra c)

-- Rules inherited from Shelley

type instance EraRule "DELEG" (ConwayEra c) = API.ShelleyDELEG (ConwayEra c)

type instance EraRule "DELEGS" (ConwayEra c) = API.ShelleyDELEGS (ConwayEra c)

type instance EraRule "DELPL" (ConwayEra c) = API.ShelleyDELPL (ConwayEra c)

type instance EraRule "LEDGERS" (ConwayEra c) = API.ShelleyLEDGERS (ConwayEra c)

type instance EraRule "MIR" (ConwayEra c) = ShelleyMIR (ConwayEra c)

type instance EraRule "NEWPP" (ConwayEra c) = ShelleyNEWPP (ConwayEra c)

type instance EraRule "POOL" (ConwayEra c) = API.ShelleyPOOL (ConwayEra c)

type instance EraRule "POOLREAP" (ConwayEra c) = API.ShelleyPOOLREAP (ConwayEra c)

type instance EraRule "RUPD" (ConwayEra c) = ShelleyRUPD (ConwayEra c)

type instance EraRule "SNAP" (ConwayEra c) = ShelleySNAP (ConwayEra c)

type instance EraRule "TICK" (ConwayEra c) = ShelleyTICK (ConwayEra c)

type instance EraRule "TICKF" (ConwayEra c) = ShelleyTICKF (ConwayEra c)

-- =================================================
