{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.Era (
  ShelleyEra,
  ShelleyBBODY,
  ShelleyDELEG,
  ShelleyDELEGS,
  ShelleyDELPL,
  ShelleyEPOCH,
  ShelleyLEDGER,
  ShelleyLEDGERS,
  ShelleyMIR,
  ShelleyNEWEPOCH,
  ShelleyNEWPP,
  ShelleyPOOL,
  ShelleyPOOLREAP,
  ShelleyPPUP,
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
  ShelleyTICKF,
  ShelleyUPEC,
  ShelleyUTXO,
  ShelleyUTXOW,
)
where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (ByronEra, Era (..), EraRule, Value)
import Cardano.Ledger.Crypto (Crypto)

data ShelleyEra c

instance Crypto c => Era (ShelleyEra c) where
  type PreviousEra (ShelleyEra c) = ByronEra c
  type EraCrypto (ShelleyEra c) = c
  type ProtVerLow (ShelleyEra c) = 2

  eraName = "Shelley"

type instance Value (ShelleyEra _c) = Coin

data ShelleyBBODY era

data ShelleyDELEG era

data ShelleyDELEGS era

data ShelleyDELPL era

data ShelleyEPOCH era

data ShelleyLEDGER era

data ShelleyLEDGERS era

data ShelleyMIR era

data ShelleyNEWEPOCH era

data ShelleyNEWPP era

data ShelleyPOOL era

data ShelleyPOOLREAP era

data ShelleyPPUP era

data ShelleyRUPD era

data ShelleySNAP era

data ShelleyTICK era

data ShelleyTICKF era

data ShelleyUPEC era

data ShelleyUTXO era

data ShelleyUTXOW era

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
