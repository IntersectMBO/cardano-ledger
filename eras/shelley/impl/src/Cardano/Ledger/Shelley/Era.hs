{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

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
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (ByronEra, Era (..), EraRule, Value)
import Cardano.Ledger.Internal.Era (ShelleyEra)

instance Era ShelleyEra where
  type PreviousEra ShelleyEra = ByronEra
  type ProtVerLow ShelleyEra = 2

  eraName = "Shelley"

type instance Value ShelleyEra = Coin

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

type instance EraRule "BBODY" ShelleyEra = ShelleyBBODY ShelleyEra

type instance EraRule "DELEG" ShelleyEra = ShelleyDELEG ShelleyEra

type instance EraRule "DELEGS" ShelleyEra = ShelleyDELEGS ShelleyEra

type instance EraRule "DELPL" ShelleyEra = ShelleyDELPL ShelleyEra

type instance EraRule "EPOCH" ShelleyEra = ShelleyEPOCH ShelleyEra

type instance EraRule "LEDGER" ShelleyEra = ShelleyLEDGER ShelleyEra

type instance EraRule "LEDGERS" ShelleyEra = ShelleyLEDGERS ShelleyEra

type instance EraRule "MIR" ShelleyEra = ShelleyMIR ShelleyEra

type instance EraRule "NEWEPOCH" ShelleyEra = ShelleyNEWEPOCH ShelleyEra

type instance EraRule "NEWPP" ShelleyEra = ShelleyNEWPP ShelleyEra

type instance EraRule "POOL" ShelleyEra = ShelleyPOOL ShelleyEra

type instance EraRule "POOLREAP" ShelleyEra = ShelleyPOOLREAP ShelleyEra

type instance EraRule "PPUP" ShelleyEra = ShelleyPPUP ShelleyEra

type instance EraRule "RUPD" ShelleyEra = ShelleyRUPD ShelleyEra

type instance EraRule "SNAP" ShelleyEra = ShelleySNAP ShelleyEra

type instance EraRule "TICK" ShelleyEra = ShelleyTICK ShelleyEra

type instance EraRule "TICKF" ShelleyEra = ShelleyTICKF ShelleyEra

type instance EraRule "UPEC" ShelleyEra = ShelleyUPEC ShelleyEra

type instance EraRule "UTXO" ShelleyEra = ShelleyUTXO ShelleyEra

type instance EraRule "UTXOW" ShelleyEra = ShelleyUTXOW ShelleyEra
