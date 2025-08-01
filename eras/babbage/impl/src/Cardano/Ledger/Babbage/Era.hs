{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Babbage.Era (
  BabbageEra,
  BabbageUTXO,
  BabbageUTXOS,
  BabbageUTXOW,
  BabbageLEDGER,
) where

import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY)
import Cardano.Ledger.Core
import Cardano.Ledger.Genesis (EraGenesis, NoGenesis)
import Cardano.Ledger.Internal.Era (BabbageEra)
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

instance EraGenesis BabbageEra

type instance TranslationContext BabbageEra = NoGenesis BabbageEra

type instance Value BabbageEra = MaryValue

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data BabbageUTXOS era

type instance EraRule "UTXOS" BabbageEra = BabbageUTXOS BabbageEra

data BabbageUTXO era

type instance EraRule "UTXO" BabbageEra = BabbageUTXO BabbageEra

data BabbageUTXOW era

type instance EraRule "UTXOW" BabbageEra = BabbageUTXOW BabbageEra

data BabbageLEDGER c

type instance EraRule "LEDGER" BabbageEra = BabbageLEDGER BabbageEra

-- Rules inherited from Alonzo

type instance EraRule "BBODY" BabbageEra = AlonzoBBODY BabbageEra

-- Rules inherited from Shelley

type instance EraRule "DELEG" BabbageEra = API.ShelleyDELEG BabbageEra

type instance EraRule "DELEGS" BabbageEra = API.ShelleyDELEGS BabbageEra

type instance EraRule "DELPL" BabbageEra = API.ShelleyDELPL BabbageEra

type instance EraRule "EPOCH" BabbageEra = ShelleyEPOCH BabbageEra

type instance EraRule "LEDGERS" BabbageEra = API.ShelleyLEDGERS BabbageEra

type instance EraRule "MIR" BabbageEra = ShelleyMIR BabbageEra

type instance EraRule "NEWEPOCH" BabbageEra = API.ShelleyNEWEPOCH BabbageEra

type instance EraRule "NEWPP" BabbageEra = ShelleyNEWPP BabbageEra

type instance EraRule "POOL" BabbageEra = API.ShelleyPOOL BabbageEra

type instance EraRule "POOLREAP" BabbageEra = API.ShelleyPOOLREAP BabbageEra

type instance EraRule "PPUP" BabbageEra = API.ShelleyPPUP BabbageEra

type instance EraRule "RUPD" BabbageEra = ShelleyRUPD BabbageEra

type instance EraRule "SNAP" BabbageEra = ShelleySNAP BabbageEra

type instance EraRule "TICK" BabbageEra = ShelleyTICK BabbageEra

type instance EraRule "TICKF" BabbageEra = ShelleyTICKF BabbageEra

type instance EraRule "UPEC" BabbageEra = ShelleyUPEC BabbageEra

-- =================================================
