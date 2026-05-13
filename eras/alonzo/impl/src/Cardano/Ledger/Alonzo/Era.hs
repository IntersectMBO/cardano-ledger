{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Alonzo.Era (
  AlonzoEra,
  AlonzoUTXO,
  AlonzoUTXOS,
  AlonzoUTXOW,
  AlonzoBBODY,
  AlonzoLEDGER,
) where

import Cardano.Ledger.Internal.Era (AlonzoEra)
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.Shelley.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

-- =====================================================

instance EraTxLevel AlonzoEra where
  type STxLevel l AlonzoEra = STxTopLevel l AlonzoEra

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

type instance EraRule "DELEG" AlonzoEra = Shelley.ShelleyDELEG AlonzoEra

type instance EraRule "DELEGS" AlonzoEra = Shelley.ShelleyDELEGS AlonzoEra

type instance EraRule "DELPL" AlonzoEra = Shelley.ShelleyDELPL AlonzoEra

type instance EraRule "EPOCH" AlonzoEra = Shelley.ShelleyEPOCH AlonzoEra

type instance EraRule "LEDGERS" AlonzoEra = Shelley.ShelleyLEDGERS AlonzoEra

type instance EraRule "MIR" AlonzoEra = Shelley.ShelleyMIR AlonzoEra

type instance EraRule "NEWEPOCH" AlonzoEra = Shelley.ShelleyNEWEPOCH AlonzoEra

type instance EraRule "NEWPP" AlonzoEra = Shelley.ShelleyNEWPP AlonzoEra

type instance EraRule "POOL" AlonzoEra = Shelley.ShelleyPOOL AlonzoEra

type instance EraRule "POOLREAP" AlonzoEra = Shelley.ShelleyPOOLREAP AlonzoEra

type instance EraRule "PPUP" AlonzoEra = Shelley.ShelleyPPUP AlonzoEra

type instance EraRule "RUPD" AlonzoEra = Shelley.ShelleyRUPD AlonzoEra

type instance EraRule "SNAP" AlonzoEra = Shelley.ShelleySNAP AlonzoEra

type instance EraRule "TICK" AlonzoEra = Shelley.ShelleyTICK AlonzoEra

type instance EraRule "TICKF" AlonzoEra = Shelley.ShelleyTICKF AlonzoEra

type instance EraRule "UPEC" AlonzoEra = Shelley.ShelleyUPEC AlonzoEra
