{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Mary.Era (MaryEra) where

import Cardano.Ledger.Allegra.Rules (AllegraUTXO, AllegraUTXOW)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.PParams (
  ShelleyPParams,
  ShelleyPParamsHKD (..),
  ShelleyPParamsUpdate,
  updatePParams,
 )
import Cardano.Ledger.Shelley.Rules

data MaryEra era

instance Crypto c => Era (MaryEra c) where
  type EraCrypto (MaryEra c) = c
  type ProtVerLow (MaryEra c) = 4

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Value (MaryEra c) = MaryValue c

instance Crypto c => EraPParams (MaryEra c) where
  type PParams (MaryEra c) = ShelleyPParams (MaryEra c)
  type PParamsUpdate (MaryEra c) = ShelleyPParamsUpdate (MaryEra c)

  applyPPUpdates = updatePParams

-- These rules are all inherited from Shelley

type instance EraRule "BBODY" (MaryEra c) = ShelleyBBODY (MaryEra c)

type instance EraRule "DELEG" (MaryEra c) = ShelleyDELEG (MaryEra c)

type instance EraRule "DELEGS" (MaryEra c) = ShelleyDELEGS (MaryEra c)

type instance EraRule "DELPL" (MaryEra c) = ShelleyDELPL (MaryEra c)

type instance EraRule "EPOCH" (MaryEra c) = ShelleyEPOCH (MaryEra c)

type instance EraRule "LEDGER" (MaryEra c) = ShelleyLEDGER (MaryEra c)

type instance EraRule "LEDGERS" (MaryEra c) = ShelleyLEDGERS (MaryEra c)

type instance EraRule "MIR" (MaryEra c) = ShelleyMIR (MaryEra c)

type instance EraRule "NEWEPOCH" (MaryEra c) = ShelleyNEWEPOCH (MaryEra c)

type instance EraRule "NEWPP" (MaryEra c) = ShelleyNEWPP (MaryEra c)

type instance EraRule "POOL" (MaryEra c) = ShelleyPOOL (MaryEra c)

type instance EraRule "POOLREAP" (MaryEra c) = ShelleyPOOLREAP (MaryEra c)

type instance EraRule "PPUP" (MaryEra c) = ShelleyPPUP (MaryEra c)

type instance EraRule "RUPD" (MaryEra c) = ShelleyRUPD (MaryEra c)

type instance EraRule "SNAP" (MaryEra c) = ShelleySNAP (MaryEra c)

type instance EraRule "TICK" (MaryEra c) = ShelleyTICK (MaryEra c)

type instance EraRule "TICKF" (MaryEra c) = ShelleyTICKF (MaryEra c)

type instance EraRule "UPEC" (MaryEra c) = ShelleyUPEC (MaryEra c)

type instance EraRule "UTXO" (MaryEra c) = AllegraUTXO (MaryEra c)

type instance EraRule "UTXOW" (MaryEra c) = AllegraUTXOW (MaryEra c)
