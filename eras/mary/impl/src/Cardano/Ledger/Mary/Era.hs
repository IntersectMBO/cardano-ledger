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
import Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.PParams (ShelleyPParams, ShelleyPParamsUpdate, updatePParams)
import Cardano.Ledger.Shelley.Rules
  ( ShelleyBBODY,
    ShelleyEPOCH,
    ShelleyMIR,
    ShelleyNEWPP,
    ShelleyRUPD,
    ShelleySNAP,
    ShelleyTICKF,
    ShelleyUPEC,
  )

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

type instance EraRule "DELEG" (MaryEra c) = API.ShelleyDELEG (MaryEra c)

type instance EraRule "DELEGS" (MaryEra c) = API.ShelleyDELEGS (MaryEra c)

type instance EraRule "DELPL" (MaryEra c) = API.ShelleyDELPL (MaryEra c)

type instance EraRule "EPOCH" (MaryEra c) = ShelleyEPOCH (MaryEra c)

type instance EraRule "LEDGER" (MaryEra c) = API.ShelleyLEDGER (MaryEra c)

type instance EraRule "LEDGERS" (MaryEra c) = API.ShelleyLEDGERS (MaryEra c)

type instance EraRule "MIR" (MaryEra c) = ShelleyMIR (MaryEra c)

type instance EraRule "NEWEPOCH" (MaryEra c) = API.ShelleyNEWEPOCH (MaryEra c)

type instance EraRule "NEWPP" (MaryEra c) = ShelleyNEWPP (MaryEra c)

type instance EraRule "POOL" (MaryEra c) = API.ShelleyPOOL (MaryEra c)

type instance EraRule "POOLREAP" (MaryEra c) = API.ShelleyPOOLREAP (MaryEra c)

type instance EraRule "PPUP" (MaryEra c) = API.ShelleyPPUP (MaryEra c)

type instance EraRule "RUPD" (MaryEra c) = ShelleyRUPD (MaryEra c)

type instance EraRule "SNAP" (MaryEra c) = ShelleySNAP (MaryEra c)

type instance EraRule "TICK" (MaryEra c) = API.ShelleyTICK (MaryEra c)

type instance EraRule "TICKF" (MaryEra c) = ShelleyTICKF (MaryEra c)

type instance EraRule "UPEC" (MaryEra c) = ShelleyUPEC (MaryEra c)

type instance EraRule "UTXO" (MaryEra c) = AllegraUTXO (MaryEra c)

type instance EraRule "UTXOW" (MaryEra c) = AllegraUTXOW (MaryEra c)
