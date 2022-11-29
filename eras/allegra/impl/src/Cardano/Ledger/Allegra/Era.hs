{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Allegra.Era
  ( AllegraEra,
    AllegraUTXO,
    AllegraUTXOW,
  )
where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (Era (EraCrypto, ProtVerLow), EraPParams (..), EraRule, Value)
import Cardano.Ledger.Crypto as CC (Crypto)
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

-- | The Allegra era
data AllegraEra c

instance Crypto c => Era (AllegraEra c) where
  type EraCrypto (AllegraEra c) = c
  type ProtVerLow (AllegraEra c) = 3

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Value (AllegraEra _) = Coin

instance Crypto c => EraPParams (AllegraEra c) where
  type PParams (AllegraEra c) = ShelleyPParams (AllegraEra c)
  type PParamsUpdate (AllegraEra c) = ShelleyPParamsUpdate (AllegraEra c)

  applyPPUpdates = updatePParams

-- These rules are all inherited from Shelley

type instance EraRule "BBODY" (AllegraEra c) = ShelleyBBODY (AllegraEra c)

type instance EraRule "DELEG" (AllegraEra c) = API.ShelleyDELEG (AllegraEra c)

type instance EraRule "DELEGS" (AllegraEra c) = API.ShelleyDELEGS (AllegraEra c)

type instance EraRule "DELPL" (AllegraEra c) = API.ShelleyDELPL (AllegraEra c)

type instance EraRule "EPOCH" (AllegraEra c) = ShelleyEPOCH (AllegraEra c)

type instance EraRule "LEDGER" (AllegraEra c) = API.ShelleyLEDGER (AllegraEra c)

type instance EraRule "LEDGERS" (AllegraEra c) = API.ShelleyLEDGERS (AllegraEra c)

type instance EraRule "MIR" (AllegraEra c) = ShelleyMIR (AllegraEra c)

type instance EraRule "NEWEPOCH" (AllegraEra c) = API.ShelleyNEWEPOCH (AllegraEra c)

type instance EraRule "NEWPP" (AllegraEra c) = ShelleyNEWPP (AllegraEra c)

type instance EraRule "POOL" (AllegraEra c) = API.ShelleyPOOL (AllegraEra c)

type instance EraRule "POOLREAP" (AllegraEra c) = API.ShelleyPOOLREAP (AllegraEra c)

type instance EraRule "PPUP" (AllegraEra c) = API.ShelleyPPUP (AllegraEra c)

type instance EraRule "RUPD" (AllegraEra c) = ShelleyRUPD (AllegraEra c)

type instance EraRule "SNAP" (AllegraEra c) = ShelleySNAP (AllegraEra c)

type instance EraRule "TICK" (AllegraEra c) = API.ShelleyTICK (AllegraEra c)

type instance EraRule "TICKF" (AllegraEra c) = ShelleyTICKF (AllegraEra c)

type instance EraRule "UPEC" (AllegraEra c) = ShelleyUPEC (AllegraEra c)

-- These rules are defined anew in the Allegra era(s)

data AllegraUTXO era

type instance EraRule "UTXO" (AllegraEra c) = AllegraUTXO (AllegraEra c)

data AllegraUTXOW era

type instance EraRule "UTXOW" (AllegraEra c) = AllegraUTXOW (AllegraEra c)
