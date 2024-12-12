{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Allegra.Era (
  AllegraEra,
  AllegraUTXO,
  AllegraUTXOW,
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Genesis (EraGenesis, NoGenesis)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Rules

-- | The Allegra era
data AllegraEra

instance Era AllegraEra where
  type PreviousEra AllegraEra = ShelleyEra
  type ProtVerLow AllegraEra = 3

  eraName = "Allegra"

instance EraGenesis AllegraEra

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

-- | No context is needed to translate from Shelley to Allegra.
type instance TranslationContext AllegraEra = NoGenesis AllegraEra

type instance Value AllegraEra = Coin

-- These rules are all inherited from Shelley

type instance EraRule "BBODY" AllegraEra = ShelleyBBODY AllegraEra

type instance EraRule "DELEG" AllegraEra = ShelleyDELEG AllegraEra

type instance EraRule "DELEGS" AllegraEra = ShelleyDELEGS AllegraEra

type instance EraRule "DELPL" AllegraEra = ShelleyDELPL AllegraEra

type instance EraRule "EPOCH" AllegraEra = ShelleyEPOCH AllegraEra

type instance EraRule "LEDGER" AllegraEra = ShelleyLEDGER AllegraEra

type instance EraRule "LEDGERS" AllegraEra = ShelleyLEDGERS AllegraEra

type instance EraRule "MIR" AllegraEra = ShelleyMIR AllegraEra

type instance EraRule "NEWEPOCH" AllegraEra = ShelleyNEWEPOCH AllegraEra

type instance EraRule "NEWPP" AllegraEra = ShelleyNEWPP AllegraEra

type instance EraRule "POOL" AllegraEra = ShelleyPOOL AllegraEra

type instance EraRule "POOLREAP" AllegraEra = ShelleyPOOLREAP AllegraEra

type instance EraRule "PPUP" AllegraEra = ShelleyPPUP AllegraEra

type instance EraRule "RUPD" AllegraEra = ShelleyRUPD AllegraEra

type instance EraRule "SNAP" AllegraEra = ShelleySNAP AllegraEra

type instance EraRule "TICK" AllegraEra = ShelleyTICK AllegraEra

type instance EraRule "TICKF" AllegraEra = ShelleyTICKF AllegraEra

type instance EraRule "UPEC" AllegraEra = ShelleyUPEC AllegraEra

-- These rules are defined anew in the Allegra era

data AllegraUTXO era

type instance EraRule "UTXO" AllegraEra = AllegraUTXO AllegraEra

data AllegraUTXOW era

type instance EraRule "UTXOW" AllegraEra = AllegraUTXOW AllegraEra
