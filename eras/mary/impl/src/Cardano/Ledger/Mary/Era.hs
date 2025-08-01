{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Mary.Era (MaryEra) where

import Cardano.Ledger.Allegra.Rules (AllegraUTXO, AllegraUTXOW)
import Cardano.Ledger.Genesis (EraGenesis, NoGenesis)
import Cardano.Ledger.Internal.Era (MaryEra)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Rules

instance EraGenesis MaryEra

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

-- | No context is needed to translate from Allegra to Mary.
type instance TranslationContext MaryEra = NoGenesis MaryEra

type instance Value MaryEra = MaryValue

-- These rules are all inherited from Shelley

type instance EraRule "BBODY" MaryEra = ShelleyBBODY MaryEra

type instance EraRule "DELEG" MaryEra = ShelleyDELEG MaryEra

type instance EraRule "DELEGS" MaryEra = ShelleyDELEGS MaryEra

type instance EraRule "DELPL" MaryEra = ShelleyDELPL MaryEra

type instance EraRule "EPOCH" MaryEra = ShelleyEPOCH MaryEra

type instance EraRule "LEDGER" MaryEra = ShelleyLEDGER MaryEra

type instance EraRule "LEDGERS" MaryEra = ShelleyLEDGERS MaryEra

type instance EraRule "MIR" MaryEra = ShelleyMIR MaryEra

type instance EraRule "NEWEPOCH" MaryEra = ShelleyNEWEPOCH MaryEra

type instance EraRule "NEWPP" MaryEra = ShelleyNEWPP MaryEra

type instance EraRule "POOL" MaryEra = ShelleyPOOL MaryEra

type instance EraRule "POOLREAP" MaryEra = ShelleyPOOLREAP MaryEra

type instance EraRule "PPUP" MaryEra = ShelleyPPUP MaryEra

type instance EraRule "RUPD" MaryEra = ShelleyRUPD MaryEra

type instance EraRule "SNAP" MaryEra = ShelleySNAP MaryEra

type instance EraRule "TICK" MaryEra = ShelleyTICK MaryEra

type instance EraRule "TICKF" MaryEra = ShelleyTICKF MaryEra

type instance EraRule "UPEC" MaryEra = ShelleyUPEC MaryEra

type instance EraRule "UTXO" MaryEra = AllegraUTXO MaryEra

type instance EraRule "UTXOW" MaryEra = AllegraUTXOW MaryEra
