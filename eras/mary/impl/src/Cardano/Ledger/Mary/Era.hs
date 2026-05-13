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

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Genesis (EraGenesis, NoGenesis)
import Cardano.Ledger.Internal.Era (MaryEra)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

instance EraGenesis MaryEra

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

-- | No context is needed to translate from Allegra to Mary.
type instance TranslationContext MaryEra = NoGenesis MaryEra

type instance Value MaryEra = MaryValue

-- These rules are all inherited from Shelley

type instance EraRule "BBODY" MaryEra = Shelley.ShelleyBBODY MaryEra

type instance EraRule "DELEG" MaryEra = Shelley.ShelleyDELEG MaryEra

type instance EraRule "DELEGS" MaryEra = Shelley.ShelleyDELEGS MaryEra

type instance EraRule "DELPL" MaryEra = Shelley.ShelleyDELPL MaryEra

type instance EraRule "EPOCH" MaryEra = Shelley.ShelleyEPOCH MaryEra

type instance EraRule "LEDGER" MaryEra = Shelley.ShelleyLEDGER MaryEra

type instance EraRule "LEDGERS" MaryEra = Shelley.ShelleyLEDGERS MaryEra

type instance EraRule "MIR" MaryEra = Shelley.ShelleyMIR MaryEra

type instance EraRule "NEWEPOCH" MaryEra = Shelley.ShelleyNEWEPOCH MaryEra

type instance EraRule "NEWPP" MaryEra = Shelley.ShelleyNEWPP MaryEra

type instance EraRule "POOL" MaryEra = Shelley.ShelleyPOOL MaryEra

type instance EraRule "POOLREAP" MaryEra = Shelley.ShelleyPOOLREAP MaryEra

type instance EraRule "PPUP" MaryEra = Shelley.ShelleyPPUP MaryEra

type instance EraRule "RUPD" MaryEra = Shelley.ShelleyRUPD MaryEra

type instance EraRule "SNAP" MaryEra = Shelley.ShelleySNAP MaryEra

type instance EraRule "TICK" MaryEra = Shelley.ShelleyTICK MaryEra

type instance EraRule "TICKF" MaryEra = Shelley.ShelleyTICKF MaryEra

type instance EraRule "UPEC" MaryEra = Shelley.ShelleyUPEC MaryEra

type instance EraRule "UTXO" MaryEra = Allegra.AllegraUTXO MaryEra

type instance EraRule "UTXOW" MaryEra = Allegra.AllegraUTXOW MaryEra
