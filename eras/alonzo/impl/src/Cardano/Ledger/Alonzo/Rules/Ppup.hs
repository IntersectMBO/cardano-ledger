{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Ppup () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (PpupEvent, ShelleyPpupPredFailure)

type instance EraRuleFailure "PPUP" AlonzoEra = ShelleyPpupPredFailure AlonzoEra

type instance EraRuleEvent "PPUP" AlonzoEra = PpupEvent AlonzoEra

instance InjectRuleFailure "PPUP" ShelleyPpupPredFailure AlonzoEra
