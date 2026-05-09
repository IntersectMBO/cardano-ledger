{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Ppup () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "PPUP" AlonzoEra = Shelley.ShelleyPpupPredFailure AlonzoEra

type instance EraRuleEvent "PPUP" AlonzoEra = Shelley.PpupEvent AlonzoEra

instance InjectRuleFailure "PPUP" Shelley.ShelleyPpupPredFailure AlonzoEra
