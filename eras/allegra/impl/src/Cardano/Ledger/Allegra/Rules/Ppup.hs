{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Ppup () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "PPUP" AllegraEra = Shelley.ShelleyPpupPredFailure AllegraEra

instance InjectRuleFailure "PPUP" Shelley.ShelleyPpupPredFailure AllegraEra
