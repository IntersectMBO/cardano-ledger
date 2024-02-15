{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Ppup () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyPpupPredFailure)

type instance EraRuleFailure "PPUP" (AllegraEra c) = ShelleyPpupPredFailure (AllegraEra c)

instance InjectRuleFailure "PPUP" ShelleyPpupPredFailure (AllegraEra c)
