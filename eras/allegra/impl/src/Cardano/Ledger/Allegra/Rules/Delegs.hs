{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Delegs () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Delpl ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELEGS" AllegraEra = Shelley.ShelleyDelegsPredFailure AllegraEra

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelegsPredFailure AllegraEra

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelplPredFailure AllegraEra where
  injectFailure = Shelley.DelplFailure

instance InjectRuleFailure "DELEGS" Shelley.ShelleyPoolPredFailure AllegraEra where
  injectFailure = Shelley.DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelegPredFailure AllegraEra where
  injectFailure = Shelley.DelplFailure . injectFailure
