{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Delegs () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Delpl ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure (..),
  ShelleyDelplPredFailure,
  ShelleyPoolPredFailure,
 )

type instance EraRuleFailure "DELEGS" AllegraEra = ShelleyDelegsPredFailure AllegraEra

instance InjectRuleFailure "DELEGS" ShelleyDelegsPredFailure AllegraEra

instance InjectRuleFailure "DELEGS" ShelleyDelplPredFailure AllegraEra where
  injectFailure = DelplFailure

instance InjectRuleFailure "DELEGS" ShelleyPoolPredFailure AllegraEra where
  injectFailure = DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" ShelleyDelegPredFailure AllegraEra where
  injectFailure = DelplFailure . injectFailure
