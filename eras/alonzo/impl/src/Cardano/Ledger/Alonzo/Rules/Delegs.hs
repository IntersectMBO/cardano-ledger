{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Delegs () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Delpl ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELEGS" AlonzoEra = Shelley.ShelleyDelegsPredFailure AlonzoEra

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelegsPredFailure AlonzoEra

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelplPredFailure AlonzoEra where
  injectFailure = Shelley.DelplFailure

instance InjectRuleFailure "DELEGS" Shelley.ShelleyPoolPredFailure AlonzoEra where
  injectFailure = Shelley.DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelegPredFailure AlonzoEra where
  injectFailure = Shelley.DelplFailure . injectFailure
