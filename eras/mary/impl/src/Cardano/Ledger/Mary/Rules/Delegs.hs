{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Delegs () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Delpl ()
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELEGS" MaryEra = Shelley.ShelleyDelegsPredFailure MaryEra

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelegsPredFailure MaryEra

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelplPredFailure MaryEra where
  injectFailure = Shelley.DelplFailure

instance InjectRuleFailure "DELEGS" Shelley.ShelleyPoolPredFailure MaryEra where
  injectFailure = Shelley.DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelegPredFailure MaryEra where
  injectFailure = Shelley.DelplFailure . injectFailure
