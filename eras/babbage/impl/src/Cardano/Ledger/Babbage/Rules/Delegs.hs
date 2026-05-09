{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Delegs () where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules.Delpl ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELEGS" BabbageEra = Shelley.ShelleyDelegsPredFailure BabbageEra

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelegsPredFailure BabbageEra

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelplPredFailure BabbageEra where
  injectFailure = Shelley.DelplFailure

instance InjectRuleFailure "DELEGS" Shelley.ShelleyPoolPredFailure BabbageEra where
  injectFailure = Shelley.DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" Shelley.ShelleyDelegPredFailure BabbageEra where
  injectFailure = Shelley.DelplFailure . injectFailure
