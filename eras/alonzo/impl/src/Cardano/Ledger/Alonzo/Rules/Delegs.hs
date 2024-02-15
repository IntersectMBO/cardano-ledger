{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Delegs () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Delpl ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure (..),
  ShelleyDelplPredFailure,
  ShelleyPoolPredFailure,
 )

type instance EraRuleFailure "DELEGS" (AlonzoEra c) = ShelleyDelegsPredFailure (AlonzoEra c)

instance InjectRuleFailure "DELEGS" ShelleyDelegsPredFailure (AlonzoEra c)

instance InjectRuleFailure "DELEGS" ShelleyDelplPredFailure (AlonzoEra c) where
  injectFailure = DelplFailure

instance InjectRuleFailure "DELEGS" ShelleyPoolPredFailure (AlonzoEra c) where
  injectFailure = DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" ShelleyDelegPredFailure (AlonzoEra c) where
  injectFailure = DelplFailure . injectFailure
