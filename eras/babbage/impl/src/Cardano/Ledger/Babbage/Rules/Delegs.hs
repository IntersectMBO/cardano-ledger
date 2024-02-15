{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Delegs () where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules.Delpl ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure (..),
  ShelleyDelplPredFailure,
  ShelleyPoolPredFailure,
 )

type instance EraRuleFailure "DELEGS" (BabbageEra c) = ShelleyDelegsPredFailure (BabbageEra c)

instance InjectRuleFailure "DELEGS" ShelleyDelegsPredFailure (BabbageEra c)

instance InjectRuleFailure "DELEGS" ShelleyDelplPredFailure (BabbageEra c) where
  injectFailure = DelplFailure

instance InjectRuleFailure "DELEGS" ShelleyPoolPredFailure (BabbageEra c) where
  injectFailure = DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" ShelleyDelegPredFailure (BabbageEra c) where
  injectFailure = DelplFailure . injectFailure
