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

type instance EraRuleFailure "DELEGS" (AllegraEra c) = ShelleyDelegsPredFailure (AllegraEra c)

instance InjectRuleFailure "DELEGS" ShelleyDelegsPredFailure (AllegraEra c) where
  injectFailure = id

instance InjectRuleFailure "DELEGS" ShelleyDelplPredFailure (AllegraEra c) where
  injectFailure = DelplFailure

instance InjectRuleFailure "DELEGS" ShelleyPoolPredFailure (AllegraEra c) where
  injectFailure = DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" ShelleyDelegPredFailure (AllegraEra c) where
  injectFailure = DelplFailure . injectFailure
