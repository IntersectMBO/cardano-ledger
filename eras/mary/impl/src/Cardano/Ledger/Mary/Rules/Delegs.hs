{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Delegs () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Delpl ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure (..),
  ShelleyDelplPredFailure,
  ShelleyPoolPredFailure,
 )

type instance EraRuleFailure "DELEGS" (MaryEra c) = ShelleyDelegsPredFailure (MaryEra c)

instance InjectRuleFailure "DELEGS" ShelleyDelegsPredFailure (MaryEra c) where
  injectFailure = id

instance InjectRuleFailure "DELEGS" ShelleyDelplPredFailure (MaryEra c) where
  injectFailure = DelplFailure

instance InjectRuleFailure "DELEGS" ShelleyPoolPredFailure (MaryEra c) where
  injectFailure = DelplFailure . injectFailure

instance InjectRuleFailure "DELEGS" ShelleyDelegPredFailure (MaryEra c) where
  injectFailure = DelplFailure . injectFailure
