{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Delpl () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Deleg ()
import Cardano.Ledger.Alonzo.Rules.Pool ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelplPredFailure (..),
  ShelleyPoolPredFailure,
 )

type instance EraRuleFailure "DELPL" (AlonzoEra c) = ShelleyDelplPredFailure (AlonzoEra c)

instance InjectRuleFailure "DELPL" ShelleyDelplPredFailure (AlonzoEra c) where
  injectFailure = id

instance InjectRuleFailure "DELPL" ShelleyPoolPredFailure (AlonzoEra c) where
  injectFailure = PoolFailure

instance InjectRuleFailure "DELPL" ShelleyDelegPredFailure (AlonzoEra c) where
  injectFailure = DelegFailure
