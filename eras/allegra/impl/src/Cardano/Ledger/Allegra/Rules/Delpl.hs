{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Delpl () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Deleg ()
import Cardano.Ledger.Allegra.Rules.Pool ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelplPredFailure (..),
  ShelleyPoolPredFailure,
 )

type instance EraRuleFailure "DELPL" (AllegraEra c) = ShelleyDelplPredFailure (AllegraEra c)

instance InjectRuleFailure "DELPL" ShelleyDelplPredFailure (AllegraEra c) where
  injectFailure = id

instance InjectRuleFailure "DELPL" ShelleyPoolPredFailure (AllegraEra c) where
  injectFailure = PoolFailure

instance InjectRuleFailure "DELPL" ShelleyDelegPredFailure (AllegraEra c) where
  injectFailure = DelegFailure
