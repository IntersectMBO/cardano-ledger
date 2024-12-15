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

type instance EraRuleFailure "DELPL" AllegraEra = ShelleyDelplPredFailure AllegraEra

instance InjectRuleFailure "DELPL" ShelleyDelplPredFailure AllegraEra

instance InjectRuleFailure "DELPL" ShelleyPoolPredFailure AllegraEra where
  injectFailure = PoolFailure

instance InjectRuleFailure "DELPL" ShelleyDelegPredFailure AllegraEra where
  injectFailure = DelegFailure
