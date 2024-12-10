{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Delpl () where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules.Deleg ()
import Cardano.Ledger.Babbage.Rules.Pool ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelplPredFailure (..),
  ShelleyPoolPredFailure,
 )

type instance EraRuleFailure "DELPL" BabbageEra = ShelleyDelplPredFailure BabbageEra

instance InjectRuleFailure "DELPL" ShelleyDelplPredFailure BabbageEra

instance InjectRuleFailure "DELPL" ShelleyPoolPredFailure BabbageEra where
  injectFailure = PoolFailure

instance InjectRuleFailure "DELPL" ShelleyDelegPredFailure BabbageEra where
  injectFailure = DelegFailure
