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

type instance EraRuleFailure "DELPL" (BabbageEra c) = ShelleyDelplPredFailure (BabbageEra c)

instance InjectRuleFailure "DELPL" ShelleyDelplPredFailure (BabbageEra c) where
  injectFailure = id

instance InjectRuleFailure "DELPL" ShelleyPoolPredFailure (BabbageEra c) where
  injectFailure = PoolFailure

instance InjectRuleFailure "DELPL" ShelleyDelegPredFailure (BabbageEra c) where
  injectFailure = DelegFailure
