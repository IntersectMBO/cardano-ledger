{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Delpl () where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules.Deleg ()
import Cardano.Ledger.Babbage.Rules.Pool ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELPL" BabbageEra = Shelley.ShelleyDelplPredFailure BabbageEra

instance InjectRuleFailure "DELPL" Shelley.ShelleyDelplPredFailure BabbageEra

instance InjectRuleFailure "DELPL" Shelley.ShelleyPoolPredFailure BabbageEra where
  injectFailure = Shelley.PoolFailure

instance InjectRuleFailure "DELPL" Shelley.ShelleyDelegPredFailure BabbageEra where
  injectFailure = Shelley.DelegFailure
