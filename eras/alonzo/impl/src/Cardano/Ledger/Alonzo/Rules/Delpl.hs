{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Delpl () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Deleg ()
import Cardano.Ledger.Alonzo.Rules.Pool ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELPL" AlonzoEra = Shelley.ShelleyDelplPredFailure AlonzoEra

instance InjectRuleFailure "DELPL" Shelley.ShelleyDelplPredFailure AlonzoEra

instance InjectRuleFailure "DELPL" Shelley.ShelleyPoolPredFailure AlonzoEra where
  injectFailure = Shelley.PoolFailure

instance InjectRuleFailure "DELPL" Shelley.ShelleyDelegPredFailure AlonzoEra where
  injectFailure = Shelley.DelegFailure
