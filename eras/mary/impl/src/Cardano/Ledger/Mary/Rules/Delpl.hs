{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Delpl () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Deleg ()
import Cardano.Ledger.Mary.Rules.Pool ()
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELPL" MaryEra = Shelley.ShelleyDelplPredFailure MaryEra

instance InjectRuleFailure "DELPL" Shelley.ShelleyDelplPredFailure MaryEra

instance InjectRuleFailure "DELPL" Shelley.ShelleyPoolPredFailure MaryEra where
  injectFailure = Shelley.PoolFailure

instance InjectRuleFailure "DELPL" Shelley.ShelleyDelegPredFailure MaryEra where
  injectFailure = Shelley.DelegFailure
