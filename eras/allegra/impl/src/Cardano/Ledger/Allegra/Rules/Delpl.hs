{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Delpl () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Deleg ()
import Cardano.Ledger.Allegra.Rules.Pool ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELPL" AllegraEra = Shelley.ShelleyDelplPredFailure AllegraEra

instance InjectRuleFailure "DELPL" Shelley.ShelleyDelplPredFailure AllegraEra

instance InjectRuleFailure "DELPL" Shelley.ShelleyPoolPredFailure AllegraEra where
  injectFailure = Shelley.PoolFailure

instance InjectRuleFailure "DELPL" Shelley.ShelleyDelegPredFailure AllegraEra where
  injectFailure = Shelley.DelegFailure
