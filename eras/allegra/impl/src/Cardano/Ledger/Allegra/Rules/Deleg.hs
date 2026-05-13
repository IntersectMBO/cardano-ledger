{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Deleg () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELEG" AllegraEra = Shelley.ShelleyDelegPredFailure AllegraEra

instance InjectRuleFailure "DELEG" Shelley.ShelleyDelegPredFailure AllegraEra
