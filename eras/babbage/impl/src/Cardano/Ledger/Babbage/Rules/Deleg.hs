{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Deleg () where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELEG" BabbageEra = Shelley.ShelleyDelegPredFailure BabbageEra

instance InjectRuleFailure "DELEG" Shelley.ShelleyDelegPredFailure BabbageEra
