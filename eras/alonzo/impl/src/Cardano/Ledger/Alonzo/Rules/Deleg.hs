{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Deleg () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyDelegPredFailure)

type instance EraRuleFailure "DELEG" (AlonzoEra c) = ShelleyDelegPredFailure (AlonzoEra c)

instance InjectRuleFailure "DELEG" ShelleyDelegPredFailure (AlonzoEra c)
