{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Deleg () where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
 )

type instance EraRuleFailure "DELEG" (BabbageEra c) = ShelleyDelegPredFailure (BabbageEra c)

instance InjectRuleFailure "DELEG" ShelleyDelegPredFailure (BabbageEra c)
