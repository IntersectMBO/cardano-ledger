{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Deleg () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
 )

type instance EraRuleFailure "DELEG" (MaryEra c) = ShelleyDelegPredFailure (MaryEra c)

instance InjectRuleFailure "DELEG" ShelleyDelegPredFailure (MaryEra c) where
  injectFailure = id
