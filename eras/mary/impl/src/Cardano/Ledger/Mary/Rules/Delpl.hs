{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Delpl () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Deleg ()
import Cardano.Ledger.Mary.Rules.Pool ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelplPredFailure (..),
  ShelleyPoolPredFailure,
 )

type instance EraRuleFailure "DELPL" (MaryEra c) = ShelleyDelplPredFailure (MaryEra c)

instance InjectRuleFailure "DELPL" ShelleyDelplPredFailure (MaryEra c) where
  injectFailure = id

instance InjectRuleFailure "DELPL" ShelleyPoolPredFailure (MaryEra c) where
  injectFailure = PoolFailure

instance InjectRuleFailure "DELPL" ShelleyDelegPredFailure (MaryEra c) where
  injectFailure = DelegFailure
