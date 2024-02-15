{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Deleg () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
 )

type instance EraRuleFailure "DELEG" (AllegraEra c) = ShelleyDelegPredFailure (AllegraEra c)

instance InjectRuleFailure "DELEG" ShelleyDelegPredFailure (AllegraEra c)
