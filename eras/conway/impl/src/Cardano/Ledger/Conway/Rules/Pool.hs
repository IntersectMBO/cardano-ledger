{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Conway.Rules.Pool () where

import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core (EraRuleFailure)
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure)

type instance
  EraRuleFailure "POOL" (ConwayEra c) =
    ShelleyPoolPredFailure (ConwayEra c)
