{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Pool () where

import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core (EraRuleEvent, EraRuleFailure)
import Cardano.Ledger.Shelley.Rules (PoolEvent, ShelleyPoolPredFailure)

type instance EraRuleFailure "POOL" (ConwayEra c) = ShelleyPoolPredFailure (ConwayEra c)

type instance EraRuleEvent "POOL" (ConwayEra c) = PoolEvent (ConwayEra c)
