{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Pool () where

import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (PoolEvent, ShelleyPoolPredFailure)

type instance EraRuleFailure "POOL" ConwayEra = ShelleyPoolPredFailure ConwayEra

type instance EraRuleEvent "POOL" ConwayEra = PoolEvent ConwayEra

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure ConwayEra

instance InjectRuleEvent "POOL" PoolEvent ConwayEra
