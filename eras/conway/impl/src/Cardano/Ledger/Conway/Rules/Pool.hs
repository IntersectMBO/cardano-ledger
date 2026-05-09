{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Pool () where

import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "POOL" ConwayEra = Shelley.ShelleyPoolPredFailure ConwayEra

type instance EraRuleEvent "POOL" ConwayEra = Shelley.PoolEvent ConwayEra

instance InjectRuleFailure "POOL" Shelley.ShelleyPoolPredFailure ConwayEra

instance InjectRuleEvent "POOL" Shelley.PoolEvent ConwayEra
