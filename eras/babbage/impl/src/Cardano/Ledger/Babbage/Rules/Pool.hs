{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Pool () where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "POOL" BabbageEra = Shelley.ShelleyPoolPredFailure BabbageEra

instance InjectRuleFailure "POOL" Shelley.ShelleyPoolPredFailure BabbageEra

type instance EraRuleEvent "POOL" BabbageEra = Shelley.PoolEvent BabbageEra

instance InjectRuleEvent "POOL" Shelley.PoolEvent BabbageEra
