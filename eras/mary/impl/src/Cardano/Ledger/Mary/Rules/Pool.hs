{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Pool () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.Rules (PoolEvent, ShelleyPoolPredFailure)

type instance EraRuleFailure "POOL" MaryEra = ShelleyPoolPredFailure MaryEra

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure MaryEra

type instance EraRuleEvent "POOL" MaryEra = PoolEvent MaryEra

instance InjectRuleEvent "POOL" PoolEvent MaryEra
