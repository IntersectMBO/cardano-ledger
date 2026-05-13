{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Pool () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "POOL" MaryEra = Shelley.ShelleyPoolPredFailure MaryEra

instance InjectRuleFailure "POOL" Shelley.ShelleyPoolPredFailure MaryEra

type instance EraRuleEvent "POOL" MaryEra = Shelley.PoolEvent MaryEra

instance InjectRuleEvent "POOL" Shelley.PoolEvent MaryEra
