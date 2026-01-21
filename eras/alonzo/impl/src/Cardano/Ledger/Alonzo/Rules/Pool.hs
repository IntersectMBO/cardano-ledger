{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Pool () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (PoolEvent, ShelleyPoolPredFailure)

type instance EraRuleFailure "POOL" AlonzoEra = ShelleyPoolPredFailure AlonzoEra

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure AlonzoEra

type instance EraRuleEvent "POOL" AlonzoEra = PoolEvent AlonzoEra

instance InjectRuleEvent "POOL" PoolEvent AlonzoEra
