{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Pool () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "POOL" AlonzoEra = Shelley.ShelleyPoolPredFailure AlonzoEra

instance InjectRuleFailure "POOL" Shelley.ShelleyPoolPredFailure AlonzoEra

type instance EraRuleEvent "POOL" AlonzoEra = Shelley.PoolEvent AlonzoEra

instance InjectRuleEvent "POOL" Shelley.PoolEvent AlonzoEra
