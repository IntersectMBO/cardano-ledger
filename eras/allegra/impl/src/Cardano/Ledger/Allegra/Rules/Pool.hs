{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Pool () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "POOL" AllegraEra = Shelley.ShelleyPoolPredFailure AllegraEra

instance InjectRuleFailure "POOL" Shelley.ShelleyPoolPredFailure AllegraEra

type instance EraRuleEvent "POOL" AllegraEra = Shelley.PoolEvent AllegraEra

instance InjectRuleEvent "POOL" Shelley.PoolEvent AllegraEra
