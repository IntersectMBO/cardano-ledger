{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Pool () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure)

type instance EraRuleFailure "POOL" AllegraEra = ShelleyPoolPredFailure AllegraEra

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure AllegraEra
