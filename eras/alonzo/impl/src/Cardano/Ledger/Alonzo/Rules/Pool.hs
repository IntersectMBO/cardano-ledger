{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Pool () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure)

type instance EraRuleFailure "POOL" (AlonzoEra c) = ShelleyPoolPredFailure (AlonzoEra c)

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure (AlonzoEra c) where
  injectFailure = id
