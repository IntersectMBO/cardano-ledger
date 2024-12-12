{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Pool () where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure)

type instance EraRuleFailure "POOL" BabbageEra = ShelleyPoolPredFailure BabbageEra

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure BabbageEra
