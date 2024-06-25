{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Pool where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Core (EraRuleEvent, EraRuleFailure)
import Cardano.Ledger.Shelley.Rules (PoolEvent, ShelleyPoolPredFailure)

type instance EraRuleFailure "POOL" (BabelEra c) = ShelleyPoolPredFailure (BabelEra c)

type instance EraRuleEvent "POOL" (BabelEra c) = PoolEvent (BabelEra c)
