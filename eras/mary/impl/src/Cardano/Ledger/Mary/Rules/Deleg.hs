{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Deleg () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELEG" MaryEra = Shelley.ShelleyDelegPredFailure MaryEra

instance InjectRuleFailure "DELEG" Shelley.ShelleyDelegPredFailure MaryEra
