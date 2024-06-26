{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Deleg where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Conway.Rules (
  ConwayDelegPredFailure,
 )
import Cardano.Ledger.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure, VoidEraRule)

type instance EraRuleFailure "DELEG" (BabelEra c) = ConwayDelegPredFailure (BabelEra c)

instance InjectRuleFailure "DELEG" ConwayDelegPredFailure (BabelEra c)

type instance EraRuleEvent "DELEG" (BabelEra c) = VoidEraRule "DELEG" (BabelEra c)
