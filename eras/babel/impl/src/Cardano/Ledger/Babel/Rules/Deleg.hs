{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Deleg where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Conway.Rules (
  ConwayDelegPredFailure,
 )
import Cardano.Ledger.Core (EraRuleFailure, InjectRuleFailure)

type instance EraRuleFailure "DELEG" (BabelEra c) = ConwayDelegPredFailure (BabelEra c)

instance InjectRuleFailure "DELEG" ConwayDelegPredFailure (BabelEra c)
