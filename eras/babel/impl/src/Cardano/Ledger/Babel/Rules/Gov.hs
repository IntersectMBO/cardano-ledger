{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Gov where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Conway.Rules (
  ConwayGovPredFailure,
 )
import Cardano.Ledger.Core (EraRuleFailure, InjectRuleFailure)

type instance EraRuleFailure "GOV" (BabelEra c) = ConwayGovPredFailure (BabelEra c)

instance InjectRuleFailure "GOV" ConwayGovPredFailure (BabelEra c)
