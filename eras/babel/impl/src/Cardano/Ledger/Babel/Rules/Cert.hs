{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Cert where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure,
 )
import Cardano.Ledger.Core (EraRuleFailure, InjectRuleFailure)

type instance EraRuleFailure "CERT" (BabelEra c) = ConwayCertPredFailure (BabelEra c)

instance InjectRuleFailure "CERT" ConwayCertPredFailure (BabelEra c)
