{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.GovCert where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Conway.Rules (
  ConwayGovCertPredFailure,
 )
import Cardano.Ledger.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure, VoidEraRule)

type instance EraRuleFailure "GOVCERT" (BabelEra c) = ConwayGovCertPredFailure (BabelEra c)

instance InjectRuleFailure "GOVCERT" ConwayGovCertPredFailure (BabelEra c)

type instance EraRuleEvent "GOVCERT" (BabelEra c) = VoidEraRule "GOVCERT" (BabelEra c)