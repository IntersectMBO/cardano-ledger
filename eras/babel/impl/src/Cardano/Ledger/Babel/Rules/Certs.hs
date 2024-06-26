{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Certs where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure (..),
  ConwayCertsPredFailure (CertFailure),
  ConwayDelegPredFailure,
  ConwayGovCertPredFailure,
 )
import Cardano.Ledger.Core (EraRuleFailure, InjectRuleFailure (injectFailure))
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure)

type instance EraRuleFailure "CERTS" (BabelEra c) = ConwayCertsPredFailure (BabelEra c)

instance InjectRuleFailure "CERTS" ConwayCertsPredFailure (BabelEra c)

instance InjectRuleFailure "CERTS" ConwayCertPredFailure (BabelEra c) where
  injectFailure = CertFailure

instance InjectRuleFailure "CERTS" ConwayDelegPredFailure (BabelEra c) where
  injectFailure = CertFailure . DelegFailure

instance InjectRuleFailure "CERTS" ShelleyPoolPredFailure (BabelEra c) where
  injectFailure = CertFailure . PoolFailure

instance InjectRuleFailure "CERTS" ConwayGovCertPredFailure (BabelEra c) where
  injectFailure = CertFailure . GovCertFailure