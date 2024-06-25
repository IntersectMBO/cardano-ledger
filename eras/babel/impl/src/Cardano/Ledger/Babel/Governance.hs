{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Governance where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.PParams ()
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  cgsCommitteeL,
  cgsConstitutionL,
  cgsDRepPulsingStateL,
  cgsProposalsL,
 )
import Cardano.Ledger.Crypto (Crypto)

instance Crypto c => ConwayEraGov (BabelEra c) where
  constitutionGovStateL = cgsConstitutionL
  proposalsGovStateL = cgsProposalsL
  drepPulsingStateGovStateL = cgsDRepPulsingStateL
  committeeGovStateL = cgsCommitteeL