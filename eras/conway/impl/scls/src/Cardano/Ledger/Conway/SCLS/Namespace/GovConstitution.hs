{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution (
  module Cardano.Ledger.SCLS.Namespace.GovConstitution.V0,
) where

import Cardano.Ledger.SCLS.Namespace.GovConstitution.V0
import Cardano.Ledger.Conway.Governance (Constitution (..))
import Cardano.Ledger.Conway (ConwayEra)

instance IsCanonicalConstitution (Constitution ConwayEra) where
  mkCanonicalConstitution Constitution {..} = CanonicalConstitution {..}
  fromCanonicalConstitution CanonicalConstitution {..} = Constitution {..}