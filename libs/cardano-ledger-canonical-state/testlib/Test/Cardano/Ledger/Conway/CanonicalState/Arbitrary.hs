{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.CanonicalState.Arbitrary () where

import Cardano.Ledger.CanonicalState.Conway (mkCanonicalConstitution)
import qualified Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 as GovConstitution.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 as GovPParams.V0
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Constitution)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck (Arbitrary (..))

instance Arbitrary GovConstitution.V0.CanonicalConstitution where
  arbitrary = mkCanonicalConstitution <$> arbitrary @(Constitution ConwayEra)

instance Arbitrary GovConstitution.V0.GovConstitutionOut where
  arbitrary = genericArbitraryU

instance Arbitrary (GovPParams.V0.GovPParamsOut ConwayEra) where
  arbitrary = genericArbitraryU
