{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.CanonicalState.Arbitrary () where

import Cardano.Ledger.CanonicalState.Conway (mkCanonicalConstitution, fromGovActionState, CanonicalGovActionState)
import qualified Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 as GovConstitution.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 as GovPParams.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovProposals.V0 as GovProposals.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.Snapshots.V0 as Snapshots.V0
import Cardano.Ledger.CanonicalState.BasicTypes
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Constitution, GovActionState)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck (Arbitrary (..))

instance Arbitrary GovConstitution.V0.CanonicalConstitution where
  arbitrary = mkCanonicalConstitution <$> arbitrary @(Constitution ConwayEra)

instance Arbitrary GovConstitution.V0.GovConstitutionOut where
  arbitrary = genericArbitraryU

instance Arbitrary (GovPParams.V0.GovPParamsOut ConwayEra) where
  arbitrary = genericArbitraryU

instance Arbitrary (GovProposals.V0.GovProposalOut CanonicalGovActionState) where
  arbitrary = snd . fromGovActionState <$> arbitrary @(GovActionState ConwayEra)

instance Arbitrary Snapshots.V0.SnapShotOut where
  arbitrary = genericArbitraryU

instance Arbitrary CanonicalCoin where
  arbitrary = genericArbitraryU

instance Arbitrary Snapshots.V0.CanonicalStakePoolParams where
  arbitrary = Snapshots.V0.mkCanonicalStakePoolParams <$> arbitrary
