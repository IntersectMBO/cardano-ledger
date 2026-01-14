{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Arbitrary (

) where

import Cardano.Ledger.Conway.SCLS.Common
import Cardano.Ledger.Conway.SCLS.Namespace.Blocks
import Cardano.Ledger.Conway.SCLS.Namespace.GovCommittee
import Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution
import Cardano.Ledger.Conway.SCLS.Namespace.GovPParams
import Cardano.Ledger.Conway.SCLS.Namespace.GovProposals
import Cardano.Ledger.Conway.SCLS.Namespace.PoolStake
import Cardano.Ledger.Conway.SCLS.Namespace.Pots
import Cardano.Ledger.Conway.SCLS.Namespace.Snapshots
import Cardano.Ledger.Conway.SCLS.Namespace.UTxO
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck.Arbitrary

deriving newtype instance Arbitrary BlockOut

deriving newtype instance Arbitrary CanonicalCommitteeState

instance Arbitrary (CanonicalCredential kr) where
  arbitrary = fmap mkCanonicalCredential arbitrary

instance Arbitrary CanonicalConstitution where
  arbitrary = genericArbitraryU

instance Arbitrary CanonicalCommitteeAuthorization where
  arbitrary = fmap mkCanonicalCommitteeAuthorization arbitrary

instance Arbitrary CanonicalPParams where
  arbitrary = fmap mkCanonicalPParams arbitrary

instance Arbitrary CanonicalGovAction where arbitrary = fmap mkCanonicalGovAction arbitrary

instance Arbitrary CanonicalGovActionState where
  arbitrary = fmap mkCanonicalGovActionState arbitrary

instance Arbitrary CanonicalProposalProcedure where
  arbitrary = fmap mkCanonicalProposalProcedure arbitrary

instance Arbitrary CanonicalPParamsUpdate where arbitrary = fmap mkCanonicalPParamsUpdate arbitrary

instance Arbitrary CanonicalDRepVotingThresholds where
  arbitrary = fmap mkCanonicalDRepVotingThresholds arbitrary

instance Arbitrary CanonicalPoolVotingThresholds where
  arbitrary = fmap mkCanonicalPoolVotingThresholds arbitrary

instance Arbitrary PoolStakeOut where arbitrary = genericArbitraryU

instance Arbitrary PotsOut where arbitrary = genericArbitraryU

instance Arbitrary SnapShotOut where arbitrary = genericArbitraryU

instance Arbitrary UtxoOut where arbitrary = genericArbitraryU

instance Arbitrary CanonicalExUnits where arbitrary = fmap (mkCanonicalExUnits) arbitrary