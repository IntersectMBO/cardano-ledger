{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Api.Arbitrary () where

import Cardano.Ledger.Api.State.Query
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

instance Arbitrary MemberStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary HotCredAuthStatus where
  arbitrary = genericArbitraryU

instance Arbitrary NextEpochChange where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultCommitteeMemberState where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultCommitteeMembersState where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultConstitution where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultDRepState where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultDelegsAndRewards where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultDRepStates where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultPoolState where
  arbitrary = QueryResultPoolState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QueryResultStakeSnapshot where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary QueryResultStakeSnapshots where
  arbitrary = genericArbitraryU
  shrink = genericShrink
