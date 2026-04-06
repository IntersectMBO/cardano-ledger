{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Api.Arbitrary () where

import Cardano.Ledger.Api.State.Query
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

instance Arbitrary MemberStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary QueryPoolStateResult where
  arbitrary = QueryPoolStateResult <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QueryResultStakeSnapshot where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary QueryResultStakeSnapshots where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary HotCredAuthStatus where
  arbitrary =
    oneof
      [ MemberAuthorized <$> arbitrary
      , pure MemberNotAuthorized
      , MemberResigned <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary NextEpochChange where
  arbitrary =
    oneof
      [ pure ToBeEnacted
      , pure ToBeRemoved
      , pure NoChangeExpected
      , pure ToBeExpired
      , TermAdjusted <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary QueryResultCommitteeMemberState where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultCommitteeMembersState where
  arbitrary = genericArbitraryU

instance Arbitrary DefaultVote where
  arbitrary = elements [DefaultNo, DefaultAbstain, DefaultNoConfidence]

instance Arbitrary QueryResultConstitution where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultDRepState where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultDRepStates where
  arbitrary = genericArbitraryU

instance Arbitrary QueryResultDelegsAndRewards where
  arbitrary = genericArbitraryU
