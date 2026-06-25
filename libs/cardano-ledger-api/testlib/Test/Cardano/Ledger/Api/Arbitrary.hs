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

instance Arbitrary StakeSnapshot where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary StakeSnapshots where
  arbitrary = genericArbitraryU
  shrink = genericShrink
