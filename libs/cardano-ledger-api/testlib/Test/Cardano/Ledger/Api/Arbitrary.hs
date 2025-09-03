{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Api.Arbitrary () where

import Cardano.Ledger.Api.State.Query (MemberStatus, QueryPoolStateResult (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

instance Arbitrary MemberStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary QueryPoolStateResult where
  arbitrary = QueryPoolStateResult <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
