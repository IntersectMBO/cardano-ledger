{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Api.Arbitrary () where

import Cardano.Ledger.Api.State.Query (MemberStatus, QueryPoolStateResult (..))
import qualified Data.Map as Map
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (genPoolParamsNoDefaultVote)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

instance Arbitrary MemberStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary QueryPoolStateResult where
  arbitrary = QueryPoolStateResult <$> genPoolParams <*> genPoolParams <*> arbitrary <*> arbitrary
    where
      genPoolParams = Map.fromList <$> listOf pair
      pair = do
        k <- arbitrary
        v <- genPoolParamsNoDefaultVote
        pure (k, v)
