{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data.Arbitrary (genOSet) where

import Data.Map.NonEmpty as NEM
import Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.OMap.Strict qualified as OMap
import Data.OSet.Strict qualified as OSet
import Data.Set qualified as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty qualified as NES
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (OSet.OSet a) where
  arbitrary = genOSet arbitrary

genOSet :: Ord a => Gen a -> Gen (OSet.OSet a)
genOSet = fmap OSet.fromFoldable . listOf

instance (Arbitrary v, OMap.HasOKey k v, Arbitrary k) => Arbitrary (OMap.OMap k v) where
  arbitrary = OMap.fromFoldable @[] <$> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (NonEmptySet a) where
  arbitrary = do
    el <- arbitrary
    fromJust . NES.fromSet . Set.insert el <$> arbitrary

  shrink nes =
    [ fromJust $ NES.fromSet xs'
    | xs' <- shrink $ NES.toSet nes
    , not (Set.null xs')
    ]

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (NonEmptyMap k v) where
  arbitrary = do
    k <- arbitrary
    v <- arbitrary
    fromJust . NEM.fromMap . Map.insert k v <$> arbitrary

  shrink nem =
    [ fromJust $ NEM.fromMap xs'
    | xs' <- shrink $ NEM.toMap nem
    , not (Map.null xs')
    ]
