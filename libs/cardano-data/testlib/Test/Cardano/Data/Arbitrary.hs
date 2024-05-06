{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data.Arbitrary (genOSet) where

import Data.Map.Strict qualified as Map
import Data.OMap.Strict qualified as OMap
import Data.OSet.Strict qualified as OSet
import Lens.Micro (set)
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (OSet.OSet a) where
  arbitrary = genOSet arbitrary

genOSet :: Ord a => Gen a -> Gen (OSet.OSet a)
genOSet = fmap OSet.fromFoldable . listOf

instance (Ord v, Arbitrary v, OMap.HasOKey k v, Arbitrary k) => Arbitrary (OMap.OMap k v) where
  arbitrary =
    fmap OMap.fromFoldable . shuffle . Map.elems . Map.mapWithKey (flip (set OMap.okeyL)) =<< arbitrary
