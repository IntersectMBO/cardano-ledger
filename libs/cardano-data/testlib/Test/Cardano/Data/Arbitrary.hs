{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data.Arbitrary (genOSet) where

import Data.OMap.Strict qualified as OMap
import Data.OSet.Strict qualified as OSet
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (OSet.OSet a) where
  arbitrary = genOSet arbitrary

genOSet :: Ord a => Gen a -> Gen (OSet.OSet a)
genOSet = fmap OSet.fromFoldable . listOf

instance (Arbitrary v, OMap.HasOKey k v, Arbitrary k) => Arbitrary (OMap.OMap k v) where
  arbitrary = OMap.fromFoldable <$> arbitrary @[v]
