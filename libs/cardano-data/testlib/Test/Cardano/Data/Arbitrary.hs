{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data.Arbitrary where

import Data.OMap.Strict qualified as OMap
import Data.OSet.Strict qualified as OSet
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (OSet.OSet a) where
  arbitrary = OSet.fromSet <$> arbitrary

instance (Ord k, Ord v, Arbitrary v, OMap.HasOKey k v) => Arbitrary (OMap.OMap k v) where
  arbitrary = OMap.fromSet <$> arbitrary
