{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data.Arbitrary where

import Data.OSet.Strict
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (OSet a) where
  arbitrary = fromSet <$> arbitrary
