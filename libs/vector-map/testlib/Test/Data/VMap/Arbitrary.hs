{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.VMap.Arbitrary () where

import qualified Data.VMap as VMap
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance
  (Ord k, VMap.Vector kv k, VMap.Vector vv v, Arbitrary k, Arbitrary v) =>
  Arbitrary (VMap.VMap kv vv k v)
  where
  arbitrary = VMap.fromMap <$> arbitrary
  shrink = fmap VMap.fromList . shrink . VMap.toList
