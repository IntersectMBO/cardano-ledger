module Test.Compact.KeyMap where

import Data.Compact.KeyMap as KeyMap
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary Key where
  arbitrary =
    oneof
      [ Key <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        Key <$> chooseAny <*> chooseAny <*> chooseAny <*> chooseAny
      ]

instance Arbitrary a => Arbitrary (KeyMap a) where
  arbitrary = do
    let go i m
          | i > 0 = do
            key <- arbitrary
            val <- arbitrary
            go (i - 1) $! insert key val m
          | otherwise = pure m
    NonNegative n <- arbitrary
    go (n :: Int) KeyMap.Empty

prop_RountripToFromList :: KeyMap Int -> Property
prop_RountripToFromList km = KeyMap.fromList (KeyMap.toList km) === km

keyMapTests :: TestTree
keyMapTests =
  testGroup "KeyMap" [testProperty "to/fromList" prop_RountripToFromList]
