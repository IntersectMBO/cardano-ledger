{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Compact.ViewMap where

import Data.Compact.ViewMap as VMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Proxy
import Test.QuickCheck
import Test.QuickCheck.Classes.Base
import Test.Tasty
import Test.Tasty.QuickCheck

type MapT = Map.Map Char Int

type VMapT = VMap VB VP Char Int

instance
  (Ord k, Vector kv k, Vector vv v, Arbitrary k, Arbitrary v) =>
  Arbitrary (VMap kv vv k v)
  where
  arbitrary =
    VMap.fromMap . Map.fromList <$> arbitrary

testLawsGroup :: TestName -> [Laws] -> TestTree
testLawsGroup name = testGroup name . fmap testLaws
  where
    testLaws Laws {..} =
      testGroup lawsTypeclass $ fmap (uncurry testProperty) lawsProperties

prop_Roundtrip :: (VMapT -> a) -> (a -> VMapT) -> VMapT -> Property
prop_Roundtrip to from km = from (to km) === km

prop_AsMapTo ::
  (Show a, Eq a) => (VMapT -> a) -> (MapT -> a) -> VMapT -> Property
prop_AsMapTo fromVM fromM vm = fromVM vm === fromM (toMap vm)

prop_AsMapFrom :: (a -> VMapT) -> (a -> MapT) -> a -> Property
prop_AsMapFrom mkVMap mkMap a = toMap (mkVMap a) === mkMap a

vMapTests :: TestTree
vMapTests =
  testGroup
    "VMap"
    [ testGroup
        "roundtrip"
        [ testProperty "to/fromAscDistinctList" $
            prop_Roundtrip VMap.toAscList VMap.fromDistinctAscList,
          testProperty "to/fromAscList" $ prop_Roundtrip VMap.toAscList VMap.fromAscList,
          testProperty "to/fromList" $ prop_Roundtrip VMap.toAscList VMap.fromList,
          testProperty "to/fromMap" $ prop_Roundtrip VMap.toMap VMap.fromMap
        ],
      testGroup
        "asMap"
        [ testProperty "fromList" $ prop_AsMapFrom VMap.fromList Map.fromList,
          testProperty "fromAscListWithKey" $ \xs f ->
            prop_AsMapFrom
              (VMap.fromAscListWithKey (applyFun3 f))
              (Map.fromAscListWithKey (applyFun3 f))
              (List.sortOn fst xs),
          testProperty "fromAscListWithKeyN" $ \n xs f ->
            prop_AsMapFrom
              (VMap.fromAscListWithKeyN n (applyFun3 f))
              (Map.fromAscListWithKey (applyFun3 f) . take n)
              (List.sortOn fst xs),
          testProperty "toAscList" $ prop_AsMapTo VMap.toAscList Map.toAscList,
          testProperty "foldMapWithKey" $ \f ->
            let f' k v = applyFun2 f k v :: String
             in prop_AsMapTo (VMap.foldMapWithKey f') (Map.foldMapWithKey f'),
          testProperty "lookup" $ \k -> prop_AsMapTo (VMap.lookup k) (Map.lookup k),
          testProperty "lookup (existing)" $ \k v xs ->
            let xs' = xs <> [(k, v)]
             in (VMap.lookup k (VMap.fromList xs' :: VMapT) === Just v)
                  .&&. (Map.lookup k (Map.fromList xs' :: MapT) === Just v),
          testProperty "finsWithDefault" $ \d k ->
            prop_AsMapTo (VMap.findWithDefault d k) (Map.findWithDefault d k),
          testProperty "finsWithDefault (existing)" $ \k v xs ->
            let xs' = xs <> [(k, v)]
             in (VMap.findWithDefault undefined k (VMap.fromList xs' :: VMapT) === v)
                  .&&. (Map.findWithDefault undefined k (Map.fromList xs' :: MapT) === v)
        ],
      testLawsGroup
        "classes"
        [ eqLaws (Proxy @VMapT),
          semigroupLaws (Proxy @VMapT),
          monoidLaws (Proxy @VMapT),
          isListLaws (Proxy @VMapT)
        ]
    ]
