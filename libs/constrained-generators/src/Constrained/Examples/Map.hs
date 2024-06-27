{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.Examples.Map where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word

import Constrained
import Constrained.Examples.Basic
import Test.QuickCheck.Gen

mapElemSpec :: Specification BaseFn (Map Int (Bool, Int))
mapElemSpec = constrained $ \m ->
  [ assert $ m /=. lit mempty
  , forAll' (rng_ m) $ \_ b ->
      [0 <. b, b <. 10]
  ]

mapPairSpec :: Specification BaseFn (Map Int Int, Set Int)
mapPairSpec = constrained' $ \m s ->
  subset_ (dom_ m) s

mapEmptyDomainSpec :: Specification BaseFn (Map Int Int)
mapEmptyDomainSpec = constrained $ \m ->
  subset_ (dom_ m) mempty -- mempty in the Monoid instance (Term fn (Set a))

mapSubSize :: Specification BaseFn (Map Int Int)
mapSubSize = constrained $ \s ->
  2 ==. 12 - (sizeOf_ s)

knownDomainMap :: Specification BaseFn (Map Int Int)
knownDomainMap = constrained $ \m ->
  [ dom_ m ==. lit (Set.fromList [1, 2])
  , not_ $ 0 `elem_` rng_ m
  ]

mapSizeConstrained :: Specification BaseFn (Map Three Int)
mapSizeConstrained = constrained $ \m -> size_ (dom_ m) <=. 3

sumRange :: Specification BaseFn (Map Word64 Word64)
sumRange = constrained $ \m -> sum_ (rng_ m) ==. lit 10

fixedRange :: Specification BaseFn (Map Int Int)
fixedRange = constrained $ \m ->
  [ forAll (rng_ m) (\x -> x ==. 5)
  , assert $ (sizeOf_ m) ==. 1
  ]

rangeHint :: Specification BaseFn (Map Int Int)
rangeHint = constrained $ \m ->
  genHint 10 (rng_ m)

rangeSumSize :: Specification BaseFn (Map Int Int)
rangeSumSize = constrained $ \m ->
  [ assert $ sizeOf_ m <=. 0
  , assert $ sum_ (rng_ m) <=. 0
  , assert $ (-1) <=. sum_ (rng_ m)
  , forAll' m $ \k v ->
      [ k ==. (-1)
      , v ==. 1
      ]
  ]

elemSpec :: Specification BaseFn (Int, Int, Map Int Int)
elemSpec = constrained' $ \k v m ->
  [ assert $ k `member_` dom_ m
  , forAll' m $ \k' v' ->
      whenTrue (k' ==. k) (v' ==. v)
  , m `dependsOn` k
  ]

lookupSpecific :: Specification BaseFn (Int, Int, Map Int Int)
lookupSpecific = constrained' $ \k v m ->
  [ m `dependsOn` k
  , assert $ lookup_ k m ==. cJust_ v
  ]

rngSetSpec :: Specification BaseFn (Map Int Int)
rngSetSpec = constrained $ \m -> 3 <=. sizeOf_ (rngSet_ m)

richRngSetSpec :: Specification BaseFn (Map Int Int)
richRngSetSpec =
  constrained $ \m ->
    [ assert $ lit (Set.fromList [20, 30]) `subset_` dom_ m
    , assert $ elem_ (lit 3) (rng_ m)
    , assert $ elem_ (lit 4) (rng_ m)
    , assert $ sizeOf_ (dom_ m) >=. 10
    , assert $ sizeOf_ (dom_ m) <=. 12
    , assert $ sizeOf_ (rngSet_ m) >=. 4
    , assert $ sizeOf_ (rngSet_ m) <=. 6
    , forAll m (\p -> match p (\k v -> [assert $ k >=. v]))
    ]

rngSetWithHint :: Specification BaseFn (Map Int Int)
rngSetWithHint = constrained $ \m ->
  [ assert $ 3 <=. sizeOf_ (rngSet_ m)
  , genHint 5 m
  ]

-- Different ways to constrain the size of a Map

sizeOfMap :: Specification BaseFn (Map Int Int)
sizeOfMap = constrained $ \m ->
  [assert $ sizeOf_ m ==. 10]

sizeOfDomMap :: Specification BaseFn (Map Int Int)
sizeOfDomMap = constrained $ \m ->
  [assert $ sizeOf_ (dom_ m) ==. 10] -- sizeOf_ works on things that are Sized

sizeDomMap :: Specification BaseFn (Map Three Int)
sizeDomMap = constrained $ \m -> size_ (dom_ m) <=. 3 -- size_ works on Sets,

sizeOfRngMap :: Specification BaseFn (Map Int Int)
sizeOfRngMap = constrained $ \m ->
  [assert $ sizeOf_ (rng_ m) ==. 10]

sizeOfRngSetMap :: Specification BaseFn (Map Int Int)
sizeOfRngSetMap = constrained $ \m ->
  [assert $ sizeOf_ (rngSet_ m) ==. 4]

sizeNotEqualEmpty :: Specification BaseFn (Map Int Int)
sizeNotEqualEmpty = constrained $ \m -> [assert $ m /=. lit mempty]

sizeEqualEmpty :: Specification BaseFn (Map Int Int)
sizeEqualEmpty = constrained $ \m -> [assert $ m ==. lit mempty]

-- | When a test on a Map spec fails, this function traces the steps, to help
--   discover what went wrong.
manualMapTest ::
  forall a b.
  (Ord a, Ord b, HasSpec BaseFn a, IsNormalType a, HasSpec BaseFn b, IsNormalType b) =>
  Specification BaseFn (Map a b) ->
  IO Bool
manualMapTest g1 = do
  let g2 :: Specification BaseFn (Map a b)
      g2 = simplifySpec g1
  putStrLn (show g2)
  let g3 :: IO (Map a b)
      g3 = generate $ genFromGenT $ genFromSpecT @BaseFn @_ @GE g2
  ans <- g3
  let rangeset = Set.toList (Set.fromList (Map.elems ans))
  putStrLn ("Answer =" ++ show (Map.toList ans) ++ ",   size=" ++ show (Map.size ans))
  putStrLn ("Range =" ++ show (Map.elems ans))
  putStrLn ("RangeSet =" ++ show rangeset ++ ",   size=" ++ show (length rangeset))
  pure (conformsToSpec @BaseFn @(Map a b) ans g2)
