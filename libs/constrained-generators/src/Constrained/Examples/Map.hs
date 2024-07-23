{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Examples.Map where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word

import Constrained
import Constrained.Examples.Basic

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

mapRestrictedValues :: Specification BaseFn (Map (Either Int ()) Int)
mapRestrictedValues = constrained $ \m ->
  [ assert $ sizeOf_ (dom_ m) ==. 6
  , forAll' m $ \k v ->
      [ caseOn
          k
          (branch $ \_ -> 20 <=. v)
          (branch $ \_ -> True)
      , v `dependsOn` k
      ]
  ]

-- NOTE: this fails if you pick the values of the map first - you're unlikely to generate
-- three values such that two of them are <= -100 and >= 100 respectively even though
-- you take satisfiability of the whole elem constraint into account. This can't be fixed
-- with a `dependsOn v k` because the issue is that we've generated a bunch of values
-- before we ever go to generate the keys.
mapRestrictedValuesThree :: Specification BaseFn (Map Three Int)
mapRestrictedValuesThree = constrained $ \m ->
  [ assert $ sizeOf_ (dom_ m) ==. 3
  , forAll' m $ \k v ->
      [ caseOn
          k
          (branch $ \_ -> v <=. (-100))
          (branch $ \_ -> 100 <=. v)
          (branch $ \_ -> True)
      , -- This is important to demonstrate the point that keys sometimes need to be solved before
        -- values
        v `dependsOn` k
      ]
  ]

mapRestrictedValuesBool :: Specification BaseFn (Map Bool Int)
mapRestrictedValuesBool = constrained $ \m ->
  [ assert $ sizeOf_ (dom_ m) ==. 2
  , forAll' m $ \k v -> [v `dependsOn` k, whenTrue k (100 <=. v)]
  ]

mapSetSmall :: Specification BaseFn (Map (Set Int) Int)
mapSetSmall = constrained $ \x ->
  forAll (dom_ x) $ \d ->
    assert $ subset_ d $ lit (Set.fromList [3 .. 4])

mapIsJust :: Specification BaseFn (Int, Int)
mapIsJust = constrained' $ \ [var| x |] [var| y |] ->
  assert $ cJust_ x ==. lookup_ y (lit $ Map.fromList [(z, z) | z <- [100 .. 102]])
