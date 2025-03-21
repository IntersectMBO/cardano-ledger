{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Examples.List where

import Data.Word

import Constrained.API
import Constrained.Examples.Basic

type Numbery a =
  ( Foldy a
  , OrdLike a
  , NumLike a
  , Ord a
  , Enum a
  )

listSum :: Numbery a => Specification [a]
listSum = constrained $ \as ->
  10 <=. sum_ as

listSumForall :: Numbery a => Specification [a]
listSumForall = constrained $ \xs ->
  [ forAll xs $ \x -> 1 <. x
  , assert $ sum_ xs ==. 20
  ]

listSumRange :: Numbery a => Specification [a]
listSumRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> 1 <. x
      , assert $ n <. 20
      , assert $ 10 <. n
      ]

listSumRangeUpper :: Numbery a => Specification [a]
listSumRangeUpper = constrained $ \xs ->
  let n = sum_ xs
   in -- All it takes is one big negative number,
      -- then we can't get enough small ones to exceed 10
      -- in the number of tries allowed.
      -- So we make x relatively large ( <. 12), If its is
      -- relatively small ( <. 5), we can get unlucky.
      [ forAll xs $ \x -> [x <. 12]
      , assert $ n <. 20
      , assert $ 10 <. n
      ]

listSumRangeRange :: Numbery a => Specification [a]
listSumRangeRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> [1 <. x, x <. 5]
      , assert $ n <. 20
      , assert $ 10 <. n
      ]

listSumElemRange :: Numbery a => Specification [a]
listSumElemRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> [1 <. x, x <. 5]
      , assert $ n `elem_` lit [10, 12 .. 20]
      ]

listSumPair :: Numbery a => Specification [(a, Int)]
listSumPair = constrained $ \xs ->
  [ assert $ foldMap_ fst_ xs ==. 100
  , forAll' xs $ \x y -> [20 <. x, x <. 30, y <. 100]
  ]

listEmpty :: Specification [Int]
listEmpty = constrained $ \xs ->
  [ forAll xs $ \_ -> False
  , assert $ length_ xs <=. 10
  ]

pairListError :: Specification [(Int, Int)]
pairListError = constrained $ \ps ->
  [ assert $ length_ ps <=. 10
  , forAll' ps $ \a b ->
      [ a `elem_` lit [1 .. 8]
      , a ==. 9
      , b ==. a
      ]
  ]

listMustSizeIssue :: Specification [Int]
listMustSizeIssue = constrained $ \xs ->
  [ 1 `elem_` xs
  , length_ xs ==. 1
  ]

-- FIX ME, generates but the unsafeExists means it is unsound
sumListBad :: Specification [Word64]
sumListBad = constrained $ \xs ->
  [ forAll xs $ \x -> unsafeExists $ \y -> y ==. x
  , assert $ sum_ xs ==. lit 10
  ]

listExistsUnfree :: Specification [Int]
listExistsUnfree = constrained $ \xs ->
  [ forAll xs $ \x -> x `satisfies` existsUnfree
  , assert $ sizeOf_ xs ==. 3
  ]

listSumShort :: Specification [Int]
listSumShort = constrained $ \ [var| xs |] ->
  [ assert $ sizeOf_ xs <=. 4
  , assert $ sum_ xs <=. 100000
  , forAll xs $ \ [var| x |] ->
      [ exists (const $ pure True) $ \b ->
          whenTrue b $ x <=. 10000000
      ]
  ]

appendSize :: Specification ([Int], [Int])
appendSize = constrained' $ \ [var| xs |] [var| ys |] ->
  [ assert $ sizeOf_ xs <=. 10
  , assert $ sizeOf_ (ys ++. xs) <=. 15
  ]

appendSingleton :: Specification Int
appendSingleton = constrained $ \ [var| x |] ->
  10 `elem_` singletonList_ x ++. lit [1, 2, 3]

singletonSubset :: Specification Int
singletonSubset = constrained $ \ [var| x |] ->
  fromList_ (singletonList_ x) `subset_` fromList_ (lit [1, 2, 3])

appendSuffix :: Specification ([Int], [Int])
appendSuffix = constrained' $
  \ [var|x|] [var|y|] -> assert $ x ==. y ++. lit [4, 5, 6]

appendForAll :: Specification ([Int], [Int])
appendForAll = constrained' $ \ [var| xs |] [var| ys |] ->
  [ forAll xs $ \x -> x `elem_` lit [2, 4 .. 10]
  , assert $ xs ==. ys ++. lit [2, 4, 6]
  ]

-- Some notable error cases that shouldn't succeed

singletonErrorTooMany :: Specification Int
singletonErrorTooMany = constrained $ \ [var| x |] ->
  fromList_ (lit [1, 2, 3]) `subset_` fromList_ (singletonList_ x)

singletonErrorTooLong :: Specification Int
singletonErrorTooLong = constrained $ \ [var| x |] ->
  2 <=. length_ (singletonList_ x)

appendTooLong :: Specification [Int]
appendTooLong = constrained $ \ [var| xs |] ->
  sizeOf_ (lit [1, 2, 3, 4] ++. xs) <=. 3

-- | Fails because the cant set is over constrained
overconstrainedAppend :: Specification ([Int], [Int])
overconstrainedAppend = constrained' $
  \ [var|x|] [var|y|] ->
    [ dependsOn y x
    , assert $ x ==. lit [1, 2, 3] ++. y
    , assert $ y ==. lit [4, 5, 6]
    , assert $ x /=. lit [1, 2, 3, 4, 5, 6]
    ]

overconstrainedPrefixes :: Specification ([Int], [Int], [Int])
overconstrainedPrefixes = constrained' $ \ [var| xs |] [var| ys |] [var| zs |] ->
  [ xs ==. lit [1, 2, 3] ++. ys
  , xs ==. lit [3, 4, 5] ++. zs
  ]

overconstrainedSuffixes :: Specification ([Int], [Int], [Int])
overconstrainedSuffixes = constrained' $ \ [var| xs |] [var| ys |] [var| zs |] ->
  [ xs ==. ys ++. lit [1, 2, 3]
  , xs ==. zs ++. lit [3, 4, 5]
  ]

appendForAllBad :: Specification ([Int], [Int])
appendForAllBad = constrained' $ \ [var| xs |] [var| ys |] ->
  [ forAll xs $ \x -> x `elem_` lit [1 .. 10]
  , assert $ xs ==. ys ++. lit [2, 4, 11]
  ]
