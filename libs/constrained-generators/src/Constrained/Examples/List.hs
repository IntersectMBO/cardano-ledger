{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Examples.List where

import Data.Word

import Constrained
import Constrained.Examples.Basic

type Numbery a =
  ( Foldy BaseFn a
  , OrdLike BaseFn a
  , NumLike BaseFn a
  , Ord a
  , Enum a
  )

listSum :: Numbery a => Specification BaseFn [a]
listSum = constrained $ \as ->
  10 <=. sum_ as

listSumForall :: Numbery a => Specification BaseFn [a]
listSumForall = constrained $ \xs ->
  [ forAll xs $ \x -> 1 <. x
  , assert $ sum_ xs ==. 20
  ]

listSumRange :: Numbery a => Specification BaseFn [a]
listSumRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> 1 <. x
      , assert $ n <. 20
      , assert $ 10 <. n
      ]

listSumRangeUpper :: Numbery a => Specification BaseFn [a]
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

listSumRangeRange :: Numbery a => Specification BaseFn [a]
listSumRangeRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> [1 <. x, x <. 5]
      , assert $ n <. 20
      , assert $ 10 <. n
      ]

listSumElemRange :: Numbery a => Specification BaseFn [a]
listSumElemRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> [1 <. x, x <. 5]
      , assert $ n `elem_` lit [10, 12 .. 20]
      ]

listSumPair :: Numbery a => Specification BaseFn [(a, Int)]
listSumPair = constrained $ \xs ->
  [ assert $ foldMap_ fst_ xs ==. 100
  , forAll' xs $ \x y -> [20 <. x, x <. 30, y <. 100]
  ]

listEmpty :: Specification BaseFn [Int]
listEmpty = constrained $ \xs ->
  [ forAll xs $ \_ -> False
  , assert $ length_ xs <=. 10
  ]

pairListError :: Specification BaseFn [(Int, Int)]
pairListError = constrained $ \ps ->
  [ assert $ length_ ps <=. 10
  , forAll' ps $ \a b ->
      [ a `elem_` lit [1 .. 8]
      , a ==. 9
      , b ==. a
      ]
  ]

listMustSizeIssue :: Specification BaseFn [Int]
listMustSizeIssue = constrained $ \xs ->
  [ 1 `elem_` xs
  , length_ xs ==. 1
  ]

sumListBad :: Specification BaseFn [Word64]
sumListBad = constrained $ \xs ->
  [ forAll xs $ \x -> unsafeExists $ \y -> y ==. x
  , assert $ sum_ xs ==. lit 10
  ]

listExistsUnfree :: Specification BaseFn [Int]
listExistsUnfree = constrained $ \xs ->
  [ forAll xs $ \x -> x `satisfies` existsUnfree
  , assert $ sizeOf_ xs ==. 3
  ]

listSumShort :: Specification BaseFn [Int]
listSumShort = constrained $ \ [var| xs |] ->
  [ assert $ sizeOf_ xs <=. 4
  , assert $ sum_ xs <=. 100000
  , forAll xs $ \ [var| x |] ->
      [ exists (const $ pure True) $ \b ->
          whenTrue b $ x <=. 10000000
      ]
  ]

appendSize :: Specification BaseFn ([Int], [Int])
appendSize = constrained' $ \ [var| xs |] [var| ys |] ->
  [ assert $ sizeOf_ xs <=. 10
  , assert $ sizeOf_ (ys ++. xs) <=. 15
  ]

appendSingleton :: Specification BaseFn Int
appendSingleton = constrained $ \ [var| x |] ->
  10 `elem_` singletonList_ x ++. lit [1, 2, 3]

singletonSubset :: Specification BaseFn Int
singletonSubset = constrained $ \ [var| x |] ->
  fromList_ (singletonList_ x) `subset_` fromList_ (lit [1, 2, 3])

-- Some notable error cases that shouldn't succeed

singletonErrorTooMany :: Specification BaseFn Int
singletonErrorTooMany = constrained $ \ [var| x |] ->
  fromList_ (lit [1, 2, 3]) `subset_` fromList_ (singletonList_ x)

singletonErrorTooLong :: Specification BaseFn Int
singletonErrorTooLong = constrained $ \ [var| x |] ->
  2 <=. length_ (singletonList_ x)

appendTooLong :: Specification BaseFn [Int]
appendTooLong = constrained $ \ [var| xs |] ->
  sizeOf_ (lit [1, 2, 3, 4] ++. xs) <=. 3
