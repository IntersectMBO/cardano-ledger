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

appendSuffix :: Specification BaseFn ([Int], [Int])
appendSuffix = constrained' $
  \ [var|x|] [var|y|] -> assert $ x ==. y ++. lit [4, 5, 6]

appendForAll :: Specification BaseFn ([Int], [Int])
appendForAll = constrained' $ \ [var| xs |] [var| ys |] ->
  [ forAll xs $ \ x -> x `elem_` lit [2,4..10]
  , assert $ xs ==. ys ++. lit [2,4,6]
  ]

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

-- | Fails because the cant set is over constrained
overconstrainedAppend :: Specification BaseFn ([Int], [Int])
overconstrainedAppend = constrained' $
  \ [var|x|] [var|y|] ->
    [ dependsOn y x
    , assert $ x ==. lit [1, 2, 3] ++. y
    , assert $ y ==. lit [4, 5, 6]
    , assert $ x /=. lit [1, 2, 3, 4, 5, 6]
    ]

{- TODO get most of these working:
-- =============================================
-- The examples

-- | Completely constrained by Split
app1 :: Specification BaseFn [Int]
app1 = constrained $
  \x -> assert $ x ==. append_ (lit [1, 2, 3]) (lit [4, 5, 6])

-- | test that the Split fails because suffix has element not even
appSuffix4 :: Specification BaseFn ([Int], [Int])
appSuffix4 = constrained' $
  \ [var|x|] [var|y|] ->
    [ forAll x $ \i -> satisfies i even
    , assert $ x ==. append_ y (lit [4, 5, 6]) -- 5 fails even
    ]

-- | test that the suffix makes the length too long for the size constraint
appSuffix5 :: Specification BaseFn ([Int], [Int])
appSuffix5 = constrained' $
  \ [var|x|] [var|y|] ->
    [ assert $ sizeOf_ x <=. 4
    , assert $ x ==. append_ y (lit [1, 2, 3, 4, 5])
    ]

-- | test that 'x' is overconstrained in the suffix.
appSuffix6 :: Specification BaseFn ([Int])
appSuffix6 = constrained $
  \ [var|x|] -> [satisfies x (endsWith [1, 2]), satisfies x (endsWith [3, 4])]

-- | test that 'x' has conflicting suffix requirements
appSuffix7 :: Specification BaseFn ([Int], [Int])
appSuffix7 = constrained' $
  \ [var|x|] [var|y|] -> [assert $ x ==. append_ (lit [1, 2, 3]) (lit [4, 5, 6]), assert $ x ==. append_ y (lit [1, 2])]

-- | another test that 'x' has conflicting suffix requirements
appSuffix8 :: Specification BaseFn ([Int], [Int])
appSuffix8 = constrained' $
  \ [var|x|] [var|y|] -> [satisfies x (endsWith [3, 4]), assert $ x ==. append_ y (lit [1, 2])]

-- | x and y only constrained by prefix
appPrefix2 :: Specification BaseFn ([Int], [Int])
appPrefix2 = constrained' $
  \ [var|x|] [var|y|] -> assert $ x ==. append_ (lit [4, 5, 6]) y

-- | test that the Split is constrained by even
appPrefix3 :: Specification BaseFn ([Int], [Int])
appPrefix3 = constrained' $
  \ [var|x|] [var|y|] ->
    [ forAll x $ \i -> satisfies i even
    , assert $ x ==. append_ (lit [4, 6]) y
    ]

-- | test that the Split fails because prefix has element not even
appPrefix4 :: Specification BaseFn ([Int], [Int])
appPrefix4 = constrained' $
  \ [var|x|] [var|y|] ->
    [ forAll x $ \i -> satisfies i even
    , assert $ x ==. append_ (lit [4, 5, 6]) y -- 5 fails even
    ]

-- | test that the prefix makes the length too long for the size constraint
appPrefix5 :: Specification BaseFn ([Int], [Int])
appPrefix5 = constrained' $
  \ [var|x|] [var|y|] ->
    [ assert $ sizeOf_ x <=. 4
    , assert $ x ==. append_ (lit [1, 2, 3, 4, 5]) y
    ]

-- | test that 'x' is overconstrained in the prefix.
appPrefix6 :: Specification BaseFn ([Int])
appPrefix6 = constrained $
  \ [var|x|] -> [satisfies x (beginsWith [1, 2]), satisfies x (beginsWith [3, 4])]

-- | test that 'y' fills out what is missing from 'x'
appPrefix7 :: Specification BaseFn ([Int], [Int])
appPrefix7 = constrained' $
  \ [var|x|] [var|y|] -> [assert $ x ==. append_ (lit [1, 2, 3]) (lit [4, 5, 6]), assert $ x ==. append_ (lit [1, 2]) y]

-}
