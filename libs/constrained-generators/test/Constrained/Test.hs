{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Constrained.Test where

import Control.Monad
import Data.Int
import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable
import Data.Word
import GHC.Natural
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Args, Fun, forAll)
import Test.QuickCheck qualified as QC

import Constrained.Examples
import Constrained.Internals
import Constrained.Properties

------------------------------------------------------------------------
-- Test suite
------------------------------------------------------------------------

testAll :: IO ()
testAll = hspec $ tests False

tests :: Bool -> Spec
tests nightly =
  describe "constrained" . modifyMaxSuccess (\ms -> if nightly then ms * 10 else ms) $ do
    -- TODO: double-shrinking
    testSpecNoShrink "reifiesMultiple" reifiesMultiple
    testSpec "assertReal" assertReal
    testSpecNoShrink "chooseBackwards" chooseBackwards
    testSpecNoShrink "chooseBackwards'" chooseBackwards'
    -- TODO: turn this on again when QuickCheck version is bumped
    -- testSpec "whenTrueExists" whenTrueExists
    testSpec "assertRealMultiple" assertRealMultiple
    -- TODO: quickcheck version
    testSpecNoShrink "setSpec" setSpec
    testSpec "leqPair" leqPair
    testSpec "setPair" setPair
    testSpecNoShrink "listEmpty" listEmpty
    -- TODO: quickcheck version
    testSpecNoShrink "compositionalSpec" compositionalSpec
    testSpec "simplePairSpec" simplePairSpec
    testSpec "trickyCompositional" trickyCompositional
    testSpec "emptyListSpec" emptyListSpec
    testSpec "eitherSpec" eitherSpec
    testSpec "maybeSpec" maybeSpec
    testSpec "eitherSetSpec" eitherSetSpec
    testSpec "fooSpec" fooSpec
    -- TODO: this spec needs double shrinking to shrink properly
    -- so we need to figure something out about double-shrinking
    testSpecNoShrink "intSpec" intSpec
    testSpec "mapElemSpec" mapElemSpec
    testSpec "mapElemKeySpec" mapElemKeySpec
    testSpec "mapPairSpec" mapPairSpec
    testSpecNoShrink "mapEmptyDomainSpec" mapEmptyDomainSpec
    -- TODO: this _can_ be shrunk, but it's incredibly expensive to do
    -- so and it's not obvious if there is a faster way without implementing
    -- more detailed shrinking of `SuspendedSpec`s
    testSpecNoShrink "setPairSpec" setPairSpec
    -- TODO: quickcheck version
    testSpecNoShrink "fixedSetSpec" fixedSetSpec
    testSpec "setOfPairLetSpec" setOfPairLetSpec
    testSpecNoShrink "emptyEitherSpec" emptyEitherSpec
    testSpecNoShrink "emptyEitherMemberSpec" emptyEitherMemberSpec
    testSpec "setSingletonSpec" setSingletonSpec
    testSpec "pairSingletonSpec" pairSingletonSpec
    testSpec "eitherSimpleSetSpec" eitherSimpleSetSpec
    testSpecNoShrink "emptySetSpec" emptySetSpec
    testSpec "forAllAnySpec" forAllAnySpec
    testSpec "notSubsetSpec" notSubsetSpec
    testSpec "maybeJustSetSpec" maybeJustSetSpec
    testSpec "weirdSetPairSpec" weirdSetPairSpec
    testSpec "knownDomainMap" knownDomainMap
    -- TODO: figure out double-shrinking
    testSpecNoShrink "testRewriteSpec" testRewriteSpec
    testSpec "parallelLet" parallelLet
    testSpec "letExists" letExists
    testSpec "letExistsLet" letExistsLet
    testSpec "notSubset" notSubset
    testSpec "unionSized" unionSized
    -- TODO: figure out double-shrinking
    testSpecNoShrink "dependencyWeirdness" dependencyWeirdness
    testSpec "foldTrueCases" foldTrueCases
    testSpec "foldSingleCase" foldSingleCase
    testSpec "listSumPair" (listSumPair @Int)
    -- TODO: figure out double-shrinking
    testSpecNoShrink "parallelLetPair" parallelLetPair
    testSpec "mapSizeConstrained" mapSizeConstrained
    testSpec "isAllZeroTree" isAllZeroTree
    testSpec "noChildrenSameTree" noChildrenSameTree
    testSpec "isBST" isBST
    testSpecNoShrink "pairListError" pairListError
    testSpecNoShrink "listMustSizeIssue" listMustSizeIssue
    testSpec "successiveChildren" successiveChildren
    testSpec "successiveChildren8" successiveChildren8
    testSpecNoShrink "roseTreeList" roseTreeList
    testSpec "orPair" orPair
    testSpec "roseTreePairs" roseTreePairs
    testSpec "roseTreeMaybe" roseTreeMaybe
    testSpec "badTreeInteraction" badTreeInteraction
    testSpec "sumRange" sumRange
    testSpec "sumListBad" sumListBad
    testSpec "listExistsUnfree" listExistsUnfree
    -- TODO: turn this on when we bump quickcheck version
    -- testSpec "listSumShort" listSumShort
    testSpec "existsUnfree" existsUnfree
    -- TODO: double shrinking
    testSpecNoShrink "reifyYucky" reifyYucky
    testSpec "fixedRange" fixedRange
    testSpec "rangeHint" rangeHint
    testSpec "basicSpec" basicSpec
    testSpec "canFollowLike" canFollowLike
    testSpec "ifElseBackwards" ifElseBackwards
    testSpecNoShrink "three" three
    testSpecNoShrink "three'" three'
    testSpecNoShrink "threeSpecific" threeSpecific
    testSpecNoShrink "threeSpecific'" threeSpecific'
    testSpecNoShrink "trueSpecUniform" trueSpecUniform
    testSpec "ifElseMany" ifElseMany
    testSpecNoShrink "propBack" propBack
    testSpecNoShrink "propBack'" propBack'
    testSpecNoShrink "propBack''" propBack''
    testSpec "complexUnion" complexUnion
    testSpec "unionBounded" unionBounded
    testSpec "elemSpec" elemSpec
    testSpec "lookupSpecific" lookupSpecific
    testSpec "specificElemConstraints" lookupSpecific
    testSpec "mapRestrictedValues" mapRestrictedValues
    testSpec "mapRestrictedValuesThree" mapRestrictedValuesThree
    testSpec "mapRestrictedValuesBool" mapRestrictedValuesBool
    testSpec "mapSetSmall" mapSetSmall
    testSpecNoShrink "powersetPickOne" powersetPickOne
    numberyTests
    sizeTests
    numNumSpecTree
    sequence_
      [ testSpec ("intRangeSpec " ++ show i) (intRangeSpec i)
      | i <- [-1000, -100, -10, 0, 10, 100, 1000]
      ]
    describe "prop_conformEmpty" $ do
      prop "Int" $ prop_conformEmpty @BaseFn @Int
      prop "Set Int" $ prop_conformEmpty @BaseFn @(Set Int)
      prop "Map Int Int" $ prop_conformEmpty @BaseFn @(Map Int Int)
      prop "[Int]" $ prop_conformEmpty @BaseFn @[Int]
      prop "[(Int, Int)]" $ prop_conformEmpty @BaseFn @[(Int, Int)]
    prop "prop_univSound @BaseFn" $
      withMaxSuccess (if nightly then 100_000 else 10_000) $
        prop_univSound @BaseFn
    describe "prop_gen_sound @BaseFn" $ do
      modifyMaxSuccess (const $ if nightly then 10_000 else 1000) $ do
        prop "Int" $ prop_gen_sound @BaseFn @Int
        prop "Bool" $ prop_gen_sound @BaseFn @Bool
        prop "(Int, Int)" $ prop_gen_sound @BaseFn @(Int, Int)
        prop "Map Int Int" $ prop_gen_sound @BaseFn @(Map Int Int)
        -- prop "Set Int" $ prop_gen_sound @BaseFn @(Set Int)
        prop "Set Bool" $ prop_gen_sound @BaseFn @(Set Bool)
        prop "[Int]" $ prop_gen_sound @BaseFn @[Int]
        prop "[(Int, Int)]" $ prop_gen_sound @BaseFn @[(Int, Int)]
        prop "Map Bool Int" $ prop_gen_sound @BaseFn @(Map Bool Int)
      -- Slow tests that shouldn't run 1000 times
      xprop "Map (Set Int) Int" $ prop_gen_sound @BaseFn @(Map (Set Int) Int)
      prop "[(Set Int, Set Bool)]" $ prop_gen_sound @BaseFn @[(Set Int, Set Bool)]
      prop "Set (Set Bool)" $ prop_gen_sound @BaseFn @(Set (Set Bool))
    negativeTests

negativeTests :: Spec
negativeTests =
  describe "negative tests" $ do
    prop "reifies 10 x id" $
      expectFailure $
        prop_complete @BaseFn @Int $
          constrained $
            \x ->
              explanation (pure "The value is decided before reifies happens") $
                reifies 10 x id
    prop "reify overconstrained" $
      expectFailure $
        prop_complete @BaseFn @Int $
          constrained $ \x ->
            explanation
              (pure "You can't constrain the variable introduced by reify as its already decided")
              $ reify x id
              $ \y -> y ==. 10

numberyTests :: Spec
numberyTests =
  describe "numbery tests" $ do
    testNumberyListSpec "listSum" listSum
    testNumberyListSpecNoShrink "listSumForall" listSumForall
    testNumberyListSpec "listSumRange" listSumRange
    testNumberyListSpec "listSumRangeUpper" listSumRangeUpper
    testNumberyListSpec "listSumRangeRange" listSumRangeRange
    testNumberyListSpec "listSumElemRange" listSumElemRange

sizeTests :: Spec
sizeTests =
  describe "SizeTests" $ do
    testSpecNoShrink "sizeAddOrSub1" sizeAddOrSub1
    testSpecNoShrink "sizeAddOrSub2" sizeAddOrSub2
    testSpecNoShrink "sizeAddOrSub3" sizeAddOrSub3
    testSpecNoShrink "sizeAddOrSub4 returns Negative Size" sizeAddOrSub4
    testSpecNoShrink "sizeAddOrSub5" sizeAddOrSub5
    testSpecNoShrink "sizeAddOrSub5" sizeAddOrSub5
    testSpec "listSubSize" listSubSize
    testSpec "listSubSize" setSubSize
    testSpec "listSubSize" mapSubSize
    testSpec "hasSizeList" hasSizeList
    testSpec "hasSizeSet" hasSizeSet
    testSpec "hasSizeMap" hasSizeMap

data NumberyType where
  N :: (Typeable a, Numbery a) => Proxy a -> NumberyType

testNumberyListSpec :: String -> (forall a. Numbery a => Specification BaseFn [a]) -> Spec
testNumberyListSpec = testNumberyListSpec' True

testNumberyListSpecNoShrink :: String -> (forall a. Numbery a => Specification BaseFn [a]) -> Spec
testNumberyListSpecNoShrink = testNumberyListSpec' False

testNumberyListSpec' :: Bool -> String -> (forall a. Numbery a => Specification BaseFn [a]) -> Spec
testNumberyListSpec' withShrink n p =
  describe n $
    sequence_
      [ testSpec' withShrink (show $ typeRep proxy) (p @a)
      | N (proxy :: Proxy a) <- numberyTypes
      ]
  where
    numberyTypes =
      [ N @Int Proxy
      , N @Integer Proxy
      , N @Natural Proxy
      , N @Word64 Proxy
      , N @Word32 Proxy
      , N @Word16 Proxy
      , N @Word8 Proxy
      , N @Int64 Proxy
      , N @Int32 Proxy
      , N @Int16 Proxy
      , N @Int8 Proxy
      ]

testSpec :: HasSpec fn a => String -> Specification fn a -> Spec
testSpec = testSpec' True

testSpecNoShrink :: HasSpec fn a => String -> Specification fn a -> Spec
testSpecNoShrink = testSpec' False

testSpec' :: HasSpec fn a => Bool -> String -> Specification fn a -> Spec
testSpec' withShrink n s =
  describe n $ do
    prop "prop_sound" $
      within 10_000_000 $
        checkCoverage $
          prop_sound s
    prop "prop_constrained_satisfies_sound" $
      within 10_000_000 $
        checkCoverage $
          prop_constrained_satisfies_sound s
    prop "prop_constrained_explained" $
      within 10_000_0000 $
        checkCoverage $
          QC.forAll arbitrary $ \es ->
            prop_sound $ constrained $ \x -> explanation es $ x `satisfies` s
    when withShrink $
      prop "prop_shrink_sound" $
        within 10_000_000 $
          checkCoverage $
            prop_shrink_sound s

------------------------------------------------------------------------
-- Test properties of the instance Num (NumSpec Integer)
------------------------------------------------------------------------

-- | When we multiply intervals, we get a bounding box, around the possible values.
--   When the intervals have infinities, the bounding box can be very loose. In fact the
--   order in which we multiply intervals with infinities can affect how loose the bounding box is.
--   So ((NegInf, n) * (a, b)) * (c,d)  AND  (NegInf, n) * ((a, b) * (c,d)) may have different bounding boxes
--   To test the associative laws we must have no infinities, and then the associative law will hold.
noInfinity :: Gen (NumSpec Integer)
noInfinity = do
  lo <- arbitrary
  hi <- suchThat arbitrary (> lo)
  pure $ NumSpecInterval (Just lo) (Just hi)

plusNegate :: NumSpec Integer -> NumSpec Integer -> Property
plusNegate x y = x - y === x + negate y

commutesNumSpec :: NumSpec Integer -> NumSpec Integer -> Property
commutesNumSpec x y = x + y === y + x

assocNumSpec :: NumSpec Integer -> NumSpec Integer -> NumSpec Integer -> Property
assocNumSpec x y z = x + (y + z) === (x + y) + z

commuteTimes :: NumSpec Integer -> NumSpec Integer -> Property
commuteTimes x y = x * y === y * x

assocNumSpecTimes :: Gen Property
assocNumSpecTimes = do
  x <- noInfinity
  y <- noInfinity
  z <- noInfinity
  pure (x * (y * z) === (x * y) * z)

negNegate :: NumSpec Integer -> Property
negNegate x = x === negate (negate x)

scaleNumSpec :: NumSpec Integer -> Property
scaleNumSpec y = y + y === 2 * y

scaleOne :: NumSpec Integer -> Property
scaleOne y = y === 1 * y

numNumSpecTree :: Spec
numNumSpecTree =
  describe "Num (NumSpec Integer) properties" $
    modifyMaxSuccess (const 10000) $ do
      prop "plusNegate(x - y == x + negate y)" plusNegate
      prop "scaleNumSpec(y + y = 2 * y)" scaleNumSpec
      prop "scaleOne(y = 1 * y)" scaleOne
      prop "negNagate(x = x == negate (negate x))" negNegate
      prop "commutesNumSpec(x+y = y+x)" commutesNumSpec
      prop "assocNumSpec(x+(y+z) == (x+y)+z)" assocNumSpec
      prop "assocNumSpecTimes(x*(y*z) == (x*y)*z)" assocNumSpecTimes
      prop "commuteTimes" commuteTimes

------------------------------------------------------------------------
-- Tests for `hasSize`
------------------------------------------------------------------------

hasSizeList :: Specification BaseFn [Int]
hasSizeList = hasSize (rangeSize 0 4)

hasSizeSet :: Specification BaseFn (Set Int)
hasSizeSet = hasSize (rangeSize 1 3)

hasSizeMap :: Specification BaseFn (Map Int Int)
hasSizeMap = hasSize (rangeSize 1 3)
