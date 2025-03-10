{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Constrained.Experiment.Tests.Test where

import Constrained.Experiment.API
import Constrained.Experiment.Examples.Basic
import Constrained.Experiment.Examples.Either
import Constrained.Experiment.Examples.Fold (
  Outcome (..),
  evenSpec,
  listSumComplex,
  logishProp,
  oddSpec,
  pickProp,
  sum3,
  sum3WithLength,
  sumProp,
  sumProp2,
  testFoldSpec,
 )
import Constrained.Experiment.Examples.List
import Constrained.Experiment.Examples.Map
import Constrained.Experiment.Examples.Set
import Constrained.Experiment.Examples.Tree
import Constrained.Experiment.Properties hiding (main)
import Constrained.Experiment.Specs.ListFoldy (narrowByFuelAndSize)
import Constrained.Experiment.TheKnot ()
import Control.Monad
import Data.Int
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable
import Data.Word
import GHC.Natural
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Args, Fun, forAll)

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
    testSpecNoShrink "eitherSetSpec" eitherSetSpec
    testSpec "fooSpec" fooSpec
    testSpec "mapElemSpec" mapElemSpec
    testSpec "mapElemKeySpec" mapElemKeySpec
    -- TODO: double shrinking
    testSpecNoShrink "mapIsJust" mapIsJust
    testSpecNoShrink "intSpec" intSpec
    testSpecNoShrink "mapPairSpec" mapPairSpec
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
    testSpecNoShrink "notSubsetSpec" notSubsetSpec
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
    testSpec "appendSize" appendSize
    testSpecNoShrink "appendSingleton" appendSingleton
    testSpec "singletonSubset" singletonSubset
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
    testSpec "mapRestrictedValues" mapRestrictedValues
    testSpec "mapRestrictedValuesThree" mapRestrictedValuesThree
    testSpec "mapRestrictedValuesBool" mapRestrictedValuesBool
    testSpec "mapSetSmall" mapSetSmall
    testSpecNoShrink "powersetPickOne" powersetPickOne
    testSpecNoShrink "appendSuffix" appendSuffix
    testSpecNoShrink "appendForAll" appendForAll
    testSpec "wtfSpec" wtfSpec
    numberyTests
    sizeTests
    numNumSpecTree
    sequence_
      [ testSpec ("intRangeSpec " ++ show i) (intRangeSpec i)
      | i <- [-1000, -100, -10, 0, 10, 100, 1000]
      ]
    describe "prop_conformEmpty" $ do
      prop "Int" $ prop_conformEmpty @Int
      prop "Set Int" $ prop_conformEmpty @(Set Int)
      prop "Map Int Int" $ prop_conformEmpty @(Map Int Int)
      prop "[Int]" $ prop_conformEmpty @[Int]
      prop "[(Int, Int)]" $ prop_conformEmpty @[(Int, Int)]
    prop "prop_univSound @BaseFn" $
      withMaxSuccess (if nightly then 100_000 else 10_000) $
        prop_univSound
    describe "prop_gen_sound" $ do
      modifyMaxSuccess (const $ if nightly then 10_000 else 1000) $ do
        prop "Int" $ prop_gen_sound @Int
        prop "Bool" $ prop_gen_sound @Bool
        prop "(Int, Int)" $ prop_gen_sound @(Int, Int)
        prop "Map Int Int" $ prop_gen_sound @(Map Int Int)
        prop "Set Int" $ prop_gen_sound @(Set Int)
        prop "Set Bool" $ prop_gen_sound @(Set Bool)
        prop "[Int]" $ prop_gen_sound @[Int]
        prop "[(Int, Int)]" $ prop_gen_sound @[(Int, Int)]
        prop "Map Bool Int" $ prop_gen_sound @(Map Bool Int)
      -- Slow tests that shouldn't run 1000 times
      xprop "Map (Set Int) Int" $ prop_gen_sound @(Map (Set Int) Int)
      prop "[(Set Int, Set Bool)]" $ prop_gen_sound @[(Set Int, Set Bool)]
      prop "Set (Set Bool)" $ prop_gen_sound @(Set (Set Bool))
    negativeTests
    prop "prop_noNarrowLoop" $ withMaxSuccess 1000 prop_noNarrowLoop
    conformsToSpecESpec
    foldWithSizeTests

negativeTests :: Spec
negativeTests =
  describe "negative tests" $ do
    prop "reifies 10 x id" $
      expectFailure $
        prop_complete @Int $
          constrained $
            \x ->
              explanation (pure "The value is decided before reifies happens") $
                reifies 10 x id
    prop "reify overconstrained" $
      expectFailure $
        prop_complete @Int $
          constrained $ \x ->
            explanation
              (pure "You can't constrain the variable introduced by reify as its already decided")
              $ reify x id
              $ \y -> y ==. 10
    testSpecFail "singletonErrorTooMany" singletonErrorTooMany
    testSpecFail "singletonErrorTooLong" singletonErrorTooLong
    testSpecFail "appendTooLong" appendTooLong
    testSpecFail "overconstrainedAppend" overconstrainedAppend
    testSpecFail "overconstrainedPrefixes" overconstrainedPrefixes
    testSpecFail "overconstrainedSuffixes" overconstrainedSuffixes
    testSpecFail "appendForAllBad" appendForAllBad

testSpecFail :: HasSpec a => String -> Specification a -> Spec
testSpecFail s spec =
  prop (s ++ " fails") $
    expectFailure $
      withMaxSuccess 1 $
        prop_complete spec

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

testNumberyListSpec :: String -> (forall a. Numbery a => Specification [a]) -> Spec
testNumberyListSpec = testNumberyListSpec' True

testNumberyListSpecNoShrink :: String -> (forall a. Numbery a => Specification [a]) -> Spec
testNumberyListSpecNoShrink = testNumberyListSpec' False

testNumberyListSpec' :: Bool -> String -> (forall a. Numbery a => Specification [a]) -> Spec
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

testSpec :: HasSpec a => String -> Specification a -> Spec
testSpec = testSpec' True

testSpecNoShrink :: HasSpec a => String -> Specification a -> Spec
testSpecNoShrink = testSpec' False

testSpec' :: HasSpec a => Bool -> String -> Specification a -> Spec
testSpec' withShrink n s = do
  let checkCoverage' = checkCoverageWith stdConfidence {certainty = 1_000_000}
  describe n $ do
    prop "prop_sound" $
      within 10_000_000 $
        checkCoverage' $
          prop_sound s
    prop "prop_constrained_satisfies_sound" $
      within 10_000_000 $
        checkCoverage' $
          prop_constrained_satisfies_sound s

    prop "prop_constrained_explained" $
      within 10_000_0000 $
        checkCoverage' $
          prop_constrained_explained s

    when withShrink $
      prop "prop_shrink_sound" $
        discardAfter 100_000 $
          checkCoverage' $
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

hasSizeList :: Specification [Int]
hasSizeList = hasSize (rangeSize 0 4)

hasSizeSet :: Specification (Set Int)
hasSizeSet = hasSize (rangeSize 1 3)

hasSizeMap :: Specification (Map Int Int)
hasSizeMap = hasSize (rangeSize 1 3)

------------------------------------------------------------------------
-- Tests for narrowing
------------------------------------------------------------------------

prop_noNarrowLoop :: Int -> Int -> Specification Int -> Specification Int -> Property
prop_noNarrowLoop f s eSpec fSpec =
  -- Make sure the fuel is non-negative
  f
    >= 0
      ==> discardAfter 100_000
    $ narrowByFuelAndSize f s (eSpec, fSpec)
    `seq` property True

-- | The test succeeds if conformsToSpec and conformsToSpecE both conform, or both fail to conform.
--   We collect answers by specType (ErrorSpec, MemberSpec, SuspendedSpec, ...) and whether
--   they both conform, or they both fail to conform.
conformsToSpecETest :: forall a. HasSpec a => a -> Specification a -> Property
conformsToSpecETest a speca =
  let resultE = conformsToSpecE a speca (pure ("ConformsToSpecETest " ++ show a ++ " " ++ show speca))
   in if conformsToSpec a speca
        then case resultE of
          Nothing -> property (collect (specType speca ++ " both conform") True)
          Just xs -> counterexample (unlines (NE.toList xs)) False
        else case resultE of
          Nothing ->
            counterexample ("conformstoSpec returns False, but conformsToSpecE returns no explanations") False
          Just _ -> property (collect (specType speca ++ " both fail to conform") True)

conformsToSpecESpec :: Spec
conformsToSpecESpec =
  describe "Testing alignment of conformsToSpec and conformsToSpecE" $
    modifyMaxSuccess (const 1000) $ do
      prop "Int" (conformsToSpecETest @Int)
      prop "Word64" (conformsToSpecETest @Word64)
      prop "Bool" (conformsToSpecETest @Bool)
      prop "[Int]" (conformsToSpecETest @[Int])
      prop "(Int,Bool)" (conformsToSpecETest @(Int, Bool))
      prop "Set Integer" (conformsToSpecETest @(Set Integer))
      prop "Set[Int]" (conformsToSpecETest @(Set [Int]))
      prop "Map Int Int" (conformsToSpecETest @(Map Int Int))

-- ======================================================================
-- Test for use of Fold with size annotations

foldWithSizeTests :: Spec
foldWithSizeTests = do
  describe "Summation tests with size. " $ do
    prop "logish is sound" logishProp
    prop "small odd/even tests" pickProp
    prop "negative small" $ sumProp (-1000) 100 TrueSpec (-400 :: Int) 4 Succeed
    prop "negative sum too small" $ sumProp (-1000) 0 TrueSpec (-8002 :: Int) 4 Fail
    prop "negative large" $ sumProp (-60000 :: Int) 0 TrueSpec (-1000) 4 Succeed
    prop "(between 50 60) small enough" $ sumProp 1 10 (between 50 60) (200 :: Int) 4 Succeed
    prop "(between 50 60) too large" $ sumProp 1 10 (between 50 60) (400 :: Int) 4 Fail
    prop "(count 2) large is fast" $ sumProp 1 5000000 TrueSpec (5000000 :: Int) 2 Succeed
    prop "(count 5) large is fast" $ sumProp 1 5000000 TrueSpec (5000000 :: Int) 5 Succeed
    prop "even succeeds on even" $ sumProp2 1 50000 ("even", even) (45876 :: Int) 5 Succeed
    prop "even succeeds on even spec" $ sumProp 1 50000 evenSpec (45876 :: Int) 5 Succeed
    prop "even fails on odd total, odd count" $ sumProp 1 50000 evenSpec (45875 :: Int) 3 Fail
    prop "odd fails on odd total, even count" $ sumProp 1 50000 oddSpec (45878 :: Int) 3 Fail
    prop "odd succeeds on odd total, odd count" $ sumProp 1 50000 oddSpec (45871 :: Int) 3 Succeed
    xprop "succeeds with large count" $
      withMaxSuccess 100 (sumProp 1 1500567 TrueSpec (1500567 :: Int) 20 Succeed)
    prop "sum3 is sound" $ prop_constrained_satisfies_sound sum3
    prop "(sum3WithLength 3) is sound" $ prop_constrained_satisfies_sound (sum3WithLength 3)
    prop "(sum3WithLength 4) is sound" $ prop_constrained_satisfies_sound (sum3WithLength 4)
    prop "(sum3WithLength 7) is sound" $ prop_constrained_satisfies_sound (sum3WithLength 7)
    prop "listSum is sound" $ prop_constrained_satisfies_sound (listSum @Int)
    prop "listSumPair is sound" $ prop_constrained_satisfies_sound (listSumPair @Word64)
    -- This, by design, will fail for inputs greater than 7
    prop "listSumComplex is sound" $ prop_constrained_satisfies_sound (listSumComplex @Integer 7)
    prop "All sizes are negative" $
      testFoldSpec @Int (between (-5) (-2)) evenSpec (MemberSpec (pure 100)) Fail
    prop "Only some sizes are negative" $
      testFoldSpec @Int (between (-5) 0) evenSpec (MemberSpec (pure 100)) Fail
    prop "total and count can only be 0 in Word type" $
      testFoldSpec @Word64 (between 0 0) evenSpec (MemberSpec (pure 0)) Succeed
    prop "something of size 2, can add to 0 in type with negative values." $
      testFoldSpec @Int (between 2 2) (between (-10) 10) (MemberSpec (pure 0)) Succeed
    prop "TEST listSum" $ prop_constrained_satisfies_sound (listSum @Int)

-- TODO Needs to sample like this: OR [pick t c | t <- total, c <- count]
-- prop "count =0, total is 0,1,2" $ testFoldSpec @Int (between 0 1) evenSpec (between 0 2) Succeed

main :: IO ()
main = testAll
