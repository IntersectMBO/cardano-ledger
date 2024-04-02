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

import Data.Int
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import Data.Word
import GHC.Generics
import GHC.Natural
import Test.QuickCheck hiding (Args, Fun, forAll)
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Constrained.Internals

-- Properties -------------------------------------------------------------

prop_conformEmpty ::
  forall fn a.
  HasSpec fn a =>
  a ->
  Property
prop_conformEmpty a = property $ conformsTo @fn a (emptySpec @fn @a)

prop_propagateSpec ::
  (HasSpec fn a, HasSpec fn b) =>
  Var b ->
  Term fn a ->
  Spec fn a ->
  Property
prop_propagateSpec var tm spec =
  let spec' = errorGE $ do
        ctx <- toCtx var tm
        pure $ propagateSpec spec ctx
   in QC.forAll (strictGen $ genFromSpec spec') $ \geval -> fromGE (\err -> counterexample (unlines err) False) $ do
        val <- geval
        res <- runTerm (singletonEnv var val) tm
        pure $ property $ conformsToSpec res spec

prop_mapSpec ::
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  fn '[a] b ->
  Spec fn a ->
  Property
prop_mapSpec fn spec =
  QC.forAll (strictGen $ genFromSpec spec) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ conformsToSpec (sem fn a) (mapSpec fn spec)

prop_genInverse ::
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  fn '[a] b ->
  Spec fn a ->
  b ->
  Property
prop_genInverse fn spec b =
  QC.forAll (strictGen $ genInverse fn spec b) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ conformsToSpec a spec && sem fn a == b

prop_sound ::
  HasSpec fn a =>
  Spec fn a ->
  Property
prop_sound spec =
  QC.forAllBlind (strictGen $ genFromSpec spec) $ \ma ->
    case ma of
      Result _ a -> cover 80 True "successful" $ counterexample (show a) $ conformsToSpecProp a spec
      _ -> cover 80 False "successful" True

-- | `prop_complete ps` assumes that `ps` is satisfiable
prop_complete :: HasSpec fn a => Spec fn a -> Property
prop_complete s =
  QC.forAllBlind (strictGen $ genFromSpec s) $ \ma -> fromGEProp $ do
    a <- ma
    -- Force the value to make sure we don't crash with `error` somewhere
    -- or fall into an inifinite loop
    pure $ length (show a) > 0

prop_shrink_sound :: HasSpec fn a => Spec fn a -> Property
prop_shrink_sound s =
  QC.forAll (strictGen $ genFromSpec s) $ \ma -> fromGEDiscard $ do
    a <- ma
    let shrinks = shrinkWithSpec s a
    pure $
      cover 40 (not $ null shrinks) "non-null shrinks" $
        if null shrinks
          then property True
          else QC.forAll (QC.elements shrinks) $ \a' ->
            conformsToSpecProp a' s

-- Test suite -------------------------------------------------------------

genTest :: HasSpec BaseFn a => Spec BaseFn a -> IO a
genTest = fmap errorGE . generate . strictGen . genFromSpec

testAll :: IO ()
testAll = defaultMain tests

tests :: TestTree
tests =
  testGroup "constrained" $
    [ testSpec "setSpec" setSpec
    , testSpec "leqPair" leqPair
    , testSpec "setPair" setPair
    , testSpecNoShrink "listEmpty" listEmpty
    , testSpec "compositionalSpec" compositionalSpec
    , testSpec "simplePairSpec" simplePairSpec
    , testSpec "trickyCompositional" trickyCompositional
    , testSpec "emptyListSpec" emptyListSpec
    , testSpec "eitherSpec" eitherSpec
    , testSpec "maybeSpec" maybeSpec
    , testSpec "eitherSetSpec" eitherSetSpec
    , testSpec "fooSpec" fooSpec
    , -- TODO: this spec needs double shrinking to shrink properly
      -- so we need to figure something out about double-shrinking
      testSpecNoShrink "intSpec" intSpec
    , testSpec "mapElemSpec" mapElemSpec
    , testSpec "mapElemKeySpec" mapElemKeySpec
    , testSpec "mapPairSpec" mapPairSpec
    , testSpecNoShrink "mapEmptyDomainSpec" mapEmptyDomainSpec
    , -- TODO: this _can_ be shrunk, but it's incredibly expensive to do
      -- so and it's not obvious if there is a faster way without implementing
      -- more detailed shrinking of `SuspendedSpec`s
      testSpecNoShrink "setPairSpec" setPairSpec
    , testSpec "fixedSetSpec" fixedSetSpec
    , testSpec "setOfPairLetSpec" setOfPairLetSpec
    , testSpecNoShrink "emptyEitherSpec" emptyEitherSpec
    , testSpecNoShrink "emptyEitherMemberSpec" emptyEitherMemberSpec
    , testSpec "setSingletonSpec" setSingletonSpec
    , testSpec "pairSingletonSpec" pairSingletonSpec
    , testSpec "eitherSimpleSetSpec" eitherSimpleSetSpec
    , testSpecNoShrink "emptySetSpec" emptySetSpec
    , testSpec "forAllAnySpec" forAllAnySpec
    , testSpec "notSubsetSpec" notSubsetSpec
    , testSpec "maybeJustSetSpec" maybeJustSetSpec
    , testSpec "weirdSetPairSpec" weirdSetPairSpec
    , testSpec "knownDomainMap" knownDomainMap
    , -- TODO: figure out double-shrinking
      testSpecNoShrink "testRewriteSpec" testRewriteSpec
    , testSpec "parallelLet" parallelLet
    , testSpec "letExists" letExists
    , testSpec "letExistsLet" letExistsLet
    , testSpec "notSubset" notSubset
    , testSpec "unionSized" unionSized
    , -- TODO: figure out double-shrinking
      testSpecNoShrink "dependencyWeirdness" dependencyWeirdness
    , testSpec "foldTrueCases" foldTrueCases
    , testSpec "foldSingleCase" foldSingleCase
    , testSpec "listSumPair" (listSumPair @Int)
    , -- TODO: figure out double-shrinking
      testSpecNoShrink "parallelLetPair" parallelLetPair
    , testSpec "mapSizeConstrained" mapSizeConstrained
    , testSpec "allZeroTree" allZeroTree
    , testSpec "noChildrenSameTree" noChildrenSameTree
    , testSpec "isBST" isBST
    , testSpecNoShrink "pairListError" pairListError
    , testSpecNoShrink "listMustSizeIssue" listMustSizeIssue
    , testSpec "successiveChildren" successiveChildren
    , testSpec "successiveChildren8" successiveChildren8
    , testSpecNoShrink "roseTreeList" roseTreeList
    , testSpec "orPair" orPair
    , testSpec "roseTreePairs" roseTreePairs
    , testSpec "roseTreeMaybe" roseTreeMaybe
    , testSpec "badTreeInteraction" badTreeInteraction
    , numberyTests
    , sizeTests
    , numNumSpecTree
    ]
      ++ [ testSpec ("intRangeSpec " ++ show i) (intRangeSpec i)
         | i <- [-1000, -100, -10, 0, 10, 100, 1000]
         ]
      ++ [ testGroup
            "prop_conformEmpty"
            [ testProperty "Int" $ prop_conformEmpty @BaseFn @Int
            , testProperty "Set Int" $ prop_conformEmpty @BaseFn @(Set Int)
            , testProperty "Map Int Int" $ prop_conformEmpty @BaseFn @(Map Int Int)
            , testProperty "[Int]" $ prop_conformEmpty @BaseFn @[Int]
            , testProperty "[(Int, Int)]" $ prop_conformEmpty @BaseFn @[(Int, Int)]
            ]
         ]

numberyTests :: TestTree
numberyTests =
  testGroup
    "numbery tests"
    [ testNumberyListSpec "listSum" listSum
    , testNumberyListSpecNoShrink "listSumForall" listSumForall
    , testNumberyListSpec "listSumRange" listSumRange
    , testNumberyListSpec "listSumRangeUpper" listSumRangeUpper
    , testNumberyListSpec "listSumRangeRange" listSumRangeRange
    , testNumberyListSpec "listSumElemRange" listSumElemRange
    ]

sizeTests :: TestTree
sizeTests =
  testGroup "SizeTests" $
    [ testSpecNoShrink "sizeAddOrSub1" sizeAddOrSub1
    , testSpecNoShrink "sizeAddOrSub2" sizeAddOrSub2
    , testSpecNoShrink "sizeAddOrSub3" sizeAddOrSub3
    , testSpecNoShrink "sizeAddOrSub4 returns Negative Size" sizeAddOrSub4
    , testSpecNoShrink "sizeAddOrSub5" sizeAddOrSub5
    , testSpecNoShrink "sizeAddOrSub5" sizeAddOrSub5
    , testSpec "listSubSize" listSubSize
    , testSpec "listSubSize" setSubSize
    , testSpec "listSubSize" mapSubSize
    , testSpec "hasSizeList" hasSizeList
    , testSpec "hasSizeSet" hasSizeSet
    , testSpec "hasSizeMap" hasSizeMap
    ]

type Numbery a =
  ( Foldy BaseFn a
  , OrdLike BaseFn a
  , NumLike BaseFn a
  , Ord a
  , Enum a
  )

data NumberyType where
  N :: (Typeable a, Numbery a) => Proxy a -> NumberyType

testNumberyListSpec :: String -> (forall a. Numbery a => Spec BaseFn [a]) -> TestTree
testNumberyListSpec = testNumberyListSpec' True

testNumberyListSpecNoShrink :: String -> (forall a. Numbery a => Spec BaseFn [a]) -> TestTree
testNumberyListSpecNoShrink = testNumberyListSpec' False

testNumberyListSpec' :: Bool -> String -> (forall a. Numbery a => Spec BaseFn [a]) -> TestTree
testNumberyListSpec' withShrink n p =
  testGroup
    n
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

testSpec :: HasSpec fn a => String -> Spec fn a -> TestTree
testSpec = testSpec' True

testSpecNoShrink :: HasSpec fn a => String -> Spec fn a -> TestTree
testSpecNoShrink = testSpec' False

testSpec' :: HasSpec fn a => Bool -> String -> Spec fn a -> TestTree
testSpec' withShrink n s = do
  sequentialTestGroup
    n
    AllSucceed
    $ [testProperty "prop_sound" $ within 10_000_000 $ checkCoverage $ prop_sound s]
      ++ [ testProperty "prop_shrink_sound" $ within 10_000_000 $ checkCoverage $ prop_shrink_sound s
         | withShrink
         ]

-- Examples ---------------------------------------------------------------

leqPair :: Spec BaseFn (Int, Int)
leqPair = constrained $ \p ->
  match p $ \x y ->
    x <=. y

setPair :: Spec BaseFn (Set (Int, Int))
setPair = constrained $ \s ->
  [ forAll s $ \p ->
      p `satisfies` leqPair
  , assert $ lit (0, 1) `member_` s
  ]

setSpec :: Spec BaseFn (Set Int)
setSpec = constrained $ \ss ->
  forAll ss $ \s ->
    s <=. 10

compositionalSpec :: Spec BaseFn (Set Int)
compositionalSpec = constrained $ \x ->
  [ satisfies x setSpec
  , assert $ 0 `member_` x
  ]

simplePairSpec :: Spec BaseFn (Int, Int)
simplePairSpec = constrained $ \p ->
  match p $ \x y ->
    [ x /=. 0
    , y /=. 0
    ]

trickyCompositional :: Spec BaseFn (Int, Int)
trickyCompositional = constrained $ \p ->
  satisfies p simplePairSpec <> assert (fst_ p ==. 1000)

newtype NotASet a = NotASet (Set a)
  deriving (Generic, Show, Eq)
instance Ord a => HasSimpleRep (NotASet a) where
  type SimpleRep (NotASet a) = [a]
  fromSimpleRep = NotASet . Set.fromList
  toSimpleRep (NotASet s) = Set.toList s
instance (Ord a, HasSpec fn a) => HasSpec fn (NotASet a)
instance Ord a => Forallable (NotASet a) a

emptyListSpec :: Spec BaseFn ([Int], NotASet (Either Int Int, Int))
emptyListSpec = constrained' $ \is ls ->
  [ forAll is $ \i -> i <=. 0
  , forAll' ls $ \l _ ->
      caseOn l (branch $ \_ -> False) (branch $ \_ -> False)
  ]

eitherSpec :: Spec BaseFn (Either Int Int)
eitherSpec = constrained $ \e ->
  (caseOn e)
    (branch $ \i -> i <=. 0)
    (branch $ \i -> 0 <=. i)

maybeSpec :: Spec BaseFn (Set (Maybe Int))
maybeSpec = constrained $ \ms ->
  forAll ms $ \m ->
    (caseOn m)
      (branch $ \_ -> False)
      (branch $ \y -> 0 <=. y)

eitherSetSpec :: Spec BaseFn (Set (Either Int Int), Set (Either Int Int), Set (Either Int Int))
eitherSetSpec = constrained' $ \es as bs ->
  [ assert $ es ==. (as <> bs)
  , forAll as $ \a ->
      (caseOn a)
        (branch $ \a' -> a' <=. 0)
        (branch $ \b' -> 1 <=. b')
  , forAll bs $ \b ->
      (caseOn b)
        (branch $ \_ -> False)
        (branch $ \b' -> 1 <=. b')
  ]

data Foo = Foo Int | Bar Int Int
  deriving (Show, Eq, Ord, Generic)

instance HasSimpleRep Foo
instance BaseUniverse fn => HasSpec fn Foo

fooSpec :: Spec BaseFn Foo
fooSpec = constrained $ \foo ->
  (caseOn foo)
    (branch $ \i -> 0 <=. i)
    (branch $ \i j -> i <=. j)

intSpec :: Spec BaseFn (Int, Int)
intSpec = constrained' $ \a b ->
  reify a (`mod` 10) $ \a' -> b ==. a'

mapElemSpec :: Spec BaseFn (Map Int (Bool, Int))
mapElemSpec = constrained $ \m ->
  [ assert $ m /=. lit mempty
  , forAll' (rng_ m) $ \_ b ->
      [0 <. b, b <. 10]
  ]

mapElemKeySpec :: Spec BaseFn Int
mapElemKeySpec = constrained $ \n ->
  letBind (pair_ n $ lit (False, 4)) $ \(p :: Term BaseFn (Int, (Bool, Int))) ->
    letBind (snd_ (snd_ p)) $ \x ->
      [x <. 10, 0 <. x, not_ $ elem_ n $ lit []]

intRangeSpec :: Int -> Spec BaseFn Int
intRangeSpec a = constrained $ \n -> n <. lit a

testRewriteSpec :: Spec BaseFn ((Int, Int), (Int, Int))
testRewriteSpec = constrained' $ \x y ->
  x ==. fromGeneric_ (toGeneric_ y)

mapPairSpec :: Spec BaseFn (Map Int Int, Set Int)
mapPairSpec = constrained' $ \m s ->
  subset_ (dom_ m) s

mapEmptyDomainSpec :: Spec BaseFn (Map Int Int)
mapEmptyDomainSpec = constrained $ \m ->
  subset_ (dom_ m) mempty -- mempty in the Monoid instance (Term fn (Set a))

setPairSpec :: Spec BaseFn (Set Int, Set Int)
setPairSpec = constrained' $ \s s' ->
  forAll s $ \x ->
    forAll s' $ \y ->
      x <=. y

fixedSetSpec :: Spec BaseFn (Set Int)
fixedSetSpec = constrained $ \s ->
  forAll s $ \x ->
    [x <=. lit (i :: Int) | i <- [1 .. 3]]

setOfPairLetSpec :: Spec BaseFn (Set (Int, Int))
setOfPairLetSpec = constrained $ \ps ->
  forAll' ps $ \x y ->
    x <=. y

setSingletonSpec :: Spec BaseFn (Set (Int, Int))
setSingletonSpec = constrained $ \ps ->
  forAll ps $ \p ->
    forAll (singleton_ (fst_ p)) $ \x ->
      x <=. 10

pairSingletonSpec :: Spec BaseFn (Int, Int)
pairSingletonSpec = constrained $ \q ->
  forAll (singleton_ q) $ \p ->
    letBind (fst_ p) $ \x ->
      letBind (snd_ p) $ \y ->
        x <=. y

eitherSimpleSetSpec :: Spec BaseFn (Set (Either Int Int))
eitherSimpleSetSpec = constrained $ \ss ->
  forAll ss $ \s ->
    (caseOn s)
      (branch $ \a -> a <=. 0)
      (branch $ \b -> 0 <=. b)

forAllAnySpec :: Spec BaseFn (Set Int)
forAllAnySpec = constrained $ \as ->
  forAll as $ \_ -> True

weirdSetPairSpec :: Spec BaseFn ([Int], Set (Either Int Int))
weirdSetPairSpec = constrained' $ \as as' ->
  [ as' `dependsOn` as
  , forAll as $ \a ->
      member_ (left_ a) as'
  , forAll as' $ \a' ->
      (caseOn a')
        (branch $ \x -> elem_ x as)
        (branch $ \_ -> False)
  ]

maybeJustSetSpec :: Spec BaseFn (Set (Maybe Int))
maybeJustSetSpec = constrained $ \ms ->
  forAll ms $ \m ->
    (caseOn m)
      (branch $ \_ -> False)
      (branch $ \y -> 0 <=. y)

notSubsetSpec :: Spec BaseFn (Set Int, Set Int)
notSubsetSpec = constrained' $ \s s' -> not_ $ subset_ s s'

emptySetSpec :: Spec BaseFn (Set Int)
emptySetSpec = constrained $ \s ->
  forAll s $ \x -> member_ x mempty

emptyEitherMemberSpec :: Spec BaseFn (Set (Either Int Int))
emptyEitherMemberSpec = constrained $ \s ->
  forAll s $ \x ->
    (caseOn x)
      (branch $ \l -> member_ l mempty)
      (branch $ \r -> member_ r mempty)

emptyEitherSpec :: Spec BaseFn (Set (Either Int Int))
emptyEitherSpec = constrained $ \s ->
  forAll s $ \x ->
    (caseOn x)
      (branch $ \_ -> False)
      (branch $ \_ -> False)

knownDomainMap :: Spec BaseFn (Map Int Int)
knownDomainMap = constrained $ \m ->
  [ dom_ m ==. lit (Set.fromList [1, 2])
  , not_ $ 0 `elem_` rng_ m
  ]

parallelLet :: Spec BaseFn (Int, Int)
parallelLet = constrained $ \p ->
  [ letBind (fst_ p) $ \x -> 0 <. x
  , letBind (snd_ p) $ \x -> x <. 0
  ]

letExists :: Spec BaseFn (Int, Int)
letExists = constrained $ \p ->
  [ letBind (fst_ p) $ \x -> 0 <. x
  , exists (\eval -> pure $ snd (eval p)) $
      \x ->
        [ x <. 0
        , snd_ p ==. x
        ]
  ]

letExistsLet :: Spec BaseFn (Int, Int)
letExistsLet = constrained $ \p ->
  [ letBind (fst_ p) $ \x -> 0 <. x
  , exists (\eval -> pure $ snd (eval p)) $
      \x ->
        [ assert $ x <. 0
        , letBind (snd_ p) $ \y ->
            [ x ==. y
            , y <. -1
            ]
        ]
  ]

notSubset :: Spec BaseFn (Set Int)
notSubset = constrained $ \s ->
  not_ $ s `subset_` lit (Set.fromList [1, 2, 3])

unionSized :: Spec BaseFn (Set Int)
unionSized = constrained $ \s ->
  10 ==. size_ (s <> lit (Set.fromList [1 .. 8]))

listSum :: Numbery a => Spec BaseFn [a]
listSum = constrained $ \as ->
  10 <=. sum_ as

listSumForall :: Numbery a => Spec BaseFn [a]
listSumForall = constrained $ \xs ->
  [ forAll xs $ \x -> 1 <. x
  , assert $ sum_ xs ==. 20
  ]

listSumRange :: Numbery a => Spec BaseFn [a]
listSumRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> 1 <. x
      , assert $ n <. 20
      , assert $ 10 <. n
      ]

listSumRangeUpper :: Numbery a => Spec BaseFn [a]
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

listSumRangeRange :: Numbery a => Spec BaseFn [a]
listSumRangeRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> [1 <. x, x <. 5]
      , assert $ n <. 20
      , assert $ 10 <. n
      ]

listSumElemRange :: Numbery a => Spec BaseFn [a]
listSumElemRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> [1 <. x, x <. 5]
      , assert $ n `elem_` lit [10, 12 .. 20]
      ]

listSumPair :: Numbery a => Spec BaseFn [(a, Int)]
listSumPair = constrained $ \xs ->
  [ assert $ foldMap_ (composeFn fstFn toGenericFn) xs ==. 100
  , forAll' xs $ \x y -> [20 <. x, x <. 30, y <. 100]
  ]

listEmpty :: Spec BaseFn [Int]
listEmpty = constrained $ \xs ->
  [ forAll xs $ \_ -> False
  , assert $ length_ xs <=. 10
  ]

dependencyWeirdness :: Spec BaseFn (Int, Int, Int)
dependencyWeirdness = constrained' $ \x y z ->
  reify (x + y) id $ \zv -> z ==. zv

foldTrueCases :: Spec BaseFn (Either Int Int)
foldTrueCases = constrained $ \x ->
  [ assert $ not_ $ member_ x (lit (Set.fromList [Left 10]))
  , letBind (pair_ x (lit (0 :: Int))) $ \p ->
      caseOn
        (fst_ p)
        (branch $ \_ -> True)
        (branch $ \_ -> True)
  ]

foldSingleCase :: Spec BaseFn Int
foldSingleCase = constrained $ \x ->
  [ assert $ not_ $ member_ x (lit (Set.fromList [10]))
  , letBind (pair_ x $ lit [(10, 20) :: (Int, Int)]) $ \p ->
      match p $ \_ p1 -> forAll p1 $ \p2 ->
        assert (0 <=. snd_ p2)
  ]

parallelLetPair :: Spec BaseFn (Int, Int)
parallelLetPair = constrained $ \p ->
  [ match p $ \x y ->
      [ assert $ x <=. y
      , y `dependsOn` x
      ]
  , match p $ \x y -> y <=. x
  ]

data Three = One | Two | Three
  deriving (Ord, Eq, Show, Generic)

instance HasSimpleRep Three
instance BaseUniverse fn => HasSpec fn Three

mapSizeConstrained :: Spec BaseFn (Map Three Int)
mapSizeConstrained = constrained $ \m -> size_ (dom_ m) <=. 3

orPair :: Spec BaseFn [(Int, Int)]
orPair = constrained $ \ps ->
  forAll' ps $ \x y ->
    x <=. 5 ||. y <=. 5

allZeroTree :: Spec BaseFn (BinTree Int)
allZeroTree = constrained $ \t ->
  [ forAll' t $ \_ a _ -> a ==. 0
  , genHint 10 t
  ]

isBST :: Spec BaseFn (BinTree Int)
isBST = constrained $ \t ->
  [ forAll' t $ \left a right ->
      -- TODO: if there was a `binTreeRoot` function on trees
      -- this wouldn't need to be quadratic as we would
      -- only check agains the head of the left and right
      -- subtrees, not _every element_
      [ forAll' left $ \_ l _ -> l <. a
      , forAll' right $ \_ h _ -> a <. h
      ]
  , genHint 10 t
  ]

noChildrenSameTree :: Spec BaseFn (BinTree Int)
noChildrenSameTree = constrained $ \t ->
  [ forAll' t $ \left a right ->
      [ forAll' left $ \_ l _ -> l /=. a
      , forAll' right $ \_ r _ -> r /=. a
      ]
  , genHint 8 t
  ]

type RoseFn = Fix (OneofL (RoseTreeFn : BaseFns))

allZeroRoseTree :: Spec RoseFn (RoseTree Int)
allZeroRoseTree = constrained $ \t ->
  [ forAll' t $ \a cs ->
      [ a ==. 0
      , length_ cs <=. 4
      ]
  , genHint (Just 2, 30) t
  ]

noSameChildrenRoseTree :: Spec RoseFn (RoseTree Int)
noSameChildrenRoseTree = constrained $ \t ->
  [ forAll' t $ \a cs ->
      [ assert $ a `elem_` lit [1 .. 8]
      , forAll cs $ \t' ->
          forAll' t' $ \b _ ->
            b /=. a
      ]
  , genHint (Just 2, 30) t
  ]

successiveChildren :: Spec RoseFn (RoseTree Int)
successiveChildren = constrained $ \t ->
  [ forAll' t $ \a cs ->
      [ forAll cs $ \t' ->
          roseRoot_ t' ==. a + 1
      ]
  , genHint (Just 2, 10) t
  ]

successiveChildren8 :: Spec RoseFn (RoseTree Int)
successiveChildren8 = constrained $ \t ->
  [ t `satisfies` successiveChildren
  , forAll' t $ \a _ -> a `elem_` lit [1 .. 5]
  ]

roseTreeList :: Spec RoseFn [RoseTree Int]
roseTreeList = constrained $ \ts ->
  [ assert $ length_ ts <=. 10
  , forAll ts $ \t ->
      [ forAll t $ \_ -> False
      ]
  ]

pairListError :: Spec BaseFn [(Int, Int)]
pairListError = constrained $ \ps ->
  [ assert $ length_ ps <=. 10
  , forAll' ps $ \a b ->
      [ a `elem_` lit [1 .. 8]
      , a ==. 9
      , b ==. a
      ]
  ]

listMustSizeIssue :: Spec BaseFn [Int]
listMustSizeIssue = constrained $ \xs ->
  [ 1 `elem_` xs
  , length_ xs ==. 1
  ]

roseTreePairs :: Spec RoseFn (RoseTree ([Int], [Int]))
roseTreePairs = constrained $ \t ->
  [ assert $ roseRoot_ t ==. lit ([1 .. 10], [1 .. 10])
  , forAll' t $ \p ts ->
      forAll ts $ \t' ->
        fst_ (roseRoot_ t') ==. snd_ p
  , genHint (Nothing, 10) t
  ]

roseTreeMaybe :: Spec RoseFn (RoseTree (Maybe (Int, Int)))
roseTreeMaybe = constrained $ \t ->
  [ forAll' t $ \mp ts ->
      forAll ts $ \t' ->
        onJust mp $ \p ->
          onJust (roseRoot_ t') $ \p' ->
            fst_ p' ==. snd_ p
  , forAll' t $ \mp _ -> isJust mp
  , genHint (Nothing, 10) t
  ]

badTreeInteraction :: Spec RoseFn (RoseTree (Either Int Int))
badTreeInteraction = constrained $ \t ->
  [ forAll' t $ \n ts' ->
      [ isCon @"Right" n
      , forAll ts' $ \_ -> True
      ]
  , forAll' t $ \n ts' ->
      forAll ts' $ \t' ->
        [ genHint (Just 4, 10) t'
        , assert $ roseRoot_ t' ==. n
        ]
  , genHint (Just 4, 10) t
  ]

sizeAddOrSub1 :: Spec BaseFn Integer
sizeAddOrSub1 = constrained $ \s ->
  4 ==. s + 2

sizeAddOrSub2 :: Spec BaseFn Integer
sizeAddOrSub2 = constrained $ \s ->
  4 ==. 2 + s

sizeAddOrSub3 :: Spec BaseFn Integer
sizeAddOrSub3 = constrained $ \s ->
  4 ==. s - 2

-- | We expect a negative Integer, so ltSpec tests for that.
sizeAddOrSub4 :: Spec BaseFn Integer
sizeAddOrSub4 = ltSpec 0 <> (constrained $ \s -> 4 ==. 2 - s)

sizeAddOrSub5 :: Spec BaseFn Integer
sizeAddOrSub5 = constrained $ \s ->
  2 ==. 12 - s

listSubSize :: Spec BaseFn [Int]
listSubSize = constrained $ \s ->
  2 ==. 12 - (sizeOf_ s)

setSubSize :: Spec BaseFn (Set Int)
setSubSize = constrained $ \s ->
  2 ==. 12 - (sizeOf_ s)

mapSubSize :: Spec BaseFn (Map Int Int)
mapSubSize = constrained $ \s ->
  2 ==. 12 - (sizeOf_ s)

hasSizeList :: Spec BaseFn [Int]
hasSizeList = hasSize (rangeSize 0 4)

hasSizeSet :: Spec BaseFn (Set Int)
hasSizeSet = hasSize (rangeSize 1 3)

hasSizeMap :: Spec BaseFn (Map Int Int)
hasSizeMap = hasSize (rangeSize 1 3)

-- ========================================================
-- Test properties of the instance Num(NumSpec Integer)

instance Arbitrary (NumSpec Integer) where
  arbitrary = do
    lo <- arbitrary
    hi <- next lo
    pure $ NumSpecInterval lo hi
    where
      next Nothing = arbitrary
      next (Just n) = frequency [(1, pure Nothing), (3, Just <$> suchThat arbitrary (> n))]

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

commuteTimes :: (NumSpec Integer) -> (NumSpec Integer) -> Property
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

numNumSpecTree :: TestTree
numNumSpecTree =
  testGroup
    "Num (NumSpec Integer) properties"
    [ testProperty "plusNegate(x - y == x + negate y)" plusNegate
    , testProperty "scaleNumSpec(y + y = 2 * y)" scaleNumSpec
    , testProperty "scaleOne(y = 1 * y)" scaleOne
    , testProperty "negNagate(x = x == negate (negate x))" negNegate
    , testProperty "commutesNumSpec(x+y = y+x)" commutesNumSpec
    , testProperty "assocNumSpec(x+(y+z) == (x+y)+z)" assocNumSpec
    , testProperty "assocNumSpecTimes(x*(y*z) == (x*y)*z)" assocNumSpecTimes
    , testProperty "commuteTimes" commuteTimes
    ]

-- ==========================================================

runTestSpec :: HasSpec BaseFn t => Spec BaseFn t -> IO ()
runTestSpec spec = defaultMain (testSpec "interactive test with runTestSpec" spec)

generateSpec :: forall fn a. HasSpec fn a => Spec fn a -> IO a
generateSpec spec = generate (genFromSpec_ @fn spec)
