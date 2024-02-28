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
{-# LANGUAGE UndecidableInstances #-}

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
  QC.forAll (strictGen $ genFromSpec spec) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ conformsToSpecProp a spec

-- | `prop_complete ps` assumes that `ps` is satisfiable
prop_complete :: HasSpec fn a => Spec fn a -> Property
prop_complete s =
  QC.forAllBlind (strictGen $ genFromSpec s) $ \ma -> fromGEProp $ do
    a <- ma
    -- Force the value to make sure we don't crash with `error` somewhere
    -- or fall into an inifinite loop
    pure $ length (show a) > 0

-- Test suite -------------------------------------------------------------

genTest :: HasSpec Fn a => Spec Fn a -> IO a
genTest = fmap errorGE . generate . strictGen . genFromSpec

testAll :: IO ()
testAll = defaultMain tests

tests :: TestTree
tests =
  testGroup "constrained" $
    [ testSpec "setSpec" setSpec
    , testSpec "leqPair" leqPair
    , testSpec "setPair" setPair
    , testSpec "compositionalSpec" compositionalSpec
    , testSpec "simplePairSpec" simplePairSpec
    , testSpec "trickyCompositional" trickyCompositional
    , testSpec "emptyListSpec" emptyListSpec
    , testSpec "eitherSpec" eitherSpec
    , testSpec "maybeSpec" maybeSpec
    , testSpec "eitherSetSpec" eitherSetSpec
    , testSpec "fooSpec" fooSpec
    , testSpec "intSpec" intSpec
    , testSpec "mapElemSpec" mapElemSpec
    , testSpec "mapElemKeySpec" mapElemKeySpec
    , testSpec "mapPairSpec" mapPairSpec
    , testSpec "mapEmptyDomainSpec" mapEmptyDomainSpec
    , testSpec "setPairSpec" setPairSpec
    , testSpec "fixedSetSpec" fixedSetSpec
    , testSpec "setOfPairLetSpec" setOfPairLetSpec
    , testSpec "emptyEitherSpec" emptyEitherSpec
    , testSpec "emptyEitherMemberSpec" emptyEitherMemberSpec
    , testSpec "setSingletonSpec" setSingletonSpec
    , testSpec "pairSingletonSpec" pairSingletonSpec
    , testSpec "eitherSimpleSetSpec" eitherSimpleSetSpec
    , testSpec "emptySetSpec" emptySetSpec
    , testSpec "forAllAnySpec" forAllAnySpec
    , testSpec "notSubsetSpec" notSubsetSpec
    , testSpec "maybeJustSetSpec" maybeJustSetSpec
    , testSpec "weirdSetPairSpec" weirdSetPairSpec
    , testSpec "knownDomainMap" knownDomainMap
    , testSpec "testRewriteSpec" testRewriteSpec
    , testSpec "parallelLet" parallelLet
    , testSpec "letExists" letExists
    , testSpec "letExistsLet" letExistsLet
    , testSpec "notSubset" notSubset
    , testSpec "unionSized" unionSized
    , testSpec "dependencyWeirdness" dependencyWeirdness
    , testSpec "foldTrueCases" foldTrueCases
    , testSpec "foldSingleCase" foldSingleCase
    , testSpec "listSumPair" (listSumPair @Int)
    , testSpec "parallelLetPair" parallelLetPair
    , testSpec "mapSizeConstrained" mapSizeConstrained
    , numberyTests
    , testSpec "andPair" andPair
    , testSpec "orPair" orPair
    ]
      ++ [ testSpec ("intRangeSpec " ++ show i) (intRangeSpec i)
         | i <- [-1000, -100, -10, 0, 10, 100, 1000]
         ]
      ++ [ testGroup
            "prop_conformEmpty"
            [ testProperty "Int" $ prop_conformEmpty @Fn @Int
            , testProperty "Set Int" $ prop_conformEmpty @Fn @(Set Int)
            , testProperty "Map Int Int" $ prop_conformEmpty @Fn @(Map Int Int)
            , testProperty "[Int]" $ prop_conformEmpty @Fn @[Int]
            , testProperty "[(Int, Int)]" $ prop_conformEmpty @Fn @[(Int, Int)]
            ]
         ]

numberyTests :: TestTree
numberyTests =
  testGroup
    "numbery tests"
    [ testNumberyListSpec "listSum" listSum
    , testNumberyListSpec "listSumForall" listSumForall
    , testNumberyListSpec "listSumRange" listSumRange
    , testNumberyListSpec "listSumRangeUpper" listSumRangeUpper
    , testNumberyListSpec "listSumRangeRange" listSumRangeRange
    , testNumberyListSpec "listSumElemRange" listSumElemRange
    ]

type Numbery a =
  ( Foldy Fn a
  , OrdLike Fn a
  , NumLike Fn a
  , Ord a
  , Enum a
  )

data NumberyType where
  N :: (Typeable a, Numbery a) => Proxy a -> NumberyType

testNumberyListSpec :: String -> (forall a. Numbery a => Spec Fn [a]) -> TestTree
testNumberyListSpec n p =
  testGroup
    n
    [ testSpec (show $ typeRep proxy) (p @a)
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

testSpec :: HasSpec Fn a => String -> Spec Fn a -> TestTree
testSpec n s = do
  sequentialTestGroup
    n
    AllSucceed
    [ -- NOTE: during development you want to uncomment the line below:
      -- testProperty "prop_complete" $ within 10_000_000 $ withMaxSuccess 100 $ prop_complete s,
      testProperty "prop_sound" $ within 10_000_000 $ withMaxSuccess 100 $ prop_sound s
    ]

-- Examples ---------------------------------------------------------------

leqPair :: Spec Fn (Int, Int)
leqPair = constrained $ \p ->
  match p $ \x y ->
    x <=. y

setPair :: Spec Fn (Set (Int, Int))
setPair = constrained $ \s ->
  [ forAll s $ \p ->
      p `satisfies` leqPair
  , assert $ lit (0, 1) `member_` s
  ]

setSpec :: Spec Fn (Set Int)
setSpec = constrained $ \ss ->
  forAll ss $ \s ->
    s <=. 10

compositionalSpec :: Spec Fn (Set Int)
compositionalSpec = constrained $ \x ->
  [ satisfies x setSpec
  , assert $ 0 `member_` x
  ]

simplePairSpec :: Spec Fn (Int, Int)
simplePairSpec = constrained $ \p ->
  match p $ \x y ->
    [ x /=. 0
    , y /=. 0
    ]

trickyCompositional :: Spec Fn (Int, Int)
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

emptyListSpec :: Spec Fn ([Int], NotASet (Either Int Int, Int))
emptyListSpec = constrained' $ \is ls ->
  [ forAll is $ \i -> i <=. 0
  , forAll' ls $ \l _ ->
      caseOn l (branch $ \_ -> False) (branch $ \_ -> False)
  ]

eitherSpec :: Spec Fn (Either Int Int)
eitherSpec = constrained $ \e ->
  (caseOn e)
    (branch $ \i -> i <=. 0)
    (branch $ \i -> 0 <=. i)

maybeSpec :: Spec Fn (Set (Maybe Int))
maybeSpec = constrained $ \ms ->
  forAll ms $ \m ->
    (caseOn m)
      (branch $ \_ -> False)
      (branch $ \y -> 0 <=. y)

eitherSetSpec :: Spec Fn (Set (Either Int Int), Set (Either Int Int), Set (Either Int Int))
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
instance IsUniverse fn => HasSpec fn Foo

fooSpec :: Spec Fn Foo
fooSpec = constrained $ \foo ->
  (caseOn foo)
    (branch $ \i -> 0 <=. i)
    (branch $ \i j -> i <=. j)

intSpec :: Spec Fn (Int, Int)
intSpec = constrained' $ \a b ->
  reify a (`mod` 10) $ \a' -> b ==. a'

mapElemSpec :: Spec Fn (Map Int (Bool, Int))
mapElemSpec = constrained $ \m ->
  [ assert $ m /=. lit mempty
  , forAll' (rng_ m) $ \_ b ->
      [0 <. b, b <. 10]
  ]

mapElemKeySpec :: Spec Fn Int
mapElemKeySpec = constrained $ \n ->
  letBind (pair_ n $ lit (False, 4)) $ \(p :: Term Fn (Int, (Bool, Int))) ->
    letBind (snd_ (snd_ p)) $ \x ->
      [x <. 10, 0 <. x, not_ $ elem_ n $ lit []]

intRangeSpec :: Int -> Spec Fn Int
intRangeSpec a = constrained $ \n -> n <. lit a

testRewriteSpec :: Spec Fn ((Int, Int), (Int, Int))
testRewriteSpec = constrained' $ \x y ->
  x ==. fromGeneric_ (toGeneric_ y)

mapPairSpec :: Spec Fn (Map Int Int, Set Int)
mapPairSpec = constrained' $ \m s ->
  subset_ (dom_ m) s

mapEmptyDomainSpec :: Spec Fn (Map Int Int)
mapEmptyDomainSpec = constrained $ \m ->
  subset_ (dom_ m) mempty

setPairSpec :: Spec Fn (Set Int, Set Int)
setPairSpec = constrained' $ \s s' ->
  forAll s $ \x ->
    forAll s' $ \y ->
      x <=. y

fixedSetSpec :: Spec Fn (Set Int)
fixedSetSpec = constrained $ \s ->
  forAll s $ \x ->
    [x <=. lit (i :: Int) | i <- [1 .. 3]]

setOfPairLetSpec :: Spec Fn (Set (Int, Int))
setOfPairLetSpec = constrained $ \ps ->
  forAll' ps $ \x y ->
    x <=. y

setSingletonSpec :: Spec Fn (Set (Int, Int))
setSingletonSpec = constrained $ \ps ->
  forAll ps $ \p ->
    forAll (singleton_ (fst_ p)) $ \x ->
      x <=. 10

pairSingletonSpec :: Spec Fn (Int, Int)
pairSingletonSpec = constrained $ \q ->
  forAll (singleton_ q) $ \p ->
    letBind (fst_ p) $ \x ->
      letBind (snd_ p) $ \y ->
        x <=. y

eitherSimpleSetSpec :: Spec Fn (Set (Either Int Int))
eitherSimpleSetSpec = constrained $ \ss ->
  forAll ss $ \s ->
    (caseOn s)
      (branch $ \a -> a <=. 0)
      (branch $ \b -> 0 <=. b)

forAllAnySpec :: Spec Fn (Set Int)
forAllAnySpec = constrained $ \as ->
  forAll as $ \_ -> True

weirdSetPairSpec :: Spec Fn ([Int], Set (Either Int Int))
weirdSetPairSpec = constrained' $ \as as' ->
  [ as' `dependsOn` as
  , forAll as $ \a ->
      member_ (left_ a) as'
  , forAll as' $ \a' ->
      (caseOn a')
        (branch $ \x -> elem_ x as)
        (branch $ \_ -> False)
  ]

maybeJustSetSpec :: Spec Fn (Set (Maybe Int))
maybeJustSetSpec = constrained $ \ms ->
  forAll ms $ \m ->
    (caseOn m)
      (branch $ \_ -> False)
      (branch $ \y -> 0 <=. y)

notSubsetSpec :: Spec Fn (Set Int, Set Int)
notSubsetSpec = constrained' $ \s s' -> not_ $ subset_ s s'

emptySetSpec :: Spec Fn (Set Int)
emptySetSpec = constrained $ \s ->
  forAll s $ \x -> member_ x mempty

emptyEitherMemberSpec :: Spec Fn (Set (Either Int Int))
emptyEitherMemberSpec = constrained $ \s ->
  forAll s $ \x ->
    (caseOn x)
      (branch $ \l -> member_ l mempty)
      (branch $ \r -> member_ r mempty)

emptyEitherSpec :: Spec Fn (Set (Either Int Int))
emptyEitherSpec = constrained $ \s ->
  forAll s $ \x ->
    (caseOn x)
      (branch $ \_ -> False)
      (branch $ \_ -> False)

knownDomainMap :: Spec Fn (Map Int Int)
knownDomainMap = constrained $ \m ->
  [ dom_ m ==. lit (Set.fromList [1, 2])
  , not_ $ 0 `elem_` rng_ m
  ]

parallelLet :: Spec Fn (Int, Int)
parallelLet = constrained $ \p ->
  [ letBind (fst_ p) $ \x -> 0 <. x
  , letBind (snd_ p) $ \x -> x <. 0
  ]

letExists :: Spec Fn (Int, Int)
letExists = constrained $ \p ->
  [ letBind (fst_ p) $ \x -> 0 <. x
  , exists (\eval -> pure $ snd (eval p)) $
      \x ->
        [ x <. 0
        , snd_ p ==. x
        ]
  ]

letExistsLet :: Spec Fn (Int, Int)
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

notSubset :: Spec Fn (Set Int)
notSubset = constrained $ \s ->
  not_ $ s `subset_` lit (Set.fromList [1, 2, 3])

unionSized :: Spec Fn (Set Int)
unionSized = constrained $ \s ->
  10 ==. size_ (s <> lit (Set.fromList [1 .. 8]))

listSum :: Numbery a => Spec Fn [a]
listSum = constrained $ \as ->
  10 <=. sum_ as

listSumForall :: Numbery a => Spec Fn [a]
listSumForall = constrained $ \xs ->
  [ forAll xs $ \x -> 1 <. x
  , assert $ sum_ xs ==. 20
  ]

listSumRange :: Numbery a => Spec Fn [a]
listSumRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> 1 <. x
      , assert $ n <. 20
      , assert $ 10 <. n
      ]

listSumRangeUpper :: Numbery a => Spec Fn [a]
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

listSumRangeRange :: Numbery a => Spec Fn [a]
listSumRangeRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> [1 <. x, x <. 5]
      , assert $ n <. 20
      , assert $ 10 <. n
      ]

listSumElemRange :: Numbery a => Spec Fn [a]
listSumElemRange = constrained $ \xs ->
  let n = sum_ xs
   in [ forAll xs $ \x -> [1 <. x, x <. 5]
      , assert $ n `elem_` lit [10, 12 .. 20]
      ]

listSumPair :: Numbery a => Spec Fn [(a, Int)]
listSumPair = constrained $ \xs ->
  [ assert $ foldMap_ (composeFn fstFn toGenericFn) xs ==. 100
  , forAll' xs $ \x y -> [20 <. x, x <. 30, y <. 100]
  ]

dependencyWeirdness :: Spec Fn (Int, Int, Int)
dependencyWeirdness = constrained' $ \x y z ->
  reify (x + y) id $ \zv -> z ==. zv

foldTrueCases :: Spec Fn (Either Int Int)
foldTrueCases = constrained $ \x ->
  [ assert $ not_ $ member_ x (lit (Set.fromList [Left 10]))
  , letBind (pair_ x (lit (0 :: Int))) $ \p ->
      caseOn
        (fst_ p)
        (branch $ \_ -> True)
        (branch $ \_ -> True)
  ]

foldSingleCase :: Spec Fn Int
foldSingleCase = constrained $ \x ->
  [ assert $ not_ $ member_ x (lit (Set.fromList [10]))
  , letBind (pair_ x $ lit [(10, 20) :: (Int, Int)]) $ \p ->
      match p $ \_ p1 -> forAll p1 $ \p2 ->
        assert (0 <=. snd_ p2)
  ]

parallelLetPair :: Spec Fn (Int, Int)
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
instance IsUniverse fn => HasSpec fn Three

mapSizeConstrained :: Spec Fn (Map Three Int)
mapSizeConstrained = constrained $ \m -> size_ (dom_ m) <=. 3

andPair :: Spec Fn (Int, Int)
andPair = constrained $ \p ->
  match p $ \x y ->
    x <=. 5 &&. y <=. 1

orPair :: Spec Fn (Int, Int)
orPair = constrained $ \p ->
  match p $ \x y ->
    x <=. 5 ||. y <=. 5
