{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Examples.ManualExamples where

import Constrained.API
import Constrained.Properties (forAllSpec)
import Data.Set (Set)
import GHC.Generics
import GHC.Natural
import Test.QuickCheck hiding (forAll)
import qualified Test.QuickCheck as QuickCheck

{- Generating from Specifications, and checking against Specifications -}

prop1 :: Gen Property
prop1 = do
  (w, x, y, z) <- arbitrary :: Gen (Int, Int, Int, Int)
  pure $ (w < x && x < y && y < z) ==> property (w < z)

spec1 :: Specification (Int, Int, Int, Int)
spec1 = constrained' $ \w x y z -> [w <. x, x <. y, y <. z]

prop2 :: Gen Property
prop2 = do
  (w, x, y, z) <- genFromSpec spec1
  pure $ (w < x && x < y && y < z) ==> property (w < z)

prop3 :: Gen Property
prop3 = do
  (w, x, y, z) <- frequency [(9, genFromSpec spec1), (1, arbitrary)]
  pure $
    if (w < x && x < y && y < z)
      then property (w < z)
      else expectFailure $ property (w < z)

leqPair :: Specification (Int, Int)
leqPair = constrained $ \p ->
  match p $ \x y ->
    assert (x <=. (y +. lit 2))

sumPair :: Specification (Int, Int)
sumPair = constrained $ \p ->
  match p $ \x y ->
    And
      [ assert $ x <=. y
      , assert $ y >=. 20
      , assert $ x + y ==. 25
      ]

ex1 :: Specification Int
ex1 = constrained $ \_x -> True

ex2 :: Specification Int
ex2 = constrained $ \x -> x ==. lit 3

ex3 :: Specification Int
ex3 = constrained $ \x -> [x <=. lit 2, x >=. lit 0]

ex4 :: Specification Int
ex4 = constrained $ \x -> assert $ x ==. lit 9

{- From Term to Pred
1.   `assert`
-}

-- assert :: IsPred p => p -> Pred
ex5 :: Specification [Int]
ex5 = constrained $ \xs -> assert $ elem_ 7 xs

{-  For all elements in a container type (List, Set, Map)
1.  `forAll`
-}

-- forAll :: (Forallable t a, HasSpec t, HasSpec a, IsPred p) =>
--           Term t -> (Term a -> p) -> Pred
-- class Forallable t e | t -> e where
-- instance Ord k => Forallable (Map k v) (k, v)
-- instance Ord a => Forallable (Set a) a
-- instance Forallable [a] a

ex6 :: Specification [Int]
ex6 = constrained $ \xs ->
  forAll xs $ \x -> [x <=. 10, x >. 1]

{-  Reification
1.  `reifies`
2.  `reify`
3.  `assertRefified`
-}

-- reifies :: (HasSpec a, HasSpec b) => Term b -> Term a -> (a -> b) -> Pred
ex7 :: Specification (Int, [Int])
ex7 = constrained $ \pair ->
  match pair $ \n xs ->
    reifies n xs sum

-- reify :: (HasSpec a, HasSpec b, IsPred p) => Term a -> (a -> b) -> (Term b -> p) -> Pred
ex8 :: Specification ([Int], [Int])
ex8 = constrained $ \pair ->
  match pair $ \xs1 xs2 ->
    [ assert $ sizeOf_ xs1 <=. 5
    , forAll xs1 $ \x -> x <=. 10
    , reify xs1 reverse $ \t -> xs2 ==. t
    ]

-- assertReified :: (HasSpec Bool, HasSpec a) => Term a -> (a -> Bool) -> Pred
ex9 :: Specification Int
ex9 = constrained $ \x ->
  [ assert $ x <=. 10
  , assertReified x (<= 10)
  ]

{- Disjunction, choosing between multiple things with the same type
1.  `CaseOn`, `branch`, `branchW`
2.  `chooseSpec`
-}

{-
caseOn
  :: (HasSpec a, HasSpec (SimpleRep a), HasSimpleRep a,
      TypeSpec a ~ TypeSpec (SimpleRep a),
      SimpleRep a
      ~ Constrained.Generic.SumOver
          (Constrained.Spec.SumProd.Cases (SimpleRep a)),
      TypeList (Constrained.Spec.SumProd.Cases (SimpleRep a))) =>
     Term a
     -> FunTy
          (MapList
             (Weighted Binder) (Constrained.Spec.SumProd.Cases (SimpleRep a)))
          Pred
-}

data Three = One Int | Two Bool | Three Int deriving (Ord, Eq, Show, Generic)

instance HasSimpleRep Three

instance HasSpec Three

ex10 :: Specification Three
ex10 = constrained $ \three ->
  caseOn
    three
    (branch $ \i -> i ==. 1) -- One
    (branch $ \b -> assert (not_ b)) -- Two
    (branch $ \j -> j ==. 3) -- Three

ex11 :: Specification Three
ex11 = constrained $ \three ->
  caseOn
    three
    (branchW 1 $ \i -> i <. 0) -- One, weight 1
    (branchW 2 $ \b -> assert b) -- Two, weight 2
    (branchW 3 $ \j -> j >. 0) -- Three, weight 3

-- chooseSpec:: HasSpec a => (Int, Specification a) -> (Int, Specification a) -> Specification a

ex12 :: Specification (Int, [Int])
ex12 =
  chooseSpec
    ( 5
    , constrained $ \pair ->
        match pair $ \tot xs -> [tot >. lit 10, sum_ xs ==. tot, sizeOf_ xs ==. lit 3]
    )
    ( 3
    , constrained $ \pair ->
        match pair $ \tot xs -> [tot <. lit 10, sum_ xs ==. tot, sizeOf_ xs ==. lit 6]
    )

{- Primed library functions which are compositions with match

1.  `forAll'`
2.  `constrained'`
3.  `reify'`
-}

ex13a :: Specification [(Int, Int)]
ex13a = constrained $ \xs ->
  forAll xs $ \x -> match x $ \a b -> a ==. negate b

ex13b :: Specification [(Int, Int)]
ex13b = constrained $ \xs ->
  forAll' xs $ \a b -> a ==. negate b

ex14a :: Specification (Int, Int, Int)
ex14a = constrained $ \triple ->
  match triple $ \a b c -> [b ==. a + lit 1, c ==. b + lit 1]

ex14b :: Specification (Int, Int, Int)
ex14b = constrained' $ \a b c -> [b ==. a + lit 1, c ==. b + lit 1]

ex15a :: Specification (Int, Int, Int)
ex15a = constrained $ \triple ->
  match triple $ \x1 x2 x3 ->
    reify x1 (\a -> (a + 1, a + 2)) $ \t ->
      match t $ \b c -> [x2 ==. b, x3 ==. c]

ex15b :: Specification (Int, Int, Int)
ex15b = constrained $ \triple ->
  match triple $ \x1 x2 x3 ->
    reify' x1 (\a -> (a + 1, a + 2)) $ \b c -> [x2 ==. b, x3 ==. c]

ex15c :: Specification (Int, Int, Int)
ex15c = constrained' $ \x1 x2 x3 ->
  reify' x1 (\a -> (a + 1, a + 2)) $ \b c -> [x2 ==. b, x3 ==. c]

{-  Construtors and Selectors
1.  `onCon`
2.  `sel`
4.  `isJust`
-}

ex16 :: Specification Three
ex16 = constrained $ \three ->
  caseOn
    three
    (branchW 1 $ \i -> i ==. lit 1) -- One, weight 1
    (branchW 2 $ \b -> assert (not_ b)) -- Two, weight 2
    (branchW 3 $ \j -> j ==. 3) -- Three, weight 3

ex17 :: Specification Three
ex17 = constrained $ \three ->
  [ onCon @"One" three (\x -> x ==. lit 1)
  , onCon @"Two" three (\x -> not_ x)
  , onCon @"Three" three (\x -> x ==. lit 3)
  ]

ex18 :: Specification Three
ex18 = constrained $ \three -> onCon @"Three" three (\x -> x ==. lit 3)

ex19 :: Specification (Maybe Bool)
ex19 = constrained $ \mb -> onCon @"Just" mb (\x -> x ==. lit False)

data Dimensions where
  Dimensions ::
    { length :: Int
    , width :: Int
    , depth :: Int
    } ->
    Dimensions
  deriving (Ord, Eq, Show, Generic)

instance HasSimpleRep Dimensions

instance HasSpec Dimensions

ex20a :: Specification Dimensions
ex20a = constrained $ \d ->
  match d $ \l w dp -> [l >. lit 10, w ==. lit 5, dp <. lit 20]

ex20b :: Specification Dimensions
ex20b = constrained $ \d ->
  [ sel @0 d >. lit 10
  , sel @1 d ==. lit 5
  , sel @2 d <. lit 20
  ]

width_ :: Term Dimensions -> Term Int
width_ d = sel @1 d

ex21 :: Specification Dimensions
ex21 = constrained $ \d -> width_ d ==. lit 1

{- Naming introduced lambda bound Term variables
1.  [var|name|]
-}

ex22a :: Specification (Int, Int)
ex22a = constrained $ \pair ->
  match pair $ \left right -> [left ==. right, left ==. right + lit 1]

ex22b :: Specification (Int, Int)
ex22b = constrained $ \ [var|pair|] ->
  match pair $ \ [var|left|] [var|right|] -> [left ==. right, left ==. right + lit 1]

{-  Existential quantifiers
1.  `exists`
2.  `unsafeExists`
-}

ex24 :: Specification Int
ex24 = constrained $ \ [var|oddx|] ->
  unsafeExists
    (\ [var|y|] -> [Assert $ oddx ==. y + y + 1])

ex25 :: Specification Int
ex25 = ExplainSpec ["odd via (y+y+1)"] $
  constrained $ \ [var|oddx|] ->
    exists
      (\eval -> pure (div (eval oddx - 1) 2))
      (\ [var|y|] -> [Assert $ oddx ==. y + y + 1])

{-  Conditionals
1.  `whenTrue`
2.  `ifElse`
-}

data Rectangle = Rectangle {wid :: Int, len :: Int, square :: Bool}
  deriving (Show, Eq, Generic)

instance HasSimpleRep Rectangle

instance HasSpec Rectangle

ex26 :: Specification Rectangle
ex26 = constrained' $ \w l sq ->
  [ assert $ w >=. lit 0
  , assert $ l >=. lit 0
  , whenTrue sq (assert $ w ==. l)
  ]

ex27 :: Specification Rectangle
ex27 = constrained' $ \w l sq ->
  ifElse
    sq
    (assert $ w ==. l)
    [ assert $ w >=. lit 0
    , assert $ l >=. lit 0
    ]

{-  `Explanantions`
1.  `assertExplain`
2.  `explanation`
3.  `ExplainSpec`
-}

ex28a :: Specification (Set Int)
ex28a = constrained $ \s ->
  [ assert $ member_ (lit 5) s
  , forAll s $ \x -> [x >. lit 6, x <. lit 20]
  ]

ex28b :: Specification (Set Int)
ex28b = ExplainSpec ["5 must be in the set"] $
  constrained $ \s ->
    [ assert $ member_ (lit 5) s
    , forAll s $ \x -> [x >. lit 6, x <. lit 20]
    ]

{-  Operations to define and use Specifications
1.  `satisfies`
2.  `equalSpec`
3.  `notEqualSpec`
4.  `notMemberSpec`
5.  `leqSpec`
6.  `ltSpec`
7.  `geqSpec`
8.  `gtSpec`
5.  `cardinality`
-}

ex29 :: Specification Int
ex29 = constrained $ \x ->
  [ assert $ x >=. lit 0
  , assert $ x <=. lit 5
  , satisfies x (notMemberSpec [2, 3])
  ]

{-  Utility functions
1.  `simplifyTerm`
2.  `simplifySpec`
3.  `genFromSpecT`
4.  `genFromSpec`
5.  `genFromSpecWithSeed`
6.  `debugSpec`
-}

{-  Escape Hatch to QuickCheck Gen monad
1.  `monitor`
-}

ex30 :: Specification (Int, Int)
ex30 = constrained $ \ [var|p|] ->
  match p $ \ [var|x|] [var|y|] ->
    [ assert $ x /=. 0
    , -- You can use `monitor` to add QuickCheck property modifiers for
      -- monitoring distribution, like classify, label, and cover, to your
      -- specification
      monitor $ \eval ->
        QuickCheck.classify (eval y > 0) "positive y"
          . QuickCheck.classify (eval x > 0) "positive x"
    ]

prop31 :: QuickCheck.Property
prop31 = forAllSpec ex30 $ \_ -> True

ex32 :: IO ()
ex32 = QuickCheck.quickCheck $ prop31

ex11m :: Specification Three
ex11m = constrained $ \three ->
  [ caseOn
      three
      (branchW 1 $ \i -> i <. 0) -- One, weight 1
      (branchW 2 $ \b -> assert b) -- Two, weight 2
      (branchW 3 $ \j -> j >. 0) -- Three, weight 3
  , monitor $ \eval ->
      case (eval three) of
        One _ -> QuickCheck.classify True "One should be about 1/6"
        Two _ -> QuickCheck.classify True "Two should be about 2/6"
        Three _ -> QuickCheck.classify True "Three should be about 3/6"
  ]

propex11 :: QuickCheck.Property
propex11 = forAllSpec ex11m $ \_ -> True

ex33 :: IO ()
ex33 = QuickCheck.quickCheck $ propex11

{-  Strategy for constraining a large type with many nested sub-components -}

data Nested = Nested Three Rectangle [Int]
  deriving (Show, Eq, Generic)

instance HasSimpleRep Nested

instance HasSpec Nested

{-
Problem using TruePred, not monomorphic enough
skeleton1 :: Specification Nested
skeleton1 = constrained $ \ [var|nest|] ->
  match nest $ \ [var|three|] [var|rect|] [var|line|] ->
    [ (caseOn (three :: Term Three))
        (branch $ \ _i -> TruePred) -- One,
        (branch $ \ _k -> TruePred) -- Two,
        (branch $ \ _j -> TruePred) -- Three,
    , match rect $ \ [var|_wid|] [var|_len|] [var|_square|] -> TruePred
    , forAll line $ \ [var|_point|] -> TruePred
    ]
-}

-- By type applying match, branch, and forAll to @Pred , makes it monomorphic
--  Note    type Pred = PredD Deps , so it fixes the type argument of PredD
skeleton2 :: Specification Nested
skeleton2 = constrained $ \ [var|nest|] ->
  match nest $ \ [var|three|] [var|rect|] [var|line|] ->
    [ (caseOn (three :: Term Three))
        (branch @Pred $ \_i -> TruePred) -- One,
        (branch @Pred $ \_k -> TruePred) -- Two,
        (branch @Pred $ \_j -> TruePred) -- Three,
    , match @Pred rect $ \ [var|_wid|] [var|_len|] [var|_square|] -> TruePred
    , forAll @Pred line $ \ [var|_point|] -> TruePred
    ]

-- We can do a similar thing by introducing `truePred` with the monomorphic type.
truePred :: Pred
truePred = TruePred

skeleton :: Specification Nested
skeleton = constrained $ \ [var|nest|] ->
  match nest $ \ [var|three|] [var|rect|] [var|line|] ->
    [ (caseOn (three :: Term Three))
        (branch $ \_i -> truePred) -- One,
        (branch $ \_k -> truePred) -- Two,
        (branch $ \_j -> truePred) -- Three,
    , match rect $ \ [var|_wid|] [var|_len|] [var|_square|] -> [truePred]
    , forAll line $ \ [var|_point|] -> truePred
    ]

-- ======================================================================

newtype Coin = Coin {unCoin :: Integer} deriving (Eq, Show)

instance HasSimpleRep Coin where
  type SimpleRep Coin = Natural
  toSimpleRep (Coin i) = case integerToNatural i of
    Nothing -> error $ "The impossible happened in toSimpleRep for (Coin " ++ show i ++ ")"
    Just w -> w
  fromSimpleRep = naturalToCoin

instance HasSpec Coin

integerToNatural :: Integer -> Maybe Natural
integerToNatural c
  | c < 0 = Nothing
  | otherwise = Just $ fromIntegral c

naturalToCoin :: Natural -> Coin
naturalToCoin = Coin . fromIntegral

ex34 :: Specification Coin
ex34 = constrained $ \coin ->
  match coin $ \nat -> [nat >=. lit 100, nat <=. lit 200]
