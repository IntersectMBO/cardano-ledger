{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Examples.Fold where

import Constrained
import Constrained.Base (Pred (..), genListWithSize, predSpecPair)
import Constrained.Examples.List (Numbery)
import Constrained.SumList
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.String (fromString)
import Prettyprinter (fillSep, punctuate, space)
import System.Random (Random)
import Test.QuickCheck hiding (forAll, total)

-- ========================================================
-- Specifications we use in the examples and in the tests

oddSpec :: Specification BaseFn Int
oddSpec = ExplainSpec ["odd via (y+y+1)"] $
  constrained $ \ [var|oddx|] ->
    exists
      (\eval -> pure (div (eval oddx - 1) 2))
      (\ [var|y|] -> [Assert $ oddx ==. y + y + 1])

evenSpec ::
  forall n.
  (TypeSpec BaseFn n ~ NumSpec BaseFn n, Integral n, HasSpec BaseFn n, MaybeBounded n) =>
  Specification BaseFn n
evenSpec = ExplainSpec ["even via (x+x)"] $
  constrained $ \ [var|evenx|] ->
    exists
      (\eval -> pure (div (eval evenx) 2))
      (\ [var|somey|] -> [Assert $ evenx ==. somey + somey])

sum3WithLength :: Integer -> Specification BaseFn ([Int], Int, Int, Int)
sum3WithLength n =
  constrained $ \ [var|quad|] ->
    match quad $ \ [var|l|] [var|n1|] [var|n2|] [var|n3|] ->
      [ Assert $ sizeOf_ l ==. lit n
      , forAll l $ \ [var|item|] -> item >=. lit 0
      , Assert $ sum_ l ==. n1 + n2 + n3
      , Assert $ n1 + n2 + n3 >=. lit (fromInteger n)
      ]

sum3 :: Specification BaseFn [Int]
sum3 = constrained $ \ [var|xs|] -> [sum_ xs ==. lit 6 + lit 9 + lit 5, sizeOf_ xs ==. 5]

listSumPair :: Numbery a => Specification BaseFn [(a, Int)]
listSumPair = constrained $ \xs ->
  [ assert $ foldMap_ fst_ xs ==. 100
  , forAll' xs $ \x y -> [20 <. x, x <. 30, y <. 100]
  ]

listSumForall :: Numbery a => Specification BaseFn [a]
listSumForall = constrained $ \xs ->
  [ forAll xs $ \x -> 1 <. x
  , assert $ sum_ xs ==. 20
  ]

-- | Complicated, because if 'a' is too large, the spec is unsatisfiable.
listSumComplex :: Numbery a => a -> Specification BaseFn [a]
listSumComplex a = constrained $ \xs ->
  [ forAll xs $ \x -> 1 <. x
  , assert $ sum_ xs ==. 20
  , assert $ sizeOf_ xs >=. lit 4
  , assert $ sizeOf_ xs <=. lit 6
  , assert $ elem_ (lit a) xs
  ]

-- ==============================================================
-- Tools for building properties that have good counterexamples

data Outcome = Succeed | Fail

propYes :: String -> Solution t -> Property
propYes _ (Yes _) = property True
propYes msg (No xs) = property (counterexample (unlines (msg : xs)) False)

propNo :: Show t => String -> Solution t -> Property
propNo msg (Yes (x :| _)) = property (counterexample (unlines [msg, "Expected to fail, but succeeds with", show x]) False)
propNo _ (No _) = property True

parensList :: [String] -> String
parensList xs = show (fillSep $ punctuate space $ map fromString xs)

-- ===============================================================
-- Functions for building properties about the functions defined
-- in module Constrained.SumList(logish,pickAll)

logishProp :: Gen Property
logishProp = do
  n <- choose (-17, 17 :: Int) -- Any bigger or smaller is out of the range of Int
  i <- choose (logRange n)
  pure (logish i === n)

picktest :: (Ord a, Num a) => a -> a -> (a -> Bool) -> a -> Int -> [a] -> Bool
picktest smallest largest p total count ans =
  smallest <= largest
    && total == sum ans
    && count == length ans
    && all p ans

-- | generate a different category of test, each time.
pickProp :: Gen Property
pickProp = do
  smallest <- elements [-4, 1 :: Int]
  count <- choose (2, 4)
  total <- (+ 20) <$> choose (smallest, 5477)
  let largest = total + 10
  (nam, p) <-
    elements
      ( concat
          [ if even total then [("even", even)] else []
          , if odd total && odd count then [("odd", odd)] else []
          , [("(>0)", (> 0)), ("true", const True)]
          ]
      )
  (_cost, ans) <- pickAll smallest largest (nam, p) total count (Cost 0)
  case ans of
    Yes result -> pure $ property $ all (picktest smallest largest p total count) result
    No msgs -> pure $ counterexample ("predicate " ++ nam ++ "\n" ++ unlines msgs) False

-- | Build properties about calls to 'genListWithSize'
testFoldSpec ::
  forall a.
  (Foldy BaseFn a, Random a, Integral a, TypeSpec BaseFn a ~ NumSpec BaseFn a) =>
  Specification BaseFn Integer ->
  Specification BaseFn a ->
  Specification BaseFn a ->
  Outcome ->
  Gen Property
testFoldSpec size elemSpec total outcome = do
  ans <- genFromGenT $ inspect $ genListWithSize size elemSpec total
  let callString = parensList ["GenListWithSize", show size, fst (predSpecPair elemSpec), show total]
      fails xs = unlines [callString, "Should fail, but it succeeds with", show xs]
      succeeds xs = unlines (callString : "Should succeed, but it fails with" : NE.toList xs)
  case (ans, outcome) of
    (Result _ _, Succeed) -> pure $ property True
    (Result _ xs, Fail) -> pure $ counterexample (fails xs) False
    (FatalError _ _, Fail) -> pure $ property True
    (FatalError _ xs, Succeed) -> pure $ counterexample (succeeds xs) False
    (GenError _ _, Fail) -> pure $ property True
    (GenError _ xs, Succeed) -> pure $ counterexample (succeeds xs) False

-- | Generate a property from a call to 'pickAll'. We can test for success or failure using 'outcome'
sumProp ::
  (Integral t, Random t, HasSpec BaseFn t) =>
  t -> t -> Specification BaseFn t -> t -> Int -> Outcome -> Gen Property
sumProp smallest largest spec total count outcome = sumProp2 smallest largest (predSpecPair spec) total count outcome

-- | Like SumProp, but instead of using a (Specification fn n) for the element predicate
--   It uses an explicit pair of a (String, n -> Bool). This means we can test things
--   using any Haskell function.
sumProp2 ::
  (Show t, Integral t, Random t) =>
  t -> t -> (String, t -> Bool) -> t -> Int -> Outcome -> Gen Property
sumProp2 smallest largest spec total count outcome = do
  (_, ans) <- pickAll smallest largest spec total count (Cost 0)
  let callString = parensList ["pickAll", show smallest, (fst spec), show total, show count]
      message Succeed = "\nShould succeed, but it fails with"
      message Fail = "\nShould fail, but it succeeds with " ++ show ans
  pure
    ( case outcome of
        Succeed -> propYes (callString ++ message outcome) ans
        Fail -> propNo callString ans
    )
