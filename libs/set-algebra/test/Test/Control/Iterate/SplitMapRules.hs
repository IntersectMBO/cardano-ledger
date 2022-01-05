{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Control.Iterate.SplitMapRules where

import Control.Iterate.BaseTypes (BaseRep (..), Sett (..))
import Control.Iterate.Exp (Exp (..))
import Control.Iterate.SetAlgebra (rewrite, runSet)
import Data.Compact.KeyMap (Key (..))
import Data.Compact.SplitMap
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude hiding (lookup)

-- ============================================================================
-- The idea is to test that the fast path (rewrite) and the slowpath (runSet)
-- compute the same thing. There is a test for every rewrite rule

t01 :: SplitMap SS Int -> Exp (Sett SS ())
t01 xs = Dom (Base SplitR xs)

t02 :: SplitMap SS Int -> Int -> Exp (Sett SS ())
t02 xs v = Dom (RRestrict (Base SplitR xs) (SetSingleton v))

t03 :: SplitMap SS Int -> Sett Int () -> Exp (Sett SS ())
t03 xs set = Dom (RRestrict (Base SplitR xs) (Base SetR set))

t04 :: SplitMap SS Int -> Sett Int () -> Exp (Sett SS ())
t04 xs set = Dom (RExclude (Base SplitR xs) (Base SetR set))

t05 :: SplitMap SS Int -> Int -> Exp (Sett SS ())
t05 xs v = Dom (RExclude (Base SplitR xs) (SetSingleton v))

t06 :: SplitMap SS Int -> SS -> Exp (Sett SS ())
t06 xs v = Dom (DRestrict (SetSingleton v) (Base SplitR xs))

t07 :: SplitMap SS Int -> Sett SS () -> Exp (Sett SS ())
t07 xs set = Dom (DRestrict (Base SetR set) (Base SplitR xs))

t08 :: SplitMap SS Int -> SS -> Exp (Sett SS ())
t08 xs v = Dom (DExclude (SetSingleton v) (Base SplitR xs))

t09 :: SplitMap SS Int -> Sett SS () -> Exp (Sett SS ())
t09 xs set = Dom (DExclude (Base SetR set) (Base SplitR xs))

t10 :: SplitMap SS Int -> Sett SS () -> Exp (SplitMap SS Int)
t10 xs set = DRestrict (Base SetR set) (Base SplitR xs)

t11 :: SplitMap SS Int -> SS -> Exp (SplitMap SS Int)
t11 xs k = DRestrict (SetSingleton k) (Base SplitR xs)

t12 :: SplitMap SS Int -> SS -> () -> Exp (SplitMap SS Int)
t12 xs k v = DRestrict (Singleton k v) (Base SplitR xs)

t13 :: SplitMap SS Char -> Map SS Int -> Exp (SplitMap SS Char)
t13 xs m = DRestrict (Dom (Base MapR m)) (Base SplitR xs)

t14 :: SplitMap SS Int -> Map SS Char -> Exp (Map SS Char)
t14 xs m = DRestrict (Dom (Base SplitR xs)) (Base MapR m)

t15 :: SplitMap SS Int -> SplitMap SS Char -> Exp (SplitMap SS Char)
t15 xs zs = DRestrict (Dom (Base SplitR xs)) (Base SplitR zs)

t16 :: SplitMap SS Char -> Map SS Int -> Int -> Exp (SplitMap SS Char)
t16 xs m k = DRestrict (Dom (RRestrict (Base MapR m) (SetSingleton k))) (Base SplitR xs)

t17 :: SplitMap SS Char -> Map SS Int -> Map Int () -> Exp (SplitMap SS Char)
t17 xs m1 m2 = DRestrict (Dom (RRestrict (Base MapR m1) (Base MapR m2))) (Base SplitR xs)

t18 :: SplitMap SS Int -> Map SS () -> Exp (SplitMap SS Int)
t18 xs set = DRestrict (Base MapR set) (Base SplitR xs)

t19 :: SplitMap SS Int -> SS -> Exp (SplitMap SS Int)
t19 xs k = DExclude (SetSingleton k) (Base SplitR xs)

t20 :: SplitMap SS Int -> SS -> Char -> Exp (SplitMap SS Int)
t20 xs k v = DExclude (Dom (Singleton k v)) (Base SplitR xs)

t21 :: SplitMap SS Int -> Int -> SS -> Exp (SplitMap SS Int)
t21 xs k v = DExclude (Rng (Singleton k v)) (Base SplitR xs)

t22 :: SplitMap SS Int -> Set.Set SS -> Exp (SplitMap SS Int)
t22 xs set = DExclude (Base SetR (Sett set)) (Base SplitR xs)

t23 :: SplitMap SS Int -> Map SS Char -> Exp (SplitMap SS Int)
t23 xs m = DExclude (Dom (Base MapR m)) (Base SplitR xs)

t24 :: SplitMap SS Int -> SplitMap SS Char -> Exp (SplitMap SS Char)
t24 xs zs = DExclude (Dom (Base SplitR xs)) (Base SplitR zs)

t25 :: SplitMap SS Int -> Int -> Exp (SplitMap SS Int)
t25 xs u = RExclude (Base SplitR xs) (SetSingleton u)

t26 :: SplitMap SS Int -> Sett Int () -> Exp (SplitMap SS Int)
t26 xs set = RExclude (Base SplitR xs) (Base SetR set)

t27 :: SplitMap SS Int -> Int -> Exp (SplitMap SS Int)
t27 xs k = RRestrict (Base SplitR xs) (SetSingleton k)

t28 :: SplitMap SS Int -> Set.Set Int -> Exp (SplitMap SS Int)
t28 xs s = RRestrict (Base SplitR xs) (Base SetR (Sett s))

-- ============================================================================
-- Abstractions for writing tests from rules. One for 1, 2, and 3 input tests

rule1 ::
  forall v a f.
  (Show a, Show (f SS v), Eq (f SS v)) =>
  String ->
  (a -> Exp (f SS v)) ->
  a ->
  Property
rule1 name f x =
  case rewrite (f x) of
    Just t ->
      counterexample
        ("1 input property " ++ name ++ "\n exp = " ++ show (f x) ++ "\n   " ++ show x)
        (t === runSet (f x))
    Nothing -> error ("1 input property " ++ name ++ "\n  exp = " ++ show (f x) ++ "\n  did not match any rule.")

rule2 ::
  forall v a1 a2 f.
  (Show a1, Show a2, Show (f SS v), Eq (f SS v)) =>
  String ->
  (a1 -> a2 -> Exp (f SS v)) ->
  a1 ->
  a2 ->
  Property
rule2 name f x y =
  case rewrite (f x y) of
    Just t ->
      counterexample
        ("2 input property " ++ name ++ "\n exp = " ++ show (f x y) ++ "\n   x=" ++ show x ++ "\n  y=" ++ show y)
        (t === runSet (f x y))
    Nothing -> error ("2 input property " ++ name ++ "\n  exp = " ++ show (f x y) ++ "\n  did not match any rule.")

rule3 ::
  forall v a1 a2 a3 f.
  (Show a1, Show a2, Show a3, Show (f SS v), Eq (f SS v)) =>
  String ->
  (a1 -> a2 -> a3 -> Exp (f SS v)) ->
  a1 ->
  a2 ->
  a3 ->
  Property
rule3 name f x y z =
  case rewrite (f x y z) of
    Just t ->
      counterexample
        ("3 input property " ++ name ++ "\n exp = " ++ show (f x y z) ++ "\n   x=" ++ show x ++ "\n  y=" ++ show y ++ "\n  z=" ++ show z)
        (t === runSet (f x y z))
    Nothing -> error ("3 input property " ++ name ++ "\n  exp = " ++ show (f x y z) ++ "\n  did not match any rule.")

-- ========================================================================================
-- Abstractions for making TestTree's that report the 'name' and the formula being tested.
-- This works because the Show instance for Exp is Lazy in its leaves, so we get the structure
-- of the test, by passing undefined for all its inputs, and then showing the result.

property2 ::
  forall v a b f.
  (Arbitrary a, Arbitrary b, Show (f SS v), Eq (f SS v), Show a, Show b) =>
  String ->
  (a -> b -> Exp (f SS v)) ->
  TestTree
property2 name tnn = testProperty (name ++ " " ++ show (tnn u u)) (rule2 @v name tnn)
  where
    u :: any
    u = undefined

property3 ::
  forall v a b c f.
  (Arbitrary a, Arbitrary b, Arbitrary c, Show (f SS v), Eq (f SS v), Show a, Show b, Show c) =>
  String ->
  (a -> b -> c -> Exp (f SS v)) ->
  TestTree
property3 name tnn = testProperty (name ++ " " ++ show (tnn u u u)) (rule3 @v name tnn)
  where
    u :: any
    u = undefined

fastSlow :: TestTree
fastSlow =
  testGroup
    "SplitMap rewrites compute the same in fast or slow mode"
    [ testProperty "t01" (rule1 "t01" t01),
      property2 "t02" t02,
      property2 "t03" t03,
      property2 "t04" t04,
      property2 "t05" t05,
      property2 "t06" t06,
      property2 "t07" t07,
      property2 "t08" t08,
      property2 "t09" t09,
      property2 "t10" t10,
      property2 "t11" t11,
      property3 "t12" t12,
      property2 "t13" t13,
      property2 "t14" t14,
      property2 "t15" t15,
      property3 "t16" t16,
      property3 "t17" t17,
      property2 "t18" t18,
      property2 "t19" t19,
      property3 "t20" t20,
      property3 "t21" t21,
      property2 "t22" t22,
      property2 "t23" t23,
      property2 "t24" t24,
      property2 "t25" t25,
      property2 "t26" t26,
      property2 "t27" t27,
      property2 "t28" t28
    ]

test :: IO ()
test = defaultMain fastSlow

-- =================================================================
-- A simple type with a Split instance

data SS = SS Int Word64
  deriving (Eq, Show)

instance Ord SS where
  compare (SS i n) (SS j m) =
    case compare i j of
      EQ -> compare n m
      other -> other

instance Split SS where
  splitKey (SS n m) = (n, Key m 0 0 0)
  joinKey n (Key m _ _ _) = SS n m

instance Arbitrary SS where
  arbitrary = joinKey <$> chooseInt (0, 25) <*> (Key <$> arbitrary <*> pure 0 <*> pure 0 <*> pure 0)

instance (Split k, Arbitrary k, Arbitrary a) => Arbitrary (SplitMap k a) where
  arbitrary = do
    let go i m
          | i > 0 = do
            key <- arbitrary
            val <- arbitrary
            go (i - 1) $! insert key val m
          | otherwise = pure m
    NonNegative n <- arbitrary
    go (n :: Int) empty

instance (Ord t, Arbitrary t) => Arbitrary (Sett t ()) where
  arbitrary = Sett <$> arbitrary
