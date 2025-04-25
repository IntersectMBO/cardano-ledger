{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.MinTuple where

import Constrained.Env
import Constrained.MinBase
import Constrained.MinModel
import Constrained.MinSyntax

import Constrained.Core (Evidence (..), Value (..), Var (..), eqVar)
import Constrained.GenT
import Constrained.List hiding (ListCtx)
import Constrained.Syntax (var)
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import Data.String (fromString)
import Data.Typeable
import GHC.Stack
import Prettyprinter
import Test.QuickCheck.Gen

-- =======================================================
-- Experiment to see if we can build tuples, using only the binary tuple
-- as the base case.  The only compilcated part is the `combineSpec` which
-- uses `guardTypeSpec` to merge the simple sub cases on smaller tuples.

instance (HasSpec a, HasSpec b, HasSpec c) => HasSpec (a, b, c) where
  type TypeSpec (a, b, c) = (Spec a, Spec (b, c))

  emptySpec = (mempty @(Spec a), mempty @(Spec (b, c)))

  combineSpec (a, b) (a', b') = guardTypeSpec (a <> a', b <> b')

  conformsTo (a, b, c) (sa, sbc) = conformsToSpec a sa && conformsToSpec (b, c) sbc

  guardTypeSpec (a, bc) =
    handleErrors
      a
      bc
      ( \x y -> case (x :: Spec a, y :: Spec (b, c)) of
          (MemberSpec xs, MemberSpec ys) -> MemberSpec (NE.fromList [(a, b, c) | a <- NE.toList xs, (b, c) <- NE.toList ys])
          -- There are probably other cases
          (specA, specBC) -> constrained $ \x -> And [satisfies (head_ x) specA, satisfies (tail_ x) specBC]
      )

  genFromTypeSpec (a, bc) = f <$> genFromSpecT a <*> genFromSpecT bc
    where
      f a (b, c) = (a, b, c)

  toPreds x (a, bc) = satisfies (head_ x) a <> satisfies (tail_ x) bc

-- =======================================================

instance (HasSpec a, HasSpec b, HasSpec c, HasSpec d) => HasSpec (a, b, c, d) where
  type TypeSpec (a, b, c, d) = (Spec (a, b), Spec (c, d))

  emptySpec = (mempty @(Spec (a, b)), mempty @(Spec (c, d)))

  combineSpec (a, b) (a', b') = guardTypeSpec (a <> a', b <> b')

  conformsTo (a, b, c, d) (sAB, sCD) = conformsToSpec (a, b) sAB && conformsToSpec (c, d) sCD

  guardTypeSpec (ab, cd) =
    handleErrors
      ab
      cd
      ( \x y -> case (x, y) of
          (MemberSpec xs, MemberSpec ys) -> MemberSpec (NE.fromList [(a, b, c, d) | (a, b) <- NE.toList xs, (c, d) <- NE.toList ys])
          -- There are probably other cases
          (specAB, specCD) -> constrained $ \x -> And [satisfies (left4_ x) specAB, satisfies (right4_ x) specCD]
      )

  genFromTypeSpec (ab, cd) = f <$> genFromSpecT ab <*> genFromSpecT cd
    where
      f (a, b) (c, d) = (a, b, c, d)

  toPreds x (ab, cd) = satisfies (left4_ x) ab <> satisfies (right4_ x) cd

-- ======================================================================
-- We need some function symbols, to break Bigger tuples into sub-tuples

data TupleSym (ds :: [Type]) r where
  Left4W :: All HasSpec '[a, b, c, d] => TupleSym '[(a, b, c, d)] (a, b)
  Right4W :: All HasSpec '[a, b, c, d] => TupleSym '[(a, b, c, d)] (c, d)
  HeadW :: All HasSpec '[a, b, c] => TupleSym '[(a, b, c)] a
  TailW :: All HasSpec '[a, b, c] => TupleSym '[(a, b, c)] (b, c)

deriving instance Eq (TupleSym ds r)
instance Show (TupleSym ds r) where show = name

instance Syntax TupleSym where
  inFix _ = False
  name Left4W = "left4_"
  name Right4W = "right4_"
  name HeadW = "head_"
  name TailW = "tail_"

instance Semantics TupleSym where
  semantics Left4W = \(a, b, c, d) -> (a, b)
  semantics Right4W = \(a, b, c, d) -> (c, d)
  semantics HeadW = \(a, b, c) -> a
  semantics TailW = \(a, b, c) -> (b, c)

instance Logic TupleSym where
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate symW (Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App symW (v' :> Nil)) (v :-> ps)
  propagate Left4W (Unary HOLE) specAB = anyRight specAB
  propagate Right4W (Unary HOLE) specCD = anyLeft specCD
  propagate HeadW (Unary HOLE) specA = anyTail specA
  propagate TailW (Unary HOLE) specBC = anyHead specBC

anyHead :: forall a b c. (HasSpec a, HasSpec b, HasSpec c) => Spec (b, c) -> Spec (a, b, c)
anyHead specBC = typeSpec @(a, b, c) (mempty @(Spec a), specBC)

anyTail :: forall a b c. (HasSpec a, HasSpec b, HasSpec c) => Spec a -> Spec (a, b, c)
anyTail specA = typeSpec (specA, mempty @(Spec (b, c)))

anyLeft ::
  forall a b c d. (HasSpec a, HasSpec b, HasSpec c, HasSpec d) => Spec (c, d) -> Spec (a, b, c, d)
anyLeft specCD = typeSpec (mempty @(Spec (a, b)), specCD)

anyRight ::
  forall a b c d. (HasSpec a, HasSpec b, HasSpec c, HasSpec d) => Spec (a, b) -> Spec (a, b, c, d)
anyRight specAB = typeSpec (specAB, mempty @(Spec (c, d)))

-- ==========================================================

head_ :: All HasSpec '[a, b, c] => Term (a, b, c) -> Term a
head_ x = App HeadW (x :> Nil)

tail_ :: All HasSpec '[a, b, c] => Term (a, b, c) -> Term (b, c)
tail_ x = App TailW (x :> Nil)

left4_ :: All HasSpec '[a, b, c, d] => Term (a, b, c, d) -> Term (a, b)
left4_ x = App Left4W (x :> Nil)

right4_ :: All HasSpec '[a, b, c, d] => Term (a, b, c, d) -> Term (c, d)
right4_ x = App Right4W (x :> Nil)

-- ======================================================================
-- The Match class, with function `match` makes using all tuples uniform
-- For any n-tuple, supply `match` with an n-ary function to bring into
-- scope variables with type Term, which can be used to make Pred

class Match t (ts :: [Type]) | t -> ts where
  match :: All HasSpec ts => Term t -> FunTy (MapList Term ts) Pred -> Pred

instance Match (a, b) '[a, b] where
  match ab f =
    Let
      (fst_ ab)
      ( bind $ \ft ->
          Let (snd_ ab) (bind $ \st -> f ft st)
      )

instance Match (a, b, c) '[a, b, c] where
  match abc f =
    Let
      (head_ abc)
      ( bind $ \h ->
          Let
            (tail_ abc)
            ( bind $ \t ->
                Let
                  (fst_ t)
                  ( bind $ \ft ->
                      Let (snd_ t) (bind $ \st -> f h ft st)
                  )
            )
      )

instance Match (a, b, c, d) '[a, b, c, d] where
  match abcd f =
    Let
      (left4_ abcd)
      ( bind $ \v5 ->
          Let
            (right4_ abcd)
            ( bind $ \v4 ->
                Let
                  (fst_ v5)
                  ( bind $ \v3 ->
                      Let
                        (snd_ v5)
                        ( bind $ \v2 ->
                            Let
                              (fst_ v4)
                              ( bind $ \v1 ->
                                  Let (snd_ v4) (bind $ \v0 -> f v3 v2 v1 v0)
                              )
                        )
                  )
            )
      )

-- =========================================================
-- Here are some examples, Notice how the arity of the function
-- passed to `match` changes as the width of the tuples changes.

spec2 :: Spec (Integer, Integer)
spec2 = constrained $ \x ->
  match x $ \a b -> Assert $ a <=. b

spec3 :: Spec (Integer, Integer, Integer)
spec3 = constrained $ \v4 ->
  match v4 $ \v3 v1 v0 -> And [Assert $ v3 <=. v1, Assert $ v1 <=. v0]

spec4 :: Spec (Integer, Integer, Integer, Integer)
spec4 = constrained $ \x ->
  match x $ \a b c d -> And [Assert $ a <=. b, Assert $ b <=. c, Assert $ c <=. d]

{-
class TypeList ts where
  uncurryList :: FunTy (MapList f ts) r -> List f ts -> r
  uncurryList_ :: (forall a. f a -> a)
                  -> FunTy ts r -> List f ts -> r
  curryList :: (List f ts -> r) -> FunTy (MapList f ts) r
  curryList_ :: (forall a. a -> f a)
                -> (List f ts -> r) -> FunTy ts r
  unfoldList :: (forall a (as :: [*]). List f as -> f a) -> List f ts
-}

args1 :: List Term '[Int, Bool, String]
args1 = Lit 5 :> Lit True :> Lit "abc" :> Nil

getTermsize :: Term Int -> Term Bool -> Term String -> Maybe Int
getTermsize (Lit n) (Lit b) (Lit s) = Just $ if b then n else length s
getTermsize _ _ _ = Nothing

-- | Fold over a (List Term ts) with 'getTermsize' which consumes a Term component for each element of the list
ex1 :: Maybe Int
ex1 = uncurryList getTermsize args1
  where
    args1 :: List Term '[Int, Bool, String]
    args1 = Lit 5 :> Lit True :> Lit "abc" :> Nil
    getTermsize :: Term Int -> Term Bool -> Term String -> Maybe Int
    getTermsize (Lit n) (Lit b) (Lit s) = Just $ if b then n else length s
    getTermsize _ _ _ = Nothing

-- Fold over a list with two parts 'unLit' and 'getSize'
ex2 :: Int
ex2 = uncurryList_ unLit getsize args2
  where
    unLit :: forall a. Term a -> a
    unLit (Lit n) = n
    getsize :: Int -> Bool -> String -> Int
    getsize n b s = if b then n else length s
    args2 :: List Term '[Int, Bool, String]
    args2 = Lit 5 :> Lit True :> Lit "abc" :> Nil

-- Construct a function from a List and function on that list.
ex3 :: Term a -> Term b -> Term c -> String
ex3 = curryList crush
  where
    crush :: (List Term '[a, b, c] -> String)
    crush (a :> b :> c :> Nil) = show a ++ show b ++ show c

-- Construct a function over from a
ex4 :: Int -> Bool -> String -> Int
ex4 = curryList_ one totalLength
  where
    totalLength :: List [] '[Int, Bool, String] -> Int
    totalLength (n :> b :> s :> Nil) = length n + length b + length s
    one :: a -> [a]
    one x = [x]

ex5 :: Spec (Set Integer)
ex5 = constrained $ \s -> Assert (size_ s ==. Lit 104)
