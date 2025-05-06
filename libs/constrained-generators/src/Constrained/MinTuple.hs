{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.MinTuple where

import Constrained.Env
import Constrained.MinBase
import Constrained.MinModel
import Constrained.MinSyntax

import Constrained.Core (Evidence (..), Value (..), Var (..), eqVar)
import Constrained.GenT
import Constrained.List hiding (ListCtx)
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

instance (HasSpec a, HasSpec b, HasSpec c) => HasSpec (a, b, c) where
  type TypeSpec (a, b, c) = (TypeSpec a, TypeSpec (b, c))

  emptySpec = (emptySpec @a, emptySpec @(b, c))

  combineSpec (a, b) (a', b') =
    handleErrors
      (combineSpec @a a a')
      (combineSpec @(b, c) b b')
      (\specA specBC -> constrained $ \x -> And [satisfies (head_ x) specA, satisfies (tail_ x) specBC])

  conformsTo (a, b, c) (sa, sbc) = conformsTo a sa && conformsTo (b, c) sbc

  guardTypeSpec (a, bc) =
    handleErrors
      (guardTypeSpec a)
      (guardTypeSpec bc)
      (\specA specBC -> constrained $ \x -> And [satisfies (head_ x) specA, satisfies (tail_ x) specBC])

  genFromTypeSpec (a, bc) = f <$> genFromTypeSpec a <*> genFromTypeSpec bc
    where
      f a (b, c) = (a, b, c)

  toPreds x (a, bc) = toPreds (head_ x) a <> toPreds (tail_ x) bc

-- =======================================================

instance (HasSpec a, HasSpec b, HasSpec c, HasSpec d) => HasSpec (a, b, c, d) where
  type TypeSpec (a, b, c, d) = (TypeSpec (a, b), TypeSpec (c, d))

  emptySpec = (emptySpec @(a, b), emptySpec @(c, d))

  combineSpec (a, b) (a', b') =
    handleErrors
      (combineSpec @(a, b) a a')
      (combineSpec @(c, d) b b')
      ( \specAB specCD -> constrained $ \x -> And [satisfies (left4_ x) specAB, satisfies (right4_ x) specCD]
      )

  conformsTo (a, b, c, d) (sAB, sCD) = conformsTo (a, b) sAB && conformsTo (c, d) sCD

  guardTypeSpec (ab, cd) =
    handleErrors
      (guardTypeSpec ab)
      (guardTypeSpec cd)
      ( \specAB specCD -> constrained $ \x -> And [satisfies (left4_ x) specAB, satisfies (right4_ x) specCD]
      )

  genFromTypeSpec (ab, cd) = f <$> genFromTypeSpec ab <*> genFromTypeSpec cd
    where
      f (a, b) (c, d) = (a, b, c, d)

  toPreds x (ab, cd) = toPreds (left4_ x) ab <> toPreds (right4_ x) cd

-- ============================================================

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
  propagate Left4W (Unary HOLE) specAB =
    constrained $ \abcd -> match abcd $ \a b c d -> And [satisfies (pair_ a b) specAB]
  propagate Right4W (Unary HOLE) specCD =
    constrained $ \abcd -> match abcd $ \a b c d -> And [satisfies (pair_ c d) specCD]
  propagate HeadW (Unary HOLE) specA =
    constrained $ \abc -> match abc $ \a b c -> And [satisfies a specA]
  propagate TailW (Unary HOLE) specBC =
    constrained $ \abc -> match abc $ \a b c -> And [satisfies (pair_ b c) specBC]

-- ==========================================================

head_ :: All HasSpec '[a, b, c] => Term (a, b, c) -> Term a
head_ x = App HeadW (x :> Nil)

tail_ :: All HasSpec '[a, b, c] => Term (a, b, c) -> Term (b, c)
tail_ x = App TailW (x :> Nil)

left4_ :: All HasSpec '[a, b, c, d] => Term (a, b, c, d) -> Term (a, b)
left4_ x = App Left4W (x :> Nil)

right4_ :: All HasSpec '[a, b, c, d] => Term (a, b, c, d) -> Term (c, d)
right4_ x = App Right4W (x :> Nil)

class Match t (ts :: [Type]) | t -> ts where
  match :: All HasSpec ts => Term t -> FunTy (MapList Term ts) Pred -> Pred

instance Match (a, b) '[a, b] where
  match ab f = Let ab (bind $ \x -> f (fst_ x) (snd_ x))

instance Match (a, b, c) '[a, b, c] where
  match abc f = Let abc (bind $ \x -> (f (head_ x) (fst_ (tail_ x)) (snd_ (tail_ x))))

instance Match (a, b, c, d) '[a, b, c, d] where
  match abcd f =
    Let
      abcd
      (bind $ \x -> (f (fst_ (left4_ x)) (snd_ (left4_ x)) (fst_ (right4_ x)) (snd_ (right4_ x))))

spec4 :: Spec (Integer, Integer, Integer, Integer)
spec4 = constrained $ \x ->
  match x $ \a b c d -> And [Assert $ a <=. b, Assert $ b <=. c, Assert $ c <=. d]

spec2T :: Spec (Integer, Integer)
spec2T = constrained $ \x ->
  match x $ \a b -> Assert $ a <=. b
