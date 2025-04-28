{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Constrained.FunctionSymbol where

import Constrained.List

import Data.Kind
import Data.Typeable

-- ========================================================================
-- Useful for testing if two function symbols are `the same` for some notion of `the same`

sameFunSym ::
  forall (t1 :: [Type] -> Type -> Type) d1 r1 (t2 :: [Type] -> Type -> Type) d2 r2.
  ( Typeable t1
  , Typeable d1
  , Typeable r1
  , Typeable t2
  , Typeable d2
  , Typeable r2
  , Eq (t1 d1 r1)
  ) =>
  t1 d1 r1 ->
  t2 d2 r2 ->
  Maybe (t1 :~: t2, d1 :~: d2, r1 :~: r2)
sameFunSym x y = do
  Refl <- eqT @t1 @t2
  Refl <- eqT @d1 @d2
  Refl <- eqT @r1 @r2
  if x == y
    then Just (Refl, Refl, Refl)
    else Nothing

-- | Here we only care about the Type 't', the dom, and the rng can be
-- anything. Useful for defining patterns.
getWitness ::
  forall t t' d r.
  ( Typeable t
  , Typeable d
  , Typeable r
  , Typeable t'
  ) =>
  t d r -> Maybe (t' d r)
getWitness = cast

-- | Semantic operations are ones that give the function symbol, meaning as a function.
--   I.e. how to apply the function to a list of arguments and return a value.
class Semantics (t :: [Type] -> Type -> Type) where
  semantics :: t d r -> FunTy d r -- e.g. FunTy '[a, Int] Bool ~ a -> Int -> Bool
