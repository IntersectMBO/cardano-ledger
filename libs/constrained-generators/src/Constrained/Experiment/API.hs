{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Experiment.API (
  FunctionSymbol (..),
  Specification (..),
  Term (..),
  Pred (..),
  HasSpec (..),
  HasSimpleRep (..),
  conformsToSpecE,
  conformsToSpec,
  satisfies,
  genFromSpecT,
  genFromSpec,
  debugSpec,
  simplifySpec,
  cardinality,
  ifElse,
  whenTrue,
  simplifyTerm,
  constrained,
  assertExplain,
  assert,
  forAll,
  exists,
  unsafeExists,
  letBind,
  reify,
  assertReified,
  explanation,
  monitor,
  reifies,
  dependsOn,
  lit,
  genHint,
  (<.),
  (<=.),
  (>=.),
  (>.),
  (==.),
  not_,
  toGeneric_,
  fromGeneric_,
  (+.),
  (-.),
  negate_,
  addFn,
  negateFn,
  Numeric,
  fst_,
  snd_,
  pair_,
)
where

import Constrained.Experiment.Base (
  FunctionSymbol (..),
  HasSpec (..),
  Pred (..),
  Specification (..),
  Term (..),
  constrained,
  fromGeneric_,
  toGeneric_,
 )
import Constrained.Experiment.Conformance (
  conformsToSpec,
  conformsToSpecE,
  satisfies,
 )
import Constrained.Experiment.Generic (HasSimpleRep (..))
import Constrained.Experiment.NumSpec (
  NumLike,
  Numeric,
  addFn,
  cardinality,
  negateFn,
 )
import Constrained.Experiment.Specs.Pairs (fst_, pair_, snd_)
import Constrained.Experiment.TheKnot (
  debugSpec,
  genFromSpec,
  genFromSpecT,
  ifElse,
  not_,
  simplifySpec,
  simplifyTerm,
  whenTrue,
  (<.),
  (<=.),
  (==.),
 )
import Constrained.Experiment.Witness ()

-- ==============================================
-- Language constructs, Haskell functions, for
-- users, who want to build (Term a) and Pred
-- ==============================================

import Constrained.Experiment.Syntax (
  assert,
  assertExplain,
  assertReified,
  dependsOn,
  exists,
  explanation,
  forAll,
  genHint,
  letBind,
  lit,
  monitor,
  reifies,
  reify,
  unsafeExists,
 )

(+.) :: NumLike a => Term a -> Term a -> Term a
(+.) = addFn

negate_ :: NumLike a => Term a -> Term a
negate_ = negateFn

(>=.) :: Numeric n => Term n -> Term n -> Term Bool
(>=.) = flip (<.)

(>.) :: Numeric n => Term n -> Term n -> Term Bool
(>.) = flip (<=.)

(-.) :: Numeric n => Term n -> Term n -> Term n
(-.) x y = addFn x (negateFn y)
