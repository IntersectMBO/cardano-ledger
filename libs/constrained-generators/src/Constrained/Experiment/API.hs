{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Experiment.API (
  FunSym (..),
  Witness (..),
  BaseW (ToGenericW, FromGenericW),
  BoolW (NotW, OrW, EqualW),
  ProdW (FstW, SndW, PairW),
  SumW (InjLeftW, InjRightW),
  NumOrdW (LessOrEqualW, LessW),
  IntW (AddW, NegateW),
  SizeW (SizeOfW),
  FunW (IdW, ComposeW, FlipW),
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
  or_,
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
  IsNormalType,
  sumleft_,
  sumright_,
  left_,
  right_,
  cJust_,
  cNothing_,
  caseOn,
  branch,
  branchW,
  forAll',
  constrained',
  reify',
  con,
  onCon,
  isCon,
  sel,
  match,
  onJust,
  isJust,
  chooseSpec,
  equalSpec,
  notEqualSpec,
  notMemberSpec,
  id_,
  flip_,
  compose_,
  foldMap_,
  sum_,
  elem_,
  singleton_,
  append_,
  sizeOf,
  sizeOf_,
  genFromSizeSpec,
  between,
  maxSpec,
)
where

import Constrained.Experiment.Base (
  BaseW (..),
  FunSym (..),
  HasSpec (..),
  Pred (..),
  Specification (..),
  Term (..),
  Witness (..),
  constrained,
  equalSpec,
  fromGeneric_,
  notEqualSpec,
  notMemberSpec,
  toGeneric_,
  (==.),
 )
import Constrained.Experiment.Conformance (
  BoolW (..),
  conformsToSpec,
  conformsToSpecE,
  not_,
  or_,
  satisfies,
 )
import Constrained.Experiment.Generic (HasSimpleRep (..))
import Constrained.Experiment.NumSpec (
  IntW (..),
  NumLike,
  NumOrdW (..),
  Numeric,
  addFn,
  cardinality,
  negateFn,
 )

-- instances only
import Constrained.Experiment.Specs.Generics (
  IsNormalType,
  SumW (..),
  branch,
  branchW,
  cJust_,
  cNothing_,
  caseOn,
  chooseSpec,
  con,
  constrained',
  forAll',
  isCon,
  isJust,
  left_,
  match,
  onCon,
  onJust,
  reify',
  right_,
  sel,
  sumleft_,
  sumright_,
 )
import Constrained.Experiment.Specs.Pairs (ProdW (..), fst_, pair_, snd_)
import Constrained.Experiment.TheKnot (
  debugSpec,
  genFromSpec,
  genFromSpecT,
  ifElse,
  simplifySpec,
  simplifyTerm,
  whenTrue,
  (<.),
  (<=.),
 )

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

import Constrained.Experiment.Specs.ListFoldy (
  FunW (..),
  append_,
  compose_,
  elem_,
  flip_,
  foldMap_,
  id_,
  singleton_,
  sum_,
 )

import Constrained.Experiment.Specs.Size (
  SizeW (..),
  Sized (sizeOf),
  between,
  genFromSizeSpec,
  maxSpec,
  sizeOf_,
 )

-- =================================================

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
