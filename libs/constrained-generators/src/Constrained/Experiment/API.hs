{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Experiment.API (
  FunSym (..),
  Witness (..),
  BaseW (ToGenericW, FromGenericW),
  BoolW (NotW, OrW, EqualW),
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
  singletonList_,
  append_,
  sizeOf,
  sizeOf_,
  genFromSizeSpec,
  between,
  maxSpec,
  SetW (..),
  SetSpec (..),
  singleton_,
  member_,
  union_,
  subset_,
  disjoint_,
  fromList_,
  pattern Equal,
  pattern ToGeneric,
  pattern FromGeneric,
  pattern InjLeft,
  pattern InjRight,
  pattern Fst,
  pattern Snd,
  pattern Pair,  
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
 )
import Constrained.Experiment.Conformance (
  BoolW (..),
  conformsToSpec,
  conformsToSpecE,
  not_,
  or_,
  (==.),
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

-- import Constrained.Experiment.Specs.Pairs (ProdW (..), fst_, pair_, snd_)
import Constrained.Experiment.Specs.Sum (
  IsNormalType,
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
 )
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
  pattern FromGeneric,
  pattern ToGeneric,
  pattern Equal,
  pattern InjLeft,
  pattern InjRight,
  pattern Fst,
  pattern Snd,
  pattern Pair,
  fst_, 
  pair_, 
  snd_,
  sumleft_,
  sumright_,
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
  singletonList_,
  sum_,
 )

import Constrained.Experiment.Specs.Set (
  SetSpec (..),
  SetW (..),
  disjoint_,
  fromList_,
  member_,
  singleton_,
  subset_,
  union_,
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

-- See  https://www.mathsisfun.com/algebra/inequality-solving.html
(>=.) :: Numeric n => Term n -> Term n -> Term Bool
(>=.) = flip (<=.)

(>.) :: Numeric n => Term n -> Term n -> Term Bool
(>.) = flip (<.)

(-.) :: Numeric n => Term n -> Term n -> Term n
(-.) x y = addFn x (negateFn y)
