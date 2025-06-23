{-# LANGUAGE PatternSynonyms #-}

module Constrained.API (
  -- * Types
  Specification,
  Pred,
  Term,
  NonEmpty ((:|)),

  -- * Type classes and constraints
  HasSpec (..),
  HasSimpleRep (..),
  Foldy (..),
  OrdLike (..),
  Forallable (..),
  HasGenHint (..),
  Sized (..),
  NumLike (..),
  GenericallyInstantiated,
  IsPred,
  Logic,
  Semantics,
  Syntax,
  Numeric,
  IsNormalType,

  -- * Core syntax
  constrained,
  constrained',
  match,
  letBind,
  assert,
  assertExplain,
  assertReified,
  forAll,
  forAll',
  exists,
  unsafeExists,
  whenTrue,
  ifElse,
  dependsOn,
  reify,
  reify',
  reifies,
  explanation,
  monitor,
  genHint,
  caseOn,
  branch,
  branchW,
  onCon,
  isCon,
  onJust,
  isJust,
  lit,
  con,
  sel,
  var,
  name,

  -- * Function symbols

  -- ** Numbers
  (<.),
  (<=.),
  (>=.),
  (>.),
  (==.),
  (/=.),
  (+.),
  (-.),
  negate_,

  -- ** Booleans
  not_,
  (||.),

  -- ** Pairs
  pair_,
  fst_,
  snd_,

  -- ** Either
  left_,
  right_,

  -- ** Maybe
  just_,
  nothing_,

  -- ** Higher-order functions
  id_,
  flip_,
  compose_,

  -- ** List
  foldMap_,
  sum_,
  elem_,
  singletonList_,
  append_,
  (++.),
  sizeOf_,
  null_,
  length_,

  -- ** Set
  singleton_,
  member_,
  union_,
  subset_,
  disjoint_,
  fromList_,

  -- ** Map
  dom_,
  rng_,
  lookup_,
  mapMember_,
  rootLabel_,

  -- ** Generics
  fromGeneric_,
  toGeneric_,

  -- * Composing specifications
  satisfies,
  chooseSpec,
  trueSpec,
  equalSpec,
  notEqualSpec,
  notMemberSpec,
  hasSize,
  explainSpec,
  rangeSize,
  between,
  typeSpec,
  defaultMapSpec,

  -- * Generation, Shrinking, and Testing

  -- ** Generating
  genFromSpec,
  genFromSpecT,
  genFromSpecWithSeed,
  genFromSizeSpec,

  -- ** Shrinking
  shrinkWithSpec,

  -- ** Debugging
  debugSpec,
  printPlan,

  -- ** Testing
  conformsToSpec,
  conformsToSpecE,
  conformsToSpecProp,

  -- ** Building properties
  monitorSpec,
  forAllSpec,
  forAllSpecShow,
  forAllSpecDiscard,
) where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.FunctionSymbol
import Constrained.Generation
import Constrained.Generic
import Constrained.NumOrd
import Constrained.Properties
import Constrained.Spec.Map
import Constrained.Spec.Set
import Constrained.Spec.SumProd
import Constrained.Spec.Tree
import Constrained.Syntax
import Constrained.TheKnot

infix 4 /=.

(/=.) :: HasSpec a => Term a -> Term a -> Term Bool
a /=. b = not_ (a ==. b)

length_ :: HasSpec a => Term [a] -> Term Integer
length_ = sizeOf_

infixr 2 ||.

(||.) ::
  Term Bool ->
  Term Bool ->
  Term Bool
(||.) = or_

infixr 5 ++.

(++.) :: HasSpec a => Term [a] -> Term [a] -> Term [a]
(++.) = append_

null_ :: (HasSpec a, Sized a) => Term a -> Term Bool
null_ xs = sizeOf_ xs ==. 0

trueSpec :: Specification a
trueSpec = TrueSpec
