{-# LANGUAGE PatternSynonyms #-}

-- | This is the main user-facing API of the library for when you just want to
-- write constraints and simple `HasSpec` instances.
module Constrained.API (
  -- * Types
  Specification,
  Pred,
  Term,

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

  -- ** Types
  GE (..),
  GenT,

  -- ** Generating
  genFromSpec,
  genFromSpecT,
  genFromSpecWithSeed,
  genFromSizeSpec,
  looseGen,
  strictGen,

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

  -- ** Building generators
  pureGen,
  listOfT,
  oneofT,
  frequencyT,
  vectorOfT,

  -- * Utilities
  unionWithMaybe,

  -- * Re-exports
  NonEmpty ((:|)),
) where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.FunctionSymbol
import Constrained.GenT
import Constrained.Generation
import Constrained.Generic
import Constrained.NumOrd
import Constrained.Properties
import Constrained.Spec.List
import Constrained.Spec.Map
import Constrained.Spec.Set
import Constrained.Spec.SumProd
import Constrained.Spec.Tree
import Constrained.Syntax
import Constrained.TheKnot

infix 4 /=.

-- | Inequality as a constraint
(/=.) :: HasSpec a => Term a -> Term a -> Term Bool
a /=. b = not_ (a ==. b)

-- | Specialized `sizeOf_`
length_ :: HasSpec a => Term [a] -> Term Integer
length_ = sizeOf_

infixr 2 ||.

-- | Another name for `or_`
(||.) ::
  Term Bool ->
  Term Bool ->
  Term Bool
(||.) = or_

infixr 5 ++.

-- | Another name for `append_`
(++.) :: HasSpec a => Term [a] -> Term [a] -> Term [a]
(++.) = append_

-- | Like `null` on `Term`
null_ :: (HasSpec a, Sized a) => Term a -> Term Bool
null_ xs = sizeOf_ xs ==. 0

-- | `mempty` for `Specification` without the extra constraints
trueSpec :: Specification a
trueSpec = TrueSpec
