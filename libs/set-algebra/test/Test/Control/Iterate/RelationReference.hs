{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Control.Iterate.RelationReference (relationTests) where

import qualified Control.Iterate.BaseTypes as SA
import qualified Control.Iterate.Exp as SA
import qualified Control.Iterate.SetAlgebra as SA
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum)
import Data.Set (Set, intersection, isSubsetOf)
import qualified Data.Set as Set
import Test.Control.Iterate.SetAlgebra ()
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, testProperty, (===))

---------------------------------------------------------------------------------
-- Domain restriction and exclusion
---------------------------------------------------------------------------------

class Relation m where
  type Domain m :: Type
  type Range m :: Type

  -- | Domain
  dom :: Ord (Domain m) => m -> Set (Domain m)

  -- | Range
  range :: Ord (Range m) => m -> Set (Range m)

  -- | Domain restriction
  --
  -- Unicode: 25c1
  (◁) :: (Ord (Domain m)) => Set (Domain m) -> m -> m

  -- | Domain exclusion
  --
  -- Unicode: 22ea
  (⋪) :: (Ord (Domain m)) => Set (Domain m) -> m -> m

  -- | Range restriction
  --
  -- Unicode: 25b7
  (▷) :: Ord (Range m) => m -> Set (Range m) -> m

  -- | Range exclusion
  --
  -- Unicode: 22eb
  (⋫) :: Ord (Range m) => m -> Set (Range m) -> m

  -- | Union
  (∪) :: (Ord (Domain m), Ord (Range m)) => m -> m -> m

  -- | Union Override Right
  (⨃) :: (Ord (Domain m), Ord (Range m)) => m -> m -> m

  -- | Is this key in the Domain,  Instances should overide this default with
  -- something more efficient
  haskey :: Ord (Domain m) => Domain m -> m -> Bool
  haskey key m = key `elem` dom m

-- | Alias for 'elem'.
--
-- Unicode: 2208
(∈) :: (Eq a, Foldable f) => a -> f a -> Bool
(∈) = elem

-- | Alias for not 'elem'.
--
-- Unicode: 2209
(∉) :: (Eq a, Foldable f) => a -> f a -> Bool
(∉) = notElem

instance Relation (Map k v) where
  type Domain (Map k v) = k
  type Range (Map k v) = v

  dom = Map.keysSet

  range = Set.fromList . Map.elems

  s ◁ r = Map.restrictKeys r s

  s ⋪ r = Map.withoutKeys r s

  r ▷ s = Map.filter (`Set.member` s) r

  r ⋫ s = Map.filter (`Set.notMember` s) r

  d0 ∪ d1 = Map.union d0 d1

  -- For union override we pass @d1@ as first argument, since 'Map.union' is left biased.
  d0 ⨃ d1 = Map.union d1 d0

  haskey = Map.member

-- | Union override plus is (A\B)∪(B\A)∪{k|->v1+v2 | k|->v1 : A /\ k|->v2 : B}
-- The library function Map.unionWith is more general, it allows any type for
-- `b` as long as (+) :: b -> b -> b
(∪+) :: (Ord a, Num b) => Map a b -> Map a b -> Map a b
(∪+) = Map.unionWith (+)

---------------------------------------------------------------------------------
-- Aliases
---------------------------------------------------------------------------------

-- | Inclusion among foldables.
--
-- Unicode: 2286
(⊆) :: (Foldable f, Foldable g, Ord a) => f a -> g a -> Bool
x ⊆ y = toSet x `isSubsetOf` toSet y

toSet :: (Foldable f, Ord a) => f a -> Set a
toSet = Set.fromList . toList

(∩) :: Ord a => Set a -> Set a -> Set a
(∩) = intersection

propUnary ::
  forall b a e.
  (Eq a, Show a, Arbitrary b, Show b, SA.Embed a e) =>
  TestName ->
  (b -> SA.Exp e) ->
  (b -> a) ->
  TestTree
propUnary name expr relExpr =
  testProperty name (\arg -> SA.eval (expr arg) === relExpr arg)

propBinary ::
  forall b c a e.
  (Eq a, Show a, Arbitrary b, Show b, Arbitrary c, Show c, SA.Embed a e) =>
  TestName ->
  (b -> c -> SA.Exp e) ->
  (b -> c -> a) ->
  TestTree
propBinary name expr relExpr =
  testProperty name (\arg1 arg2 -> SA.eval (expr arg1 arg2) === relExpr arg1 arg2)

type M = Map Int (Sum Float)

relationTests :: TestTree
relationTests =
  testGroup
    "RelationTests - check conformance with the original implementation"
    [ propUnary @M "dom" SA.dom dom,
      propUnary @M "range" SA.rng range,
      propBinary @_ @M "∈" (\k m -> k SA.∈ range m) (∈),
      propBinary @_ @M "∉" (\k m -> k SA.∉ range m) (∉),
      propBinary @_ @M "haskey" (\k m -> k SA.∈ dom m) haskey,
      propBinary @_ @M "◁" (SA.◁) (◁),
      propBinary @_ @M "⋪" (SA.⋪) (⋪),
      propBinary @M "▷" (SA.▷) (▷),
      propBinary @M "⋫" (SA.⋫) (⋫),
      propBinary @M "∪" (SA.∪) (∪),
      propBinary @M "⨃" (SA.⨃) (⨃),
      propBinary @M "∪+" (SA.∪+) (∪+),
      propBinary @M @M "⊆" (\m1 m2 -> SA.rng m1 SA.⊆ SA.rng m2) (⊆),
      propBinary @(Set Int) "∩" (SA.∩) (∩)
    ]
