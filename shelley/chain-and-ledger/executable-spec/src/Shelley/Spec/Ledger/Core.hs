{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.Core
  ( Relation
      ( (⨃),
        (∪),
        dom,
        range,
        (◁),
        (<|),
        (▷),
        (|>),
        singleton,
        (⋪),
        (</|),
        (⋫),
        (|/>),
        Domain,
        Range,
        -- below are methods not used anywhere
        size,
        (<=◁),
        (▷<=),
        (▷>=)
      ),
    (⊆),
    (∪+),
    (∈),
    (∉),
    (∩),
  )
where

import Data.Foldable (elem, toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set, intersection, isSubsetOf)
import qualified Data.Set as Set

---------------------------------------------------------------------------------
-- Domain restriction and exclusion
---------------------------------------------------------------------------------

class Relation m where
  type Domain m :: *
  type Range m :: *

  singleton :: Domain m -> Range m -> m

  -- | Domain
  dom :: Ord (Domain m) => m -> Set (Domain m)

  -- | Range
  range :: Ord (Range m) => m -> Set (Range m)

  -- | Domain restriction
  --
  -- Unicode: 25c1
  (◁), (<|) :: (Ord (Domain m), Foldable f) => f (Domain m) -> m -> m
  s <| r = s ◁ r

  -- | Domain exclusion
  --
  -- Unicode: 22ea
  (⋪), (</|) :: (Ord (Domain m), Foldable f) => f (Domain m) -> m -> m
  s </| r = s ⋪ r

  -- | Range restriction
  --
  -- Unicode: 25b7
  (▷), (|>) :: Ord (Range m) => m -> Set (Range m) -> m
  s |> r = s ▷ r

  -- | Range exclusion
  --
  -- Unicode: 22eb
  (⋫), (|/>) :: Ord (Range m) => m -> Set (Range m) -> m
  s |/> r = s ⋫ r

  -- | Union
  (∪) :: (Ord (Domain m), Ord (Range m)) => m -> m -> m

  -- | Union Override Right
  (⨃) :: (Ord (Domain m), Ord (Range m), Foldable f) => m -> f (Domain m, Range m) -> m

  -- | Restrict domain to values less or equal than the given value.
  --
  -- Unicode: 25c1
  (<=◁) :: Ord (Domain m) => Domain m -> m -> m

  infixl 5 <=◁

  -- | Restrict range to values less or equal than the given value
  --
  -- Unicode: 25b7
  (▷<=) :: (Ord (Range m)) => m -> Range m -> m

  infixl 5 ▷<=

  -- | Restrict range to values greater or equal than the given value
  --
  -- Unicode: 25b7
  (▷>=) :: (Ord (Range m)) => m -> Range m -> m

  infixl 5 ▷>=

  -- | Size of the relation
  size :: Integral n => m -> n

-- | Alias for 'elem'.
--
-- Unicode: 2208
(∈) :: (Eq a, Foldable f) => a -> f a -> Bool
a ∈ f = elem a f

-- | Alias for not 'elem'.
--
-- Unicode: 2209
(∉) :: (Eq a, Foldable f) => a -> f a -> Bool
a ∉ f = not $ elem a f

infixl 4 ∉

instance Relation (Map k v) where
  type Domain (Map k v) = k
  type Range (Map k v) = v

  singleton = Map.singleton

  dom = Map.keysSet
  range = Set.fromList . Map.elems

  s ◁ r = Map.restrictKeys r (toSet s)

  s ⋪ r = Map.filterWithKey (\k _ -> k `Set.notMember` toSet s) r

  r ▷ s = Map.filter (flip Set.member s) r

  r ⋫ s = Map.filter (flip Set.notMember s) r

  d0 ∪ d1 = Map.union d0 d1

  -- For union override we pass @d1@ as first argument, since 'Map.union' is
  -- left biased.
  d0 ⨃ d1 = Map.union (Map.fromList . toList $ d1) d0

  vmax <=◁ r = Map.filterWithKey (\k _ -> k <= vmax) r

  r ▷<= vmax = Map.filter (<= vmax) r

  r ▷>= vmin = Map.filter (>= vmin) r

  size = fromIntegral . Map.size

-- | Union override plus is (A\B)∪(B\A)∪{k|->v1+v2 | k|->v1 : A /\ k|->v2 : B}
(∪+) :: (Ord a, Ord b, Num b) => Map a b -> Map a b -> Map a b
a ∪+ b = ((dom a) ⋪ b) ∪ ((dom b) ⋪ a) ∪ (Map.unionWith (+) a b)

instance Relation (Set (a, b)) where
  type Domain (Set (a, b)) = a
  type Range (Set (a, b)) = b

  singleton a b = Set.singleton (a, b)

  dom = Set.map fst

  range = Set.map snd

  s ◁ r = Set.filter (\(k, _) -> k `Set.member` toSet s) r

  s ⋪ r = Set.filter (\(k, _) -> k `Set.notMember` toSet s) r

  r ▷ s = Set.filter (\(_, v) -> Set.member v s) r

  r ⋫ s = Set.filter (\(_, v) -> Set.notMember v s) r

  (∪) = Set.union

  d0 ⨃ d1 = d1' ∪ ((dom d1') ⋪ d0)
    where
      d1' = toSet d1

  vmax <=◁ r = Set.filter ((<= vmax) . fst) $ r

  r ▷<= vmax = Set.filter ((<= vmax) . snd) $ r

  r ▷>= vmax = Set.filter ((>= vmax) . snd) $ r

  size = fromIntegral . Set.size

-- The [(a,b)] instance is used in `stakeDistr` in the file LedgerState.hs

instance Relation [(a, b)] where
  type Domain [(a, b)] = a
  type Range [(a, b)] = b

  singleton a b = [(a, b)]

  dom = toSet . fmap fst

  range = toSet . fmap snd

  s ◁ r = filter ((`Set.member` toSet s) . fst) r

  s ⋪ r = filter ((`Set.notMember` toSet s) . fst) r

  r ▷ s = filter ((`Set.member` toSet s) . snd) r

  r ⋫ s = filter ((`Set.notMember` toSet s) . snd) r

  (∪) = (++)

  -- In principle a list of pairs allows for duplicated keys.
  d0 ⨃ d1 = d0 ++ toList d1

  vmax <=◁ r = filter ((<= vmax) . fst) r

  r ▷<= vmax = filter ((<= vmax) . snd) r

  r ▷>= vmin = filter ((vmin <=) . snd) r

  size = fromIntegral . length

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
