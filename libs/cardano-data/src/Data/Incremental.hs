{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Introduce the Incremental Lambda Calculus embodied in the ILC class.
--   Instances for two patterns of use involving Maps.
module Data.Incremental where

import Control.DeepSeq (NFData (..))
import Data.Kind
import Data.Map.Internal (Map (..))
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic (..))

-- ===================================================
-- Incremental lambda calculus

class ILC t where
  data Diff t :: Type
  applyDiff :: t -> Diff t -> t
  extend :: Diff t -> Diff t -> Diff t
  zero :: Diff t

infixr 0 $$
($$) :: ILC t => t -> Diff t -> t
x $$ y = applyDiff x y

-- | Every (Diff t) is a Semigroup
instance ILC t => Semigroup (Diff t) where
  x <> y = extend x y

-- | Every (Diff t) is a Monoid
instance ILC t => Monoid (Diff t) where
  mempty = zero

-- ==============================================================
-- Delta types.
-- We are going to give the type (Map dom rng) an ILC instance.
-- It turns out there are two reasonable choices for Map. The two
-- reasonable choices differ on what properties the range of the Map
-- has. If the range of the Map is a monoid, there are 3 ways the map
-- might change.
-- 1) entry is deleted,
-- 2) an entry is changed or created, so there is a new range value
-- 3) the range of an entry is combined (using monoid (actually semigroup) <>) with another value.
--
-- If the range is not a Monoid there are only two ways the map might change
-- 1) entry is deleted,
-- 2) an entry is changed or created, so there is a new range value
--
-- To do this we introduce two datatypes MonoidRngD and BinaryRngD. They
-- will become part of the definition for the Diff(Map dom rng). It also
-- turns out thet Both of them are Semigroups (but not Monoids as neither
-- has a notion of No-Change. This is deliberate, but might be reconsidered
-- at some point)

-- | The range is deleted, overwritten, or combined using a Monoid
data MonoidRngD v = Del | Write !v | Comb !v
  deriving (Show, Eq, Generic, NFData)

instance (Semigroup t) => Semigroup (MonoidRngD t) where
  Del <> Del = Del
  Del <> Write _ = Del
  Del <> Comb _ = Del
  Comb x <> Del = Write x
  Comb x <> Write y = Write (x <> y)
  Comb x <> Comb y = Comb (x <> y)
  Write x <> Del = Write x
  Write x <> Comb _ = Write x
  Write x <> Write _ = Write x

-- | The range is deleted or changed
data BinaryRngD v = Omit | Edit !v
  deriving (Eq, Generic, NFData)

-- The show instance is manual because it supports cutting and pasting
-- error messages, to get values for exploring failures. With out the
-- parantheses they often won't read properly.
instance Show v => Show (BinaryRngD v) where
  show Omit = "Omit"
  show (Edit d) = "Edit(" ++ show d ++ ")"

instance Semigroup (BinaryRngD t) where
  Omit <> Omit = Omit
  Omit <> Edit _ = Omit
  Edit x <> Omit = Edit x
  Edit x <> Edit _ = Edit x

-- ============================================================
-- Since there are two reasonable ILC instances for the Map
-- type we wrap the map in a newtype for the first instance.
-- This is the special case of a Map where the range is a
-- Monoid. We provide tools to enforce the invariant, that in a
-- MonoidMap, we never store 'mempty' of the Monoid.

newtype MonoidMap k v = MM (Map k v)
  deriving newtype (Show, Eq, NFData)

unMM :: MonoidMap k v -> Map k v
unMM (MM x) = x

monoidInsertWith :: (Monoid v, Eq v, Ord k) => k -> v -> MonoidMap k v -> MonoidMap k v
monoidInsertWith k !v1 (MM m) = MM (alter ok k m)
  where
    ok Nothing = if v1 == mempty then Nothing else Just v1
    ok (Just v2) = if total == mempty then Nothing else Just total
      where
        total = v1 <> v2
{-# INLINEABLE monoidInsertWith #-}

monoidInsert :: (Monoid v, Eq v, Ord k) => k -> v -> MonoidMap k v -> MonoidMap k v
monoidInsert k !v1 (MM m) = if v1 == mempty then MM (delete k m) else MM (insert k v1 m)
{-# INLINEABLE monoidInsert #-}

-- =========================================
-- ILC instances

-- | Monoidal maps have special properties, so they get their
--   own instance (wrapped in the newtype).
instance (Ord k, Eq v, ILC v, Monoid v) => ILC (MonoidMap k v) where
  newtype Diff (MonoidMap k v) = Dm (Map k (MonoidRngD (Diff v)))
  applyDiff mm (Dm md) = Map.foldlWithKey' accum mm md
    where
      accum :: MonoidMap k v -> k -> MonoidRngD (Diff v) -> MonoidMap k v
      accum (MM ans) cred Del = MM (Map.delete cred ans)
      accum ans cred (Comb dv) =
        monoidInsertWith cred (applyDiff mempty dv) ans
      accum ans cred (Write dv) = monoidInsert cred (applyDiff mempty dv) ans
  {-# INLINEABLE applyDiff #-}
  zero = Dm Map.empty
  extend (Dm x) (Dm y) = Dm (Map.unionWith (<>) x y)

instance (Show (Diff v), Show k) => Show (Diff (MonoidMap k v)) where
  show (Dm x) = show (Map.toList x)

deriving newtype instance (NFData k, NFData (Diff v)) => NFData (Diff (MonoidMap k v))

-- | Normal map can only be deleted or updated so they use BinaryRngD
instance Ord k => ILC (Map k v) where
  newtype Diff (Map k v) = Dn (Map k (BinaryRngD v))
  applyDiff m (Dn md) = Map.foldlWithKey' accum m md
    where
      accum ans k Omit = Map.delete k ans
      accum ans k (Edit drep) = Map.insert k drep ans
  {-# INLINEABLE applyDiff #-}
  zero = Dn Map.empty
  extend (Dn x) (Dn y) = Dn (Map.unionWith (<>) x y)

instance (Show k, Show v) => Show (Diff (Map k v)) where
  show (Dn x) = show (Map.toList x)

deriving newtype instance (NFData k, NFData v) => NFData (Diff (Map k v))

-- =================================================================
-- helper functions for making binary derivatives

-- | insert a change (MonoidRngD c) into a Map.
--   Note that if we wrap the (result :: Map k (MonoidRngD c)) with the constructor 'Dn'
--   Dn :: Map k (BinaryRngD v) -> Diff (Map k v)
--   then we get Diff(Map k v)
insertC ::
  (Ord k, Monoid c) =>
  k ->
  MonoidRngD c ->
  Map k (MonoidRngD c) ->
  Map k (MonoidRngD c)
insertC d m x = insertWith (<>) d m x

-- | Split two maps, x and y, into three parts
--   1) the key appears only in x
--   2) the key appears in both x and y
--   3) the key appears only in y
--   Given three 'C'ontinuation style functions, reduce
--   the three parts to a single value.
inter3C ::
  Ord k =>
  a ->
  Map k u ->
  Map k v ->
  (a -> k -> u -> a) ->
  (a -> k -> (u, v) -> a) ->
  (a -> k -> v -> a) ->
  a
inter3C ans0 m0 n0 c1 c2 c3 = go ans0 m0 n0
  where
    go ans Tip Tip = ans
    go !ans m Tip = Map.foldlWithKey' c1 ans m
    go !ans Tip n = Map.foldlWithKey' c3 ans n
    go !ans (Bin _ kx x l r) n = case Map.splitLookup kx n of
      (ln, Nothing, rn) -> go (go (c1 ans kx x) l ln) r rn
      (ln, Just y, rn) -> go (go (c2 ans kx (x, y)) l ln) r rn
