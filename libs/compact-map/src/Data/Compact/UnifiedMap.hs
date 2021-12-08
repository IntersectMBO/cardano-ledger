{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Compact.UnifiedMap where

{-
import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Data.Coders(decodeRecordNamed,decodeMap,encodeMap)
-}

import Control.DeepSeq (NFData (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), allNoThunks)
import Prelude hiding (lookup)

-- =====================================================

data UnifiedMap coin ptr stakeid poolid where
  UnifiedMap ::
    (Ord ptr, Ord stakeid, Monoid coin) =>
    Map stakeid (Triple coin ptr poolid) ->
    Map ptr stakeid ->
    UnifiedMap coin ptr stakeid poolid

data Triple coin ptr poolid = Triple
  { coinT :: !coin,
    ptrT :: !(Set ptr),
    poolidT :: !(StrictMaybe poolid)
  }
  deriving (Show, Eq, Generic, NoThunks, NFData)

data ViewMap k v where
  Rewards :: UnifiedMap coin ptr stakeid poolid -> ViewMap stakeid coin
  Delegations :: UnifiedMap coin ptr stakeid poolid -> ViewMap stakeid poolid
  Ptrs :: UnifiedMap coin ptr stakeid poolid -> ViewMap ptr stakeid

-- | This is expensive, use it wisely (like maybe once per epoch boundary to make a SnapShot)
unUnify :: ViewMap k v -> Map k v
unUnify (Rewards (UnifiedMap tripmap _)) = Map.map coinT tripmap
unUnify (Delegations (UnifiedMap tripmap _)) = Map.foldlWithKey' accum Map.empty tripmap
  where
    accum ans k (Triple _ _ (SJust v)) = Map.insert k v ans
    accum ans _ _ = ans
unUnify (Ptrs (UnifiedMap _ ptrmap)) = ptrmap

-- =======================================================
-- Instances

instance (Ord ptr, Monoid coin) => Semigroup (Triple coin ptr poolid) where
  (<>) (Triple c1 ptrs1 x) (Triple c2 ptrs2 y) = Triple (c1 <> c2) (Set.union ptrs1 ptrs2) (add x y)
    where
      add SNothing SNothing = SNothing
      add (SJust w) SNothing = SJust w
      add SNothing (SJust z) = SJust z
      add (SJust w) (SJust _) = SJust w

deriving instance (Show coin, Show ptr, Show stakeid, Show poolid) => Show (UnifiedMap coin ptr stakeid poolid)

instance
  (NoThunks coin, NoThunks ptr, NoThunks stakeid, NoThunks poolid) =>
  NoThunks (UnifiedMap coin ptr stakeid poolid)
  where
  showTypeOf _ = "UnifiedMap"
  wNoThunks ctxt (UnifiedMap tripmap ptrmap) =
    allNoThunks
      [ noThunks ctxt tripmap,
        noThunks ctxt ptrmap
      ]

instance NFData (UnifiedMap coin ptr stakeid poolid) where
  rnf _ = undefined

deriving instance (Eq coin, Eq ptr, Eq stakeid, Eq poolid) => Eq (UnifiedMap coin ptr stakeid poolid)

-- ==================================================
-- short hand constructors

rewards ::
  (Ord ptr, Ord stakeid, Monoid coin) =>
  Map stakeid (Triple coin ptr poolid) ->
  Map ptr stakeid ->
  ViewMap stakeid coin
rewards x y = Rewards (UnifiedMap x y)

delegations ::
  (Ord ptr, Ord stakeid, Monoid coin) =>
  Map stakeid (Triple coin ptr poolid) ->
  Map ptr stakeid ->
  ViewMap stakeid poolid
delegations x y = Delegations (UnifiedMap x y)

ptrs ::
  (Ord ptr, Ord stakeid, Monoid coin) =>
  Map stakeid (Triple coin ptr poolid) ->
  Map ptr stakeid ->
  ViewMap ptr stakeid
ptrs x y = Ptrs (UnifiedMap x y)

-- ================================================================
-- Iter Operations

next :: ViewMap k v -> Maybe (k, v, ViewMap k v)
next (Rewards (UnifiedMap tripmap _)) =
  case mapNext tripmap of
    Nothing -> Nothing
    Just (k, Triple coin _ _, tripmap2) -> Just (k, coin, rewards tripmap2 Map.empty)
next (Delegations (UnifiedMap tripmap _)) =
  case mapNext tripmap of
    Nothing -> Nothing
    Just (k, Triple _ _ (SJust poolid), tripmap2) -> Just (k, poolid, delegations tripmap2 Map.empty)
    Just (_, Triple _ _ SNothing, _) -> Nothing
next (Ptrs (UnifiedMap tripmap ptrmap)) =
  case mapNext ptrmap of
    Nothing -> Nothing
    Just (k, stakeid, m2) -> Just (k, stakeid, ptrs (pick tripmap Map.empty) m2)

pick :: x -> x -> x
pick _old new = new

leastUpperBound :: k -> ViewMap k v -> Maybe (k, v, ViewMap k v)
leastUpperBound stakeid (Rewards (UnifiedMap tripmap _)) =
  case mapLub stakeid tripmap of
    Nothing -> Nothing
    Just (k, Triple coin _ _, tripmap2) -> Just (k, coin, rewards tripmap2 Map.empty)
leastUpperBound stakeid (Delegations (UnifiedMap tripmap _)) =
  case mapLub stakeid tripmap of
    Nothing -> Nothing
    Just (k, Triple _ _ (SJust poolid), tripmap2) -> Just (k, poolid, delegations tripmap2 Map.empty)
    Just (_, Triple _ _ SNothing, tripmap2) -> next (Delegations (UnifiedMap tripmap2 Map.empty))
leastUpperBound ptr (Ptrs (UnifiedMap tripmap ptrmap)) =
  case mapLub ptr ptrmap of
    Nothing -> Nothing
    Just (k, stakeid, m2) -> Just (k, stakeid, ptrs (pick tripmap Map.empty) m2)

-- ==============================================================
-- Basic operations on ViewMap

delete :: k -> ViewMap k v -> ViewMap k v
delete stakeid (Rewards (UnifiedMap tripmap ptrmap)) = rewards (Map.update ok stakeid tripmap) ptrmap
  where
    ok (Triple _ ptr poolid) = Just (Triple mempty ptr poolid)
delete stakeid (Delegations (UnifiedMap tripmap ptrmap)) = delegations (Map.update ok stakeid tripmap) ptrmap
  where
    ok (Triple _ ptr _) = Just (Triple mempty ptr SNothing)
delete ptr (Ptrs (UnifiedMap tripmap ptrmap)) =
  case Map.lookup ptr ptrmap of
    Nothing -> Ptrs (UnifiedMap tripmap ptrmap)
    Just stakeid -> ptrs (Map.update ok stakeid tripmap) (Map.delete ptr ptrmap)
      where
        ok (Triple coin ptrset poolid) = Just (Triple coin (Set.delete ptr ptrset) poolid)

-- | insertWith (\ old new -> old) k v xs  Keeps the value already in the ViewMap if the key 'k' is already there
--   insertWith (\ old new -> new) k v xs  Replaces the value already in the ViewMap with 'v', if key 'k' is already there
--   insertWith (\ old new -> old+new) k v xs  Replaces the value already in the ViewMap with the sum, if key 'k' is already there
--   insertWith combine k v xs,  Ignores 'combine' if the key 'k' is NOT already in the ViewMap, and inserts 'v'
insertWith :: (v -> v -> v) -> k -> v -> ViewMap k v -> ViewMap k v
insertWith comb stakeid coin (Rewards (UnifiedMap tripmap ptrmap)) =
  rewards (Map.insertWith comb2 stakeid (Triple coin Set.empty SNothing) tripmap) ptrmap
  where
    comb2 (Triple new _ _) (Triple old x y) = Triple (comb old new) x y
insertWith comb stakeid poolid (Delegations (UnifiedMap tripmap ptrmap)) =
  delegations (Map.insertWith comb2 stakeid (Triple mempty Set.empty (SJust poolid)) tripmap) ptrmap
  where
    comb2 (Triple _ _ (SJust new)) (Triple x y (SJust old)) = Triple x y (SJust (comb old new))
    comb2 (Triple _ _ (SJust new)) (Triple x y SNothing) = Triple x y (SJust new)
    comb2 triple _ = triple
insertWith comb ptr stakeid (Ptrs (UnifiedMap tripmap ptrmap)) =
  ptrs
    (Map.insertWith (<>) stakeid (Triple mempty (Set.singleton ptr) SNothing) tripmap)
    (Map.insertWith (\new old -> comb old new) ptr stakeid ptrmap)

insert :: k -> v -> ViewMap k v -> ViewMap k v
insert stakeid coin (Rewards (UnifiedMap tripmap ptrmap)) =
  rewards (Map.insertWith (<>) stakeid (Triple coin Set.empty SNothing) tripmap) ptrmap
insert stakeid poolid (Delegations (UnifiedMap tripmap ptrmap)) =
  delegations (Map.insertWith (<>) stakeid (Triple mempty Set.empty (SJust poolid)) tripmap) ptrmap
insert ptr stakeid (Ptrs (UnifiedMap tripmap ptrmap)) =
  ptrs
    (Map.insertWith (<>) stakeid (Triple mempty (Set.singleton ptr) SNothing) tripmap)
    (Map.insert ptr stakeid ptrmap)

lookup :: k -> ViewMap k v -> Maybe v
lookup stakeid (Rewards (UnifiedMap tripmap _)) = coinT <$> Map.lookup stakeid tripmap
lookup stakeid (Delegations (UnifiedMap tripmap _)) =
  case Map.lookup stakeid tripmap of
    Nothing -> Nothing
    Just (Triple _ _ SNothing) -> Nothing
    Just (Triple _ _ (SJust x)) -> Just x
lookup ptr (Ptrs (UnifiedMap _ ptrmap)) = Map.lookup ptr ptrmap

member :: k -> ViewMap k v -> Bool
member k x = case lookup k x of Nothing -> False; Just _ -> True

isNull :: ViewMap k v -> Bool
isNull (Rewards (UnifiedMap tripmap _)) = Map.null tripmap
isNull (Delegations (UnifiedMap tripmap _)) = all nothing tripmap
  where
    nothing (Triple _ _ SNothing) = True
    nothing (Triple _ _ (SJust _)) = False
isNull (Ptrs (UnifiedMap _ ptrmap)) = Map.null ptrmap

domain :: ViewMap k v -> Set k
domain (Rewards (UnifiedMap tripmap _)) = Map.keysSet tripmap
domain (Delegations (UnifiedMap tripmap _)) = Map.foldlWithKey' accum Set.empty tripmap
  where
    accum ans k (Triple _ _ (SJust _)) = Set.insert k ans
    accum ans _k (Triple _ _ SNothing) = ans
domain (Ptrs (UnifiedMap _ ptrmap)) = Map.keysSet ptrmap

range :: Ord v => ViewMap k v -> Set v
range (Rewards (UnifiedMap tripmap _)) = Map.foldlWithKey' accum Set.empty tripmap
  where
    accum ans _ (Triple coin _ _) = Set.insert coin ans
range (Delegations (UnifiedMap tripmap _)) = Map.foldlWithKey' accum Set.empty tripmap
  where
    accum ans _ (Triple _ _ (SJust v)) = Set.insert v ans
    accum ans _ (Triple _ _ SNothing) = ans
range (Ptrs (UnifiedMap tripmap _ptrmap)) = Map.keysSet tripmap -- tripmap is the inverse of ptrmap

-- ===============================================================

mapNext :: Map k v -> Maybe (k, v, Map k v)
mapNext m =
  case Map.minViewWithKey m of
    Nothing -> Nothing
    Just ((k, v), m2) -> Just (k, v, m2)

mapLub :: Ord k => k -> Map k v -> Maybe (k, v, Map k v)
mapLub k m =
  case Map.splitLookup k m of
    (_, Nothing, m2) -> mapNext m2
    (_, Just v, m2) -> Just (k, v, m2)

-- ========================================

(∪+),
  unionplus ::
    Semigroup coin =>
    UnifiedMap coin ptr stake pool ->
    Map stake coin ->
    UnifiedMap coin ptr stake pool
unionplus (UnifiedMap tripmap ptrmap) mp = UnifiedMap tripmap2 ptrmap
  where
    tripmap2 = Map.mergeWithKey accum (const Map.empty) (const Map.empty) tripmap mp
    accum _k (Triple c1 x y) c2 = Just (Triple (c1 <> c2) x y)
(∪+) = unionplus
