{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A SplitMap is a nested map, when the key can be broken into two parts: 1) an Int, and 2) a Key.
--   Cutom designed for maps whose Domain is a TxIn. Should take up significantly less space than Data.Map
module Data.Compact.SplitMap where

import Data.Compact.KeyMap (Key (..), KeyMap, PDoc, ppKeyMap, ppList, ppSexp)
import qualified Data.Compact.KeyMap as KeyMap
import Data.Foldable (foldl')
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Map.Internal (Map (..), link, link2)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.Internal as IT
import Data.Text (pack)
import Prettyprinter
import Prelude hiding (lookup)

class Split k where
  splitKey :: k -> (Int, Key)
  joinKey :: Int -> Key -> k

instance Split (Int, Key) where
  splitKey = id
  joinKey = (,)

data SplitMap k v where
  SplitMap :: Split k => IntMap (KeyMap v) -> SplitMap k v

-- | There is an invariant that every Int in the IntMap has a non-null KeyMap
--   this should hold for every 'k' and 'SplitMap k v'
invariant :: k -> SplitMap k v -> Bool
invariant k (SplitMap imap) =
  let (n, _key) = splitKey k
   in case IntMap.lookup n imap of
        Just kmap -> KeyMap.isNotEmpty kmap
        Nothing -> True

-- | To maintain the invariant, we define 'insertNormForm'
--   Insert a KeyMap into an IntMap, unless it is empty, if so, return the IntMap unchanged
--   we assume the Int 'n' is not already in the IntMap 'imap', and each call site should check this invariant.
insertNormForm :: Split k => IntMap.Key -> KeyMap.KeyMap v -> IntMap.IntMap (KeyMap.KeyMap v) -> SplitMap k v
insertNormForm _ KeyMap.Empty imap = SplitMap imap
insertNormForm n kmap imap = SplitMap (IntMap.insert n kmap imap)

empty :: forall k v. Split k => SplitMap k v
empty = SplitMap IntMap.empty

null :: SplitMap k v -> Bool
null (SplitMap imap) = IntMap.null imap

-- ================================================
-- Insert and delete operations

insertWithKey :: forall k v. (k -> v -> v -> v) -> k -> v -> SplitMap k v -> SplitMap k v
{- Maybe we should benchmark these two
insertWithKey combine k v (SplitMap imap) =
  SplitMap (IntMap.insertWith combine2 n (KeyMap.insert key v KeyMap.Empty) imap)
 where
    (n, key) = splitKey k
    combine2 :: KeyMap v -> KeyMap v -> KeyMap v
    combine2 km1 km2 = KeyMap.unionWith (combine k) km1 km2
-}
insertWithKey combine k v (SplitMap imap) =
  SplitMap (IntMap.insertWith combine2 n (KeyMap.insert key v KeyMap.Empty) imap)
  where
    (n, key) = splitKey k
    combine2 :: KeyMap v -> KeyMap v -> KeyMap v
    combine2 _km1 km2 = KeyMap.insertWith @v (combine k) key v km2

insertWith :: forall k v. (v -> v -> v) -> k -> v -> SplitMap k v -> SplitMap k v
insertWith comb k v mp = insertWithKey (\_ x y -> comb x y) k v mp

insert :: forall k v. k -> v -> SplitMap k v -> SplitMap k v
insert k v mp = insertWithKey (\_k v1 _v2 -> v1) k v mp

delete :: k -> SplitMap k v -> SplitMap k v
delete k (SplitMap imap) = SplitMap (IntMap.update fix n imap)
  where
    (n, key) = splitKey k
    fix keymap = case KeyMap.delete key keymap of
      KeyMap.Empty -> Nothing
      !other -> Just other

-- ==================================================================
-- lookup and map and Functor instance

lookup :: k -> SplitMap k v -> Maybe v
lookup k (SplitMap imap) =
  case IntMap.lookup n imap of
    Nothing -> Nothing
    Just keymap -> KeyMap.lookup key keymap
  where
    (n, key) = splitKey k

member :: k -> SplitMap k v -> Bool
member k smap = case lookup k smap of
  Nothing -> False
  Just _ -> True

mapWithKey :: forall k v u. (k -> v -> u) -> SplitMap k v -> SplitMap k u
mapWithKey f (SplitMap imap) = SplitMap (IntMap.mapWithKey g imap)
  where
    g :: Int -> KeyMap v -> KeyMap u
    g n kmap = KeyMap.mapWithKey (\key v -> f (joinKey n key) v) kmap

instance Functor (SplitMap k) where
  fmap f x = mapWithKey (\_ v -> f v) x

filterWithKey :: (k -> v -> Bool) -> SplitMap k v -> SplitMap k v
filterWithKey p smap@(SplitMap _) = foldlWithKey' accum empty smap
  where
    accum ans k v = if p k v then insert k v ans else ans

-- =====================================================================
-- Union operations

unionWithKey :: forall k v. (k -> v -> v -> v) -> SplitMap k v -> SplitMap k v -> SplitMap k v
unionWithKey combine (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.unionWithKey comb imap1 imap2)
  where
    comb :: Int -> KeyMap v -> KeyMap v -> KeyMap v
    comb n x y = KeyMap.unionWithKey (\key v1 v2 -> combine (joinKey n key) v1 v2) x y

unionWith :: forall k v. (v -> v -> v) -> SplitMap k v -> SplitMap k v -> SplitMap k v
unionWith combine (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.unionWith comb imap1 imap2)
  where
    comb :: KeyMap v -> KeyMap v -> KeyMap v
    comb x y = KeyMap.unionWith combine x y

union :: forall k v. SplitMap k v -> SplitMap k v -> SplitMap k v
union (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.unionWith comb imap1 imap2)
  where
    comb :: KeyMap v -> KeyMap v -> KeyMap v
    comb x y = KeyMap.unionWith (\v _ -> v) x y

-- ============================================================================
-- Intersection operations

intersectionWithKey :: forall k u v w. (k -> u -> v -> w) -> SplitMap k u -> SplitMap k v -> SplitMap k w
intersectionWithKey combine (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.intersectionWithKey comb imap1 imap2)
  where
    comb :: Int -> KeyMap u -> KeyMap v -> KeyMap w
    comb n x y = KeyMap.intersectionWithKey (\key v1 v2 -> combine (joinKey n key) v1 v2) x y

-- | The intersection of 'x' and 'y', where the 'combine' function computes the range.
intersectionWith :: forall k u v w. (u -> v -> w) -> SplitMap k u -> SplitMap k v -> SplitMap k w
intersectionWith combine x y = intersectionWithKey (\_k u v -> combine u v) x y

-- | The subset of 'x' with where the domain of 'x' appears in the domain of 'y'
intersection :: forall k u v. SplitMap k u -> SplitMap k v -> SplitMap k u
intersection x y = intersectionWithKey (\_ u _ -> u) x y

-- | Like intersectionWithKey, except if the 'combine' function returns Nothing, the common
--   key is NOT placed in the intersectionWhen result.
intersectionWhen :: forall k u v w. (k -> u -> v -> Maybe w) -> SplitMap k u -> SplitMap k v -> SplitMap k w
intersectionWhen combine (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.intersectionWithKey comb imap1 imap2)
  where
    comb :: Int -> KeyMap u -> KeyMap v -> KeyMap w
    comb n x y = KeyMap.intersectionWhen (\key v1 v2 -> combine (joinKey n key) v1 v2) x y

foldOverIntersection :: forall ans k u v. (ans -> k -> u -> v -> ans) -> ans -> SplitMap k u -> SplitMap k v -> ans
foldOverIntersection accum ans0 (SplitMap imap1) (SplitMap imap2) = foldIntersectIntMap accum2 ans0 imap1 imap2
  where
    accum2 :: Int -> KeyMap u -> KeyMap v -> ans -> ans
    accum2 n km1 km2 ans1 = KeyMap.foldOverIntersection (accum3 n) ans1 km1 km2
    accum3 :: Int -> ans -> Key -> u -> v -> ans
    accum3 n ans2 key u v = accum ans2 (joinKey n key) u v

foldIntersectIntMap :: (Int -> a -> b -> ans -> ans) -> ans -> IntMap a -> IntMap b -> ans
foldIntersectIntMap accum ans0 imap1 imap2 =
  case (IntMap.minViewWithKey imap1, IntMap.minViewWithKey imap2) of
    (Nothing, _) -> ans0
    (_, Nothing) -> ans0
    (Just ((k1, v1), jmap1), Just ((k2, v2), jmap2)) ->
      case compare k1 k2 of
        EQ -> foldIntersectIntMap accum (accum k1 v1 v2 ans0) jmap1 jmap2
        LT -> case IntMap.splitLookup k2 imap1 of
          (_, Nothing, ys) -> foldIntersectIntMap accum ans0 ys jmap2
          (_, Just v3, ys) -> foldIntersectIntMap accum (accum k2 v3 v2 ans0) ys jmap2
        GT -> case IntMap.splitLookup k1 imap2 of
          (_, Nothing, ys) -> foldIntersectIntMap accum ans0 jmap1 ys
          (_, Just v3, ys) -> foldIntersectIntMap accum (accum k1 v1 v3 ans0) jmap1 ys

-- | The intersection over the common domain 'k' of a Data.Map and a SplitMap
--   Only those keys 'k' that meet the predicate 'p' are included.
--   The result isa Data.Map
intersectMapSplit :: (k -> v -> u -> Bool) -> Map.Map k v -> SplitMap k u -> Map.Map k v
intersectMapSplit _ Tip _ = Tip
intersectMapSplit p (Bin _ k v l r) sp =
  case splitLookup k sp of
    (sl, Nothing, sr) -> link2 (intersectMapSplit p l sl) (intersectMapSplit p r sr)
    (sl, Just u, sr) ->
      if p k v u
        then link k v (intersectMapSplit p l sl) (intersectMapSplit p r sr)
        else link2 (intersectMapSplit p l sl) (intersectMapSplit p r sr)

-- | The intersection over the common domain 'k' of a Data.Set and a SplitMap
--   Only those keys 'k' that meet the predicate 'p' are included.
--   The result is a Data.Set
intersectSetSplit :: (k -> u -> Bool) -> Set k -> SplitMap k u -> Set k
intersectSetSplit _ IT.Tip _ = IT.Tip
intersectSetSplit p (IT.Bin _ k l r) sp =
  case splitLookup k sp of
    (sl, Nothing, sr) -> IT.merge (intersectSetSplit p l sl) (intersectSetSplit p r sr)
    (sl, Just u, sr) ->
      if p k u
        then IT.link k (intersectSetSplit p l sl) (intersectSetSplit p r sr)
        else IT.merge (intersectSetSplit p l sl) (intersectSetSplit p r sr)

-- | The intersection over the common domain 'k' of a SplitMap and a Data.Map.
--   Only those keys 'k' that meet the predicate 'p' are included.
--   The result isa SplitMap
intersectSplitMap :: (k -> v -> u -> Bool) -> SplitMap k v -> Map.Map k u -> SplitMap k v
intersectSplitMap _ (SplitMap _) Tip = empty
intersectSplitMap p sp (Bin _ k u l r) =
  case splitLookup k sp of
    (sl, Nothing, sr) -> union (intersectSplitMap p sl l) (intersectSplitMap p sr r)
    (sl, Just v, sr) ->
      if p k v u
        then insert k v (union (intersectSplitMap p sl l) (intersectSplitMap p sr r))
        else union (intersectSplitMap p sl l) (intersectSplitMap p sr r)

-- ============================================================================
-- Fold operations

-- | Strict fold over the pairs in descending order of keys.
foldlWithKey' :: forall k v ans. (ans -> k -> v -> ans) -> ans -> SplitMap k v -> ans
foldlWithKey' comb ans0 (SplitMap imap) = IntMap.foldlWithKey' comb2 ans0 imap
  where
    comb2 :: ans -> Int -> KeyMap v -> ans
    comb2 ans1 n kmap = KeyMap.foldWithAscKey comb3 ans1 kmap
      where
        comb3 :: ans -> Key -> v -> ans
        comb3 ans2 key v = comb ans2 (joinKey n key) v

-- | Strict fold over the pairs in ascending order of keys.
foldrWithKey' :: forall k v ans. (k -> ans -> v -> ans) -> ans -> SplitMap k v -> ans
foldrWithKey' comb ans0 (SplitMap imap) = IntMap.foldrWithKey' comb2 ans0 imap
  where
    comb2 :: Int -> KeyMap v -> ans -> ans
    comb2 n kmap ans1 = KeyMap.foldWithDescKey comb3 ans1 kmap
      where
        comb3 :: Key -> v -> ans -> ans
        comb3 key v ans2 = comb (joinKey n key) ans2 v

-- =================================================================================
-- These 'restrictKeys' functions assume the structure holding the 'good' keys is small
-- An alternate approach is to use cross-type 'intersection' operations

restrictKeysSet :: forall k a. SplitMap k a -> Set k -> SplitMap k a
restrictKeysSet splitmap@(SplitMap _) kset = Set.foldl' comb (SplitMap IntMap.empty) kset
  where
    comb :: SplitMap k a -> k -> SplitMap k a
    comb smap k = case lookup k splitmap of
      Nothing -> smap
      Just a -> insert k a smap

restrictKeysMap :: forall k a b. SplitMap k a -> Map k b -> SplitMap k a
restrictKeysMap splitmap@(SplitMap _) kmap = Map.foldlWithKey' comb (SplitMap IntMap.empty) kmap
  where
    comb :: SplitMap k a -> k -> b -> SplitMap k a
    comb smap k _ = case lookup k splitmap of
      Nothing -> smap
      Just a -> insert k a smap

restrictKeysSplit :: forall k a b. SplitMap k a -> SplitMap k b -> SplitMap k a
restrictKeysSplit splitmap@(SplitMap _) ksplit = foldlWithKey' comb (SplitMap IntMap.empty) ksplit
  where
    comb :: SplitMap k a -> k -> b -> SplitMap k a
    comb smap k _ = case lookup k splitmap of
      Nothing -> smap
      Just a -> insert k a smap

-- | Restrict the keys using the intersection operation
restrictKeys :: forall k a b. Split k => SplitMap k a -> SplitMap k b -> SplitMap k a
restrictKeys smap1 smap2 = foldOverIntersection comb empty smap1 smap2
  where
    comb ans k a _b = insert k a ans

-- =================================================================================
-- These 'withoutKeys' functions assume the structure holding the 'bad' keys is small
-- An alternate approach is to use cross-type 'intersection' operations

withoutKeysSet :: forall k a. SplitMap k a -> Set k -> SplitMap k a
withoutKeysSet splitmap@(SplitMap _) kset = Set.foldl' comb splitmap kset
  where
    comb :: SplitMap k a -> k -> SplitMap k a
    comb smap k = delete k smap

withoutKeysMap :: forall k a b. SplitMap k a -> Map k b -> SplitMap k a
withoutKeysMap splitmap@(SplitMap _) kset = Map.foldlWithKey' comb splitmap kset
  where
    comb :: SplitMap k a -> k -> b -> SplitMap k a
    comb smap k _ = delete k smap

withoutKeysSplit :: forall k a b. SplitMap k a -> SplitMap k b -> SplitMap k a
withoutKeysSplit splitmap@(SplitMap _) kset = foldlWithKey' comb splitmap kset
  where
    comb :: SplitMap k a -> k -> b -> SplitMap k a
    comb smap k _ = delete k smap

-- | Remove the keys using the intersection operation. if 'smap2' is small use one of the other withoutKeysXX functions.
withoutKeys :: forall k a b. SplitMap k a -> SplitMap k b -> SplitMap k a
withoutKeys smap1 smap2 = foldOverIntersection comb smap1 smap1 smap2
  where
    comb ans k _a _b = delete k ans

-- ==============================================================
-- Min Max operations

lookupMin :: SplitMap k v -> Maybe (k, v)
lookupMin (SplitMap imap) =
  case IntMap.minViewWithKey imap of
    Nothing -> Nothing
    Just ((n, kmap), _) ->
      case KeyMap.lookupMin kmap of
        Nothing -> Nothing
        Just (k, v) -> Just (joinKey n k, v)

lookupMax :: SplitMap k v -> Maybe (k, v)
lookupMax (SplitMap imap) =
  case IntMap.maxViewWithKey imap of
    Nothing -> Nothing
    Just ((n, kmap), _) ->
      case KeyMap.lookupMax kmap of
        Nothing -> Nothing
        Just (k, v) -> Just (joinKey n k, v)

minViewWithKey :: SplitMap k v -> Maybe ((k, v), SplitMap k v)
minViewWithKey (SplitMap imap) =
  case IntMap.minViewWithKey imap of
    Nothing -> Nothing
    Just ((n, kmap), imap2) ->
      case KeyMap.minViewWithKey kmap of
        Nothing -> Nothing
        Just ((k, v), kmap2) -> Just ((joinKey n k, v), insertNormForm n kmap2 imap2)

maxViewWithKey :: SplitMap k v -> Maybe ((k, v), SplitMap k v)
maxViewWithKey (SplitMap imap) =
  case IntMap.maxViewWithKey imap of
    Nothing -> Nothing
    Just ((n, kmap), imap2) ->
      case KeyMap.maxViewWithKey kmap of
        Nothing -> Nothing
        Just ((k, v), kmap2) -> Just ((joinKey n k, v), insertNormForm n kmap2 imap2)

-- ====================================================

-- | Performs a split but also returns whether the pivot key was found in the original map.
splitLookup :: k -> SplitMap k a -> (SplitMap k a, Maybe a, SplitMap k a)
splitLookup k (SplitMap imap) =
  let (n, key) = splitKey k
   in case IntMap.splitLookup n imap of
        (ileft, Nothing, iright) -> (SplitMap ileft, Nothing, SplitMap iright)
        (ileft, Just x, iright) ->
          case KeyMap.splitLookup key x of
            (kleft, Nothing, kright) ->
              (insertNormForm n kleft ileft, Nothing, insertNormForm n kright iright)
            (kleft, Just a, kright) ->
              (insertNormForm n kleft ileft, Just a, insertNormForm n kright iright)

-- ===============================================================
-- Interaction with lists

-- | In case of duplicate keys, the pair closer to the end of the list wins.
fromList :: Split k => [(k, v)] -> SplitMap k v
fromList pairs = foldl' accum empty pairs
  where
    accum mp (k, v) = insert k v mp

-- | Generates the list in ascending order of k
toList :: SplitMap k v -> [(k, v)]
toList mp = foldrWithKey' accum [] mp
  where
    accum k pairs v = (k, v) : pairs

instance (Split k, Eq k, Eq v) => Eq (SplitMap k v) where
  x == y = toList x == toList y

instance (Split k, Show k, Show v) => Show (SplitMap k v) where
  show x = show (ppSplitMap x)

ppSplitMap :: forall k v. Show v => SplitMap k v -> PDoc
ppSplitMap (SplitMap imap) = ppList ppitem (IntMap.toList imap)
  where
    ppitem :: (Int, KeyMap v) -> PDoc
    ppitem (n, kmap) = ppSexp (pack "Split") [viaShow n, ppKeyMap viaShow ppv kmap]
    ppv :: v -> PDoc
    ppv x = viaShow x
