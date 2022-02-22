{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Compact.SplitMap
  ( SplitMap,
    Split,

    -- * Map operations
    Map.empty,
    Map.singleton,
    Map.null,
    Map.size,
    Map.valid,

    -- * Element operations
    Map.insert,
    Map.delete,
    Map.lookup,
    Map.member,
    Map.notMember,
    Map.mapWithKey,

    -- * Filter
    Map.filter,
    Map.filterWithKey,

    -- * Fold
    Map.foldl',
    Map.foldrWithKey',
    Map.foldlWithKey',

    -- * Intersection
    Map.disjoint,
    Map.intersection,
    Map.intersectionWith,
    Map.intersectionWithKey,

    -- * Union
    Map.union,
    Map.unionWith,
    Map.unionWithKey,
    Map.partition,
    Map.partitionWithKey,
    Map.isSubmapOf,

    -- * Restrict/without
    extractKeysSet,
    (◁),
    restrictKeysSet,
    restrictKeysMap,
    restrictKeysSplit,
    restrictKeys,
    withoutKeysSet,
    withoutKeysMap,
    withoutKeysSplit,
    withoutKeys,

    -- * Conversion
    Map.toList,
    Map.fromList,
    Map.elems,
    Map.keys,
    toMap,
    fromMap,
    toSet,
    Map.fromSet,
  )
where

-- import Control.DeepSeq
-- import Data.Compact.KeyMap (Key (..), KeyMap, PDoc, ppKeyMap, ppList, ppSexp)
-- import qualified Data.Compact.KeyMap as KeyMap
-- import qualified Data.Foldable as F
-- import Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
-- import Data.Text (pack)
-- import qualified GHC.Exts as Exts
-- import NoThunks.Class
-- import Prettyprinter
import Prelude hiding (lookup, null)

type Split k = Ord k

type SplitMap k v = Map k v

-- -- | There is an invariant that every Int in the IntMap has a non-null KeyMap
-- --   this should hold for every 'k' and 'SplitMap k v'
-- valid :: SplitMap k v -> Bool
-- valid (SplitMap im) =
--   IntMap.foldl' (\acc km -> acc && KeyMap.isNotEmpty km && KeyMap.valid km) True im

-- -- | To maintain the invariant, we define 'insertNormForm' Insert a KeyMap into
-- --   an IntMap, unless it is empty, if so, return the IntMap unchanged we assume
-- --   the Int 'n' is not already in the IntMap 'imap', and each call site should
-- --   check this invariant.
-- insertNormForm ::
--   IntMap.Key ->
--   KeyMap.KeyMap v ->
--   IntMap.IntMap (KeyMap.KeyMap v) ->
--   SplitMap k v
-- insertNormForm _ KeyMap.Empty imap = SplitMap imap
-- insertNormForm n kmap imap = SplitMap (IntMap.insert n kmap imap)

-- intersectionWithKeyNormal ::
--   (IntMap.Key -> t1 -> t2 -> KeyMap v) ->
--   IntMap t1 ->
--   IntMap t2 ->
--   IntMap (KeyMap v)
-- intersectionWithKeyNormal f =
--   let f' k x1 x2 =
--         let keyMap = f k x1 x2
--          in if KeyMap.isEmpty keyMap then Nothing else Just keyMap
--    in IntMap.mergeWithKey f' (const IntMap.empty) (const IntMap.empty)

-- empty :: forall k v. SplitMap k v
-- empty = SplitMap IntMap.empty

-- singleton :: Split k => k -> v -> SplitMap k v
-- singleton k v = insert k v empty

-- null :: SplitMap k v -> Bool
-- null (SplitMap imap) = IntMap.null imap

-- size :: SplitMap k v -> Int
-- size (SplitMap imap) = IntMap.foldl' (\acc km -> acc + KeyMap.size km) 0 imap

-- -- ================================================
-- -- Insert and delete operations

-- insertWithKey :: forall k v. Split k => (k -> v -> v -> v) -> k -> v -> SplitMap k v -> SplitMap k v
-- {- Maybe we should benchmark these two
-- insertWithKey combine k v (SplitMap imap) =
--   SplitMap (IntMap.insertWith combine2 n (KeyMap.insert key v KeyMap.Empty) imap)
--  where
--     (n, key) = splitKey k
--     combine2 :: KeyMap v -> KeyMap v -> KeyMap v
--     combine2 km1 km2 = KeyMap.unionWith (combine k) km1 km2
-- -}
-- insertWithKey combine k v (SplitMap imap) =
--   SplitMap (IntMap.insertWith combine2 n (KeyMap.insert key v KeyMap.Empty) imap)
--   where
--     (n, key) = splitKey k
--     combine2 :: KeyMap v -> KeyMap v -> KeyMap v
--     combine2 _km1 km2 = KeyMap.insertWith @v (combine k) key v km2

-- insertWith :: forall k v. Split k => (v -> v -> v) -> k -> v -> SplitMap k v -> SplitMap k v
-- insertWith comb = insertWithKey (const comb)

-- insert :: forall k v. Split k => k -> v -> SplitMap k v -> SplitMap k v
-- insert = insertWithKey (\_k v1 _v2 -> v1)

-- delete :: Split k => k -> SplitMap k v -> SplitMap k v
-- delete k (SplitMap imap) = SplitMap (IntMap.update fix n imap)
--   where
--     (n, key) = splitKey k
--     fix keymap = case KeyMap.delete key keymap of
--       KeyMap.Empty -> Nothing
--       !other -> Just other

-- -- ==================================================================
-- -- lookup and map and Functor instance

-- lookup :: Split k => k -> SplitMap k v -> Maybe v
-- lookup k (SplitMap imap) =
--   case IntMap.lookup n imap of
--     Nothing -> Nothing
--     Just keymap -> KeyMap.lookup key keymap
--   where
--     (n, key) = splitKey k

-- member :: Split k => k -> SplitMap k v -> Bool
-- member k smap = case lookup k smap of
--   Nothing -> False
--   Just _ -> True

-- notMember :: Split k => k -> SplitMap k v -> Bool
-- notMember k = not . member k

-- mapWithKey :: forall k v u. Split k => (k -> v -> u) -> SplitMap k v -> SplitMap k u
-- mapWithKey f (SplitMap imap) = SplitMap (IntMap.mapWithKey g imap)
--   where
--     g :: Int -> KeyMap v -> KeyMap u
--     g n kmap = KeyMap.mapWithKey (f . joinKey n) kmap

-- traverseWithKey ::
--   forall f k v u.
--   (Applicative f, Split k) =>
--   (k -> v -> f u) ->
--   SplitMap k v ->
--   f (SplitMap k u)
-- traverseWithKey f (SplitMap imap) =
--   SplitMap <$> IntMap.traverseWithKey g imap
--   where
--     g :: Int -> KeyMap v -> f (KeyMap u)
--     g n kmap = KeyMap.traverseWithKey (f . joinKey n) kmap

-- instance Split k => Functor (SplitMap k) where
--   fmap f x = mapWithKey (\_ v -> f v) x

-- filter :: Split k => (v -> Bool) -> SplitMap k v -> SplitMap k v
-- filter p = filterWithKey (const p)

-- filterWithKey :: Split k => (k -> v -> Bool) -> SplitMap k v -> SplitMap k v
-- filterWithKey p smap@(SplitMap _) = foldlWithKey' accum empty smap
--   where
--     accum ans k v = if p k v then insert k v ans else ans

-- -- =====================================================================
-- -- Union operations

-- unionWithKey ::
--   forall k v.
--   Split k =>
--   (k -> v -> v -> v) ->
--   SplitMap k v ->
--   SplitMap k v ->
--   SplitMap k v
-- unionWithKey combine (SplitMap imap1) (SplitMap imap2) =
--   SplitMap (IntMap.unionWithKey comb imap1 imap2)
--   where
--     comb :: Int -> KeyMap v -> KeyMap v -> KeyMap v
--     comb n x y = KeyMap.unionWithKey (combine . joinKey n) x y

-- unionWith :: forall k v. (v -> v -> v) -> SplitMap k v -> SplitMap k v -> SplitMap k v
-- unionWith combine (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.unionWith comb imap1 imap2)
--   where
--     comb :: KeyMap v -> KeyMap v -> KeyMap v
--     comb x y = KeyMap.unionWith combine x y

-- union :: forall k v. SplitMap k v -> SplitMap k v -> SplitMap k v
-- union (SplitMap imap1) (SplitMap imap2) = SplitMap (IntMap.unionWith comb imap1 imap2)
--   where
--     comb :: KeyMap v -> KeyMap v -> KeyMap v
--     comb x y = KeyMap.unionWith const x y

-- disjoint :: Split k => SplitMap k v -> SplitMap k u -> Bool
-- disjoint m1 m2 = null $ intersection m1 m2

-- -- ============================================================================
-- -- Intersection operations

-- intersectionWithKey ::
--   forall k u v w.
--   Split k =>
--   (k -> u -> v -> w) ->
--   SplitMap k u ->
--   SplitMap k v ->
--   SplitMap k w
-- intersectionWithKey combine (SplitMap imap1) (SplitMap imap2) =
--   SplitMap (intersectionWithKeyNormal comb imap1 imap2)
--   where
--     comb :: Int -> KeyMap u -> KeyMap v -> KeyMap w
--     comb n = KeyMap.intersectionWithKey (combine . joinKey n)

-- -- | The intersection of 'x' and 'y', where the 'combine' function computes the range.
-- intersectionWith :: forall k u v w. Split k => (u -> v -> w) -> SplitMap k u -> SplitMap k v -> SplitMap k w
-- intersectionWith combine = intersectionWithKey (const combine)

-- -- | The subset of 'x' with where the domain of 'x' appears in the domain of 'y'
-- intersection :: forall k u v. Split k => SplitMap k u -> SplitMap k v -> SplitMap k u
-- intersection = intersectionWithKey (\_ u _ -> u)

-- -- | Like intersectionWithKey, except if the 'combine' function returns Nothing, the common
-- --   key is NOT placed in the intersectionWhen result.
-- intersectionWhen ::
--   forall k u v w.
--   Split k =>
--   (k -> u -> v -> Maybe w) ->
--   SplitMap k u ->
--   SplitMap k v ->
--   SplitMap k w
-- intersectionWhen combine (SplitMap imap1) (SplitMap imap2) =
--   SplitMap (intersectionWithKeyNormal comb imap1 imap2)
--   where
--     comb :: Int -> KeyMap u -> KeyMap v -> KeyMap w
--     comb n = KeyMap.intersectionWhen (combine . joinKey n)

-- foldOverIntersection ::
--   forall ans k u v.
--   Split k =>
--   (ans -> k -> u -> v -> ans) ->
--   ans ->
--   SplitMap k u ->
--   SplitMap k v ->
--   ans
-- foldOverIntersection accum ans0 (SplitMap imap1) (SplitMap imap2) =
--   foldIntersectIntMap accum2 ans0 imap1 imap2
--   where
--     accum2 :: Int -> KeyMap u -> KeyMap v -> ans -> ans
--     accum2 n km1 km2 ans1 = KeyMap.foldOverIntersection (accum3 n) ans1 km1 km2
--     accum3 :: Int -> ans -> Key -> u -> v -> ans
--     accum3 n ans2 key u v = accum ans2 (joinKey n key) u v

-- foldIntersectIntMap :: (Int -> a -> b -> ans -> ans) -> ans -> IntMap a -> IntMap b -> ans
-- foldIntersectIntMap accum ans0 imap1 imap2 =
--   case (IntMap.minViewWithKey imap1, IntMap.minViewWithKey imap2) of
--     (Nothing, _) -> ans0
--     (_, Nothing) -> ans0
--     (Just ((k1, v1), jmap1), Just ((k2, v2), jmap2)) ->
--       case compare k1 k2 of
--         EQ -> foldIntersectIntMap accum (accum k1 v1 v2 ans0) jmap1 jmap2
--         LT -> case IntMap.splitLookup k2 imap1 of
--           (_, Nothing, ys) -> foldIntersectIntMap accum ans0 ys jmap2
--           (_, Just v3, ys) -> foldIntersectIntMap accum (accum k2 v3 v2 ans0) ys jmap2
--         GT -> case IntMap.splitLookup k1 imap2 of
--           (_, Nothing, ys) -> foldIntersectIntMap accum ans0 jmap1 ys
--           (_, Just v3, ys) -> foldIntersectIntMap accum (accum k1 v1 v3 ans0) jmap1 ys

-- -- | The intersection over the common domain 'k' of a Data.Map and a SplitMap
-- --   Only those keys 'k' that meet the predicate 'p' are included.
-- --   The result isa Data.Map
-- intersectMapSplit :: Split k => (k -> v -> u -> Bool) -> Map.Map k v -> SplitMap k u -> Map.Map k v
-- intersectMapSplit = Map.intersectionWithKey

-- -- | The intersection over the common domain 'k' of a Data.Set and a SplitMap
-- --   Only those keys 'k' that meet the predicate 'p' are included.
-- --   The result is a Data.Set
-- intersectSetSplit :: Split k => (k -> u -> Bool) -> Set k -> SplitMap k u -> Set k
-- intersectSetSplit p s sm = Set.filter (\k -> maybe False (p k) $ lookup k sm) s

-- -- | The intersection over the common domain 'k' of a SplitMap and a Data.Map.
-- --   Only those keys 'k' that meet the predicate 'p' are included.
-- --   The result isa SplitMap
-- intersectSplitMap :: (Ord k, Split k) => (k -> v -> u -> Bool) -> SplitMap k v -> Map.Map k u -> SplitMap k v
-- intersectSplitMap p sm m = filterWithKey (\k v -> maybe False (p k v) $ Map.lookup k m) sm

-- partition ::
--   Split k => (v -> Bool) -> SplitMap k v -> (SplitMap k v, SplitMap k v)
-- partition p = partitionWithKey (const p)

-- partitionWithKey ::
--   Split k =>
--   (k -> v -> Bool) ->
--   SplitMap k v ->
--   (SplitMap k v, SplitMap k v)
-- partitionWithKey p =
--   foldlWithKey'
--     ( \(lsm, rsm) k v ->
--         if p k v
--           then (insert k v lsm, rsm)
--           else (lsm, insert k v rsm)
--     )
--     (empty, empty)

-- isSubmapOf :: Split k => SplitMap k v -> SplitMap k u -> Bool
-- isSubmapOf sm1 sm2 = foldlWithKey' (\acc k _ -> acc && member k sm2) True sm1

-- -- ============================================================================
-- -- Fold operations
-- foldl' :: forall k v ans. Split k => (ans -> v -> ans) -> ans -> SplitMap k v -> ans
-- foldl' comb = foldlWithKey' (\acc _ -> comb acc)
-- {-# INLINE foldl' #-}

-- -- | Strict fold over the pairs in descending order of keys.
-- foldlWithKey' :: forall k v ans. Split k => (ans -> k -> v -> ans) -> ans -> SplitMap k v -> ans
-- foldlWithKey' comb ans0 (SplitMap imap) = IntMap.foldlWithKey' comb2 ans0 imap
--   where
--     comb2 :: ans -> Int -> KeyMap v -> ans
--     comb2 ans1 n kmap = KeyMap.foldWithAscKey comb3 ans1 kmap
--       where
--         comb3 :: ans -> Key -> v -> ans
--         comb3 ans2 key v = comb ans2 (joinKey n key) v
-- {-# INLINE foldlWithKey' #-}

-- foldr' :: forall k v ans. Split k => (v -> ans -> ans) -> ans -> SplitMap k v -> ans
-- foldr' comb = foldrWithKey' (const comb)
-- {-# INLINE foldr' #-}

-- -- | Strict fold over the pairs in ascending order of keys.
-- foldrWithKey' :: forall k v ans. Split k => (k -> v -> ans -> ans) -> ans -> SplitMap k v -> ans
-- foldrWithKey' comb ans0 (SplitMap imap) = IntMap.foldrWithKey' comb2 ans0 imap
--   where
--     comb2 :: Int -> KeyMap v -> ans -> ans
--     comb2 n kmap ans1 = KeyMap.foldWithDescKey comb3 ans1 kmap
--       where
--         comb3 :: Key -> v -> ans -> ans
--         comb3 key = comb (joinKey n key)

-- =================================================================================
-- These 'restrictKeys' functions assume the structure holding the 'good' keys is small
-- An alternate approach is to use cross-type 'intersection' operations

-- | Partition the SplitMap according to keys in the Set. This is equivalent to:
--
-- > extractKeysSet m s === (withoutKeysSet m s, restrictKeysSet m s)
extractKeysSet :: forall k a. Split k => SplitMap k a -> Set k -> (SplitMap k a, SplitMap k a)
extractKeysSet sm = Set.foldl' f (sm, Map.empty)
  where
    f acc@(without, restrict) k =
      case Map.lookup k without of
        Nothing -> acc
        Just v ->
          let !without' = Map.delete k without
              !restrict' = Map.insert k v restrict
           in (without', restrict')

(◁) :: Split k => Set k -> SplitMap k a -> SplitMap k a
(◁) = flip restrictKeysSet

restrictKeysSet :: forall k a. Split k => SplitMap k a -> Set k -> SplitMap k a
restrictKeysSet = Map.restrictKeys

restrictKeysMap :: forall k a b. Split k => SplitMap k a -> Map k b -> SplitMap k a
restrictKeysMap m1 m2 = Map.filterWithKey (\k _ -> k `Map.member` m2) m1

restrictKeysSplit :: forall k a b. Split k => SplitMap k a -> SplitMap k b -> SplitMap k a
restrictKeysSplit = restrictKeysMap

-- | Restrict the keys using the intersection operation
restrictKeys :: forall k a b. Split k => SplitMap k a -> SplitMap k b -> SplitMap k a
restrictKeys = restrictKeysMap

-- =================================================================================
-- These 'withoutKeys' functions assume the structure holding the 'bad' keys is small
-- An alternate approach is to use cross-type 'intersection' operations

withoutKeysSet :: forall k a. Split k => SplitMap k a -> Set k -> SplitMap k a
withoutKeysSet = Map.withoutKeys

withoutKeysMap :: forall k a b. Split k => SplitMap k a -> Map k b -> SplitMap k a
withoutKeysMap m1 m2 = Map.filterWithKey (\k _ -> k `Map.notMember` m2) m1

withoutKeysSplit :: forall k a b. Split k => SplitMap k a -> SplitMap k b -> SplitMap k a
withoutKeysSplit = withoutKeysMap

-- | Remove the keys using the intersection operation. if 'smap2' is small use
-- one of the other withoutKeysXX functions.
withoutKeys :: forall k a b. Split k => SplitMap k a -> SplitMap k b -> SplitMap k a
withoutKeys = withoutKeysMap

-- -- ==============================================================
-- -- Min Max operations

-- lookupMin :: Split k => SplitMap k v -> Maybe (k, v)
-- lookupMin (SplitMap imap) =
--   case IntMap.minViewWithKey imap of
--     Nothing -> Nothing
--     Just ((n, kmap), _) ->
--       case KeyMap.lookupMin kmap of
--         Nothing -> Nothing
--         Just (k, v) -> Just (joinKey n k, v)

-- lookupMax :: Split k => SplitMap k v -> Maybe (k, v)
-- lookupMax (SplitMap imap) =
--   case IntMap.maxViewWithKey imap of
--     Nothing -> Nothing
--     Just ((n, kmap), _) ->
--       case KeyMap.lookupMax kmap of
--         Nothing -> Nothing
--         Just (k, v) -> Just (joinKey n k, v)

-- minViewWithKey :: Split k => SplitMap k v -> Maybe ((k, v), SplitMap k v)
-- minViewWithKey (SplitMap imap) =
--   case IntMap.minViewWithKey imap of
--     Nothing -> Nothing
--     Just ((n, kmap), imap2) ->
--       case KeyMap.minViewWithKey kmap of
--         Nothing -> Nothing
--         Just ((k, v), kmap2) -> Just ((joinKey n k, v), insertNormForm n kmap2 imap2)

-- maxViewWithKey :: Split k => SplitMap k v -> Maybe ((k, v), SplitMap k v)
-- maxViewWithKey (SplitMap imap) =
--   case IntMap.maxViewWithKey imap of
--     Nothing -> Nothing
--     Just ((n, kmap), imap2) ->
--       case KeyMap.maxViewWithKey kmap of
--         Nothing -> Nothing
--         Just ((k, v), kmap2) -> Just ((joinKey n k, v), insertNormForm n kmap2 imap2)

-- -- ====================================================

-- -- | Performs a split but also returns whether the pivot key was found in the original map.
-- splitLookup :: Split k => k -> SplitMap k a -> (SplitMap k a, Maybe a, SplitMap k a)
-- splitLookup k (SplitMap imap) =
--   let (n, key) = splitKey k
--    in case IntMap.splitLookup n imap of
--         (ileft, Nothing, iright) -> (SplitMap ileft, Nothing, SplitMap iright)
--         (ileft, Just x, iright) ->
--           case KeyMap.splitLookup key x of
--             (kleft, Nothing, kright) ->
--               (insertNormForm n kleft ileft, Nothing, insertNormForm n kright iright)
--             (kleft, Just a, kright) ->
--               (insertNormForm n kleft ileft, Just a, insertNormForm n kright iright)

-- -- ===============================================================
-- -- Interaction with lists

-- -- | In case of duplicate keys, the pair closer to the end of the list wins.
-- fromList :: Split k => [(k, v)] -> SplitMap k v
-- fromList = F.foldl' accum empty
--   where
--     accum mp (k, v) = insert k v mp

-- -- | Generates the list in ascending order of k
-- toList :: Split k => SplitMap k v -> [(k, v)]
-- toList = foldrWithKey' (\k v acc -> (k, v) : acc) []
-- {-# NOINLINE [0] toList #-} -- needed for list fusion, see below

-- keys ::Split k =>  SplitMap k v -> [k]
-- keys = foldrWithKey' (\k _ acc -> k : acc) []
-- {-# NOINLINE [0] keys #-} -- needed for list fusion, see below

-- elems :: Split k => SplitMap k v -> [v]
-- elems = foldrWithKey' (\_ v acc -> v : acc) []
-- {-# NOINLINE [0] elems #-} -- needed for list fusion, see below

toMap :: SplitMap k v -> Map.Map k v
toMap = id

fromMap :: Map.Map k v -> SplitMap k v
fromMap = id

toSet :: SplitMap k v -> Set.Set k
toSet = Map.keysSet

-- fromSet :: Split k => (k -> v) -> Set.Set k -> SplitMap k v
-- fromSet f = Set.foldr' (\k -> insert k (f k)) mempty

-- instance (Split k, Eq k, Eq v) => Eq (SplitMap k v) where
--   x == y = toList x == toList y

-- instance (Split k, Show k, Show v) => Show (SplitMap k v) where
--   showsPrec d m =
--     showParen (d > 10) $
--       showString "fromList " . shows (toList m)

-- ppSplitMap :: forall k v. Show v => SplitMap k v -> PDoc
-- ppSplitMap (SplitMap imap) = ppList ppitem (IntMap.toList imap)
--   where
--     ppitem :: (Int, KeyMap v) -> PDoc
--     ppitem (n, kmap) = ppSexp (pack "Split") [viaShow n, ppKeyMap viaShow ppv kmap]
--     ppv :: v -> PDoc
--     ppv x = viaShow x
