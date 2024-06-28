{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.OMap.Strict (
  HasOKey (okeyL),
  OMap (Empty, (:<|:), (:|>:)),
  null,
  size,
  empty,
  singleton,
  lookup,
  member,
  (!?),
  mapUnsafe,
  fromSet,
  fromFoldable,
  fromFoldableDuplicates,
  toMap,
  assocList,
  elems,
  toStrictSeq,
  toStrictSeqOKeys,
  toStrictSeqOfPairs,
  invariantHolds,
  invariantHolds',
  (|>),
  (<|),
  (<||),
  (||>),
  (|><),
  (><|),
  elem,
  extractKeys,
  adjust,
  filter,
)
where

import Cardano.Ledger.Binary (
  DecCBOR,
  EncCBOR (encCBOR),
  decodeListLenOrIndef,
  decodeListLikeEnforceNoDuplicates,
  encodeStrictSeq,
 )
import Cardano.Ledger.Binary.Decoding (DecCBOR (decCBOR))
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..))
import Data.Default.Class (Default (..))
import Data.Foldable qualified as F
import Data.Map.Strict qualified as Map
import Data.MapExtras qualified as MapE
import Data.Maybe (isJust)
import Data.Sequence.Strict qualified as SSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Exts qualified as Exts
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Prelude hiding (elem, filter, lookup, null, seq)

-- | Class of types that can be mapped by a lens or a projection to an
-- Ord type.
--
-- For a type @V@, defines a lens from @V@ to and Ord type @K@.
class Ord k => HasOKey k v | v -> k where
  okeyL :: Lens' v k

-- | A general-purpose finite, insert-ordered, map that is strict in its
-- keys and values.
--
-- The strictness is enforced by the underlying strict `Map` that can
-- be looked-up by a projection or lens. and the ordering is maintained
-- by the constructing functions, leveraging `StrictSeq` to hold the
-- insert-order of the keys.
--
-- TODO: DecShareCBOR instance
data OMap k v = OMap
  { omSSeq :: !(SSeq.StrictSeq k)
  , omMap :: !(Map.Map k v)
  }
  deriving (Generic, Eq)

instance (Show v, Ord k, Show k) => Show (OMap k v) where
  show = show . toStrictSeqOfPairs

deriving instance (NoThunks k, NoThunks v) => NoThunks (OMap k v)

deriving instance (NFData k, NFData v) => NFData (OMap k v)

-- | \(O(1)\).
empty :: OMap k v
empty = OMap SSeq.Empty Map.empty

instance Default (OMap k v) where
  def = empty

-- | \(O(1)\). Shallow invariant using just `length` and `size`.
invariantHolds :: OMap k v -> Bool
invariantHolds (OMap sseq kv) = SSeq.length sseq == Map.size kv

-- | \(O(n \log n)\). Deep, costly invariant using membership check for each
-- value. By the pigeon-hole principle, this check is exhaustive.
invariantHolds' :: Ord k => OMap k v -> Bool
invariantHolds' omap@(OMap sseq kv) =
  invariantHolds omap && all (\k -> isJust $ Map.lookup k kv) sseq

-- | \(O(1)\).
null :: OMap k v -> Bool
null (OMap sseq _) = SSeq.null sseq

-- | \(O(1)\).
size :: OMap k v -> Int
size (OMap sseq _) = SSeq.length sseq

-- | \(O(1)\). Strict in its arguments.
singleton :: HasOKey k v => v -> OMap k v
singleton !v =
  let k = v ^. okeyL
   in OMap (SSeq.singleton k) (Map.singleton k v)

-- | \(O(\log n)\). If the key is not present 'lookup' returns
-- 'Nothing'.
lookup :: Ord k => k -> OMap k v -> Maybe v
lookup k (OMap _seq kv) = Map.lookup k kv

-- | `flip`ed version of `lookup`
(!?) :: Ord k => OMap k v -> k -> Maybe v
(!?) = flip lookup

-- | \(O(\log n)\). Checks membership before cons'ing.
cons :: HasOKey k v => v -> OMap k v -> OMap k v
cons v omap@(OMap sseq kv)
  | Map.member k kv = omap
  | otherwise = OMap (k SSeq.<| sseq) (Map.insert k v kv)
  where
    k = v ^. okeyL

-- | \(O(\log n)\). Checks membership before cons'ing.
(<|) :: HasOKey k v => v -> OMap k v -> OMap k v
(<|) = cons

infixr 5 <|

-- | \(O(\log n)\). Checks membership before cons'ing. Overwrites a
-- duplicate.
cons' :: HasOKey k v => v -> OMap k v -> OMap k v
cons' v (OMap sseq kv)
  | Map.member k kv = OMap sseq kv'
  | otherwise = OMap (k SSeq.<| sseq) kv'
  where
    k = v ^. okeyL
    kv' = Map.insert k v kv

-- | \(O(\log n)\). Checks membership before cons'ing. Overwrites a
-- duplicate.
(<||) :: HasOKey k v => v -> OMap k v -> OMap k v
(<||) = cons'

infixr 5 <||

-- | \(O(\log n)\). Checks membership before snoc'ing.
snoc :: HasOKey k v => OMap k v -> v -> OMap k v
snoc omap@(OMap sseq kv) v
  | Map.member k kv = omap
  | otherwise = OMap (sseq SSeq.|> k) (Map.insert k v kv)
  where
    k = v ^. okeyL

-- | \(O(\log n)\). Checks membership before snoc'ing.
(|>) :: HasOKey k v => OMap k v -> v -> OMap k v
(|>) = snoc

infixl 5 |>

-- | \(O(\log n)\). Checks membership before snoc'ing. Overwrites a
-- duplicate.
snoc' :: HasOKey k v => OMap k v -> v -> OMap k v
snoc' (OMap sseq kv) v
  | Map.member k kv = OMap sseq kv'
  | otherwise = OMap (sseq SSeq.|> k) kv'
  where
    k = v ^. okeyL
    kv' = Map.insert k v kv

-- | \(O(\log n)\). Checks membership before snoc'ing. Overwrites a
-- duplicate.
(||>) :: HasOKey k v => OMap k v -> v -> OMap k v
(||>) = snoc'

infixl 5 ||>

-- | \(O(\log n)\).
uncons :: Ord k => OMap k v -> Maybe (v, OMap k v)
uncons (OMap sseq kv) = case sseq of
  SSeq.Empty -> Nothing
  k SSeq.:<| ks ->
    case Map.lookup k kv of
      Just v -> Just (v, OMap ks (Map.delete k kv))
      Nothing -> error "Invariant falsified! In OMap, key from sequence not found in corresponding map"

-- | \(O(\log n)\).
unsnoc :: Ord k => OMap k v -> Maybe (OMap k v, v)
unsnoc (OMap sseq kv) = case sseq of
  SSeq.Empty -> Nothing
  ks SSeq.:|> k ->
    case Map.lookup k kv of
      Just v -> Just (OMap ks (Map.delete k kv), v)
      Nothing -> error "Invariant falsified! In OMap, key from sequence not found in corresponding map"

-- | \(O(n \log n)\). Checks membership before snoc'ing.
-- De-duplicates the StrictSeq without overwriting.
-- Starts from the left or head, using `foldl'`
fromFoldable :: (Foldable f, HasOKey k v) => f v -> OMap k v
fromFoldable = F.foldl' snoc empty

-- | \(O(n \log n)\). Checks membership before snoc'ing.
-- De-duplicates the StrictSeq and collects and returns the duplicates found.
-- Starts from the left or head, using `foldl'`
fromFoldableDuplicates :: (Foldable f, HasOKey k v, Ord v) => f v -> (Set.Set v, OMap k v)
fromFoldableDuplicates = F.foldl' snoc_ (Set.empty, empty)
  where
    snoc_ :: (HasOKey k v, Ord v) => (Set.Set v, OMap k v) -> v -> (Set.Set v, OMap k v)
    snoc_ (duplicates, omap@(OMap sseq kv)) v =
      let k = v ^. okeyL
       in if Map.member k kv
            then (Set.insert v duplicates, omap)
            else (duplicates, OMap (sseq SSeq.|> k) (Map.insert k v kv))

-- | \(O(n \log n)\).
fromSet :: HasOKey k v => Set.Set v -> OMap k v
fromSet = fromFoldable

-- | \(O(1)\).
toMap :: OMap k v -> Map.Map k v
toMap = omMap

-- | \(O(n \log n)\).
toStrictSeq :: Ord k => OMap k v -> SSeq.StrictSeq v
toStrictSeq (OMap sseq kv) = sseq <&> \k -> let !v = kv Map.! k in v

-- | \(O(1)\).
toStrictSeqOKeys :: OMap k v -> SSeq.StrictSeq k
toStrictSeqOKeys = omSSeq

-- | \(O(n \log n)\).
toStrictSeqOfPairs :: Ord k => OMap k v -> SSeq.StrictSeq (k, v)
toStrictSeqOfPairs (OMap sseq kv) = sseq <&> \k -> let !v = kv Map.! k in (k, v)

-- | \(O(\log n)\). Key membership check.
member :: Ord k => k -> OMap k v -> Bool
member k (OMap _sseq kv) = Map.member k kv

-- | \(O(\log n)\). Value membership check.
elem :: (HasOKey k v, Eq v) => v -> OMap k v -> Bool
elem v = (Just v ==) . lookup (v ^. okeyL)

-- | \(O(n)\). Given a `Set` of @k@s, and an `OMap` @k@ @v@ return
-- a pair of `Map` and `OMap` where the @k@s in the `Set` have been
-- removed from the `OMap` and presented as a separate `Map`.
extractKeys :: Ord k => Set.Set k -> OMap k v -> (OMap k v, Map.Map k v)
extractKeys ks (OMap sseq kv) =
  let (kv', extractedKv) = MapE.extractKeys kv ks
      sseq' =
        F.foldl'
          (\accum k -> if Set.member k ks then accum else accum SSeq.|> k)
          SSeq.empty
          sseq
   in (OMap sseq' kv', extractedKv)

-- | \(O(n)\). Like `Map.adjust`.
--
-- Returns the original `OMap` unaltered when the key does not exist.
--
-- If the key exists, then the function is applied to the value, but we need to consider
-- three possible cases:
--
--     1. The modified value's `okeyL` is unaltered
--            - we return omap with the adjusted value,
--     2. The modified value's `okeyL` is altered, but not a duplicate
--            - we return the omap with adjusted key (in place) and value
--     3. The modified value's `okeyL` is altered and is a duplicate
--            - we return the omap with the old key deleted from the sequence but
--              without inserting the new key since it is a duplicate, and
--              deleting old value and inserting the new value in place of its duplicate.
--
-- Examples:
--
-- >>> import Data.OMap.Strict
-- >>> import Lens.Micro
-- >>> instance HasOKey Int (Int, Char) where okeyL = _1
-- >>> let m = fromFoldable $ zip [1,2] ['a','b'] :: OMap Int (Int, Char)
-- >>> m
-- StrictSeq {fromStrict = fromList [(1,(1,'a')),(2,(2,'b'))]}
-- >>> let adjustingFn (k, v) = (k, succ v) -- Changes the value
-- >>> let overwritingAdjustingFn (k,v) = (succ k, v) -- Changes the `okeyL`.
-- >>> adjust adjustingFn 1 m
-- StrictSeq {fromStrict = fromList [(1,(1,'b')),(2,(2,'b'))]}
-- >>> adjust overwritingAdjustingFn  1 m
-- StrictSeq {fromStrict = fromList [(2,(2,'a'))]}
adjust :: HasOKey k v => (v -> v) -> k -> OMap k v -> OMap k v
adjust f k omap@(OMap sseq kv) =
  case Map.lookup k kv of
    Nothing -> omap
    Just v ->
      let v' = f v
          k' = v' ^. okeyL
       in if k' == k
            then OMap sseq (Map.insert k v' kv)
            else
              let kv' = Map.insert k' v' $ Map.delete k kv
                  (lseq, rseq) = case SSeq.spanl (/= k) sseq of
                    (l, _ SSeq.:<| r) -> (l, r)
                    _ -> error "Impossible: supplied key expected to be in the sequence"
               in case Map.lookup k' kv of
                    Nothing -> OMap (lseq <> (k' SSeq.:<| rseq)) kv'
                    Just _ -> OMap (lseq <> rseq) kv'

-- | This mapping function is only safe when the key stored in the new value matches the
-- key stored in the new value. This invariant is not checked for performance reasons
mapUnsafe :: (v1 -> v2) -> OMap k v1 -> OMap k v2
mapUnsafe f (OMap sseq kv) = OMap sseq (Map.map f kv)

-- | \(O(1)\)
pattern Empty :: OMap k v
pattern Empty <- (null -> True)
  where
    Empty = empty

-- | \(O(\log n)\).
pattern (:<|:) :: (HasOKey k v, Ord k) => v -> OMap k v -> OMap k v
pattern x :<|: xs <- (uncons -> Just (x, xs))
  where
    x :<|: xs = x <| xs

infixr 5 :<|:

-- | \(O(\log n)\).
pattern (:|>:) :: (HasOKey k v, Ord k) => OMap k v -> v -> OMap k v
pattern xs :|>: x <- (unsnoc -> Just (xs, x))
  where
    xs :|>: x = xs |> x

infixl 5 :|>:

{-# COMPLETE Empty, (:|>:) #-}
{-# COMPLETE Empty, (:<|:) #-}

-- | \( O(n \log m) \). For every uncons-ed element from the sequence on the right,
-- check its membership in the sequence on the left, before snoc'ing it.
-- Preserve order. Remove duplicates from sequence on the right.
(|><) :: HasOKey k v => OMap k v -> OMap k v -> OMap k v
omapl |>< omapr = case omapr of
  Empty -> omapl
  r :<|: rs -> (omapl |> r) |>< rs

infixl 5 |><

-- | \( O(m \log n) \). For every unsnoc-ed element from the sequence on the left,
-- check its membership in the sequence on the right, before cons'ing it.
-- Preserve order. Remove duplicates from sequence on the left.
(><|) :: HasOKey k v => OMap k v -> OMap k v -> OMap k v
omapl ><| omapr = case omapl of
  Empty -> omapr
  ls :|>: l -> ls ><| (l <| omapr)

infixr 5 ><|

instance HasOKey k v => Exts.IsList (OMap k v) where
  type Item (OMap k v) = v
  fromList = fromFoldable
  toList = F.toList

assocList :: Ord k => OMap k v -> [(k, v)]
assocList = F.toList . toStrictSeqOfPairs

elems :: Ord k => OMap k v -> [v]
elems = F.toList . toStrictSeq

instance (HasOKey k v, ToJSON v) => ToJSON (OMap k v) where
  toJSON = toJSON . toStrictSeq
  toEncoding = toEncoding . toStrictSeq

instance HasOKey k v => Semigroup (OMap k v) where
  (<>) = (|><)

instance HasOKey k v => Monoid (OMap k v) where
  mempty = empty

instance Ord k => Foldable (OMap k) where
  foldMap f (OMap sseq kv) = F.foldMap (\k -> f (kv Map.! k)) sseq
  {-# INLINEABLE foldMap #-}
  foldr f z (OMap sseq kv) = F.foldr (\k -> f (kv Map.! k)) z sseq
  {-# INLINEABLE foldr #-}
  foldl f z (OMap sseq kv) = F.foldl (\acc k -> f acc (kv Map.! k)) z sseq
  {-# INLINEABLE foldl #-}
  foldr' f z (OMap sseq kv) = F.foldr' (\k -> f (kv Map.! k)) z sseq
  {-# INLINEABLE foldr' #-}
  foldl' f z (OMap sseq kv) = F.foldl' (\acc k -> f acc (kv Map.! k)) z sseq
  {-# INLINEABLE foldl' #-}
  length = Map.size . omMap
  {-# INLINE length #-}
  null = Map.null . omMap
  {-# INLINE null #-}

instance (Typeable k, EncCBOR v, Ord k) => EncCBOR (OMap k v) where
  encCBOR omap = encodeStrictSeq encCBOR (toStrictSeq omap)

instance (Typeable k, HasOKey k v, DecCBOR v, Eq v) => DecCBOR (OMap k v) where
  decCBOR =
    decodeListLikeEnforceNoDuplicates
      decodeListLenOrIndef
      (flip snoc)
      (\omap -> (size omap, omap))
      decCBOR

-- | \( O(n \log n) \)
filter :: Ord k => (v -> Bool) -> OMap k v -> OMap k v
filter f (OMap sseq kv) =
  let kv' = Map.filter f kv
      sseq' =
        F.foldl' (\accum k -> if Map.member k kv' then accum SSeq.:|> k else accum) SSeq.empty sseq
   in OMap sseq' kv'
