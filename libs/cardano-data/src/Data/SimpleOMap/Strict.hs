{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- FIXME: this is a duplicate of the OMap implementation, and will eventually
-- become the only OMap once we lift the key-in-values constraint of the
-- previous version.
module Data.SimpleOMap.Strict (
  OMap (Empty, (:<|:), (:|>:)),
  null,
  size,
  empty,
  singleton,
  lookup,
  member,
  (!?),
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
  decodeOMap,
) where

import Cardano.Ledger.Binary (
  DecCBOR,
  Decoder,
  EncCBOR (encCBOR),
  decodeListLikeEnforceNoDuplicates,
  decodeMapLenOrIndef,
  encodeMapLen,
 )
import Cardano.Ledger.Binary.Decoding (DecCBOR (decCBOR))
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..))
import Data.Default (Default (..))
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.MapExtras qualified as MapE
import Data.Maybe (isJust)
import Data.Sequence.Strict qualified as SSeq
import Data.Set qualified as Set
import GHC.Exts qualified as Exts
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Prelude hiding (elem, filter, lookup, null, seq)

-- | A general-purpose finite, insert-ordered, map that is strict in its
-- keys and values.
--
-- The strictness is enforced by the underlying strict `Map`, and the ordering
-- is maintained by the constructing functions, leveraging `StrictSeq` to hold
-- the insert-order of the keys.
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
singleton :: k -> v -> OMap k v
singleton !k !v = OMap (SSeq.singleton k) (Map.singleton k v)

-- | \(O(\log n)\). If the key is not present 'lookup' returns
-- 'Nothing'.
lookup :: Ord k => k -> OMap k v -> Maybe v
lookup k (OMap _seq kv) = Map.lookup k kv

-- | `flip`ed version of `lookup`
(!?) :: Ord k => OMap k v -> k -> Maybe v
(!?) = flip lookup

-- | \(O(\log n)\). Checks membership before cons'ing.
cons :: Ord k => k -> v -> OMap k v -> OMap k v
cons k v omap@(OMap sseq kv)
  | Map.member k kv = omap
  | otherwise = OMap (k SSeq.<| sseq) (Map.insert k v kv)

-- | \(O(\log n)\). Checks membership before cons'ing.
(<|) :: Ord k => (k, v) -> OMap k v -> OMap k v
(<|) = uncurry cons

infixr 5 <|

-- | \(O(\log n)\). Checks membership before cons'ing. Overwrites a
-- duplicate.
cons' :: Ord k => k -> v -> OMap k v -> OMap k v
cons' k v (OMap sseq kv)
  | Map.member k kv = OMap sseq kv'
  | otherwise = OMap (k SSeq.<| sseq) kv'
  where
    kv' = Map.insert k v kv

-- | \(O(\log n)\). Checks membership before cons'ing. Overwrites a
-- duplicate.
(<||) :: Ord k => (k, v) -> OMap k v -> OMap k v
(<||) = uncurry cons'

infixr 5 <||

-- TODO: export along with others that are hidden or remove them completely.

-- | \(O(\log n)\). Checks membership before snoc'ing.
snoc :: Ord k => OMap k v -> k -> v -> OMap k v
snoc omap@(OMap sseq kv) k v
  | Map.member k kv = omap
  | otherwise = OMap (sseq SSeq.|> k) (Map.insert k v kv)

-- | \(O(\log n)\). Checks membership before snoc'ing.
(|>) :: Ord k => OMap k v -> (k, v) -> OMap k v
omap |> (k, v) = snoc omap k v

infixl 5 |>

-- | \(O(\log n)\). Checks membership before snoc'ing. Overwrites a
-- duplicate.
snoc' :: Ord k => OMap k v -> k -> v -> OMap k v
snoc' (OMap sseq kv) k v
  | Map.member k kv = OMap sseq kv'
  | otherwise = OMap (sseq SSeq.|> k) kv'
  where
    kv' = Map.insert k v kv

-- | \(O(\log n)\). Checks membership before snoc'ing. Overwrites a
-- duplicate.
(||>) :: Ord k => OMap k v -> (k, v) -> OMap k v
omap ||> (k, v) = snoc' omap k v

infixl 5 ||>

-- | \(O(\log n)\).
uncons :: Ord k => OMap k v -> Maybe ((k, v), OMap k v)
uncons (OMap sseq kv) = case sseq of
  SSeq.Empty -> Nothing
  k SSeq.:<| ks ->
    case Map.lookup k kv of
      Just v -> Just ((k, v), OMap ks (Map.delete k kv))
      Nothing -> error "Invariant falsified! In OMap, key from sequence not found in corresponding map"

-- | \(O(\log n)\).
unsnoc :: Ord k => OMap k v -> Maybe (OMap k v, (k, v))
unsnoc (OMap sseq kv) = case sseq of
  SSeq.Empty -> Nothing
  ks SSeq.:|> k ->
    case Map.lookup k kv of
      Just v -> Just (OMap ks (Map.delete k kv), (k, v))
      Nothing -> error "Invariant falsified! In OMap, key from sequence not found in corresponding map"

-- | \(O(n \log n)\). Checks membership before snoc'ing.
-- De-duplicates the StrictSeq without overwriting.
-- Starts from the left or head, using `foldl'`
fromFoldable :: (Foldable f, Ord k) => f (k, v) -> OMap k v
fromFoldable = F.foldl' (|>) empty

-- | \(O(n \log n)\). Checks membership before snoc'ing.
-- De-duplicates the StrictSeq and collects and returns the duplicates found.
-- Starts from the left or head, using `foldl'`
fromFoldableDuplicates :: (Foldable f, Ord k, Ord v) => f (k, v) -> (Set.Set (k, v), OMap k v)
fromFoldableDuplicates = F.foldl' snoc_ (Set.empty, empty)
  where
    snoc_ :: (Ord k, Ord v) => (Set.Set (k, v), OMap k v) -> (k, v) -> (Set.Set (k, v), OMap k v)
    snoc_ (duplicates, omap@(OMap sseq kv)) (k, v) =
      if Map.member k kv
        then (Set.insert (k, v) duplicates, omap)
        else (duplicates, OMap (sseq SSeq.|> k) (Map.insert k v kv))

-- | \(O(n \log n)\).
fromSet :: Ord k => Set.Set (k, v) -> OMap k v
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

-- | \(O(\log n)\). Key-value pair membership check.
elem :: (Ord k, Eq v) => k -> v -> OMap k v -> Bool
elem k v = (Just v ==) . lookup k

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

-- | \(O(\log n)\). Like `Map.adjust`.
--
-- Update a value at a specific key with the result of the provided function.
-- Returns the original `OMap` unaltered when the key does not exist.
--
-- Examples:
--
-- >>> import Data.SimpleOMap.Strict
-- >>> let m = fromFoldable $ zip [1,2] ['a','b'] :: OMap Int Char
-- >>> m
-- StrictSeq {fromStrict = fromList [(1,'a'),(2,'b')]}
-- >>> adjust succ 1 m
-- StrictSeq {fromStrict = fromList [(1,'b'),(2,'b')]}
-- >>> adjust succ 3 m
-- StrictSeq {fromStrict = fromList [(1,'a'),(2,'b')]}
adjust :: Ord k => (v -> v) -> k -> OMap k v -> OMap k v
adjust f k (OMap sseq kv) = OMap sseq (Map.adjust f k kv)

-- | \(O(1)\)
pattern Empty :: OMap k v
pattern Empty <- (null -> True)
  where
    Empty = empty

-- | \(O(\log n)\).
pattern (:<|:) :: Ord k => (k, v) -> OMap k v -> OMap k v
pattern x :<|: xs <- (uncons -> Just (x, xs))
  where
    x :<|: xs = x <| xs

infixr 5 :<|:

-- | \(O(\log n)\).
pattern (:|>:) :: Ord k => OMap k v -> (k, v) -> OMap k v
pattern xs :|>: x <- (unsnoc -> Just (xs, x))
  where
    xs :|>: x = xs |> x

infixl 5 :|>:

{-# COMPLETE Empty, (:|>:) #-}

{-# COMPLETE Empty, (:<|:) #-}

-- | \( O(n \log m) \). For every uncons-ed element from the sequence on the right,
-- check its membership in the sequence on the left, before snoc'ing it.
-- Preserve order. Remove duplicates from sequence on the right.
(|><) :: Ord k => OMap k v -> OMap k v -> OMap k v
omapl |>< omapr = case omapr of
  Empty -> omapl
  r :<|: rs -> (omapl |> r) |>< rs

infixl 5 |><

-- | \( O(m \log n) \). For every unsnoc-ed element from the sequence on the left,
-- check its membership in the sequence on the right, before cons'ing it.
-- Preserve order. Remove duplicates from sequence on the left.
(><|) :: Ord k => OMap k v -> OMap k v -> OMap k v
omapl ><| omapr = case omapl of
  Empty -> omapr
  ls :|>: l -> ls ><| (l <| omapr)

infixr 5 ><|

instance Ord k => Exts.IsList (OMap k v) where
  type Item (OMap k v) = (k, v)
  fromList = fromFoldable
  toList = assocList

assocList :: Ord k => OMap k v -> [(k, v)]
assocList = F.toList . toStrictSeqOfPairs

elems :: Ord k => OMap k v -> [v]
elems = F.toList . toStrictSeq

instance (Ord k, ToJSON k, ToJSON v) => ToJSON (OMap k v) where
  toJSON = toJSON . toStrictSeqOfPairs
  toEncoding = toEncoding . toStrictSeqOfPairs

instance Ord k => Semigroup (OMap k v) where
  (<>) = (|><)

instance Ord k => Monoid (OMap k v) where
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

instance (EncCBOR k, EncCBOR v, Ord k) => EncCBOR (OMap k v) where
  encCBOR omap =
    encodeMapLen (fromIntegral (size omap))
      <> foldMap (\(k, v) -> encCBOR k <> encCBOR v) (toStrictSeqOfPairs omap)

instance (Ord k, DecCBOR k, DecCBOR v, Eq v) => DecCBOR (OMap k v) where
  decCBOR = decodeOMap decCBOR decCBOR

-- | Decode a CBOR map of definite or indefinite length, keeping the entries in the order
-- they appear on the wire. Fails on duplicate keys.
decodeOMap :: Ord k => Decoder s k -> Decoder s v -> Decoder s (OMap k v)
decodeOMap decKey decValue =
  decodeListLikeEnforceNoDuplicates
    decodeMapLenOrIndef
    (flip (|>))
    (\omap -> (size omap, omap))
    ((,) <$> decKey <*> decValue)

-- | \( O(n \log n) \)
filter :: Ord k => (v -> Bool) -> OMap k v -> OMap k v
filter f (OMap sseq kv) =
  let kv' = Map.filter f kv
      sseq' =
        F.foldl' (\accum k -> if Map.member k kv' then accum SSeq.:|> k else accum) SSeq.empty sseq
   in OMap sseq' kv'
