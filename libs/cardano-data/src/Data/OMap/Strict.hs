{-# LANGUAGE BangPatterns #-}
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
  fromSet,
  fromStrictSeq,
  fromStrictSeqDuplicates,
  toStrictSeq,
  toStrictSeqOKeys,
  toStrictSeqOfPairs,
  invariantHolds,
  invariantHolds',
  (|>),
  (<|),
  (|><),
  (><|),
  elem,
  filter,
)
where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR (encCBOR), decodeDeduplicate, encodeStrictSeq, encodeTag, setTag)
import Cardano.Ledger.Binary.Decoding (DecCBOR (decCBOR))
import Data.Foldable qualified as F
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as SSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Exts (IsList (..))
import Lens.Micro
import Prelude hiding (elem, filter, lookup, null, seq)

-- | Class of types that can be mapped by a lens or a projection to a
-- Ord type.
--
-- For a type @V@, defines a lens from @V@ to and Ord type @K@.
class HasOKey k v | v -> k where
  okeyL :: Lens' v k

-- | A general-purpose finite, insert-ordered, map that is strict in its
-- keys and values.
--
-- The strictness is enforced by the underlying strict `Map` that can
-- be looked-up by a projection or lens. and the ordering is maintained
-- by the constructing functions, leveraging `StrictSeq` to hold the
-- insert-order of the values.
--
-- TODO: DecShareCBOR instance
data OMap k v = OMap
  { omSSeq :: !(SSeq.StrictSeq v)
  , _omMap :: !(HasOKey k v => Map.Map k v)
  }

deriving instance (Show k, Show v, HasOKey k v) => Show (OMap k v)

instance (Eq v, HasOKey k v) => Eq (OMap k v) where
  (OMap seql _kvl) == (OMap seqr _kvr) = seql == seqr

instance (HasOKey k v, Ord k) => Semigroup (OMap k v) where
  (<>) = (|><)

instance (HasOKey k v, Ord k) => Monoid (OMap k v) where
  mempty = empty

instance (HasOKey k v, Ord k) => IsList (OMap k v) where
  type Item (OMap k v) = v
  fromList = fromStrictSeq . SSeq.fromList
  toList = F.toList . omSSeq

instance Foldable (OMap k) where
  foldMap f = F.foldMap f . SSeq.fromStrict . omSSeq
  {-# INLINEABLE foldMap #-}
  foldr f z = F.foldr f z . SSeq.fromStrict . omSSeq
  {-# INLINEABLE foldr #-}
  foldl f z = F.foldl f z . SSeq.fromStrict . omSSeq
  {-# INLINEABLE foldl #-}
  foldr' f z = F.foldr' f z . SSeq.fromStrict . omSSeq
  {-# INLINEABLE foldr' #-}
  foldl' f z = F.foldl' f z . SSeq.fromStrict . omSSeq
  {-# INLINEABLE foldl' #-}
  foldr1 f = F.foldr1 f . SSeq.fromStrict . omSSeq
  {-# INLINEABLE foldr1 #-}
  foldl1 f = F.foldl1 f . SSeq.fromStrict . omSSeq
  {-# INLINEABLE foldl1 #-}
  length = F.length . SSeq.fromStrict . omSSeq
  {-# INLINE length #-}
  null = F.null . SSeq.fromStrict . omSSeq
  {-# INLINE null #-}

instance (Typeable k, EncCBOR v) => EncCBOR (OMap k v) where
  encCBOR (OMap seq _kv) = encodeTag setTag <> encodeStrictSeq encCBOR seq

instance (Typeable k, HasOKey k v, DecCBOR v, Ord k) => DecCBOR (OMap k v) where
  decCBOR = decodeDeduplicate decCBOR isMember insert
    where
      isMember = member . (^. okeyL)
      insert v omap = omap |> (v ^. okeyL, v)

-- | \(O(1)\). Shallow invariant using just `length` and `size`.
invariantHolds :: HasOKey k v => OMap k v -> Bool
invariantHolds (OMap seq kv) = SSeq.length seq == Map.size kv

-- | \(O(n^2)\). Deep, costly invariant using membership check for each
-- value. By the pigeon-hole principle, this check is exhaustive.
invariantHolds' :: (HasOKey k v, Eq v) => OMap k v -> Bool
invariantHolds' omap@(OMap seq kv) =
  let vs = Map.elems kv
   in invariantHolds omap && all (`F.elem` vs) seq

-- | \(O(1)\).
null :: OMap k v -> Bool
null (OMap seq _) = SSeq.null seq

-- | \(O(1)\).
size :: OMap k v -> Int
size (OMap seq _) = SSeq.length seq

-- | \(O(1)\).
empty :: OMap k v
empty = OMap SSeq.Empty Map.empty

-- | \(O(1)\). Strict in its arguments.
singleton :: k -> v -> OMap k v
singleton !k !v = OMap (SSeq.singleton v) (Map.singleton k v)

-- | \(O(\log n)\). The element at the specified position, counting from
-- 0. If the specified position is negative or at least the length of
-- the sequence, 'lookup' returns 'Nothing'.
lookup :: (HasOKey k v, Ord k) => k -> OMap k v -> Maybe v
lookup k (OMap _seq kv) = Map.lookup k kv

-- | `flip`ed version of `lookup`
(!?) :: (HasOKey k v, Ord k) => OMap k v -> k -> Maybe v
(!?) = flip lookup

-- | \(O(\log n)\). Checks membership before cons'ing.
cons :: (HasOKey k v, Ord k) => (k, v) -> OMap k v -> OMap k v
cons (k, v) omap@(OMap seq kv) =
  case Map.lookup k kv of
    Just _ -> omap
    Nothing -> OMap (v SSeq.<| seq) (Map.insert k v kv)

(<|) :: (HasOKey k v, Ord k) => (k, v) -> OMap k v -> OMap k v
(<|) = cons

infixr 5 <|

-- | \(O(\log n)\). Checks membership before snoc'ing.
snoc :: (HasOKey k v, Ord k) => OMap k v -> (k, v) -> OMap k v
snoc omap@(OMap seq kv) (k, v) =
  case Map.lookup k kv of
    Just _ -> omap
    Nothing -> OMap (seq SSeq.|> v) (Map.insert k v kv)

(|>) :: (HasOKey k v, Ord k) => OMap k v -> (k, v) -> OMap k v
(|>) = snoc

infixl 5 |>

-- | \(O(\log n)\).
uncons :: (HasOKey k v, Ord k) => OMap k v -> Maybe ((k, v), OMap k v)
uncons (OMap seq kv) = case seq of
  SSeq.Empty -> Nothing
  v SSeq.:<| vs ->
    let k = v ^. okeyL
     in if Map.member k kv
          then Just ((k, v), OMap vs (Map.delete k kv))
          else error "Invariant falsified! In OMap, element from sequence not found in corresponding map"

-- | \(O(\log n)\).
unsnoc :: (HasOKey k v, Ord k) => OMap k v -> Maybe (OMap k v, (k, v))
unsnoc (OMap seq kv) = case seq of
  SSeq.Empty -> Nothing
  vs SSeq.:|> v ->
    let k = v ^. okeyL
     in if Map.member k kv
          then Just (OMap vs (Map.delete k kv), (k, v))
          else error "Invariant falsified! In OMap, element from sequence not found in corresponding map"

-- | \(O(n\log n)\). Checks membership before snoc'ing.
-- De-duplicates the StrictSeq without overwriting.
-- Starts from the left or head, using `foldl'`
fromStrictSeq :: (HasOKey k v, Ord k) => SSeq.StrictSeq v -> OMap k v
fromStrictSeq = F.foldl' snoc' empty
  where
    snoc' :: (HasOKey k v, Ord k) => OMap k v -> v -> OMap k v
    snoc' omap@(OMap seq kv) v =
      let k = v ^. okeyL
       in if Map.member k kv
            then omap
            else OMap (seq SSeq.|> v) (Map.insert k v kv)

-- | \(O(n\log n)\). Checks membership before snoc'ing.
-- De-duplicates the StrictSeq and collects and returns the duplicates found.
-- Starts from the left or head, using `foldl'`
fromStrictSeqDuplicates :: (HasOKey k v, Ord k, Ord v) => SSeq.StrictSeq v -> (Set.Set v, OMap k v)
fromStrictSeqDuplicates = F.foldl' snoc' (Set.empty, empty)
  where
    snoc' :: (HasOKey k v, Ord k, Ord v) => (Set.Set v, OMap k v) -> v -> (Set.Set v, OMap k v)
    snoc' (duplicates, omap@(OMap seq kv)) v =
      let k = v ^. okeyL
       in if Map.member k kv
            then (Set.insert v duplicates, omap)
            else (duplicates, OMap (seq SSeq.|> v) (Map.insert k v kv))

-- | \(O(n\log n)\).
fromSet :: (HasOKey k v, Ord k) => Set.Set v -> OMap k v
fromSet = fromStrictSeq . SSeq.fromList . Set.elems

-- | \(O(1)\).
toStrictSeq :: OMap k v -> SSeq.StrictSeq v
toStrictSeq = omSSeq

-- | \(O(n\log n)\).
toStrictSeqOKeys :: HasOKey k v => OMap k v -> SSeq.StrictSeq k
toStrictSeqOKeys = fmap (^. okeyL) . omSSeq

-- | \(O(n\log n)\).
toStrictSeqOfPairs :: HasOKey k v => OMap k v -> SSeq.StrictSeq (k, v)
toStrictSeqOfPairs = fmap (\v -> (v ^. okeyL, v)) . omSSeq

-- | \(O(\log n)\). Key membership check.
member :: (HasOKey k v, Ord k) => k -> OMap k v -> Bool
member k (OMap _seq kv) = Map.member k kv

-- | \(O(\log n)\). Value membership check.
elem :: (HasOKey k v, Ord k) => v -> OMap k v -> Bool
elem v = member (v ^. okeyL)

-- -- | \(O(n\log n)\)
filter :: (HasOKey k v, Ord k) => (v -> Bool) -> OMap k v -> OMap k v
filter f = F.foldl' combine empty
  where
    combine accum v =
      if f v
        then accum |> (v ^. okeyL, v)
        else accum

-- | \(O(1)\)
pattern Empty :: OMap k v
pattern Empty <- (null -> True)
  where
    Empty = empty

-- | \(O(\log n)\).
pattern (:<|:) :: (HasOKey k v, Ord k) => (k, v) -> OMap k v -> OMap k v
pattern x :<|: xs <- (uncons -> Just (x, xs))
  where
    x :<|: xs = x <| xs

infixr 5 :<|:

-- | \(O(\log n)\).
pattern (:|>:) :: (HasOKey k v, Ord k) => OMap k v -> (k, v) -> OMap k v
pattern xs :|>: x <- (unsnoc -> Just (xs, x))
  where
    xs :|>: x = xs |> x

infixl 5 :|>:

{-# COMPLETE Empty, (:|>:) #-}
{-# COMPLETE Empty, (:<|:) #-}

-- | \( O(n\log(m*n)) \). For every uncons-ed element from the sequence on the right,
-- check its membership in the sequence on the left, before snoc'ing it.
-- Preserve order. Remove duplicates from sequence on the right.
(|><) :: (HasOKey k v, Ord k) => OMap k v -> OMap k v -> OMap k v
omapl |>< omapr = case omapr of
  Empty -> omapl
  r :<|: rs -> (omapl |> r) |>< rs

infixl 5 |><

-- | \( O(m\log(m+n)) \). For every unsnoc-ed element from the sequence on the left,
-- check its membership in the sequence on the right, before cons'ing it.
-- Preserve order. Remove duplicates from sequence on the left.
(><|) :: (HasOKey k v, Ord k) => OMap k v -> OMap k v -> OMap k v
omapl ><| omapr = case omapl of
  Empty -> omapr
  ls :|>: l -> ls ><| (l <| omapr)

infixr 5 ><|
