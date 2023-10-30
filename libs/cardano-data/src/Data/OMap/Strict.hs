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
{-# LANGUAGE TupleSections #-}
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
  toMap,
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
  elem',
  extractKeys,
  adjust,
  filter,
)
where

import Cardano.Ledger.Binary (
  DecCBOR,
  EncCBOR (encCBOR),
  decodeSetLikeEnforceNoDuplicates,
  encodeStrictSeq,
  encodeTag,
  setTag,
 )
import Cardano.Ledger.Binary.Decoding (DecCBOR (decCBOR))
import Cardano.Ledger.TreeDiff (ToExpr (..))
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..))
import Data.Default.Class (Default (..))
import Data.Foldable qualified as F
import Data.Map.Strict qualified as Map
import Data.MapExtras qualified as MapE
import Data.Maybe (fromJust, isJust)
import Data.Sequence qualified as Seq
import Data.Sequence.Strict qualified as SSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Prelude hiding (elem, filter, lookup, null, seq)

-- | Class of types that can be mapped by a lens or a projection to an
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
-- insert-order of the keys.
--
-- TODO: DecShareCBOR instance
data OMap k v = OMap
  { omSSeq :: !(SSeq.StrictSeq k)
  , omMap :: !(Map.Map k v)
  }
  deriving (Generic, Show, Eq)

deriving instance (NoThunks k, NoThunks v) => NoThunks (OMap k v)

deriving instance (NFData k, NFData v) => NFData (OMap k v)

-- | \(O(1)\).
empty :: OMap k v
empty = OMap SSeq.Empty Map.empty

instance Default (OMap k v) where
  def = empty

-- | \(O(1)\). Shallow invariant using just `length` and `size`.
invariantHolds :: OMap k v -> Bool
invariantHolds (OMap seq kv) = SSeq.length seq == Map.size kv

-- | \(O(n \log n)\). Deep, costly invariant using membership check for each
-- value. By the pigeon-hole principle, this check is exhaustive.
invariantHolds' :: Ord k => OMap k v -> Bool
invariantHolds' omap@(OMap seq kv) =
  invariantHolds omap && all (\k -> isJust $ Map.lookup k kv) seq

-- | \(O(1)\).
null :: OMap k v -> Bool
null (OMap seq _) = SSeq.null seq

-- | \(O(1)\).
size :: OMap k v -> Int
size (OMap seq _) = SSeq.length seq

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
cons :: (HasOKey k v, Ord k) => v -> OMap k v -> OMap k v
cons v omap@(OMap seq kv) =
  let k = v ^. okeyL
   in case Map.lookup k kv of
        Just _ -> omap
        Nothing -> OMap (k SSeq.<| seq) (Map.insert k v kv)

-- | \(O(\log n)\). Checks membership before cons'ing.
(<|) :: (HasOKey k v, Ord k) => v -> OMap k v -> OMap k v
(<|) = cons

infixr 5 <|

-- | \(O(\log n)\). Checks membership before cons'ing. Overwrites a
-- duplicate.
cons' :: (HasOKey k v, Ord k) => v -> OMap k v -> OMap k v
cons' v (OMap seq kv) =
  let k = v ^. okeyL
   in case Map.lookup k kv of
        Just _ -> OMap seq (Map.insert k v kv)
        Nothing -> OMap (k SSeq.<| seq) (Map.insert k v kv)

-- | \(O(\log n)\). Checks membership before cons'ing. Overwrites a
-- duplicate.
(<||) :: (HasOKey k v, Ord k) => v -> OMap k v -> OMap k v
(<||) = cons'

infixr 5 <||

-- | \(O(\log n)\). Checks membership before snoc'ing.
snoc :: (HasOKey k v, Ord k) => OMap k v -> v -> OMap k v
snoc omap@(OMap seq kv) v =
  let k = v ^. okeyL
   in case Map.lookup k kv of
        Just _ -> omap
        Nothing -> OMap (seq SSeq.|> k) (Map.insert k v kv)

-- | \(O(\log n)\). Checks membership before snoc'ing.
(|>) :: (HasOKey k v, Ord k) => OMap k v -> v -> OMap k v
(|>) = snoc

infixl 5 |>

-- | \(O(\log n)\). Checks membership before snoc'ing. Overwrites a
-- duplicate.
snoc' :: (HasOKey k v, Ord k) => OMap k v -> v -> OMap k v
snoc' (OMap seq kv) v =
  let k = v ^. okeyL
   in case Map.lookup k kv of
        Just _ -> OMap seq (Map.insert k v kv)
        Nothing -> OMap (seq SSeq.|> k) (Map.insert k v kv)

-- | \(O(\log n)\). Checks membership before snoc'ing. Overwrites a
-- duplicate.
(||>) :: (HasOKey k v, Ord k) => OMap k v -> v -> OMap k v
(||>) = snoc'

infixl 5 ||>

-- | \(O(\log n)\).
uncons :: Ord k => OMap k v -> Maybe (v, OMap k v)
uncons (OMap seq kv) = case seq of
  SSeq.Empty -> Nothing
  k SSeq.:<| ks ->
    case Map.lookup k kv of
      Just v -> Just (v, OMap ks (Map.delete k kv))
      Nothing -> error "Invariant falsified! In OMap, key from sequence not found in corresponding map"

-- | \(O(\log n)\).
unsnoc :: Ord k => OMap k v -> Maybe (OMap k v, v)
unsnoc (OMap seq kv) = case seq of
  SSeq.Empty -> Nothing
  ks SSeq.:|> k ->
    case Map.lookup k kv of
      Just v -> Just (OMap ks (Map.delete k kv), v)
      Nothing -> error "Invariant falsified! In OMap, key from sequence not found in corresponding map"

-- | \(O(n \log n)\). Checks membership before snoc'ing.
-- De-duplicates the StrictSeq without overwriting.
-- Starts from the left or head, using `foldl'`
fromStrictSeq :: (HasOKey k v, Ord k) => SSeq.StrictSeq v -> OMap k v
fromStrictSeq = F.foldl' snoc_ empty
  where
    snoc_ :: (HasOKey k v, Ord k) => OMap k v -> v -> OMap k v
    snoc_ omap@(OMap seq kv) v =
      let k = v ^. okeyL
       in if Map.member k kv
            then omap
            else OMap (seq SSeq.|> k) (Map.insert k v kv)

-- | \(O(n \log n)\). Checks membership before snoc'ing.
-- De-duplicates the StrictSeq and collects and returns the duplicates found.
-- Starts from the left or head, using `foldl'`
fromStrictSeqDuplicates :: (HasOKey k v, Ord k, Ord v) => SSeq.StrictSeq v -> (Set.Set v, OMap k v)
fromStrictSeqDuplicates = F.foldl' snoc_ (Set.empty, empty)
  where
    snoc_ :: (HasOKey k v, Ord k, Ord v) => (Set.Set v, OMap k v) -> v -> (Set.Set v, OMap k v)
    snoc_ (duplicates, omap@(OMap seq kv)) v =
      let k = v ^. okeyL
       in if Map.member k kv
            then (Set.insert v duplicates, omap)
            else (duplicates, OMap (seq SSeq.|> k) (Map.insert k v kv))

-- | \(O(n + n \log n)\).
fromSet :: (HasOKey k v, Ord k) => Set.Set v -> OMap k v
fromSet = fromStrictSeq . SSeq.fromList . Set.elems

-- | \(O(1)\).
toMap :: OMap k v -> Map.Map k v
toMap = omMap

-- | \(O(n \log n)\). Uses partial `fromJust`.
toStrictSeq :: Ord k => OMap k v -> SSeq.StrictSeq v
toStrictSeq (OMap seq kv) = fromJust $ traverse (`Map.lookup` kv) seq

-- | \(O(1)\).
toStrictSeqOKeys :: OMap k v -> SSeq.StrictSeq k
toStrictSeqOKeys = omSSeq

-- | \(O(n \log n)\). Uses partial `fromJust`.
toStrictSeqOfPairs :: Ord k => OMap k v -> SSeq.StrictSeq (k, v)
toStrictSeqOfPairs (OMap seq kv) = fromJust $ traverse (\k -> (k,) <$> Map.lookup k kv) seq

-- | \(O(\log n)\). Key membership check.
member :: Ord k => k -> OMap k v -> Bool
member k (OMap _seq kv) = Map.member k kv

-- | \(O(\log n)\). Value membership check, via key membership.
elem :: (HasOKey k v, Ord k) => v -> OMap k v -> Bool
elem v = member (v ^. okeyL)

-- | \(O(n)\). Expensive value membership check.
elem' :: Eq v => v -> OMap k v -> Bool
elem' v (OMap _seq kv) = F.elem v $ Map.elems kv

-- | \(O(n)\). Given a `Set` of @k@s, and an `OMap` @k@ @v@ return
-- a pair of `Map` and `OMap` where the @k@s in the `Set` have been
-- removed from the `OMap` and presented as a separate `Map`.
extractKeys :: Ord k => Set.Set k -> OMap k v -> (OMap k v, Map.Map k v)
extractKeys ks (OMap seq kv) =
  let (kv', extractedKv) = MapE.extractKeys kv ks
      seq' =
        F.foldl'
          (\accum k -> if Set.member k ks then accum else accum SSeq.|> k)
          SSeq.empty
          seq
   in (OMap seq' kv', extractedKv)

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
adjust :: (HasOKey k v, Ord k) => (v -> v) -> k -> OMap k v -> OMap k v
adjust f k omap@(OMap seq kv) =
  case Map.lookup k kv of
    Nothing -> omap
    Just v ->
      let v' = f v
          k' = v' ^. okeyL
          i = fromJust $ SSeq.findIndexL (== k) seq -- since lookup succeeded
       in if k' == k
            then OMap seq (Map.insert k v' kv)
            else case Map.lookup k' kv of
              Nothing ->
                OMap (updateStrictSeq i k' seq) (Map.insert k' v' $ Map.delete k kv)
              Just _ ->
                OMap (deleteAtStrictSeq i seq) (Map.insert k' v' $ Map.delete k kv)

-- TODO: Add this to cardano-base as soon as possible.
-- This is currently a very expensive operation, unnecessarily.
updateStrictSeq :: Int -> a -> SSeq.StrictSeq a -> SSeq.StrictSeq a
updateStrictSeq i a = SSeq.forceToStrict . Seq.update i a . SSeq.fromStrict

-- TODO: Add this to cardano-base as soon as possible.
-- This is currently a very expensive operation, unnecessarily.
deleteAtStrictSeq :: Int -> SSeq.StrictSeq a -> SSeq.StrictSeq a
deleteAtStrictSeq i = SSeq.forceToStrict . Seq.deleteAt i . SSeq.fromStrict

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
(|><) :: (HasOKey k v, Ord k) => OMap k v -> OMap k v -> OMap k v
omapl |>< omapr = case omapr of
  Empty -> omapl
  r :<|: rs -> (omapl |> r) |>< rs

infixl 5 |><

-- | \( O(m \log n) \). For every unsnoc-ed element from the sequence on the left,
-- check its membership in the sequence on the right, before cons'ing it.
-- Preserve order. Remove duplicates from sequence on the left.
(><|) :: (HasOKey k v, Ord k) => OMap k v -> OMap k v -> OMap k v
omapl ><| omapr = case omapl of
  Empty -> omapr
  ls :|>: l -> ls ><| (l <| omapr)

infixr 5 ><|

instance (HasOKey k v, Ord k) => IsList (OMap k v) where
  type Item (OMap k v) = v
  fromList = fromStrictSeq . SSeq.fromList
  toList = F.toList . toStrictSeq

instance (HasOKey k v, ToExpr v, Ord k) => ToExpr (OMap k v) where
  listToExpr = listToExpr . F.toList
  toExpr = toExpr . F.toList

instance (HasOKey k v, ToJSON v, Ord k) => ToJSON (OMap k v) where
  toJSON = toJSON . toStrictSeq
  toEncoding = toEncoding . toStrictSeq

instance (HasOKey k v, Ord k) => Semigroup (OMap k v) where
  (<>) = (|><)

instance (HasOKey k v, Ord k) => Monoid (OMap k v) where
  mempty = empty

instance Ord k => Foldable (OMap k) where
  foldMap f = F.foldMap f . SSeq.fromStrict . toStrictSeq
  {-# INLINEABLE foldMap #-}
  foldr f z = F.foldr f z . SSeq.fromStrict . toStrictSeq
  {-# INLINEABLE foldr #-}
  foldl f z = F.foldl f z . SSeq.fromStrict . toStrictSeq
  {-# INLINEABLE foldl #-}
  foldr' f z = F.foldr' f z . SSeq.fromStrict . toStrictSeq
  {-# INLINEABLE foldr' #-}
  foldl' f z = F.foldl' f z . SSeq.fromStrict . toStrictSeq
  {-# INLINEABLE foldl' #-}
  foldr1 f = F.foldr1 f . SSeq.fromStrict . toStrictSeq
  {-# INLINEABLE foldr1 #-}
  foldl1 f = F.foldl1 f . SSeq.fromStrict . toStrictSeq
  {-# INLINEABLE foldl1 #-}
  length = F.length . SSeq.fromStrict . toStrictSeq
  {-# INLINE length #-}
  null = F.null . SSeq.fromStrict . toStrictSeq
  {-# INLINE null #-}

instance (Typeable k, EncCBOR v, Ord k) => EncCBOR (OMap k v) where
  encCBOR omap = encodeTag setTag <> encodeStrictSeq encCBOR (toStrictSeq omap)

instance (Typeable k, HasOKey k v, DecCBOR v, Ord k) => DecCBOR (OMap k v) where
  decCBOR = decodeSetLikeEnforceNoDuplicates isMember insert decCBOR
    where
      isMember = elem
      insert v omap = omap |> v

-- | \( O(n \log n) \)
filter :: Ord k => (v -> Bool) -> OMap k v -> OMap k v
filter f (OMap seq kv) =
  let kv' = Map.filter f kv
      seq' = F.foldl' (\accum k -> if Map.member k kv' then accum SSeq.:|> k else accum) SSeq.empty seq
   in OMap seq' kv'
