{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.OSet.Strict (
  OSet (Empty, (:<|:), (:|>:)),
  null,
  size,
  empty,
  singleton,
  lookup,
  member,
  (!?),
  fromList,
  fromStrictSeq,
  fromStrictSeqDuplicates,
  toStrictSeq,
  toSet,
  fromSet,
  fromFoldable,
  fromFoldableDuplicates,
  invariantHolds,
  invariantHolds',
  (|>),
  (<|),
  (|><),
  (><|),
  filter,
)
where

import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  decodeSetLikeEnforceNoDuplicates,
  encodeStrictSeq,
  encodeTag,
  setTag,
 )
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (toJSON))
import Data.Foldable qualified as F
import Data.Sequence.Strict qualified as SSeq
import Data.Set qualified as Set
import GHC.Exts qualified as GHC (IsList (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Prelude hiding (filter, lookup, null, seq)

-- | A general-purpose finite, ordered, set that is strict in its
-- values.
--
-- The strictness is enforced by the underlying `StrictSeq` from base,
-- and the uniqueness of the values is enforced in this module, by the
-- constructing functions by leveraging an accompanying `Set`.
--
-- TODO: @aniketd Implement DecShareCBOR
data OSet a = OSet
  { osSSeq :: !(SSeq.StrictSeq a)
  , osSet :: !(Set.Set a)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (NoThunks, NFData)

instance Ord a => Semigroup (OSet a) where
  (<>) = (|><)

instance Ord a => Monoid (OSet a) where
  mempty = empty

instance Ord a => GHC.IsList (OSet a) where
  type Item (OSet a) = a
  fromList = fromList
  toList = F.toList . osSSeq

instance Foldable OSet where
  foldMap f = F.foldMap f . SSeq.fromStrict . osSSeq
  {-# INLINEABLE foldMap #-}
  foldr f z = F.foldr f z . SSeq.fromStrict . osSSeq
  {-# INLINEABLE foldr #-}
  foldl f z = F.foldl f z . SSeq.fromStrict . osSSeq
  {-# INLINEABLE foldl #-}
  foldr' f z = F.foldr' f z . SSeq.fromStrict . osSSeq
  {-# INLINEABLE foldr' #-}
  foldl' f z = F.foldl' f z . SSeq.fromStrict . osSSeq
  {-# INLINEABLE foldl' #-}
  foldr1 f = F.foldr1 f . SSeq.fromStrict . osSSeq
  {-# INLINEABLE foldr1 #-}
  foldl1 f = F.foldl1 f . SSeq.fromStrict . osSSeq
  {-# INLINEABLE foldl1 #-}
  length = F.length . SSeq.fromStrict . osSSeq
  {-# INLINE length #-}
  null = F.null . SSeq.fromStrict . osSSeq
  {-# INLINE null #-}

instance EncCBOR a => EncCBOR (OSet a) where
  encCBOR (OSet seq _set) = encodeTag setTag <> encodeStrictSeq encCBOR seq

instance (Show a, Ord a, DecCBOR a) => DecCBOR (OSet a) where
  decCBOR = decodeSetLikeEnforceNoDuplicates (flip snoc) (\oset -> (size oset, oset)) decCBOR

instance ToJSON a => ToJSON (OSet a) where
  toJSON = toJSON . F.toList

-- | \( O(1) \). Shallow invariant using just `length` and `size`.
invariantHolds :: OSet a -> Bool
invariantHolds (OSet seq set) = SSeq.length seq == Set.size set

-- | \( O(n \log n) \). Deep invariant using set membership check for
-- each value. By the pigeon-hole principle, this check is exhaustive.
invariantHolds' :: Ord a => OSet a -> Bool
invariantHolds' oset@(OSet seq set) =
  invariantHolds oset && all (`Set.member` set) (SSeq.fromStrict seq)

-- | \( O(1) \).
null :: OSet a -> Bool
null (OSet _ set) = Set.null set

-- | \( O(1) \).
size :: OSet a -> Int
size (OSet _ set) = Set.size set

-- | \( O(1) \).
empty :: OSet a
empty = OSet SSeq.Empty Set.empty

-- | \( O(1) \). Strict in its argument.
singleton :: a -> OSet a
singleton !x = OSet (SSeq.singleton x) (Set.singleton x)

-- | \( O(\log(\min(i,n-i))) \). The element at the specified position,
-- counting from 0. If the specified position is negative or at least
-- the length of the sequence, 'lookup' returns 'Nothing'.
lookup :: Int -> OSet a -> Maybe a
lookup i (OSet seq _set) = SSeq.lookup i seq

-- | `flip`ed version of `lookup`
(!?) :: OSet a -> Int -> Maybe a
(!?) = flip lookup

-- | \( O(\log n) \). Checks membership before cons'ing.
cons :: Ord a => a -> OSet a -> OSet a
cons x oset@(OSet seq set) =
  if x `Set.member` set
    then oset
    else OSet (x SSeq.<| seq) (x `Set.insert` set)

(<|) :: Ord a => a -> OSet a -> OSet a
(<|) = cons

infixr 5 <|

-- | \( O(\log n) \). Checks membership before snoc'ing.
snoc :: Ord a => OSet a -> a -> OSet a
snoc oset@(OSet seq set) x =
  if x `Set.member` set
    then oset
    else OSet (seq SSeq.|> x) (x `Set.insert` set)

(|>) :: Ord a => OSet a -> a -> OSet a
(|>) = snoc

infixl 5 |>

-- | \( O(\log n) \).
uncons :: Ord a => OSet a -> Maybe (a, OSet a)
uncons (OSet seq set) = case seq of
  SSeq.Empty -> Nothing
  x SSeq.:<| xs' -> Just (x, OSet xs' (x `Set.delete` set))

-- | \( O(\log n) \).
unsnoc :: Ord a => OSet a -> Maybe (OSet a, a)
unsnoc (OSet seq set) = case seq of
  SSeq.Empty -> Nothing
  xs' SSeq.:|> x -> Just (OSet xs' (x `Set.delete` set), x)

-- | \(O(n \log n)\). Convert to an OSet from a List.
fromList :: Ord a => [a] -> OSet a
fromList = fromFoldable

-- | Using a `Foldable` instance of the source data structure convert it to an `OSet`
fromFoldable :: (Foldable f, Ord a) => f a -> OSet a
fromFoldable = F.foldl' snoc empty

-- | \(O(n \log n)\). Checks membership before snoc-ing.
-- De-duplicates the StrictSeq without overwriting.
fromStrictSeq :: Ord a => SSeq.StrictSeq a -> OSet a
fromStrictSeq = fromFoldable

-- | \(O(n \log n)\). Checks membership before snoc-ing.
-- Returns a 2-tuple, with `fst` as a `Set` of duplicates found
-- and the `snd` as the de-duplicated `OSet` without overwriting.
fromFoldableDuplicates :: (Foldable f, Ord a) => f a -> (Set.Set a, OSet a)
fromFoldableDuplicates = F.foldl' snoc' (Set.empty, empty)
  where
    snoc' :: Ord a => (Set.Set a, OSet a) -> a -> (Set.Set a, OSet a)
    snoc' (!duplicates, !oset@(OSet seq set)) x =
      if x `Set.member` set
        then (x `Set.insert` duplicates, oset)
        else (duplicates, OSet (seq SSeq.|> x) (x `Set.insert` set))

-- | \(O(n \log n)\). Checks membership before snoc-ing.
-- Returns a 2-tuple, with `fst` as a `Set` of duplicates found
-- and the `snd` as the de-duplicated `OSet` without overwriting.
-- Starts from the left or head, using `foldl'`
fromStrictSeqDuplicates :: Ord a => SSeq.StrictSeq a -> (Set.Set a, OSet a)
fromStrictSeqDuplicates = fromFoldableDuplicates

-- | \( O(1) \) -  Extract underlying strict sequence
toStrictSeq :: OSet a -> SSeq.StrictSeq a
toStrictSeq = osSSeq

-- | \( O(1) \) -  Extract underlying Set
toSet :: OSet a -> Set.Set a
toSet = osSet

-- | \( O(n) \).
fromSet :: Set.Set a -> OSet a
fromSet set = OSet (SSeq.fromList $ Set.elems set) set

-- | \(O(\log n)\). Membership check.
member :: Ord a => a -> OSet a -> Bool
member x (OSet _seq set) = x `Set.member` set

-- | \( O(n \log n) \)
filter :: Ord a => (a -> Bool) -> OSet a -> OSet a
filter f = F.foldl' (\xs x -> if f x then xs |> x else xs) empty

pattern Empty :: OSet a
pattern Empty <- (null -> True)
  where
    Empty = empty

-- | \(O(\log n)\).
pattern (:<|:) :: Ord a => a -> OSet a -> OSet a
pattern x :<|: xs <- (uncons -> Just (x, xs))
  where
    x :<|: xs = x <| xs

infixr 5 :<|:

-- | \(O(\log n)\).
pattern (:|>:) :: Ord a => OSet a -> a -> OSet a
pattern xs :|>: x <- (unsnoc -> Just (xs, x))
  where
    xs :|>: x = xs |> x

infixl 5 :|>:

{-# COMPLETE Empty, (:|>:) #-}
{-# COMPLETE Empty, (:<|:) #-}

-- | \( O(n\log(m*n)) \). For every uncons-ed element from the sequence on the right,
-- checks its membership in the sequence on the left, before snoc'ing it.
-- Preserves order. Remove duplicates from sequence on the right.
(|><) :: Ord a => OSet a -> OSet a -> OSet a
osetl |>< osetr = case osetr of
  Empty -> osetl
  r :<|: rs -> (osetl |> r) |>< rs

infixl 5 |><

-- | \( O(m\log(m+n)) \). For every unsnoc-ed element from the sequence on the left,
-- checks its membership in the sequence on the right, before cons'ing it.
-- Preserves order. Remove duplicates from sequence on the left.
(><|) :: Ord a => OSet a -> OSet a -> OSet a
osetl ><| osetr = case osetl of
  Empty -> osetr
  ls :|>: l -> ls ><| (l <| osetr)

infixr 5 ><|
