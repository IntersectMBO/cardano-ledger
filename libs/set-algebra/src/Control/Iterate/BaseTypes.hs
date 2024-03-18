{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Defines what types can be used in the SetAlgebra, and
--   what operations those types must support (Iter, Basic, Embed)
module Control.Iterate.BaseTypes where

import Control.Iterate.Collect (Collect (..), hasElem, isempty, none, one, when)
import Data.List (sortBy)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.MapExtras (StrictTriple (..), splitMemberSet)
import Data.Set (Set)
import qualified Data.Set as Set

--  $ClassesForSetAlgebra

-- ================= The Iter class =================================================

-- | The Set algebra include types that encode finite sets and maps of some type. They
-- have a finite domain, and for each domain element they pair a single range
-- element (unit for sets). We are interested in those finite maps that can iterate their
-- pairs in ascending domain order. The operations are: `nxt` and `lub` .
-- lub can skip over many items in sub-linear time, it can make things really fast.
-- Many finite maps can support a support lub operation in sub-linear time. Some examples:
-- Balanced binary trees, Arrays (using binary search), Tries, etc. There are basic and compound
-- Iter instances. Compound types include components with types that have Iter instances.
class Iter f where
  nxt :: f a b -> Collect (a, b, f a b)
  lub :: Ord k => k -> f k b -> Collect (k, b, f k b)

  -- | The next few methods can all be defined via nxt and lub, but for base types there often exist
  -- much more efficent means, so the default definitions should be overwritten for such basic types.
  -- For compound types with Guards, these are often the only way to define them.
  hasNxt :: f a b -> Maybe (a, b, f a b)
  hasNxt f = hasElem (nxt f)

  hasLub :: Ord k => k -> f k b -> Maybe (k, b, f k b)
  hasLub a f = hasElem (lub a f)
  haskey :: Ord key => key -> f key b -> Bool
  haskey k x = case hasLub k x of Nothing -> False; Just (key, _, _) -> k == key
  isnull :: f k v -> Bool
  isnull f = isempty (nxt f)
  lookup :: Ord key => key -> f key rng -> Maybe rng
  lookup k x = case hasLub k x of Nothing -> Nothing; Just (key, v, _) -> if k == key then Just v else Nothing
  element :: Ord k => k -> f k v -> Collect ()
  element k f = when (haskey k f)

-- ==================================================================================================

-- | In order to build typed 'Exp' (which are a typed deep embedding) of map and set operations, we need to know
-- what kind of basic types can be used this way. Every Basic type has a few operations
-- for creating one from a list, for adding and removing key-value pairs, looking up a value given a key.
-- Instances of this algebra are functional in that every key has exactly one value associated with it.
class Basic f where
  -- | in addpair the new value always prevails, to make a choice use 'addkv' which has a combining function that allows choice.
  addpair :: Ord k => k -> v -> f k v -> f k v
  addpair k v f = addkv (k, v) f (\_old new -> new)

  -- | use (\ old new -> old) if you want the v in (f k v) to prevail, and use (\ old new -> new) if you want the v in (k,v) to prevail
  addkv :: Ord k => (k, v) -> f k v -> (v -> v -> v) -> f k v

  -- | remove the pair with key 'k', if it is there.
  removekey :: Ord k => k -> f k v -> f k v

  -- | the set of keys
  domain :: Ord k => f k v -> Set k

  -- | the set of values.
  range :: Ord v => f k v -> Set v

-- ===============================================================================================

-- $Deep embedding

-- | BaseRep witnesses Basic types. I.e. those types that are instances of both Basic and Iter.
--   Pattern matching against a constructor of type BaseRep, determines which base type. For example
--   data Tag f k v = Tag (BaseRep f k v) (f k v)
--   case Tag MapR x ->  -- here we know x :: Map.Map k v
data BaseRep f k v where
  MapR :: Basic Map.Map => BaseRep Map.Map k v
  SetR :: Basic Sett => BaseRep Sett k ()
  ListR :: Basic List => BaseRep List k v
  SingleR :: Basic Single => BaseRep Single k v

instance Show (BaseRep f k v) where
  show MapR = "Map"
  show SetR = "Set"
  show ListR = "List"
  show SingleR = "Single"

-- ==================================================================
-- Now for each Basic type we provide instances
-- ==================================================================

-- MapAndSetTypes

-- ========== Basic List ==============

-- | Maps stored as lists. Sorted [(key,value)] pairs, with no duplicate keys.
-- The constructor for List is hidden, since it requires some invariants. Use 'fromPairs' to build an initial List.
data List k v where UnSafeList :: Ord k => [(k, v)] -> List k v

unList :: List k v -> [(k, v)]
unList (UnSafeList xs) = xs

deriving instance (Eq k, Eq v) => Eq (List k v)

instance (Show k, Show v) => Show (List k v) where
  show (UnSafeList xs) = show xs

instance Basic List where
  addkv (k, v) (UnSafeList xs) comb = UnSafeList (insert xs)
    where
      insert [] = [(k, v)]
      insert ((key, u) : ys) =
        case compare key k of
          LT -> (key, u) : insert ys
          GT -> (k, v) : (key, u) : ys
          EQ -> (key, comb u v) : ys
  removekey k (UnSafeList xs) = UnSafeList (remove xs)
    where
      remove [] = []
      remove ((key, u) : ys) = if key == k then ys else (k, u) : (remove ys)
  domain (UnSafeList xs) = foldr (\(k, _v) ans -> Set.insert k ans) Set.empty xs
  range (UnSafeList xs) = foldr (\(_k, v) ans -> Set.insert v ans) Set.empty xs

fromPairs :: Ord k => (v -> v -> v) -> [(k, v)] -> List k v
fromPairs combine xs = UnSafeList (normalize combine (sortBy (\x y -> compare (fst x) (fst y)) xs))

normalize :: Ord k => (v -> v -> v) -> [(k, v)] -> [(k, v)]
normalize _combine [] = []
normalize _combine [(k, v)] = [(k, v)]
normalize combine ((k1, v1) : (k2, v2) : more) | k1 == k2 = normalize combine ((k1, combine v1 v2) : more)
normalize combine (p : pairs) = p : normalize combine pairs

instance Iter List where -- List is the only basic instance with non-linear nxt and lub. It also depends on
  nxt (UnSafeList []) = none -- key-value pairs being stored in ascending order. For small Lists (10 or so elements) this is OK.
  nxt (UnSafeList ((k, v) : xs)) = one (k, v, UnSafeList xs)
  lub k (UnSafeList xs) = case dropWhile (\(key, _v) -> key < k) xs of
    [] -> none
    ((key, v) : ys) -> one (key, v, UnSafeList ys)
  isnull (UnSafeList xs) = null xs
  lookup k (UnSafeList xs) = List.lookup k xs
  hasNxt (UnSafeList []) = Nothing
  hasNxt (UnSafeList (((k, v) : ps))) = Just (k, v, UnSafeList ps)

-- ================ Basic Single ===============

-- | Maps and sets with zero or a single pair. Iteration is trivial. Succeeds at most once.
data Single k v where
  Single :: k -> v -> Single k v
  Fail :: Single k v
  SetSingle :: k -> Single k ()

deriving instance (Eq k, Eq v) => Eq (Single k v)

-- Since we can only store one key, we have to choose who wins
-- We use the combine function to decide. (\ old new -> old) keeps
-- the orginal value. (\ old new -> new) overwrites the stored value.
-- Something else like (\ old new -> old+new) overwrites with a combination

instance Basic Single where
  addkv (k, v) set comb =
    case set of
      (Single a b) -> Single a (comb b v)
      (SetSingle a) -> SetSingle a
      Fail -> Single k v

  removekey key (Single a b) = if key == a then Fail else (Single a b)
  removekey key (SetSingle a) = if key == a then Fail else (SetSingle a)
  removekey _key Fail = Fail
  domain (Single a _b) = Set.singleton a
  domain (SetSingle a) = Set.singleton a
  domain Fail = Set.empty
  range (Single _a b) = Set.singleton b
  range (SetSingle _a) = Set.singleton ()
  range Fail = Set.empty

instance Iter Single where
  nxt (Single k v) = Collect (\ans f -> f (k, v, Fail) ans)
  nxt (SetSingle k) = Collect (\ans f -> f (k, (), Fail) ans)
  nxt Fail = Collect (\ans _f -> ans)
  lub key (Single k v) = Collect (\ans f -> if k <= key then f (k, v, Fail) ans else ans)
  lub key (SetSingle k) = Collect (\ans f -> if k <= key then f (k, (), Fail) ans else ans)
  lub _key Fail = Collect (\ans _f -> ans)
  haskey k (SetSingle a) = k == a
  haskey k (Single a _b) = k == a
  haskey _k Fail = False
  isnull Fail = True
  isnull _ = False
  lookup k (SetSingle a) = if k == a then Just () else Nothing
  lookup k (Single a b) = if k == a then Just b else Nothing
  lookup _k Fail = Nothing

instance (Show k, Show v) => Show (Single k v) where
  show (Single k v) = "(Single " ++ show k ++ " " ++ show v ++ ")"
  show (SetSingle k) = "(SetSingle " ++ show k ++ ")"
  show Fail = "Fail"

-- ================= Basic Set =====================

data Sett k v where
  Sett :: Set.Set k -> Sett k ()

instance Basic Sett where
  addpair key _unit (Sett m) = Sett (Set.insert key m)
  addkv (k, _unit) (Sett m) _comb = Sett (Set.insert k m) -- We can ignore comb since there is only one function at type: () -> () -> ()
  removekey k (Sett m) = Sett (Set.delete k m)
  domain (Sett xs) = xs
  range (Sett _xs) = Set.singleton ()

instance Show key => Show (Sett key ()) where
  show (Sett ss) = show ss

deriving instance Eq k => Eq (Sett k ())

instance Iter Sett where
  nxt (Sett m) =
    Collect
      ( \ans f -> if Set.null m then ans else let (k, nextm) = Set.deleteFindMin m in f (k, (), Sett nextm) ans
      )
  lub key (Sett m) =
    Collect
      ( \ans f ->
          if Set.null m
            then ans
            else case splitMemberSet key m of -- NOTE in Log time, we skip over all those tuples in _left
              StrictTriple _left True right -> f (key, (), Sett right) ans
              StrictTriple _left False right ->
                if Set.null right
                  then ans
                  else let (k, nextm) = Set.deleteFindMin right in f (k, (), Sett nextm) ans
      )
  haskey key (Sett m) = Set.member key m
  isnull (Sett x) = Set.null x
  lookup k (Sett m) = if Set.member k m then Just () else Nothing

-- ============== Basic Map =========================

instance Basic Map.Map where
  -- Data.Map uses(\ new old -> ...) while our convention is (\ old new -> ...)
  addkv (k, v) m comb = Map.insertWith (flip comb) k v m
  removekey k m = Map.delete k m
  domain x = Map.keysSet x
  range xs = Map.foldrWithKey (\_k v ans -> Set.insert v ans) Set.empty xs

-- emptyc = Map.empty

instance Iter Map.Map where
  nxt m =
    Collect
      ( \ans f ->
          case Map.minViewWithKey m of
            Nothing -> ans
            Just ((k, v), nextm) -> f (k, v, nextm) ans
      )
  lub key m =
    Collect
      ( \ans f ->
          case Map.splitLookup key m of -- NOTE in Log time, we skip over all those tuples in _left
            (_left, Just v, right) -> f (key, v, right) ans
            (_left, Nothing, right) | Map.null right -> ans
            (_left, Nothing, right) -> f (k, v, m3) ans
              where
                ((k, v), m3) = Map.deleteFindMin right
      )
  haskey x m = case Map.lookup x m of Just _ -> True; Nothing -> False
  isnull = Map.null
  lookup = Map.lookup

-- ===========================================================================

-- | Every iterable type type forms an isomorphism with some Base type. For most
-- Base types the isomorphism is the identity in both directions, but for some,
-- like List and Sett, the embeddings are not the trivial identities because the
-- concrete types are not binary type constructors. The Embed class also allows
-- us to add 'newtypes' which encode some Base type to the system.
class Embed concrete base | concrete -> base where
  toBase :: concrete -> base
  fromBase :: base -> concrete

instance Ord k => Embed [(k, v)] (List k v) where
  toBase xs = UnSafeList (sortBy (\x y -> compare (fst x) (fst y)) xs)
  fromBase (UnSafeList xs) = xs

instance Embed (Set.Set k) (Sett k ()) where
  toBase xs = Sett xs
  fromBase (Sett xs) = xs

instance Embed (Map.Map k v) (Map.Map k v) where
  toBase xs = xs
  fromBase xs = xs

instance Embed (Single k v) (Single k v) where
  toBase xs = xs
  fromBase xs = xs

-- Necessary when asking Boolean queries like: (⊆),(∈),(∉)
instance Embed Bool Bool where
  toBase xs = xs
  fromBase xs = xs
