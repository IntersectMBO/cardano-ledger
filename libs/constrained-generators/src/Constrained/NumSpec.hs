{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- Random Natural, Arbitrary Natural, Uniform Natural
{-# OPTIONS_GHC -Wno-orphans #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.NumSpec where

import Constrained.Base
import Constrained.Conformance ()
import Constrained.Core (NonEmpty ((:|)), unionWithMaybe)
import Constrained.GenT (GenT, MonadGenError (..), pureGen, sizeT)
import Constrained.Generic
import Constrained.List
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.Kind
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Typeable (typeOf)
import Data.Word
import GHC.Int
import GHC.Natural
import GHC.Real
import GHC.TypeLits
import System.Random.Stateful (Random (..), Uniform (..))
import Test.QuickCheck (Arbitrary (arbitrary, shrink), choose, frequency)

-- ====================================================================
-- NumOrdW  witnesses for comparison operations (<=. and <.) on numbers
-- The other operations are defined in terms of these. These things
-- will eventually get Logic instances
-- =====================================================================

data NumOrdW (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  LessOrEqualW :: OrdLike a => NumOrdW "<=." '[a, a] Bool
  LessW :: OrdLike a => NumOrdW "<." '[a, a] Bool
  GreaterOrEqualW :: OrdLike a => NumOrdW ">=." '[a, a] Bool
  GreaterW :: OrdLike a => NumOrdW ">." '[a, a] Bool

deriving instance Eq (NumOrdW s ds r)

instance Show (NumOrdW s ds r) where
  show LessOrEqualW = "<=."
  show LessW = "<."
  show GreaterOrEqualW = ">=."
  show GreaterW = ">."

instance Semantics NumOrdW where
  semantics LessOrEqualW = (<=)
  semantics LessW = (<)
  semantics GreaterW = (>)
  semantics GreaterOrEqualW = (>=)

instance Syntax NumOrdW where
  inFix LessOrEqualW = True
  inFix LessW = True
  inFix GreaterOrEqualW = True
  inFix GreaterW = True

-- =============================================
-- OrdLike. Ord for Numbers in the Logic
-- =============================================

class (Ord a, HasSpec a) => OrdLike a where
  leqSpec :: a -> Specification a
  default leqSpec ::
    ( TypeSpec a ~ TypeSpec (SimpleRep a)
    , HasSimpleRep a
    , OrdLike (SimpleRep a)
    ) =>
    a ->
    Specification a
  leqSpec = fromSimpleRepSpec . leqSpec . toSimpleRep

  ltSpec :: a -> Specification a
  default ltSpec ::
    ( TypeSpec a ~ TypeSpec (SimpleRep a)
    , HasSimpleRep a
    , OrdLike (SimpleRep a)
    ) =>
    a ->
    Specification a
  ltSpec = fromSimpleRepSpec . ltSpec . toSimpleRep

  geqSpec :: a -> Specification a
  default geqSpec ::
    ( TypeSpec a ~ TypeSpec (SimpleRep a)
    , HasSimpleRep a
    , OrdLike (SimpleRep a)
    ) =>
    a ->
    Specification a
  geqSpec = fromSimpleRepSpec . geqSpec . toSimpleRep

  gtSpec :: a -> Specification a
  default gtSpec ::
    ( TypeSpec a ~ TypeSpec (SimpleRep a)
    , HasSimpleRep a
    , OrdLike (SimpleRep a)
    ) =>
    a ->
    Specification a
  gtSpec = fromSimpleRepSpec . gtSpec . toSimpleRep

-- | This instance should be general enough for every type of Number that has a NumSpec as its TypeSpec
instance {-# OVERLAPPABLE #-} (Ord a, HasSpec a, MaybeBounded a, Num a, TypeSpec a ~ NumSpec a) => OrdLike a where
  leqSpec l = typeSpec $ NumSpecInterval Nothing (Just l)
  ltSpec l
    | Just b <- lowerBound
    , l == b =
        ErrorSpec (pure ("ltSpec @" ++ show (typeOf l) ++ " " ++ show l))
    | otherwise = typeSpec $ NumSpecInterval Nothing (Just (l - 1))
  geqSpec l = typeSpec $ NumSpecInterval (Just l) Nothing
  gtSpec l
    | Just b <- upperBound
    , l == b =
        ErrorSpec (pure ("gtSpec @" ++ show (typeOf l) ++ " " ++ show l))
    | otherwise = typeSpec $ NumSpecInterval (Just (l + 1)) Nothing

-- ========================================================================
-- helper functions for the TypeSpec for Numbers
-- ========================================================================

class MaybeBounded a where
  lowerBound :: Maybe a
  upperBound :: Maybe a

  default lowerBound :: Bounded a => Maybe a
  lowerBound = Just minBound

  default upperBound :: Bounded a => Maybe a
  upperBound = Just maxBound

instance MaybeBounded Int
instance MaybeBounded Int64
instance MaybeBounded Int32
instance MaybeBounded Int16
instance MaybeBounded Int8
instance MaybeBounded Word64
instance MaybeBounded Word32
instance MaybeBounded Word16
instance MaybeBounded Word8

instance MaybeBounded Integer where
  lowerBound = Nothing
  upperBound = Nothing

instance MaybeBounded (Ratio Integer) where
  lowerBound = Nothing
  upperBound = Nothing

instance MaybeBounded Natural where
  lowerBound = Just 0
  upperBound = Nothing

instance MaybeBounded Float where
  lowerBound = Nothing
  upperBound = Nothing

-- ===================================================================
-- The TypeSpec for numbers
-- ===================================================================

data NumSpec n = NumSpecInterval (Maybe n) (Maybe n)

instance Ord n => Eq (NumSpec n) where
  NumSpecInterval ml mh == NumSpecInterval ml' mh'
    | isEmpty ml mh = isEmpty ml' mh'
    | isEmpty ml' mh' = isEmpty ml mh
    | otherwise = ml == ml' && mh == mh'
    where
      isEmpty (Just a) (Just b) = a > b
      isEmpty _ _ = False

instance Show n => Show (NumSpec n) where
  show (NumSpecInterval ml mu) = lb ++ ".." ++ ub
    where
      lb = "[" ++ maybe "" show ml
      ub = maybe "" show mu ++ "]"

instance Ord n => Semigroup (NumSpec n) where
  NumSpecInterval ml mu <> NumSpecInterval ml' mu' =
    NumSpecInterval
      (unionWithMaybe max ml ml')
      (unionWithMaybe min mu mu')

instance Ord n => Monoid (NumSpec n) where
  mempty = NumSpecInterval Nothing Nothing

-- ===========================================
-- Arbitrary for Num like things
-- ===========================================

instance (Arbitrary a, Ord a) => Arbitrary (NumSpec a) where
  arbitrary = do
    m <- arbitrary
    m' <- arbitrary
    frequency [(10, pure $ mkLoHiInterval m m'), (1, pure $ NumSpecInterval m m')]
    where
      mkLoHiInterval (Just a) (Just b) = NumSpecInterval (Just $ min a b) (Just $ max a b)
      mkLoHiInterval m m' = NumSpecInterval m m'
  shrink (NumSpecInterval m m') =
    uncurry NumSpecInterval <$> shrink (m, m')

instance Arbitrary Natural where
  arbitrary = wordToNatural . abs <$> arbitrary
  shrink n = [wordToNatural w | w <- shrink (naturalToWord n)]

instance Uniform Natural where
  uniformM g = wordToNatural . abs <$> uniformM g
instance Random Natural where
  randomR (lo, hi) g = first fromIntegral $ randomR (toInteger lo, toInteger hi) g

instance Random (Ratio Integer) where
  randomR (lo, hi) g =
    let (r, g') = random g
     in (lo + (hi - lo) * r, g')
  random g =
    let (d, g') = first ((+ 1) . abs) $ random g
        (n, g'') = randomR (0, d) g'
     in (n % d, g'')

-- ==============================================================================
-- Operations on NumSpec, that give it the required properties of a TypeSpec
-- ==============================================================================

emptyNumSpec :: Ord a => NumSpec a
emptyNumSpec = mempty

guardNumSpec ::
  (Ord n, HasSpec n, TypeSpec n ~ NumSpec n) =>
  [String] -> NumSpec n -> Specification n
guardNumSpec msg s@(NumSpecInterval (Just a) (Just b))
  | a > b = ErrorSpec ("NumSpec has low bound greater than hi bound" :| (("   " ++ show s) : msg))
  | a == b = equalSpec a
guardNumSpec _ s = typeSpec s

combineNumSpec ::
  (HasSpec n, Ord n, TypeSpec n ~ NumSpec n) =>
  NumSpec n ->
  NumSpec n ->
  Specification n
combineNumSpec s s' = guardNumSpec ["when combining two NumSpecs", "   " ++ show s, "   " ++ show s'] (s <> s')

genFromNumSpec ::
  (MonadGenError m, Show n, Random n, Ord n, Num n, MaybeBounded n) =>
  NumSpec n ->
  GenT m n
genFromNumSpec (NumSpecInterval ml mu) = do
  n <- sizeT
  pureGen . choose =<< constrainInterval (ml <|> lowerBound) (mu <|> upperBound) (fromIntegral n)

-- TODO: fixme (?)
shrinkWithNumSpec :: Arbitrary n => NumSpec n -> n -> [n]
shrinkWithNumSpec _ = shrink

constrainInterval ::
  (MonadGenError m, Ord a, Num a, Show a) => Maybe a -> Maybe a -> Integer -> m (a, a)
constrainInterval ml mu r =
  case (ml, mu) of
    (Nothing, Nothing) -> pure (-r', r')
    (Just l, Nothing)
      | l < 0 -> pure (max l (negate r'), r')
      | otherwise -> pure (l, l + 2 * r')
    (Nothing, Just u)
      | u > 0 -> pure (negate r', min u r')
      | otherwise -> pure (u - r' - r', u)
    (Just l, Just u)
      | l > u -> genError (pure ("bad interval: " ++ show l ++ " " ++ show u))
      | u < 0 -> pure (safeSub l (safeSub l u r') r', u)
      | l >= 0 -> pure (l, safeAdd u (safeAdd u l r') r')
      -- TODO: this is a bit suspect if the bounds are lopsided
      | otherwise -> pure (max l (-r'), min u r')
  where
    r' = abs $ fromInteger r
    safeSub l a b
      | a - b > a = l
      | otherwise = max l (a - b)
    safeAdd u a b
      | a + b < a = u
      | otherwise = min u (a + b)

conformsToNumSpec :: Ord n => n -> NumSpec n -> Bool
conformsToNumSpec i (NumSpecInterval ml mu) = maybe True (<= i) ml && maybe True (i <=) mu

-- =======================================================================
-- Several of the methods of HasSpec that have default implementations
-- could benefit from type specific implementations for numbers. Those
-- implementations are found here
-- =====================================================================

-- | Strip out duplicates (in n-log(n) time, by building an intermediate Set)
nubOrd :: Ord a => [a] -> [a]
nubOrd =
  loop mempty
  where
    loop _ [] = []
    loop s (a : as)
      | a `Set.member` s = loop s as
      | otherwise =
          let s' = Set.insert a s in s' `seq` a : loop s' as

-- | Builds a MemberSpec, but returns an Error spec if the list is empty
nubOrdMemberSpec :: Ord a => String -> [a] -> Specification a
nubOrdMemberSpec message xs =
  memberSpecList
    (nubOrd xs)
    ( NE.fromList
        [ "In call to nubOrdMemberSpec"
        , "Called from context"
        , message
        , "The input is the empty list."
        ]
    )

lowBound :: Bounded n => Maybe n -> n
lowBound Nothing = minBound
lowBound (Just n) = n

highBound :: Bounded n => Maybe n -> n
highBound Nothing = maxBound
highBound (Just n) = n

-- | The exact count of the number elements in a Bounded NumSpec
countSpec :: forall n. (Bounded n, Integral n) => NumSpec n -> Integer
countSpec (NumSpecInterval lo hi) = if lo > hi then 0 else toInteger high - toInteger low + 1
  where
    high = highBound hi
    low = lowBound lo

-- | The exact number of elements in a Bounded Integral type.
finiteSize :: forall n. (Integral n, Bounded n) => Integer
finiteSize = toInteger (maxBound @n) - toInteger (minBound @n) + 1

-- | This is an optimizing version of  TypeSpec :: TypeSpec n -> [n] -> Specification n
--   for Bounded NumSpecs.
--                    notInNumSpec :: Bounded n => TypeSpec n -> [n] -> Specification n
--   We use this function to specialize the (HasSpec t) method 'typeSpecOpt' for Bounded n.
--   So given (TypeSpec interval badlist) we might want to transform it to (MemberSpec goodlist)
--   There are 2 opportunities where this can payoff big time.
--   1) Suppose the total count of the elements in the interval is < length badlist
--      we can then return (MemberSpec (filter elements (`notElem` badlist)))
--      this must be smaller than (TypeSpec interval badlist) because the filtered list must be smaller than badlist
--   2) Suppose the type 't' is finite with size N. If the length of the badlist > (N/2), then the number of possible
--      good things must be smaller than (length badlist), because (possible good + bad == N), so regardless of the
--      count of the interval (MemberSpec (filter elements (`notElem` badlist))) is better. Sometimes much better.
--      Example, let 'n' be the finite set {0,1,2,3,4,5,6,7,8,9} and the bad list be [0,1,3,4,5,6,8,9]
--      (TypeSpec [0..9]  [0,1,3,4,5,6,8,9]) = filter  {0,1,2,3,4,5,6,7,8,9} (`notElem` [0,1,3,4,5,6,8,9]) = [2,7]
--      So (MemberSpec [2,7]) is better than  (TypeSpec [0..9]  [0,1,3,4,5,6,8,9]). This works no matter what
--      the count of interval is. We only need the (length badlist > (N/2)).
notInNumSpec ::
  forall n.
  ( HasSpec n
  , TypeSpec n ~ NumSpec n
  , Bounded n
  , Integral n
  ) =>
  NumSpec n ->
  [n] ->
  Specification n
notInNumSpec ns@(NumSpecInterval a b) bad
  | toInteger (length bad) > (finiteSize @n `div` 2) || countSpec ns < toInteger (length bad) =
      nubOrdMemberSpec
        ("call to: (notInNumSpec " ++ show ns ++ " " ++ show bad ++ ")")
        [x | x <- [lowBound a .. highBound b], notElem x bad]
  | otherwise = TypeSpec @n ns bad

-- ==========================================================================
-- Num n => (NumSpec n) can support operation of Num as interval arithmetic.
-- So we will make a (Num (NumSpec Integer)) instance. We won't make other
-- instances, because  they would be subject to overflow.
-- Given operator ☉, then (a,b) ☉ (c,d) = (minimum s, maximum s) where s = [a ☉ c, a ☉ d, b ☉ c, b ☉ d]
-- There are simpler rules for (+) and (-), but for (*) we need to use the general rule.
-- ==========================================================================

guardEmpty :: (Ord n, Num n) => Maybe n -> Maybe n -> NumSpec n -> NumSpec n
guardEmpty (Just a) (Just b) s
  | a <= b = s
  | otherwise = NumSpecInterval (Just 1) (Just 0)
guardEmpty _ _ s = s

addNumSpec :: (Ord n, Num n) => NumSpec n -> NumSpec n -> NumSpec n
addNumSpec (NumSpecInterval x y) (NumSpecInterval a b) =
  guardEmpty x y $
    guardEmpty a b $
      NumSpecInterval ((+) <$> x <*> a) ((+) <$> y <*> b)

subNumSpec :: (Ord n, Num n) => NumSpec n -> NumSpec n -> NumSpec n
subNumSpec (NumSpecInterval x y) (NumSpecInterval a b) =
  guardEmpty x y $
    guardEmpty a b $
      NumSpecInterval ((-) <$> x <*> b) ((-) <$> y <*> a)

multNumSpec :: (Ord n, Num n) => NumSpec n -> NumSpec n -> NumSpec n
multNumSpec (NumSpecInterval a b) (NumSpecInterval c d) =
  guardEmpty a b $
    guardEmpty c d $
      NumSpecInterval (unT (minimum s)) (unT (maximum s))
  where
    s = [multT (neg a) (neg c), multT (neg a) (pos d), multT (pos b) (neg c), multT (pos b) (pos d)]

negNumSpec :: Num n => NumSpec n -> NumSpec n
negNumSpec (NumSpecInterval lo hi) = NumSpecInterval (negate <$> hi) (negate <$> lo)

instance Num (NumSpec Integer) where
  (+) = addNumSpec
  (-) = subNumSpec
  (*) = multNumSpec
  negate = negNumSpec
  fromInteger n = NumSpecInterval (Just (fromInteger n)) (Just (fromInteger n))
  abs = error "No abs in the Num (NumSpec  Integer) instance"
  signum = error "No signum in the Num (NumSpec  Integer) instance"

-- ========================================================================
-- Helper functions for interval multiplication
--  (a,b) * (c,d) = (minimum s, maximum s) where s = [a * c, a * d, b * c, b * d]

-- | T is a sort of special version of Maybe, with two Nothings.
--   Given:: NumSpecInterval (Maybe n) (Maybe n) -> Numspec
--   We can't distinguish between the two Nothings in (NumSpecInterval Nothing Nothing)
--   But using (NumSpecInterval NegInf PosInf) we can, In fact we can make a total ordering on 'T'
--   So an ascending Sorted [T x] would all the NegInf on the left and all the PosInf on the right, with
--   the Ok's sorted in between. I.e. [NegInf, NegInf, Ok 3, Ok 6, Ok 12, Pos Inf]
data T x = NegInf | Ok x | PosInf
  deriving (Show)

instance Ord x => Eq (T x) where
  x == y = compare x y == EQ

instance Ord x => Ord (T x) where
  compare NegInf NegInf = EQ
  compare NegInf _ = LT
  compare (Ok _) NegInf = GT
  compare (Ok x) (Ok y) = compare x y
  compare (Ok _) PosInf = LT
  compare PosInf PosInf = EQ
  compare PosInf _ = GT

-- \| Conversion between (T x) and (Maybe x)
unT :: T x -> Maybe x
unT (Ok x) = Just x
unT _ = Nothing

-- | Use this on the lower bound. I.e. lo from pair (lo,hi)
neg :: Maybe x -> T x
neg Nothing = NegInf
neg (Just x) = Ok x

-- | Use this on the upper bound. I.e. hi from pair (lo,hi)
pos :: Maybe x -> T x
pos Nothing = PosInf
pos (Just x) = Ok x

-- | multiply two (T x), correctly handling the infinities NegInf and PosInf
multT :: Num x => T x -> T x -> T x
multT NegInf NegInf = PosInf
multT NegInf PosInf = NegInf
multT NegInf (Ok _) = NegInf
multT (Ok _) NegInf = NegInf
multT (Ok x) (Ok y) = Ok (x * y)
multT (Ok _) PosInf = PosInf
multT PosInf PosInf = PosInf
multT PosInf NegInf = NegInf
multT PosInf (Ok _) = PosInf

-- ========================================================================
-- We have
-- (1) Num Integer
-- (2) Num (NumSpec Integer)   And we need
-- (3) Num (Specification Integer)
-- We need this to implement the method cardinalTypeSpec of (HasSpec t).
-- cardinalTypeSpec :: HasSpec a => TypeSpec a -> Specification Integer
-- Basically for defining these two cases
-- cardinalTypeSpec (Cartesian x y) = (cardinality x) * (cardinality y)
-- cardinalTypeSpec (SumSpec leftspec rightspec) = (cardinality leftspec) + (cardinality rightspec)
-- So we define addSpecInt for (+)   and  multSpecInt for (*)

-- | What constraints we need to make HasSpec instance for a Haskell numeric type.
--   By abstracting over this, we can avoid making actual HasSpec instances until
--   all the requirements (HasSpec Bool, HasSpec(Sum a b)) have been met in
--   Constrained.TheKnot.
type Number n = (Num n, Enum n, TypeSpec n ~ NumSpec n, Num (NumSpec n), HasSpec n, Ord n)

addSpecInt ::
  Number n =>
  Specification n -> Specification n -> Specification n
addSpecInt x y = operateSpec " + " (+) (+) x y

subSpecInt ::
  Number n =>
  Specification n -> Specification n -> Specification n
subSpecInt x y = operateSpec " - " (-) (-) x y

multSpecInt ::
  Number n =>
  Specification n -> Specification n -> Specification n
multSpecInt x y = operateSpec " * " (*) (*) x y

-- | let 'n' be some numeric type, and 'f' and 'ft' be operations on 'n' and (TypeSpec n)
--   Then lift these operations from (TypeSpec n) to (Specification n)
--   Normally 'f' will be a (Num n) instance method (+,-,*) on n,
--   and 'ft' will be a a (Num (TypeSpec n)) instance method (+,-,*) on (TypeSpec n)
--   But this will work for any operations 'f' and 'ft' with the right types
operateSpec ::
  Number n =>
  String ->
  (n -> n -> n) ->
  (TypeSpec n -> TypeSpec n -> TypeSpec n) ->
  Specification n ->
  Specification n ->
  Specification n
operateSpec operator f ft (ExplainSpec es x) y = explainSpecOpt es $ operateSpec operator f ft x y
operateSpec operator f ft x (ExplainSpec es y) = explainSpecOpt es $ operateSpec operator f ft x y
operateSpec operator f ft x y = case (x, y) of
  (ErrorSpec xs, ErrorSpec ys) -> ErrorSpec (xs <> ys)
  (ErrorSpec xs, _) -> ErrorSpec xs
  (_, ErrorSpec ys) -> ErrorSpec ys
  (TrueSpec, _) -> TrueSpec
  (_, TrueSpec) -> TrueSpec
  (_, SuspendedSpec _ _) -> TrueSpec
  (SuspendedSpec _ _, _) -> TrueSpec
  (TypeSpec a bad1, TypeSpec b bad2) -> TypeSpec (ft a b) [f b1 b2 | b1 <- bad1, b2 <- bad2]
  (MemberSpec xs, MemberSpec ys) ->
    nubOrdMemberSpec
      (show x ++ operator ++ show y)
      [f x1 y1 | x1 <- NE.toList xs, y1 <- NE.toList ys]
  -- This block is all (MemberSpec{}, TypeSpec{}) with MemberSpec on the left
  (MemberSpec ys, TypeSpec (NumSpecInterval (Just i) (Just j)) bad) ->
    let xs = NE.toList ys
     in nubOrdMemberSpec
          (show x ++ operator ++ show y)
          [f x1 y1 | x1 <- xs, y1 <- [i .. j], not (elem y1 bad)]
  -- Somewhat loose spec here, but more accurate then TrueSpec, it is exact if 'xs' has one element (i.e. 'xs' = [i])
  (MemberSpec ys, TypeSpec (NumSpecInterval lo hi) bads) ->
    -- We use the specialized version of 'TypeSpec' 'typeSpecOpt'
    let xs = NE.toList ys
     in typeSpecOpt
          (NumSpecInterval (f (minimum xs) <$> lo) (f (maximum xs) <$> hi))
          [f x1 b | x1 <- xs, b <- bads]
  -- we flip the arguments, so we need to flip the functions as well
  (sleft, sright) -> operateSpec operator (\a b -> f b a) (\u v -> ft v u) sright sleft

-- | This is very liberal, since in lots of cases it returns TrueSpec.
--  for example all operations on SuspendedSpec, and certain
--  operations between TypeSpec and MemberSpec. Perhaps we should
--  remove it. Only the addSpec (+) and multSpec (*) methods are used.
--  But, it is kind of cool ...
--  In Fact we can use this to make Num(Specification n) instance for any 'n'.
--  But, only Integer is safe, because in all other types (+) and especially
--  (-) can lead to overflow or underflow failures.
instance Number Integer => Num (Specification Integer) where
  (+) = addSpecInt
  (-) = subSpecInt
  (*) = multSpecInt
  fromInteger n = TypeSpec (NumSpecInterval (Just n) (Just n)) []
  abs _ = TrueSpec
  signum _ = TrueSpec

-- ===========================================================================

-- | Put some (admittedly loose bounds) on the number of solutions that
--   'genFromTypeSpec' might return. For lots of types, there is no way to be very accurate.
--   Here we lift the HasSpec methods 'cardinalTrueSpec' and 'cardinalTypeSpec'
--   from (TypeSpec Integer) to (Specification Integer)
cardinality ::
  forall a. (Number Integer, HasSpec a) => Specification a -> Specification Integer
cardinality (ExplainSpec es s) = explainSpecOpt es (cardinality s)
cardinality TrueSpec = cardinalTrueSpec @a
cardinality (MemberSpec es) = equalSpec (toInteger $ length (nub (NE.toList es)))
cardinality ErrorSpec {} = equalSpec 0
cardinality (TypeSpec s cant) =
  subSpecInt
    (cardinalTypeSpec @a s)
    (equalSpec (toInteger $ length (nub $ filter (\c -> conformsTo @a c s) cant)))
cardinality SuspendedSpec {} = cardinalTrueSpec @a

-- | A generic function to use as an instance for the HasSpec method
--   cardinalTypeSpec :: HasSpec a => TypeSpec a -> Specification Integer
--   for types 'n' such that (TypeSpec n ~ NumSpec n)
cardinalNumSpec ::
  forall n. (Integral n, MaybeBounded n) => NumSpec n -> Specification Integer
cardinalNumSpec (NumSpecInterval (Just lo) (Just hi)) =
  if hi >= lo then MemberSpec (pure (toInteger hi - toInteger lo + 1)) else MemberSpec (pure 0)
cardinalNumSpec (NumSpecInterval Nothing (Just hi)) =
  case lowerBound @n of
    Just lo -> MemberSpec (pure (toInteger hi - toInteger lo))
    Nothing -> TrueSpec
cardinalNumSpec (NumSpecInterval (Just lo) Nothing) =
  case upperBound @n of
    Just hi -> MemberSpec (pure (toInteger hi - toInteger lo))
    Nothing -> TrueSpec
cardinalNumSpec (NumSpecInterval Nothing Nothing) = TrueSpec

-- ====================================================================
-- Now the operations on Numbers

class (Num a, HasSpec a) => NumLike a where
  subtractSpec :: a -> TypeSpec a -> Specification a
  default subtractSpec ::
    ( TypeSpec a ~ TypeSpec (SimpleRep a)
    , HasSimpleRep a
    , NumLike (SimpleRep a)
    ) =>
    a ->
    TypeSpec a ->
    Specification a
  subtractSpec a ts = fromSimpleRepSpec $ subtractSpec (toSimpleRep a) ts

  negateSpec :: TypeSpec a -> Specification a
  default negateSpec ::
    ( TypeSpec a ~ TypeSpec (SimpleRep a)
    , HasSimpleRep a
    , NumLike (SimpleRep a)
    ) =>
    TypeSpec a ->
    Specification a
  negateSpec = fromSimpleRepSpec . negateSpec @(SimpleRep a)

  safeSubtract :: a -> a -> Maybe a
  default safeSubtract ::
    (HasSimpleRep a, NumLike (SimpleRep a)) =>
    a ->
    a ->
    Maybe a
  safeSubtract a b = fromSimpleRep <$> safeSubtract @(SimpleRep a) (toSimpleRep a) (toSimpleRep b)

data IntW (s :: Symbol) (as :: [Type]) b where
  AddW :: NumLike a => IntW "addFn" '[a, a] a
  NegateW :: NumLike a => IntW "negateFn" '[a] a

deriving instance Eq (IntW s dom rng)

instance Show (IntW s d r) where
  show AddW = "addFn"
  show NegateW = "negateFn"

instance Semantics IntW where
  semantics AddW = (+)
  semantics NegateW = negate

instance Syntax IntW -- Use default methods

type Numeric a = (HasSpec a, Ord a, Num a, TypeSpec a ~ NumSpec a, MaybeBounded a)

instance {-# OVERLAPPABLE #-} Numeric a => NumLike a where
  subtractSpec a ts@(NumSpecInterval ml mu)
    | Just u <- mu
    , a > 0
    , Nothing <- safeSubtract a u =
        ErrorSpec $
          NE.fromList
            [ "Underflow in subtractSpec (" ++ showType @a ++ "):"
            , "  a = " ++ show a
            , "  ts = " ++ show ts
            ]
    | Just l <- ml
    , a < 0
    , Nothing <- safeSubtract a l =
        ErrorSpec $
          NE.fromList
            [ "Overflow in subtractSpec (" ++ showType @a ++ "):"
            , "  a = " ++ show a
            , "  ts = " ++ show ts
            ]
    | otherwise = typeSpec $ NumSpecInterval (safeSub a <$> ml) (safeSub a <$> mu)
    where
      safeSub :: a -> a -> a
      safeSub a1 x
        | Just r <- safeSubtract a1 x = r
        | a1 < 0 = fromJust upperBound
        | otherwise = fromJust lowerBound
  negateSpec (NumSpecInterval ml mu) = typeSpec $ NumSpecInterval (negate <$> mu) (negate <$> ml)

  safeSubtract a x
    | a > 0
    , Just lb <- lowerBound
    , lb + a > x =
        Nothing
    | a < 0
    , Just ub <- upperBound
    , ub + a < x =
        Nothing
    | otherwise = Just $ x - a

instance NumLike a => Num (Term a) where
  (+) = addFn
  negate = negateFn
  fromInteger = Lit . fromInteger
  (*) = error "(*) not implemented for Term Fn Int"
  abs = error "abs not implemented for Term Fn Int"
  signum = error "signum not implemented for Term Fn Int"

-- | Just a note that these instances won't work until we are in a context where
--   there is a HasSpec instance of 'a', which (NumLike a) demands.
--   This happens in Constrained.Experiment.TheKnot
instance NumLike a => Logic "addFn" IntW '[a, a] a where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context AddW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App AddW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context AddW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App AddW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context AddW (i :|> HOLE :<> End)) spec =
    propagate (Context AddW (HOLE :<> i :<| End)) spec
  propagate (Context AddW (HOLE :<> i :<| End)) (TypeSpec ts cant) =
    subtractSpec i ts <> notMemberSpec (mapMaybe (safeSubtract i) cant)
  propagate (Context AddW (HOLE :<> i :<| End)) (MemberSpec es) =
    memberSpecList
      (nub $ mapMaybe (safeSubtract i) (NE.toList es))
      ( NE.fromList
          [ "propagateSpecFn on (" ++ show i ++ " +. HOLE)"
          , "The Spec is a MemberSpec = " ++ show (MemberSpec es)
          , "We can't safely subtract " ++ show i ++ " from any choice in the MemberSpec."
          ]
      )
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for AddW with wrong number of arguments. " ++ show ctx)

addFn :: forall a. NumLike a => Term a -> Term a -> Term a
addFn = appTerm AddW

instance NumLike a => Logic "negateFn" IntW '[a] a where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context NegateW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App NegateW (v' :> Nil)) (v :-> ps)
  propagate (Context NegateW (HOLE :<> End)) (TypeSpec ts cant) =
    negateSpec @a ts <> notMemberSpec (map negate cant)
  propagate (Context NegateW (HOLE :<> End)) (MemberSpec es) =
    MemberSpec $ NE.nub $ fmap negate es
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for NegateW with wrong number of arguments. " ++ show ctx)

  mapTypeSpec NegateW (ts) = negateSpec ts

negateFn :: forall a. NumLike a => Term a -> Term a
negateFn = appTerm NegateW

-- ============================================================
