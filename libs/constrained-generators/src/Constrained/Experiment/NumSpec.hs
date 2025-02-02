{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}   -- Random Natural, Arbitrary Natural, Uniform Natural

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.Experiment.NumSpec where

import Constrained.GenericExperiment
import Constrained.WitnessExperiment
import Constrained.BaseExperiment
import Constrained.SyntaxExperiment(app)
import Constrained.ConformanceExperiment(satisfies,conformsToSpec)

import Prettyprinter
import Data.String(fromString)
import Data.Semigroup (getSum)
import qualified Data.Semigroup as Semigroup
import Control.Arrow (first)
import Control.Applicative((<|>))
-- import Data.Typeable(typeOf)
import Data.Typeable(typeOf,Typeable,Proxy(..))
import Constrained.GenT(MonadGenError(..),GenT,pureGen,sizeT,frequencyT)
import Constrained.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.List(nub)
import Data.Word
import GHC.Int
import GHC.Natural
import GHC.Real
import Test.QuickCheck(Arbitrary(shrink,arbitrary),frequency,choose)
import System.Random.Stateful(Random(..),Uniform(..))
import qualified Data.Set as Set
import Data.Kind
import GHC.TypeLits
-- import GHC.TypeLits(SSymbol,KnownSymbol,Symbol,symbolSing,symbolVal,pattern SSymbol,KnownNat)
import Constrained.Core(unionWithMaybe,Evidence(..))
import Data.Maybe(maybeToList)
import Data.Foldable(fold)
import Data.Maybe(fromMaybe)


-- STUBS


genFromSpecT :: Specification a -> GenT m a
genFromSpecT = undefined
shrinkWithSpec :: Specification a -> a -> [a]
shrinkWithSpec = undefined


-- ====================================================================
-- NumOrdW  witnesses for comparison operations (<=. and <.) on numbers
-- The other operations are defined in terms of these.
-- =====================================================================  

data NumOrdW (c :: Constraint) (sym :: Symbol) (dom :: [Type]) (rng :: Type) where 
  LessOrEqualW :: (OrdLike a) => NumOrdW (OrdLike a) "<=." '[a,a] Bool
  LessW :: (OrdLike a) => NumOrdW (OrdLike a) "<." '[a,a] Bool

instance Eq (NumOrdW c s ds r) where
  LessOrEqualW == LessOrEqualW  = True
  LessW == LessW  = True

instance Show (NumOrdW c s ds r) where
  show LessOrEqualW = "<=."
  show LessW = "<."

instance Witness NumOrdW
  where
  semantics LessOrEqualW = (<=)
  semantics LessW = (<)
  getevidence (LessOrEqualW @a) = Evidence @(OrdLike a)
  getevidence (LessW @a) = Evidence @(OrdLike a)

instance (Typeable a,HasSpec Bool,HasSpec a) => 
         FunctionSymbol (OrdLike a) "<=." NumOrdW '[a,a] Bool where
    witness = "LessOrEqualW[<=.]"    
    simplepropagate (Context LessOrEqualW (l :>| (HOLE End))) spec = Right $
      caseBoolSpec spec $ \case
        True ->  geqSpec l
        False -> ltSpec l    
    simplepropagate (Context LessOrEqualW (HOLE (l :<| End))) spec = Right $
      caseBoolSpec spec $ \case
        True -> leqSpec l
        False -> gtSpec l
    simplepropagate ctx _ = Left (NE.fromList["LessOrEqual[<=.]","Unreachable context, too many args",show ctx])        

infixr 4 <=.
(<=.) :: forall a . (HasSpec Bool,OrdLike a) => Term a -> Term a -> Term Bool
(<=.) = appTerm LessOrEqualW

instance (Typeable a,HasSpec a) => 
         FunctionSymbol (OrdLike a) "<." NumOrdW '[a,a] Bool where
    witness = "LessW[<.]"    
    simplepropagate (Context LessW (l :>| (HOLE End))) spec = Right $
      caseBoolSpec spec $ \case
        True -> gtSpec l
        False -> leqSpec l   
    simplepropagate (Context LessW (HOLE (l :<| End))) spec = Right $
      caseBoolSpec spec $ \case
        True -> ltSpec l
        False -> geqSpec l
    simplepropagate ctx _ = Left (NE.fromList["LessOrEqual[<=.]","Unreachable context, too many args",show ctx])  

infixr 4 <.
(<.) :: forall a . OrdLike a => Term a -> Term a -> Term Bool
(<.) = appTerm LessW

-- =============================================
-- OrdLike. Ord for Numbers in the Logic
-- =============================================

class (Ord a,HasSpec a,Num a,TypeSpec a ~ NumSpec a,MaybeBounded a) => OrdLike a where
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
      | l > u -> genError (pure("bad interval: " ++ show l ++ " " ++ show u))
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

toPredsNumSpec ::
  OrdLike n =>
  Term n ->
  NumSpec n ->
  Pred 
toPredsNumSpec v (NumSpecInterval ml mu) =
  fold $
    [assert $ Lit l <=. v | l <- maybeToList ml]
      ++ [assert $ v <=. Lit u | u <- maybeToList mu]


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

-- ===================================================
-- HasSpec instances for many kinds of numbers
-- ===================================================    

instance HasSpec Int where
  type TypeSpec Int = NumSpec Int
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Integer where
  type TypeSpec Integer = NumSpec Integer
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec (Ratio Integer) where
  type TypeSpec (Ratio Integer) = NumSpec (Ratio Integer)
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec _ = TrueSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Natural where
  type TypeSpec Natural = NumSpec Natural
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec (NumSpecInterval (Just lo) (Just hi)) =
    if hi >= lo
      then MemberSpec (pure (fromIntegral @Natural @Integer (hi - lo + 1)))
      else MemberSpec (pure 0)
  cardinalTypeSpec (NumSpecInterval Nothing (Just hi)) =
    MemberSpec (pure (fromIntegral @Natural @Integer hi + 1))
  cardinalTypeSpec _ = TrueSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Word8 where
  type TypeSpec Word8 = NumSpec Word8
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  typeSpecOpt = notInNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Word16 where
  type TypeSpec Word16 = NumSpec Word16
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Word32 where
  type TypeSpec Word32 = NumSpec Word32
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Word64 where
  type TypeSpec Word64 = NumSpec Word64
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Int8 where
  type TypeSpec Int8 = NumSpec Int8
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Int16 where
  type TypeSpec Int16 = NumSpec Int16
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Int32 where
  type TypeSpec Int32 = NumSpec Int32
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Int64 where
  type TypeSpec Int64 = NumSpec Int64
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Float where
  type TypeSpec Float = NumSpec Float
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec _ = TrueSpec
  guardTypeSpec = guardNumSpec

-- ==========================================================================
-- Num n => (NumSpec n) can support operation of Num as interval arithmetic. 
-- So we will make a (Num (NumSpec Integer)) instance. We won't make other 
-- instances, because  they would be subject to overflow.
-- Given operator ☉, then (a,b) ☉ (c,d) = (minimum s, maximum s) where s = [a ☉ c, a ☉ d, b ☉ c, b ☉ d]
-- There are simpler rules for (+) and (-), but for (*) we need to use the general rule.
-- ==========================================================================

guardEmpty :: (Ord n, Num n) => Maybe n -> Maybe n -> NumSpec  n -> NumSpec  n
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

multNumSpec :: (Ord n, Num n) => NumSpec n -> NumSpec n -> NumSpec  n
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

  -- | Conversion between (T x) and (Maybe x)
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
-- cardinalTypeSpec :: HasSpec fn a => TypeSpec fn a -> Specification fn Integer
-- Basically for defining these two cases
-- cardinalTypeSpec (Cartesian x y) = (cardinality x) * (cardinality y)
-- cardinalTypeSpec (SumSpec leftspec rightspec) = (cardinality leftspec) + (cardinality rightspec)
-- So we define addSpecInt for (+)   and  multSpecInt for (*)

addSpecInt :: 
  Specification Integer -> Specification Integer -> Specification Integer
addSpecInt x y = operateSpec " + " (+) (+) x y

subSpecInt :: 
  Specification Integer -> Specification Integer -> Specification Integer
subSpecInt x y = operateSpec " - " (-) (-) x y

multSpecInt :: 
  Specification Integer -> Specification Integer -> Specification Integer
multSpecInt x y = operateSpec " * " (*) (*) x y

-- | let 'n' be some numeric type, and 'f' and 'ft' be operations on 'n' and (TypeSpec fn n)
--   Then lift these operations from (TypeSpec fn n) to (Specification fn n)
--   Normally 'f' will be a (Num n) instance method (+,-,*) on n,
--   and 'ft' will be a a (Num (TypeSpec fn n)) instance method (+,-,*) on (TypeSpec fn n)
--   But this will work for any operations 'f' and 'ft' with the right types
operateSpec ::
  (TypeSpec n ~ NumSpec n, Enum n, Ord n, HasSpec n) =>
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
instance Num (Specification Integer) where
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
--   from (TypeSpec fn Integer) to (Specification fn Integer)
cardinality ::
  forall a. HasSpec a => Specification a -> Specification Integer
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

-- =======================================================
{-
instance HasSimpleRep Bool where type SimpleRep Bool = Sum () ()
instance HasSpec ()=> HasSpec Bool where
  shrinkWithTypeSpec _ = shrink
  cardinalTypeSpec (SumSpec _ a b) =
    MemberSpec (NE.fromList [0, 1, 2]) <> addSpecInt (cardinality a) (cardinality b)
  cardinalTrueSpec = MemberSpec (pure 2)   
-}  

-- ================================================================
-- Now that we can talk abput cardinality we can make some
-- HasSpec instances, which by definition, need to know about
-- cardinality, which is only just defined in this file.


guardSumSpec :: forall a b.
  (HasSpec a, HasSpec b, KnownNat (CountCases b)) =>
  [String] ->
  SumSpec a b ->
  Specification (Sum a b)
guardSumSpec msgs s@(SumSpecRaw tString _ sa sb)
  | isErrorLike sa
  , isErrorLike sb =
      ErrorSpec $
        NE.fromList $
          msgs ++ ["All branches in a caseOn" ++ sumType tString ++ " simplify to False.", show s]
  | otherwise = typeSpec s
-- | The Specification for Sums.
data SumSpec a b
  = SumSpecRaw
      (Maybe String) -- A String which is the type of arg in (caseOn arg branch1 .. branchN)
      (Maybe (Int, Int))
      (Specification a)
      (Specification b)

pattern SumSpec ::
  (Maybe (Int, Int)) -> (Specification a) -> (Specification b) -> SumSpec a b
pattern SumSpec a b c <- SumSpecRaw _ a b c
  where
    SumSpec a b c = SumSpecRaw Nothing a b c

{-# COMPLETE SumSpec #-}
{-# COMPLETE SumSpecRaw #-} 

instance (KnownNat (CountCases b), HasSpec a, HasSpec b) => Show (SumSpec a b) where
  show sumspec@(SumSpecRaw tstring hint l r) = case alternateShow @(Sum a b) sumspec of
    (BinaryShow _ ps) -> show $ parens (fromString ("SumSpec" ++ sumType tstring) /> vsep ps)
    NonBinary ->
      "(SumSpec"
        ++ sumType tstring
        ++ show (sumWeightL hint)
        ++ " ("
        ++ show l
        ++ ") "
        ++ show (sumWeightR hint)
        ++ " ("
        ++ show r
        ++ "))"


combTypeName :: Maybe String -> Maybe String -> Maybe String
combTypeName (Just x) (Just y) =
  if x == y then Just x else Just ("(" ++ x ++ " | " ++ y ++ ")")
combTypeName (Just x) Nothing = Just x
combTypeName Nothing (Just x) = Just x
combTypeName Nothing Nothing = Nothing

instance (HasSpec a, HasSpec b) => Semigroup (SumSpec a b) where
  SumSpecRaw t h sa sb <> SumSpecRaw t' h' sa' sb' =
    SumSpecRaw (combTypeName t t') (unionWithMaybe mergeH h h') (sa <> sa') (sb <> sb')
    where
      -- TODO: think more carefully about this, now weights like 2 2 and 10 15 give more weight to 10 15
      -- than would be the case if you had 2 2 and 2 3. But on the other hand this approach is associative
      -- whereas actually averaging the ratios is not. One could keep a list. Future work.
      mergeH (fA, fB) (fA', fB') = (fA + fA', fB + fB')

instance forall a b. (HasSpec a, HasSpec b, KnownNat (CountCases b)) => Monoid (SumSpec a b) where
  mempty = SumSpec Nothing mempty mempty

type family CountCases a where
  CountCases (Sum a b) = 1 + CountCases b
  CountCases _ = 1

countCases :: forall a. KnownNat (CountCases a) => Int
countCases = fromIntegral (natVal @(CountCases a) Proxy)


totalWeight :: List (Weighted f) as -> Maybe Int
totalWeight = fmap getSum . foldMapList (fmap Semigroup.Sum . weight)



instance (HasSpec a, HasSpec b, KnownNat (CountCases b)) => HasSpec (Sum a b) where
  type TypeSpec (Sum a b) = SumSpec a b

  type Prerequisites (Sum a b) = (HasSpec a, HasSpec b)

  emptySpec = mempty

  combineSpec s s' = guardSumSpec ["When combining SumSpecs", "  " ++ show s, "  " ++ show s'] (s <> s')

  conformsTo (SumLeft a) (SumSpec _ sa _) = conformsToSpec a sa
  conformsTo (SumRight b) (SumSpec _ _ sb) = conformsToSpec b sb

  genFromTypeSpec (SumSpec h sa sb)
    | emptyA, emptyB = genError $ pure("genFromTypeSpec @SumSpec: empty")
    | emptyA = SumRight <$> genFromSpecT sb
    | emptyB = SumLeft <$> genFromSpecT sa
    | fA == 0, fB == 0 = genError $ pure("All frequencies 0")
    | otherwise =
        frequencyT
          [ (fA, SumLeft <$> genFromSpecT sa)
          , (fB, SumRight <$> genFromSpecT sb)
          ]
    where
      (max 0 -> fA, max 0 -> fB) = fromMaybe (1, countCases @b) h
      emptyA = isErrorLike sa
      emptyB = isErrorLike sb

  shrinkWithTypeSpec (SumSpec _ sa _) (SumLeft a) = SumLeft <$> shrinkWithSpec sa a
  shrinkWithTypeSpec (SumSpec _ _ sb) (SumRight b) = SumRight <$> shrinkWithSpec sb b

  toPreds ct (SumSpec h sa sb) =
    Case
      ct
      ( (Weighted (fst <$> h) $ bind $ \a -> satisfies a sa)
          :> (Weighted (snd <$> h) $ bind $ \b -> satisfies b sb)
          :> Nil
      )

  cardinalTypeSpec (SumSpec _ leftspec rightspec) = addSpecInt (cardinality leftspec) (cardinality rightspec)

  typeSpecHasError (SumSpec _ x y) =
    case (isErrorLike x, isErrorLike y) of
      (True, True) -> Just $ (errorLikeMessage x <> errorLikeMessage y)
      _ -> Nothing

  alternateShow (SumSpec h left right@(TypeSpec r [])) =
    case alternateShow @b r of
      (BinaryShow "SumSpec" ps) -> BinaryShow "SumSpec" ("|" <+> sumWeightL h <+> viaShow left : ps)
      (BinaryShow "Cartesian" ps) ->
        BinaryShow "SumSpec" ("|" <+> sumWeightL h <+> viaShow left : [parens ("Cartesian" /> vsep ps)])
      _ ->
        BinaryShow "SumSpec" ["|" <+> sumWeightL h <+> viaShow left, "|" <+> sumWeightR h <+> viaShow right]
  alternateShow (SumSpec h left right) =
    BinaryShow "SumSpec" ["|" <+> sumWeightL h <+> viaShow left, "|" <+> sumWeightR h <+> viaShow right]

sumType :: (Maybe String) -> String
sumType Nothing = ""
sumType (Just x) = " type=" ++ x

sumWeightL, sumWeightR :: Maybe (Int, Int) -> Doc a
sumWeightL Nothing = "1"
sumWeightL (Just (x, _)) = fromString (show x)
sumWeightR Nothing = "1"
sumWeightR (Just (_, x)) = fromString (show x)

instance (Arbitrary (Specification a), Arbitrary (Specification b)) => Arbitrary (SumSpec a b) where
  arbitrary =
    SumSpec
      <$> frequency
        [ (3, pure Nothing)
        , (10, Just <$> ((,) <$> choose (0, 100) <*> choose (0, 100)))
        , (1, arbitrary)
        ]
      <*> arbitrary
      <*> arbitrary
  shrink (SumSpec h a b) = [SumSpec h' a' b' | (h', a', b') <- shrink (h, a, b)]


mapSpec ::
  forall a b c sym t.
  ( HasSpec a
  , FunctionSymbol c sym t '[a] b
  ) =>
  t c sym '[a] b ->
  Specification a ->
  Specification b
mapSpec f (ExplainSpec es s) = explainSpecOpt es (mapSpec f s)
mapSpec f TrueSpec = mapTypeSpec @c @sym @t @'[a] @b f (emptySpec @a)
mapSpec _ (ErrorSpec err) = ErrorSpec err
mapSpec f (MemberSpec as) = MemberSpec $ NE.nub $ fmap (semantics f) as
mapSpec f (SuspendedSpec x p) =
  constrained $ \x' ->
    Exists (\_ -> fatalError (pure "mapSpec")) (x :-> fold [assert $ x' ==. app f (V x), p])
mapSpec f (TypeSpec ts cant) = mapTypeSpec @c @sym @t @'[a] @b f ts <> notMemberSpec (map (semantics f) cant)


instance HasSimpleRep Bool
instance (HasSpec ()) => HasSpec Bool where
  shrinkWithTypeSpec _ = shrink
  cardinalTypeSpec (SumSpec _ a b) =
    MemberSpec (NE.fromList [0, 1, 2]) <> addSpecInt (cardinality a) (cardinality b)
  cardinalTrueSpec = MemberSpec (pure 2)

caseBoolSpec :: HasSpec a => Specification Bool -> (Bool -> Specification a) -> Specification a
caseBoolSpec spec cont = case possibleValues spec of
     [] -> ErrorSpec (NE.fromList ["No possible values in caseBoolSpec"])
     [b] -> cont b
     _ -> mempty
  -- where possibleValues s = filter (flip conformsToSpec (simplifySpec s)) [True, False]  
  -- This will always get the same result, and probably faster since running 2
  -- conformsToSpec on True and False takes less time than simplifying the spec.
  where possibleValues s = filter (flip conformsToSpec s) [True, False]  


instance FunctionSymbol () "not_" BaseW '[Bool] Bool where
    witness = "NotW[not_]"
    
    simplepropagate (Context NotW (HOLE End)) spec = Right $ caseBoolSpec spec (equalSpec . not)
    simplepropagate ctx _ = Left (NE.fromList["NotW (not_)","Unreachable context, too many args",show ctx])

not_ :: Term Bool -> Term Bool
not_ = appTerm NotW

instance HasSpec a => FunctionSymbol (Eq a) "==."  BaseW '[a, a] Bool where
    witness = "EqualW[==.]"
    
    simplepropagate (Context EqualW (HOLE (a :<| End)))  spec =  
      Right $ caseBoolSpec spec $ \case {True -> equalSpec a; False -> notEqualSpec a}
    simplepropagate (Context EqualW (a :>| (HOLE End))) spec = 
      Right $ caseBoolSpec spec $ \case { True -> equalSpec a; False -> notEqualSpec a}
    simplepropagate ctx _ = Left (NE.fromList["EqualW ( ==. )","Unreachable context, too many args",show ctx])      
  
(==.) :: forall a. HasSpec a => Term a -> Term a -> Term Bool
(==.) = appTerm EqualW

