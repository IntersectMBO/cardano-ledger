{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module NumSpecExperiment where

import Constrained.BaseExperiment
import Constrained.BaseExperiment(FunctionSymbol(witness))
import Constrained.GenericExperiment
import Constrained.SyntaxExperiment


import Constrained.Core
import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable
import Data.Kind
import Data.List (intersect, isPrefixOf, isSuffixOf, nub, partition, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Semigroup (Any (..), Max (..), getAll, getMax, sconcat)
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Typeable
import Data.Word
import GHC.Generics
import GHC.Int
import GHC.Natural
import GHC.Real
import GHC.Stack
import GHC.TypeLits
import Prettyprinter hiding (cat)
import System.Random
import System.Random.Stateful
import Test.QuickCheck hiding (Args, Fun, forAll,Witness,witness)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Constrained.Core
import Constrained.Env
import Constrained.GenT
import Constrained.Graph hiding (dependency, irreflexiveDependencyOn, noDependencies)
import qualified Constrained.Graph as Graph
import Constrained.List
import Constrained.SumList (Cost (..), Solution (..), pickAll)
import Constrained.Univ
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE


-- ====================================
-- OrdLike
-- ===================================

data OrdWitness (sym :: Symbol) (dom :: [Type]) (rng :: Type) where 
  LessOrEqualW :: (Ord a, OrdLike a) => OrdWitness "<=." '[a,a] Bool
  LessW :: (Ord a, OrdLike a) => OrdWitness "<=." '[a,a] Bool

class HasSpec a => OrdLike a where
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
instance {-# OVERLAPPABLE #-} (HasSpec a, MaybeBounded a, Num a, TypeSpec a ~ NumSpec a) => OrdLike a where
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
-- Numbers ----------------------------------------------------------------
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
      | l > u -> genError1 ("bad interval: " ++ show l ++ " " ++ show u)
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
  ( Ord n
  , OrdLike n
  ) =>
  Term n ->
  NumSpec n ->
  Pred 
toPredsNumSpec v (NumSpecInterval ml mu) =
  fold $
    [assert $ Lit l <=. v | l <- maybeToList ml]
      ++ [assert $ v <=. Lit u | u <- maybeToList mu]

-- STUB
(<=.) :: OrdLike n => Term n -> Term n -> Term Bool
(<=.) = undefined      

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

-- =======================================================
-- All he Foldy class instances are intimately tied to 
-- Numbers. But that is not required, but this is a 
-- convenient place to put the code.
-- =======================================================    

data FunWitness (sym :: Symbol) (dom :: [Type]) (rng :: Type) where 
  IdW :: forall a. FunWitness "id_" '[a] a
  ComposeW :: forall s1 t1 s2 t2 a b c . 
              ( FunctionSymbol s1 t1 '[b] c
              , FunctionSymbol s2 t2 '[a] b ) =>
              t1 s1 '[b] c -> 
              t2 s2 '[a] b ->
              FunWitness "composeFn" '[a] c
  FlipW :: forall sym t a b c . 
           FunctionSymbol sym t '[a,b] c => 
           t sym '[a,b] c ->  FunWitness "flip_" '[b,a] c

instance KnownSymbol s => Show (FunWitness s dom rng) where
  show IdW = "IdW[id_]"
  show (FlipW f) = "(FlipW "++show f++")[flip_]"
  show (ComposeW x y) = "(ComposeW "++show x++" "++show y++")[composeFn]"

instance Eq (FunWitness s dom rng) where
  IdW == IdW = True
  FlipW t1 == FlipW t2 = compareWit t1 t2
  ComposeW f f' == ComposeW g g' = compareWit f g && compareWit g g'

compareWit :: 
  forall s1 t1 bs1 c1 s2 t2 bs2 c2. 
         (FunctionSymbol s1 t1 bs1 c1, FunctionSymbol s2 t2 bs2 c2) => 
         t1 s1 bs1 c1 -> t2 s2 bs2 c2 -> Bool
compareWit x y = case (eqT @t1 @t2, eqT @s1 @s2, eqT @bs1 @bs2, eqT @c1 @c2) of
    (Just Refl, Just Refl, Just Refl, Just Refl) -> x==y
    _ -> False

funSem :: FunWitness sym dom rng -> FunTy dom rng
funSem IdW = id
funSem (ComposeW f g) = (\ a -> semantics f (semantics g a))
funSem (FlipW f) = flip (semantics f)

instance (HasSpec a) => FunctionSymbol "id_" FunWitness '[a] a where
    witness = IdW
    type Wit "id_" = FunWitness
    semantics = funSem
    propagate ctxt (ExplainSpec [] s) = propagate ctxt s
    propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
    propagate ctxt (ErrorSpec msgs) = ErrorSpec msgs
    propagate (Context IdW (HOLE End)) spec = spec
    propagate ctxt _ = ErrorSpec (NE.fromList["IdW (id_)","Unreachable context, too many args",show ctxt])


instance (HasSpec b, HasSpec a,forall sym t. FunctionSymbol sym t '[a,b] c,All Typeable [a,b,c]) =>
          FunctionSymbol "flip_" FunWitness '[b,a] c  where
    witness =  undefined -- FlipW $ getwitness @sym @t @('[a,b]) @c
    type Wit "flip_" = FunWitness
    semantics = funSem
    propagate ctxt (ExplainSpec [] s) = propagate ctxt s
    propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
    propagate ctxt (ErrorSpec msgs) = ErrorSpec msgs
    propagate (Context (FlipW f) (HOLE (v :<| End))) spec = propagate (Context f (v :>| HOLE End)) spec
    propagate (Context (FlipW f) (v :>| (HOLE End))) spec = propagate (Context f (HOLE $ v :<| End)) spec
    propagate ctxt _ = ErrorSpec (NE.fromList["FlipW (flip_)","Unreachable context, too many args",show ctxt])

getwitness :: forall sym t dom rng . FunctionSymbol sym t dom rng => t sym dom rng
getwitness = witness @sym @t @dom @rng

instance ( HasSpec a,HasSpec c,All Typeable [a,c]
         , forall s1 t1 b. FunctionSymbol s1 t1 '[b] c
         , forall s2 t2 b. FunctionSymbol s2 t2 '[a] b ) => 
        FunctionSymbol "composeFn" FunWitness '[a] c where
    witness =  undefined -- FlipW $ getwitness @sym @t @('[a,b]) @c
    type Wit "composeFn" = FunWitness  
    semantics = funSem
    propagate ctxt (ExplainSpec [] s) = propagate ctxt s
    propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
    propagate ctxt (ErrorSpec msgs) = ErrorSpec msgs  
    propagate (Context (ComposeW f g) (HOLE End)) spec = 
        propagate (Context g (HOLE End)) $ propagate (Context f (HOLE End)) spec
    propagate ctxt _ = ErrorSpec (NE.fromList["ComposeW (composeFn)","Unreachable context, too many args",show ctxt])    


{-

class HasSpec a => Foldy a where
  genList ::
    (MonadGenError m) => Specification a -> Specification a -> GenT m [a]
  theAddFn :: '[a, a] a
  theZero :: a
  genSizedList ::
    (MonadGenError m) =>
    Specification Integer -> Specification a -> Specification a -> GenT m [a]
  noNegativeValues :: Bool

adds :: forall a. Foldy a => [a] -> a
adds = foldr (sem $ theAddFn @fn) (theZero @fn)

data FoldSpec a where
  NoFold :: FoldSpec a
  FoldSpec ::
    forall b a.
    ( HasSpec a
    , HasSpec b
    , Foldy b
    , Member (ListFn fn) fn
    , BaseUniverse fn
    ) =>
    '[a] b ->
    Specification b ->
    FoldSpec a

instance {-# OVERLAPPABLE #-} (Arbitrary (TypeSpec a), Foldy a, BaseUniverse fn) => Arbitrary (FoldSpec a) where
  arbitrary = oneof [FoldSpec idFn <$> arbitrary, pure NoFold]
  shrink NoFold = []
  shrink (FoldSpec (extractFn @(FunFn fn) @fn -> Just Id) spec) = FoldSpec idFn <$> shrink spec
  shrink FoldSpec {} = [NoFold]

preMapFoldSpec :: HasSpec a => '[a] b -> FoldSpec b -> FoldSpec a
preMapFoldSpec _ NoFold = NoFold
preMapFoldSpec f (FoldSpec g s) = FoldSpec (composeFn g f) s

combineFoldSpec :: FoldSpec a -> FoldSpec a -> Either [String] (FoldSpec a)
combineFoldSpec NoFold s = pure s
combineFoldSpec s NoFold = pure s
combineFoldSpec (FoldSpec (f :: as b) s) (FoldSpec (f' :: fn' as' b') s')
  | Just Refl <- eqT @b @b'
  , Just Refl <- eqT @fn @fn'
  , f == f' =
      pure $ FoldSpec f (s <> s')
  | otherwise =
      Left ["Can't combine fold specs on different functions", "  " ++ show f, "  " ++ show f']

conformsToFoldSpec :: forall a. [a] -> FoldSpec a -> Bool
conformsToFoldSpec _ NoFold = True
conformsToFoldSpec xs (FoldSpec f s) = adds @fn (map (sem f) xs) `conformsToSpec` s

toPredsFoldSpec :: forall a. BaseUniverse => Term [a] -> FoldSpec a -> Pred fn
toPredsFoldSpec _ NoFold = TruePred
toPredsFoldSpec x (FoldSpec sspec) =
  satisfies (app (foldMapFn fn) x) sspec
-}  
