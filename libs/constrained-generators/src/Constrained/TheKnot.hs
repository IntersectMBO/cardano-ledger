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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-name-shadowing #-}

-- | All the things that are mutually recursive.
module Constrained.TheKnot where

import Constrained.Base (
  AppRequires,
  BinaryShow (..),
  Binder (..),
  Forallable (..),
  Fun (..),
  HOLE (..),
  HasGenHint (..),
  HasSpec (..),
  IsPred,
  Logic (..),
  Pred (..),
  Semantics (..),
  Specification (..),
  Syntax (..),
  Term (..),
  TypeSpec,
  Weighted (..),
  WithPrec (..),
  addToErrorSpec,
  appFun,
  appTerm,
  bind,
  cardinalTypeSpec,
  combineSpec,
  conformsTo,
  constrained,
  emptySpec,
  equalSpec,
  errorLikeMessage,
  explainSpec,
  explainSpecOpt,
  flipCtx,
  fromGESpec,
  fromSimpleRepSpec,
  genFromTypeSpec,
  getWitness,
  guardTypeSpec,
  isErrorLike,
  mapWeighted,
  memberSpecList,
  notEqualSpec,
  notMemberSpec,
  parensIf,
  prettyPrec,
  propagateSpec,
  sameFunSym,
  short,
  showType,
  shrinkWithTypeSpec,
  toCtx,
  toPred,
  toPreds,
  traverseWeighted,
  typeSpec,
  typeSpecOpt,
  vsep',
  (/>),
  pattern FromGeneric,
  pattern Unary,
  pattern (:<:),
  pattern (:>:),
 )
import Constrained.SumList

import Constrained.Conformance (
  checkPred,
  checkPredsE,
  conformsToSpec,
  satisfies,
 )
import Constrained.Core (
  Evidence (..),
  Value (..),
  Var (..),
  eqVar,
  freshen,
  unValue,
  unionWithMaybe,
 )
import Constrained.Env (
  Env,
  extendEnv,
  findEnv,
  lookupEnv,
  singletonEnv,
 )
import Constrained.GenT (
  GE (..),
  GenT,
  MonadGenError (..),
  catMessageList,
  catMessages,
  catchGen,
  errorGE,
  explain,
  fatalError,
  frequencyT,
  genError,
  genFromGenT,
  getMode,
  inspect,
  listFromGE,
  listOfT,
  listOfUntilLenT,
  pureGen,
  push,
  pushGE,
  runGE,
  suchThatT,
 )
import Constrained.Generic (
  HasSimpleRep,
  Prod (..),
  SimpleRep,
  Sum (..),
  SumOver,
  toSimpleRep,
 )
import Constrained.Graph (
  deleteNode,
  dependencies,
  nodes,
  opGraph,
  subtractGraph,
  topsort,
  transitiveClosure,
 )
import qualified Constrained.Graph as Graph
import Constrained.List (
  -- All,
  FunTy,
  List (..),
  ListCtx (..),
  -- TypeList,
  curryList,
  foldMapList,
  lengthList,
  mapList,
  mapMList,
  uncurryList_,
 )
import Constrained.NumSpec (
  IntW (..),
  NumLike,
  NumOrdW (..),
  NumSpec (..),
  Numeric,
  OrdLike,
  addFn,
  addSpecInt,
  cardinalNumSpec,
  cardinality,
  combineNumSpec,
  conformsToNumSpec,
  emptyNumSpec,
  genFromNumSpec,
  geqSpec,
  gtSpec,
  guardNumSpec,
  leqSpec,
  ltSpec,
  negateFn,
  notInNumSpec,
  shrinkWithNumSpec,
 )

-- TODO: some strange things here, why is SolverStage in here?!
-- Because it is mutually recursive with something else in here.
import Constrained.Syntax
import Control.Applicative
import Control.Monad
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable
import Data.Int
import Data.Kind
import Data.List (isPrefixOf, isSuffixOf, nub, partition, (\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Ratio
import Data.Semigroup (Any (..), getSum)
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Typeable
import Data.Word
import GHC.Natural
import GHC.Stack
import GHC.TypeLits
import Prettyprinter hiding (cat)
import Test.QuickCheck hiding (Args, Fun, Witness, forAll, witness)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random hiding (left, right)
import Prelude hiding (cycle, pred)

-- ===================================================================================
-- We call this module TheKnot because it binds a mutually recursive set of things
-- The heart of TheKNot is "genFromSpecT" the user interface to generating random instances of a Spec.
-- It is mutually recursive with the 3 simplest HasSpec instances (Bool,Integer,Sum), and 'simplifySpec'.
-- Every HasSpec instance is dependant on HasSpec Integer because the Cardinality properties
-- are expressed in terms of Integer. Generic HasSpec instances (including Bool) are
-- implemented in terms of a Sum of Product Simple Rep. And every HasSpec instance has
-- a genFromTypeSpec method, on which GenFromSpecT depends. There is no avoiding the Knot.
-- The only saving grace, is that genFromTypeSpec is a HasSpec method, so new things
-- depending only on things defined here, or things defined in the same file as the
-- the HasSpec instance can escape from TheKnot.
--
-- Here is a graph of the dependencies.
--
--      +---->HasSpec Integer
--      |      ^            ^
--      |      |             \
--      |      v              \
--      |     HasSpec Bool---->HasSpec Sum
--      |        ^  \               /   ^
--      |        |   \             /    |
--      <.       |    \           /     |
--      <=.      |     \         /      |
--      |        |      v       v       |
--      |        |      genFromSpecT    |
--      |        |            |         |
--      |        |            |         |
--      +-------caseBoolSpec  |    caseSum
--                      ^     |    ^
--                      |     |    |
--                      |     v    |
--                     simplifySpec
--
-- ===============================================================================
-- Things left TODO
-- 1) Use of UnionPat below requires HasSpec(Set a) instance

-- ==================================================================
-- HasSpec for Sums
-- ==================================================================

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

guardSumSpec ::
  forall a b.
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

-- | The HasSpec Sum instance
instance (HasSpec a, HasSpec b, KnownNat (CountCases b)) => HasSpec (Sum a b) where
  type TypeSpec (Sum a b) = SumSpec a b

  type Prerequisites (Sum a b) = (HasSpec a, HasSpec b)

  emptySpec = mempty

  combineSpec s s' = guardSumSpec ["When combining SumSpecs", "  " ++ show s, "  " ++ show s'] (s <> s')

  conformsTo (SumLeft a) (SumSpec _ sa _) = conformsToSpec a sa
  conformsTo (SumRight b) (SumSpec _ _ sb) = conformsToSpec b sb

  genFromTypeSpec (SumSpec h sa sb)
    | emptyA, emptyB = genError "genFromTypeSpec @SumSpec: empty"
    | emptyA = SumRight <$> genFromSpecT sb
    | emptyB = SumLeft <$> genFromSpecT sa
    | fA == 0, fB == 0 = genError "All frequencies 0"
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

-- ======================================
-- Here are the Logic Instances for Sum

data SumW dom rng where
  InjLeftW :: (HasSpec a, HasSpec b) => SumW '[a] (Sum a b)
  InjRightW :: (HasSpec a, HasSpec b) => SumW '[b] (Sum a b)

instance Show (SumW dom rng) where
  show InjLeftW = "injLeft_"
  show InjRightW = "injRight_"

deriving instance (Eq (SumW dom rng))

instance Syntax SumW

instance Semantics SumW where
  semantics InjLeftW = SumLeft
  semantics InjRightW = SumRight

instance Logic SumW where
  propagateTypeSpec InjLeftW (Unary HOLE) (SumSpec _ sl _) cant = sl <> foldMap notEqualSpec [a | SumLeft a <- cant]
  propagateTypeSpec InjRightW (Unary HOLE) (SumSpec _ _ sr) cant = sr <> foldMap notEqualSpec [a | SumRight a <- cant]

  propagateMemberSpec InjLeftW (Unary HOLE) es =
    case [a | SumLeft a <- NE.toList es] of
      (x : xs) -> MemberSpec (x :| xs)
      [] ->
        ErrorSpec $
          pure $
            "propMemberSpec (sumleft_ HOLE) on (MemberSpec es) with no SumLeft in es: " ++ show (NE.toList es)
  propagateMemberSpec InjRightW (Unary HOLE) es =
    case [a | SumRight a <- NE.toList es] of
      (x : xs) -> MemberSpec (x :| xs)
      [] ->
        ErrorSpec $
          pure $
            "propagate(InjRight HOLE) on (MemberSpec es) with no SumLeft in es: " ++ show (NE.toList es)

  mapTypeSpec InjLeftW ts = typeSpec $ SumSpec Nothing (typeSpec ts) (ErrorSpec (pure "mapTypeSpec InjLeftW"))
  mapTypeSpec InjRightW ts = typeSpec $ SumSpec Nothing (ErrorSpec (pure "mapTypeSpec InjRightW")) (typeSpec ts)

injLeft_ :: (HasSpec a, HasSpec b, KnownNat (CountCases b)) => Term a -> Term (Sum a b)
injLeft_ = appTerm InjLeftW

injRight_ :: (HasSpec a, HasSpec b, KnownNat (CountCases b)) => Term b -> Term (Sum a b)
injRight_ = appTerm InjRightW

pattern InjRight ::
  forall c.
  () =>
  forall a b.
  ( c ~ Sum a b
  , AppRequires SumW '[b] c
  ) =>
  Term b ->
  Term c
pattern InjRight x <- (App (getWitness -> Just InjRightW) (x :> Nil))

pattern InjLeft ::
  forall c.
  () =>
  forall a b.
  ( c ~ Sum a b
  , AppRequires SumW '[a] c
  ) =>
  Term a ->
  Term c
pattern InjLeft x <- App (getWitness -> Just InjLeftW) (x :> Nil)

-- ===========================================================================
-- HasSpec Bool
-- ===========================================================================

instance HasSpec Bool where
  shrinkWithTypeSpec _ = shrink
  cardinalTypeSpec (SumSpec _ a b) =
    MemberSpec (NE.fromList [0, 1, 2]) <> addSpecInt (cardinality a) (cardinality b)
  cardinalTrueSpec = MemberSpec (pure 2)

caseBoolSpec ::
  HasSpec a => Specification Bool -> (Bool -> Specification a) -> Specification a
caseBoolSpec spec cont = case possibleValues spec of
  [] -> ErrorSpec (NE.fromList ["No possible values in caseBoolSpec"])
  [b] -> cont b
  _ -> mempty
  where
    -- where possibleValues s = filter (flip conformsToSpec (simplifySpec s)) [True, False]
    -- This will always get the same result, and probably faster since running 2
    -- conformsToSpec on True and False takes less time than simplifying the spec.
    -- Since we are in TheKnot, we could keep the simplifySpec. Is there a good reason to?
    possibleValues s = filter (flip conformsToSpec s) [True, False]

-- | Operations on Bool
data BoolW (dom :: [Type]) (rng :: Type) where
  NotW :: BoolW '[Bool] Bool
  OrW :: BoolW '[Bool, Bool] Bool

deriving instance Eq (BoolW dom rng)

instance Show (BoolW dom rng) where
  show NotW = "not_"
  show OrW = "or_"

boolSem :: BoolW dom rng -> FunTy dom rng
boolSem NotW = not
boolSem OrW = (||)

instance Semantics BoolW where
  semantics = boolSem

instance Syntax BoolW

-- ======= Logic instance BoolW

instance Logic BoolW where
  propagate f ctxt (ExplainSpec [] s) = propagate f ctxt s
  propagate f ctxt (ExplainSpec es s) = ExplainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate NotW (Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App NotW (v' :> Nil)) (v :-> ps)
  propagate NotW (Unary HOLE) spec =
    caseBoolSpec spec (equalSpec . not)
  propagate OrW (HOLE :<: x) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App OrW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate OrW (x :>: HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App OrW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate OrW (HOLE :<: s) spec =
    caseBoolSpec spec (okOr s)
  propagate OrW (s :>: HOLE) spec =
    caseBoolSpec spec (okOr s)

  mapTypeSpec NotW (SumSpec h a b) = typeSpec $ SumSpec h b a

not_ :: Term Bool -> Term Bool
not_ = appTerm NotW

-- ======= Logic instance OrW(or_)

-- | We have something like ('constant' ||. HOLE) must evaluate to 'need'.
--   Return a (Specification Bool) for HOLE, that makes that True.
okOr :: Bool -> Bool -> Specification Bool
okOr constant need = case (constant, need) of
  (True, True) -> TrueSpec
  (True, False) ->
    ErrorSpec
      (pure ("(" ++ show constant ++ "||. HOLE) must equal False. That cannot be the case."))
  (False, False) -> MemberSpec (pure False)
  (False, True) -> MemberSpec (pure True)

or_ :: Term Bool -> Term Bool -> Term Bool
or_ = appTerm OrW

-- ======= Logic instance EqualW(==.)  CAN WE MOVE THIS OUT OF TheKnot?

data EqW :: [Type] -> Type -> Type where
  EqualW :: (Eq a, HasSpec a) => EqW '[a, a] Bool

deriving instance Eq (EqW dom rng)

instance Show (EqW d r) where
  show EqualW = "==."

instance Syntax EqW where
  inFix EqualW = True
  prettyWit _ _ _ = Nothing

instance Semantics EqW where
  semantics EqualW = (==)

instance Logic EqW where
  propagate f ctxt (ExplainSpec es s) = explainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate EqualW (HOLE :? Value x :> Nil) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App EqualW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate EqualW (Value x :! Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App EqualW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate EqualW (HOLE :? Value s :> Nil) spec =
    caseBoolSpec spec $ \case
      True -> equalSpec s
      False -> notEqualSpec s
  propagate EqualW (Value s :! Unary HOLE) spec =
    caseBoolSpec spec $ \case
      True -> equalSpec s
      False -> notEqualSpec s

  rewriteRules EqualW (t :> t' :> Nil) Evidence
    | t == t' = Just $ lit True
    | otherwise = Nothing

  saturate EqualW (FromGeneric (InjLeft _) :> t :> Nil) = [toPreds t (SumSpec Nothing TrueSpec (ErrorSpec (pure "saturatePred")))]
  saturate EqualW (FromGeneric (InjRight _) :> t :> Nil) = [toPreds t (SumSpec Nothing (ErrorSpec (pure "saturatePred")) TrueSpec)]
  saturate _ _ = []

infix 4 ==.
(==.) :: HasSpec a => Term a -> Term a -> Term Bool
(==.) = appTerm EqualW

pattern Equal ::
  forall b.
  () =>
  forall a.
  (b ~ Bool, Eq a, HasSpec a) =>
  Term a ->
  Term a ->
  Term b
pattern Equal x y <-
  ( App
      (getWitness -> Just EqualW)
      (x :> y :> Nil)
    )

-- ===========================================================================
-- HasSpec for Integer
-- ===========================================================================

toPredsNumSpec ::
  OrdLike n =>
  Term n ->
  NumSpec n ->
  Pred
toPredsNumSpec v (NumSpecInterval ml mu) =
  fold $
    [assert $ Lit l <=. v | l <- maybeToList ml]
      ++ [assert $ v <=. Lit u | u <- maybeToList mu]

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

instance Logic NumOrdW where
  propagate f ctxt (ExplainSpec [] s) = propagate f ctxt s
  propagate f ctxt (ExplainSpec es s) = ExplainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate GreaterW (HOLE :? x :> Nil) spec =
    propagate LessW (x :! Unary HOLE) spec
  propagate GreaterW (x :! Unary HOLE) spec =
    propagate LessW (HOLE :? x :> Nil) spec
  propagate LessOrEqualW (HOLE :? Value x :> Nil) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App LessOrEqualW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate LessOrEqualW (Value x :! Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App LessOrEqualW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate LessOrEqualW (HOLE :? Value l :> Nil) spec =
    caseBoolSpec spec $ \case True -> leqSpec l; False -> gtSpec l
  propagate LessOrEqualW (Value l :! Unary HOLE) spec =
    caseBoolSpec spec $ \case True -> geqSpec l; False -> ltSpec l
  propagate GreaterOrEqualW (HOLE :? Value x :> Nil) spec =
    propagate LessOrEqualW (Value x :! Unary HOLE) spec
  propagate GreaterOrEqualW (x :! Unary HOLE) spec =
    propagate LessOrEqualW (HOLE :? x :> Nil) spec
  propagate LessW (HOLE :? Value x :> Nil) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App LessW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate LessW (Value x :! Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App LessW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate LessW (HOLE :? Value l :> Nil) spec =
    caseBoolSpec spec $ \case True -> ltSpec l; False -> geqSpec l
  propagate LessW (Value l :! Unary HOLE) spec =
    caseBoolSpec spec $ \case True -> gtSpec l; False -> leqSpec l

infixr 4 <=.
(<=.) :: forall a. OrdLike a => Term a -> Term a -> Term Bool
(<=.) = appTerm LessOrEqualW

infixr 4 <.
(<.) :: forall a. OrdLike a => Term a -> Term a -> Term Bool
(<.) = appTerm LessW

infixr 4 >=.
(>=.) :: forall a. OrdLike a => Term a -> Term a -> Term Bool
(>=.) = appTerm GreaterOrEqualW

infixr 4 >.
(>.) :: forall a. OrdLike a => Term a -> Term a -> Term Bool
(>.) = appTerm GreaterW

-- ===========================================================================
-- SimplifySpec
-- ===========================================================================

simplifySpec :: HasSpec a => Specification a -> Specification a
simplifySpec spec = case regularizeNames spec of
  SuspendedSpec x p ->
    let optP = optimisePred p
     in fromGESpec $
          explain
            ("\nWhile calling simplifySpec on var " ++ show x ++ "\noptP=\n" ++ show optP ++ "\n")
            (computeSpecSimplified x optP)
  MemberSpec xs -> MemberSpec xs
  ErrorSpec es -> ErrorSpec es
  TypeSpec ts cant -> TypeSpec ts cant
  TrueSpec -> TrueSpec
  ExplainSpec es s -> explainSpecOpt es (simplifySpec s)

instance Numeric a => Complete a where
  simplifyA = simplifySpec
  genFromSpecA = genFromSpecT

-- | If the `Specification Bool` doesn't constrain the boolean you will get a `TrueSpec` out.
ifElse :: (IsPred p, IsPred q) => Term Bool -> p -> q -> Pred
ifElse b p q = whenTrue b p <> whenTrue (not_ b) q

whenTrue :: forall p. IsPred p => Term Bool -> p -> Pred
whenTrue (Lit True) (toPred -> p) = p
whenTrue (Lit False) _ = TruePred
whenTrue b (toPred -> FalsePred {}) = assert (not_ b)
whenTrue _ (toPred -> TruePred) = TruePred
whenTrue b (toPred -> p) = When b p

-- | Is the variable x pinned to some free term in p? (free term
-- meaning that all the variables in the term are free in p).
--
-- TODO: complete this with more cases!
pinnedBy :: forall a. HasSpec a => Var a -> Pred -> Maybe (Term a)
-- pinnedBy x (Assert (App (extractFn @EqFn @fn -> Just EqualW) (t :> t' :> Nil)))
pinnedBy x (Assert (Equal t t'))
  | V x' <- t, Just Refl <- eqVar x x' = Just t'
  | V x' <- t', Just Refl <- eqVar x x' = Just t
pinnedBy x (And ps) = listToMaybe $ catMaybes $ map (pinnedBy x) ps
pinnedBy _ _ = Nothing

-- ------- Stages of simplifying -------------------------------

-- TODO: it might be necessary to run aggressiveInlining again after the let floating etc.
optimisePred :: Pred -> Pred
optimisePred p =
  simplifyPred
    . letSubexpressionElimination
    . letFloating
    . aggressiveInlining
    . simplifyPred
    $ p

aggressiveInlining :: Pred -> Pred
aggressiveInlining pred
  | inlined = aggressiveInlining pInlined
  | otherwise = pred
  where
    (pInlined, Any inlined) = runWriter $ go (freeVars pred) [] pred

    underBinder fvs x p = fvs `without` [Name x] <> singleton (Name x) (countOf (Name x) p)

    underBinderSub :: HasSpec a => Subst -> Var a -> Subst
    underBinderSub sub x =
      [ x' := t
      | x' := t <- sub
      , isNothing $ eqVar x x'
      ]

    -- NOTE: this is safe because we only use the `Subst` when it results in a literal so there
    -- is no risk of variable capture.
    goBinder :: FreeVars -> Subst -> Binder a -> Writer Any (Binder a)
    goBinder fvs sub (x :-> p) = (x :->) <$> go (underBinder fvs x p) (underBinderSub sub x) p

    -- Check that the name `n` is only ever used as the only variable
    -- in the expressions where it appears. This ensures that it doesn't
    -- interact with anything.
    onlyUsedUniquely n p = case p of
      Assert t
        | n `appearsIn` t -> Set.size (freeVarSet t) == 1
        | otherwise -> True
      And ps -> all (onlyUsedUniquely n) ps
      -- TODO: we can (and should) probably add a bunch of cases to this.
      _ -> False

    go fvs sub pred2 = case pred2 of
      ElemPred bool t xs
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ ElemPred bool (Lit a) xs
        | otherwise -> pure $ ElemPred bool t xs
      Subst x t p -> go fvs sub (substitutePred x t p)
      Reifies t' t f
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ Reifies t' (Lit a) f
        | otherwise -> pure $ Reifies t' t f
      ForAll set b
        | not (isLit set)
        , Lit a <- substituteAndSimplifyTerm sub set -> do
            tell $ Any True
            pure $ foldMap (`unBind` b) (forAllToList a)
        | otherwise -> ForAll set <$> goBinder fvs sub b
      Case t bs
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ runCaseOn a (mapList thing bs) $ \x v p -> substPred (singletonEnv x v) p
        | (Weighted w (x :-> p) :> Nil) <- bs -> do
            let t' = substituteAndSimplifyTerm sub t
            p' <- go (underBinder fvs x p) (x := t' : sub) p
            pure $ Case t (Weighted w (x :-> p') :> Nil)
        | otherwise -> Case t <$> mapMList (traverseWeighted $ goBinder fvs sub) bs
      When b tp
        | not (isLit b)
        , Lit a <- substituteAndSimplifyTerm sub b -> do
            tell $ Any True
            pure $ if a then tp else TruePred
        | otherwise -> whenTrue b <$> go fvs sub tp
      Let t (x :-> p)
        | all (\n -> count n fvs <= 1) (freeVarSet t) -> do
            tell $ Any True
            pure $ substitutePred x t p
        | onlyUsedUniquely (Name x) p -> do
            tell $ Any True
            pure $ substitutePred x t p
        | not $ Name x `appearsIn` p -> do
            tell $ Any True
            pure p
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ unBind a (x :-> p)
        | otherwise -> Let t . (x :->) <$> go (underBinder fvs x p) (x := t : sub) p
      Exists k b -> Exists k <$> goBinder fvs sub b
      And ps -> fold <$> mapM (go fvs sub) ps
      Assert t
        | not (isLit t)
        , Lit b <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ toPred b
        | otherwise -> pure pred2
      -- If the term turns into a literal, there is no more generation to do here
      -- so we can ignore the `GenHint`
      GenHint _ t
        | not (isLit t)
        , Lit {} <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure TruePred
        | otherwise -> pure pred2
      DependsOn t t'
        | not (isLit t)
        , Lit {} <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ TruePred
        | not (isLit t')
        , Lit {} <- substituteAndSimplifyTerm sub t' -> do
            tell $ Any True
            pure $ TruePred
        | otherwise -> pure pred2
      TruePred -> pure pred2
      FalsePred {} -> pure pred2
      Monitor {} -> pure pred2
      Explain es p -> Explain es <$> go fvs sub p

-- | Apply a substitution and simplify the resulting term if the
-- substitution changed the term.
substituteAndSimplifyTerm :: Subst -> Term a -> Term a
substituteAndSimplifyTerm sub t =
  case runWriter $ substituteTerm' sub t of
    (t', Any b)
      | b -> simplifyTerm t'
      | otherwise -> t'

-- | Simplify a Term, if the Term is an 'App', apply the rewrite rules
--   chosen by the (Logic sym t bs a) instance attached
--   to the function witness 'f'
simplifyTerm :: forall a. Term a -> Term a
simplifyTerm = \case
  V v -> V v
  Lit l -> Lit l
  App (f :: t bs a) (mapList simplifyTerm -> ts)
    | Just vs <- fromLits ts -> Lit $ uncurryList_ unValue (semantics f) vs
    | Just t <- rewriteRules f ts (Evidence @(AppRequires t bs a)) -> simplifyTerm t
    | otherwise -> App f ts

simplifyPred :: Pred -> Pred
simplifyPred = \case
  -- If the term simplifies away to a literal, that means there is no
  -- more generation to do so we can get rid of `GenHint`
  GenHint h t -> case simplifyTerm t of
    Lit {} -> TruePred
    t' -> GenHint h t'
  p@(ElemPred bool t xs) -> case simplifyTerm t of
    Lit x -> case (elem x xs, bool) of
      (True, True) -> TruePred
      (True, False) -> FalsePred ("notElemPred reduces to True" :| [show p])
      (False, True) -> FalsePred ("elemPred reduces to False" :| [show p])
      (False, False) -> TruePred
    t' -> ElemPred bool t' xs
  Subst x t p -> simplifyPred $ substitutePred x t p
  Assert t -> Assert $ simplifyTerm t
  Reifies t' t f -> case simplifyTerm t of
    Lit a ->
      -- Assert $ simplifyTerm t' ==. Lit (f a)
      ElemPred True (simplifyTerm t') (pure (f a))
    t'' -> Reifies (simplifyTerm t') t'' f
  ForAll (ts :: Term t) (b :: Binder a) -> case simplifyTerm ts of
    Lit as -> foldMap (`unBind` b) (forAllToList as)
    -- (App (extractW (UnionW @t) -> Just Refl) xs) -> error "MADE IT"
    {- Has to wait until we have HasSpec(Set a) instance
    UnionPat (xs :: Term (Set a)) ys ->
       let b' = simplifyBinder b
       in mkForAll xs b' <> mkForAll ys b' -}
    set' -> case simplifyBinder b of
      _ :-> TruePred -> TruePred
      b' -> ForAll set' b'
  DependsOn _ Lit {} -> TruePred
  DependsOn Lit {} _ -> TruePred
  DependsOn x y -> DependsOn x y
  -- Here is where we need the SumSpec instance
  Case t bs -> mkCase (simplifyTerm t) (mapList (mapWeighted simplifyBinder) bs)
  When b p -> whenTrue (simplifyTerm b) (simplifyPred p)
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  And ps -> fold (simplifyPreds ps)
  Let t b -> case simplifyTerm t of
    t'@App {} -> Let t' (simplifyBinder b)
    -- Variable or literal
    t' | x :-> p <- b -> simplifyPred $ substitutePred x t' p
  Exists k b -> case simplifyBinder b of
    _ :-> TruePred -> TruePred
    -- This is to get rid of exisentials like:
    -- `constrained $ \ x -> exists $ \ y -> [x ==. y, y + 2 <. 10]`
    x :-> p | Just t <- pinnedBy x p -> simplifyPred $ substitutePred x t p
    b' -> Exists k b'
  Monitor {} -> TruePred
  -- TODO: This is a bit questionable. On the one hand we could get rid of `Explain` here
  -- and just return `simplifyPred p` but doing so risks missing explanations when things
  -- do go wrong.
  Explain es p -> explanation es $ simplifyPred p

simplifyPreds :: [Pred] -> [Pred]
simplifyPreds = go [] . map simplifyPred
  where
    go acc [] = reverse acc
    go _ (FalsePred err : _) = [FalsePred err]
    go acc (TruePred : ps) = go acc ps
    go acc (p : ps) = go (p : acc) ps

simplifyBinder :: Binder a -> Binder a
simplifyBinder (x :-> p) = x :-> simplifyPred p

-- --------------------------------------------------------------------
-- Turning Preds into Specifications. Here is where Propagation occurs

-- | Precondition: the `Pred` defines the `Var a`
-- Runs in `GE` in order for us to have detailed context on failure.
computeSpecSimplified ::
  forall a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Specification a)
computeSpecSimplified x pred3 = localGESpec $ case simplifyPred pred3 of
  ElemPred True t xs -> propagateSpec (MemberSpec xs) <$> toCtx x t
  ElemPred False (t :: Term b) xs -> propagateSpec (TypeSpec @b (emptySpec @b) (NE.toList xs)) <$> toCtx x t
  Monitor {} -> pure mempty
  GenHint h t -> propagateSpec (giveHint h) <$> toCtx x t
  Subst x' t p' -> computeSpec x (substitutePred x' t p') -- NOTE: this is impossible as it should have gone away already
  TruePred -> pure mempty
  FalsePred es -> genErrorNE es
  And ps -> do
    spec <- fold <$> mapM (computeSpecSimplified x) ps
    case spec of
      ExplainSpec es (SuspendedSpec y ps') -> pure $ explainSpecOpt es (SuspendedSpec y $ simplifyPred ps')
      SuspendedSpec y ps' -> pure $ SuspendedSpec y $ simplifyPred ps'
      s -> pure s
  Let t b -> pure $ SuspendedSpec x (Let t b)
  Exists k b -> pure $ SuspendedSpec x (Exists k b)
  Assert (Lit True) -> pure mempty
  Assert (Lit False) -> genError (show pred3)
  Assert (Elem _ (Lit [])) -> pure (ErrorSpec (NE.fromList ["Empty list in ElemPat", show pred3]))
  Assert (Elem t (Lit (y : ys))) -> propagateSpec (MemberSpec (y :| ys)) <$> toCtx x t
  Assert t -> propagateSpec (equalSpec True) <$> toCtx x t
  ForAll (Lit s) b -> fold <$> mapM (\val -> computeSpec x $ unBind val b) (forAllToList s)
  ForAll t b -> do
    bSpec <- computeSpecBinderSimplified b
    propagateSpec (fromForAllSpec bSpec) <$> toCtx x t
  Case (Lit val) bs -> runCaseOn val (mapList thing bs) $ \va vaVal psa -> computeSpec x (substPred (singletonEnv va vaVal) psa)
  Case t branches -> do
    branchSpecs <- mapMList (traverseWeighted computeSpecBinderSimplified) branches
    propagateSpec (caseSpec (Just (showType @a)) branchSpecs) <$> toCtx x t
  When (Lit b) tp -> if b then computeSpecSimplified x tp else pure TrueSpec
  -- This shouldn't happen a lot of the time because when the body is trivial we mostly get rid of the `When` entirely
  When {} -> pure $ SuspendedSpec x pred3
  Reifies (Lit a) (Lit val) f
    | f val == a -> pure TrueSpec
    | otherwise ->
        pure $
          ErrorSpec (NE.fromList ["Value does not reify to literal: " ++ show val ++ " -/> " ++ show a])
  Reifies t' (Lit val) f ->
    propagateSpec (equalSpec (f val)) <$> toCtx x t'
  Reifies Lit {} _ _ ->
    fatalErrorNE $ NE.fromList ["Dependency error in computeSpec: Reifies", "  " ++ show pred3]
  Explain es p -> do
    -- In case things crash in here we want the explanation
    s <- pushGE (NE.toList es) (computeSpecSimplified x p)
    -- This is because while we do want to propagate `explanation`s into `SuspendedSpec`
    -- we probably don't want to propagate the full "currently simplifying xyz" explanation.
    case s of
      SuspendedSpec x2 p2 -> pure $ SuspendedSpec x2 (explanation es p2)
      _ -> pure $ addToErrorSpec es s
  -- Impossible cases that should be ruled out by the dependency analysis and linearizer
  DependsOn {} ->
    fatalErrorNE $
      NE.fromList
        [ "The impossible happened in computeSpec: DependsOn"
        , "  " ++ show x
        , show $ indent 2 (pretty pred3)
        ]
  Reifies {} ->
    fatalErrorNE $
      NE.fromList
        ["The impossible happened in computeSpec: Reifies", "  " ++ show x, show $ indent 2 (pretty pred3)]
  where
    -- We want `genError` to turn into `ErrorSpec` and we want `FatalError` to turn into `FatalError`
    localGESpec ge = case ge of
      (GenError xs) -> Result $ ErrorSpec (catMessageList xs)
      (FatalError es) -> FatalError es
      (Result v) -> Result v

-- | Precondition: the `Pred fn` defines the `Var a`.
--   Runs in `GE` in order for us to have detailed context on failure.
computeSpec ::
  forall a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Specification a)
computeSpec x p = computeSpecSimplified x (simplifyPred p)

computeSpecBinder :: Binder a -> GE (Specification a)
computeSpecBinder (x :-> p) = computeSpec x p

computeSpecBinderSimplified :: Binder a -> GE (Specification a)
computeSpecBinderSimplified (x :-> p) = computeSpecSimplified x p

-- --------------- Simplification of Sum types --------------------

sumWeightL, sumWeightR :: Maybe (Int, Int) -> Doc a
sumWeightL Nothing = "1"
sumWeightL (Just (x, _)) = fromString (show x)
sumWeightR Nothing = "1"
sumWeightR (Just (_, x)) = fromString (show x)

-- | Turn a list of branches into a SumSpec. If all the branches fail return an ErrorSpec.
--   Note the requirement of HasSpec(SumOver).
caseSpec ::
  forall as.
  HasSpec (SumOver as) =>
  Maybe String ->
  List (Weighted (Specification)) as ->
  Specification (SumOver as)
caseSpec tString ss
  | allBranchesFail ss =
      ErrorSpec
        ( NE.fromList
            [ "When simplifying SumSpec, all branches in a caseOn" ++ sumType tString ++ " simplify to False."
            , show spec
            ]
        )
  | True = spec
  where
    spec = loop tString ss

    allBranchesFail :: forall as2. List (Weighted Specification) as2 -> Bool
    allBranchesFail Nil = error "The impossible happened in allBranchesFail"
    allBranchesFail (Weighted _ s :> Nil) = isErrorLike s
    allBranchesFail (Weighted _ s :> ss2@(_ :> _)) = isErrorLike s && allBranchesFail ss2

    loop ::
      forall as3.
      HasSpec (SumOver as3) =>
      Maybe String ->
      List (Weighted Specification) as3 ->
      Specification (SumOver as3)
    loop _ Nil = error "The impossible happened in caseSpec"
    loop _ (s :> Nil) = thing s
    loop mTypeString (s :> ss1@(_ :> _))
      | Evidence <- prerequisites @(SumOver as3) =
          (typeSpec $ SumSpecRaw mTypeString theWeights (thing s) (loop Nothing ss1))
      where
        theWeights =
          case (weight s, totalWeight ss1) of
            (Nothing, Nothing) -> Nothing
            (a, b) -> Just (fromMaybe 1 a, fromMaybe (lengthList ss1) b)

-- =======================================================================================
-- Generating from Specifications
-- 1) Simplify
-- 2) Compute a dependency ordering
-- 3) Then generate for each variable in turn, then substituting into the remaining vars
-- =======================================================================================

-- | Generate a value that satisfies the spec. This function can fail if the
-- spec is inconsistent, there is a dependency error, or if the underlying
-- generators are not flexible enough.
genFromSpecT ::
  forall a m. (HasCallStack, HasSpec a, MonadGenError m) => Specification a -> GenT m a
genFromSpecT (simplifySpec -> spec) = case spec of
  ExplainSpec [] s -> genFromSpecT s
  ExplainSpec es s -> push es (genFromSpecT s)
  MemberSpec as -> explain ("genFromSpecT on spec" ++ show spec) $ pureGen (elements (NE.toList as))
  TrueSpec -> genFromSpecT (typeSpec $ emptySpec @a)
  SuspendedSpec x p
    -- NOTE: If `x` isn't free in `p` we still have to try to generate things
    -- from `p` to make sure `p` is sat and then we can throw it away. A better
    -- approach would be to only do this in the case where we don't know if `p`
    -- is sat. The proper way to implement such a sat check is to remove
    -- sat-but-unnecessary variables in the optimiser.
    | not $ Name x `appearsIn` p -> do
        !_ <- genFromPreds mempty p
        genFromSpecT TrueSpec
    | otherwise -> do
        env <- genFromPreds mempty p
        findEnv env x
  TypeSpec s cant -> do
    mode <- getMode
    explainNE
      ( NE.fromList
          [ "genFromSpecT on (TypeSpec tspec cant) at type " ++ showType @a
          , "tspec = "
          , show s
          , "cant = " ++ show (short cant)
          , "with mode " ++ show mode
          ]
      )
      $
      -- TODO: we could consider giving `cant` as an argument to `genFromTypeSpec` if this
      -- starts giving us trouble.
      genFromTypeSpec s `suchThatT` (`notElem` cant)
  ErrorSpec e -> genErrorNE e

-- | A version of `genFromSpecT` that simply errors if the generator fails
genFromSpec :: forall a. (HasCallStack, HasSpec a) => Specification a -> Gen a
genFromSpec spec = do
  res <- catchGen $ genFromSpecT @a @GE spec
  either (error . ('\n' :) . catMessages) pure res

-- | A version of `genFromSpecT` that takes a seed and a size and gives you a result
genFromSpecWithSeed ::
  forall a. (HasCallStack, HasSpec a) => Int -> Int -> Specification a -> a
genFromSpecWithSeed seed size spec = unGen (genFromSpec spec) (mkQCGen seed) size

-- | A version of `genFromSpecT` that runs in the IO monad. Good for debugging.
debugSpec :: forall a. HasSpec a => Specification a -> IO ()
debugSpec spec = do
  ans <- generate $ genFromGenT $ inspect (genFromSpecT spec)
  let f x = putStrLn (unlines (NE.toList x))
      ok x =
        if conformsToSpec x spec
          then putStrLn "True"
          else putStrLn "False, perhaps there is an unsafeExists in the spec?"
  case ans of
    FatalError xs -> mapM_ f xs
    GenError xs -> mapM_ f xs
    Result x -> print spec >> print x >> ok x

-- ---------------- Dependency Graphs ------------------------------------

type DependGraph = Graph.Graph Name

dependency :: HasVariables t => Name -> t -> DependGraph
dependency x (freeVarSet -> xs) = Graph.dependency x xs

irreflexiveDependencyOn ::
  forall t t'. (HasVariables t, HasVariables t') => t -> t' -> DependGraph
irreflexiveDependencyOn (freeVarSet -> xs) (freeVarSet -> ys) = Graph.irreflexiveDependencyOn xs ys

noDependencies :: HasVariables t => t -> DependGraph
noDependencies (freeVarSet -> xs) = Graph.noDependencies xs

type Hints = DependGraph

respecting :: Hints -> DependGraph -> DependGraph
respecting hints g = g `subtractGraph` opGraph hints

solvableFrom :: Name -> Set Name -> DependGraph -> Bool
solvableFrom x s g =
  let less = dependencies x g
   in s `Set.isSubsetOf` less && not (x `Set.member` less)

computeDependencies :: Pred -> DependGraph
computeDependencies = \case
  ElemPred _bool term _xs -> computeTermDependencies term
  Monitor {} -> mempty
  Subst x t p -> computeDependencies (substitutePred x t p)
  Assert t -> computeTermDependencies t
  Reifies t' t _ -> t' `irreflexiveDependencyOn` t
  ForAll set b ->
    let innerG = computeBinderDependencies b
     in innerG <> set `irreflexiveDependencyOn` nodes innerG
  x `DependsOn` y -> x `irreflexiveDependencyOn` y
  Case t bs ->
    let innerG = foldMapList (computeBinderDependencies . thing) bs
     in innerG <> t `irreflexiveDependencyOn` nodes innerG
  When b p ->
    let pG = computeDependencies p
        oG = nodes pG `irreflexiveDependencyOn` b
     in oG <> pG
  TruePred -> mempty
  FalsePred {} -> mempty
  And ps -> foldMap computeDependencies ps
  Exists _ b -> computeBinderDependencies b
  Let t b -> noDependencies t <> computeBinderDependencies b
  GenHint _ t -> noDependencies t
  Explain _ p -> computeDependencies p

computeBinderDependencies :: Binder a -> DependGraph
computeBinderDependencies (x :-> p) =
  deleteNode (Name x) $ computeDependencies p

computeTermDependencies :: Term a -> DependGraph
computeTermDependencies = fst . computeTermDependencies'

computeTermDependencies' :: Term a -> (DependGraph, Set Name)
computeTermDependencies' = \case
  (App _ args) -> go args
  Lit {} -> (mempty, mempty)
  (V x) -> (noDependencies (Name x), Set.singleton (Name x))
  where
    go :: List Term as -> (DependGraph, Set Name)
    go Nil = (mempty, mempty)
    go (t :> ts) =
      let (gr, ngr) = go ts
          (tgr, ntgr) = computeTermDependencies' t
       in (ntgr `irreflexiveDependencyOn` ngr <> tgr <> gr, ngr <> ntgr)

-- ----------------------- Shrinking -------------------------------

shrinkWithSpec :: forall a. HasSpec a => Specification a -> a -> [a]
-- TODO: possibly allow for ignoring the `conformsToSpec` check in the `TypeSpec`
-- case when you know what you're doing
shrinkWithSpec (simplifySpec -> spec) a = filter (`conformsToSpec` spec) $ case spec of
  ExplainSpec _ s -> shrinkWithSpec s a
  -- TODO: filter on can't if we have a known to be sound shrinker
  TypeSpec s _ -> shrinkWithTypeSpec s a
  -- TODO: The better way of doing this is to compute the dependency graph,
  -- shrink one variable at a time, and fixup the rest of the variables
  SuspendedSpec {} -> shr a
  MemberSpec {} -> shr a
  TrueSpec -> shr a
  ErrorSpec {} -> []
  where
    shr = shrinkWithTypeSpec (emptySpec @a)

shrinkFromPreds :: HasSpec a => Pred -> Var a -> a -> [a]
shrinkFromPreds p
  | Result plan <- prepareLinearization p = \x a -> listFromGE $ do
      -- NOTE: we do this to e.g. guard against bad construction functions in Exists
      xaGood <- checkPred (singletonEnv x a) p
      unless xaGood $
        fatalError "Trying to shrink a bad value, don't do that!"
      -- Get an `env` for the original value
      initialEnv <- envFromPred (singletonEnv x a) p
      return
        [ a'
        | -- Shrink the initialEnv
        env' <- shrinkEnvFromPlan initialEnv plan
        , -- Get the value of the constrained variable `x` in the shrunk env
        Just a' <- [lookupEnv env' x]
        , -- NOTE: this is necessary because it's possible that changing
        -- a particular value in the env during shrinking might not result
        -- in the value of `x` changing and there is no better way to know than
        -- to do this.
        a' /= a
        ]
  | otherwise = error "Bad pred"

-- Start with a valid Env for the plan and try to shrink it
shrinkEnvFromPlan :: Env -> SolverPlan -> [Env]
shrinkEnvFromPlan initialEnv SolverPlan {..} = go mempty solverPlan
  where
    go :: Env -> [SolverStage] -> [Env]
    go _ [] = [] -- In this case we decided to keep every variable the same so nothing to return
    go env ((substStage env -> SolverStage {..}) : plan) = do
      Just a <- [lookupEnv initialEnv stageVar]
      -- Two cases:
      --  - either we shrink this value and try to fixup every value later on in the plan or
      [ env' <> fixedEnv
        | a' <- shrinkWithSpec stageSpec a
        , let env' = extendEnv stageVar a' env
        , Just fixedEnv <- [fixupPlan env' plan]
        ]
        --  - we keep this value the way it is and try to shrink some later value
        ++ go (extendEnv stageVar a env) plan

    -- Fix the rest of the plan given an environment `env` for the plan so far
    fixupPlan :: Env -> [SolverStage] -> Maybe Env
    fixupPlan env [] = pure env
    fixupPlan env ((substStage env -> SolverStage {..}) : plan) =
      case lookupEnv initialEnv stageVar >>= fixupWithSpec stageSpec of
        Nothing -> Nothing
        Just a -> fixupPlan (extendEnv stageVar a env) plan

-- ---------------------- Building a plan -----------------------------------

substStage :: Env -> SolverStage -> SolverStage
substStage env (SolverStage y ps spec) = normalizeSolverStage $ SolverStage y (substPred env <$> ps) spec

normalizeSolverStage :: SolverStage -> SolverStage
normalizeSolverStage (SolverStage x ps spec) = SolverStage x ps'' (spec <> spec')
  where
    (ps', ps'') = partition ((1 ==) . Set.size . freeVarSet) ps
    spec' = fromGESpec $ computeSpec x (And ps')

-- Try to fix a value w.r.t a specification
fixupWithSpec :: forall a. HasSpec a => Specification a -> a -> Maybe a
fixupWithSpec spec a
  | a `conformsToSpec` spec = Just a
  | otherwise = case spec of
      MemberSpec (x :| _) -> Just x
      _ -> listToMaybe $ filter (`conformsToSpec` spec) (shrinkWithSpec TrueSpec a)

-- TODO: here we can compute both the explicit hints (i.e. constraints that
-- define the order of two variables) and any whole-program smarts.
computeHints :: [Pred] -> Hints
computeHints ps =
  transitiveClosure $ fold [x `irreflexiveDependencyOn` y | DependsOn x y <- ps]

-- | Linearize a predicate, turning it into a list of variables to solve and
-- their defining constraints such that each variable can be solved independently.
prepareLinearization :: Pred -> GE SolverPlan
prepareLinearization p = do
  let preds = concatMap saturatePred $ flattenPred p
      hints = computeHints preds
      graph = transitiveClosure $ hints <> respecting hints (foldMap computeDependencies preds)
  plan <-
    explainNE
      ( NE.fromList
          [ "Linearizing"
          , show $ "  preds: " <> pretty preds
          , show $ "  graph: " <> pretty graph
          ]
      )
      $ linearize preds graph
  pure $ backPropagation $ SolverPlan plan graph

-- | Flatten nested `Let`, `Exists`, and `And` in a `Pred fn`. `Let` and
-- `Exists` bound variables become free in the result.
flattenPred :: Pred -> [Pred]
flattenPred pIn = go (freeVarNames pIn) [pIn]
  where
    go _ [] = []
    go fvs (p : ps) = case p of
      And ps' -> go fvs (ps' ++ ps)
      -- NOTE: the order of the arguments to `==.` here are important.
      -- The whole point of `Let` is that it allows us to solve all of `t`
      -- before we solve the variables in `t`.
      Let t b -> goBinder fvs b ps (\x -> (assert (t ==. (V x)) :))
      Exists _ b -> goBinder fvs b ps (const id)
      When b pp -> map (When b) (go fvs [pp]) ++ go fvs ps
      Explain es pp -> map (explanation es) (go fvs [pp]) ++ go fvs ps
      _ -> p : go fvs ps

    goBinder ::
      Set Int ->
      Binder a ->
      [Pred] ->
      (HasSpec a => Var a -> [Pred] -> [Pred]) ->
      [Pred]
    goBinder fvs (x :-> p) ps k = k x' $ go (Set.insert (nameOf x') fvs) (p' : ps)
      where
        (x', p') = freshen x p fvs

-- Consider: A + B = C + D
-- We want to fail if A and B are independent.
-- Consider: A + B = A + C, A <- B
-- Here we want to consider this constraint defining for A
linearize ::
  MonadGenError m => [Pred] -> DependGraph -> m [SolverStage]
linearize preds graph = do
  sorted <- case topsort graph of
    Left cycle ->
      fatalError
        ( show $
            "linearize: Dependency cycle in graph:"
              /> vsep'
                [ "cycle:" /> pretty cycle
                , "graph:" /> pretty graph
                ]
        )
    Right sorted -> pure sorted
  go sorted [(freeVarSet ps, ps) | ps <- filter isRelevantPred preds]
  where
    isRelevantPred TruePred = False
    isRelevantPred DependsOn {} = False
    isRelevantPred (Assert (Lit True)) = False
    isRelevantPred _ = True

    go [] [] = pure []
    go [] ps
      | null $ foldMap fst ps =
          case checkPredsE (pure "Linearizing fails") mempty (map snd ps) of
            Nothing -> pure []
            Just msgs -> genErrorNE msgs
      | otherwise =
          fatalErrorNE $
            NE.fromList
              [ "Dependency error in `linearize`: "
              , show $ indent 2 $ "graph: " /> pretty graph
              , show $
                  indent 2 $
                    "the following left-over constraints are not defining constraints for a unique variable:"
                      /> vsep' (map (pretty . snd) ps)
              ]
    go (n@(Name x) : ns) ps = do
      let (nps, ops) = partition (isLastVariable n . fst) ps
      (normalizeSolverStage (SolverStage x (map snd nps) mempty) :) <$> go ns ops

    isLastVariable n set = n `Set.member` set && solvableFrom n (Set.delete n set) graph

-- =================================
-- Operations on Stages and Plans

-- | Does nothing if the variable is not in the plan already.
mergeSolverStage :: SolverStage -> [SolverStage] -> [SolverStage]
mergeSolverStage (SolverStage x ps spec) plan =
  [ case eqVar x y of
      Just Refl ->
        SolverStage
          y
          (ps ++ ps')
          ( addToErrorSpec
              ( NE.fromList
                  ( [ "Solving var " ++ show x ++ " fails."
                    , "Merging the Specs"
                    , "   1. " ++ show spec
                    , "   2. " ++ show spec'
                    ]
                  )
              )
              (spec <> spec')
          )
      Nothing -> stage
  | stage@(SolverStage y ps' spec') <- plan
  ]

prettyPlan :: HasSpec a => Specification a -> Doc ann
prettyPlan (simplifySpec -> spec)
  | SuspendedSpec _ p <- spec
  , Result plan <- prepareLinearization p =
      vsep'
        [ "Simplified spec:" /> pretty spec
        , pretty plan
        ]
  | otherwise = "Simplfied spec:" /> pretty spec

printPlan :: HasSpec a => Specification a -> IO ()
printPlan = print . prettyPlan

isEmptyPlan :: SolverPlan -> Bool
isEmptyPlan (SolverPlan plan _) = null plan

stepPlan :: MonadGenError m => Env -> SolverPlan -> GenT m (Env, SolverPlan)
stepPlan env plan@(SolverPlan [] _) = pure (env, plan)
stepPlan env (SolverPlan (SolverStage x ps spec : pl) gr) = do
  (spec', specs) <- runGE
    $ explain
      ( show
          ( "Computing specs for variable "
              <> pretty x
                /> vsep' (map pretty ps)
          )
      )
    $ do
      ispecs <- mapM (computeSpec x) ps
      pure $ (fold ispecs, ispecs)
  val <-
    genFromSpecT
      ( addToErrorSpec
          ( NE.fromList
              ( ( "\nStepPlan for variable: "
                    ++ show x
                    ++ " fails to produce Specification, probably overconstrained."
                    ++ "PS = "
                    ++ unlines (map show ps)
                )
                  : ("Original spec " ++ show spec)
                  : "Predicates"
                  : zipWith
                    (\pred specx -> "  pred " ++ show pred ++ " -> " ++ show specx)
                    ps
                    specs
              )
          )
          (spec <> spec')
      )
  let env1 = extendEnv x val env
  pure (env1, backPropagation $ SolverPlan (substStage env1 <$> pl) (deleteNode (Name x) gr))

-- | Generate a satisfying `Env` for a `p : Pred fn`. The `Env` contains values for
-- all the free variables in `flattenPred p`.
genFromPreds :: forall m. MonadGenError m => Env -> Pred -> GenT m Env
-- TODO: remove this once optimisePred does a proper fixpoint computation
genFromPreds env0 (optimisePred . optimisePred -> preds) =
  {- explain1 (show $ "genFromPreds fails\nPreds are:" /> pretty preds) -} do
    -- NOTE: this is just lazy enough that the work of flattening,
    -- computing dependencies, and linearizing is memoized in
    -- properties that use `genFromPreds`.
    plan <- runGE $ prepareLinearization preds
    go env0 plan
  where
    go :: Env -> SolverPlan -> GenT m Env
    go env plan | isEmptyPlan plan = pure env
    go env plan = do
      (env', plan') <-
        explain (show $ "Stepping the plan:" /> vsep [pretty plan, pretty env]) $ stepPlan env plan
      go env' plan'

-- | Push as much information we can backwards through the plan.
backPropagation :: SolverPlan -> SolverPlan
-- backPropagation (SolverPlan _plan _graph) =
backPropagation (SolverPlan initplan graph) = SolverPlan (go [] (reverse initplan)) graph
  where
    go :: [SolverStage] -> [SolverStage] -> [SolverStage]
    go acc [] = acc
    go acc (s@(SolverStage (x :: Var a) ps spec) : plan) = go (s : acc) plan'
      where
        newStages = concatMap (newStage spec) ps
        plan' = foldr mergeSolverStage plan newStages
        -- Note use of the Term Pattern Equal
        newStage specl (Assert (Equal (V x') t)) =
          termVarEqCases specl x' t
        newStage specr (Assert (Equal t (V x'))) =
          termVarEqCases specr x' t
        newStage _ _ = []

        termVarEqCases :: HasSpec b => Specification a -> Var b -> Term b -> [SolverStage]
        termVarEqCases (MemberSpec vs) x' t
          | Set.singleton (Name x) == freeVarSet t =
              [SolverStage x' [] $ MemberSpec (NE.nub (fmap (\v -> errorGE $ runTerm (singletonEnv x v) t) vs))]
        termVarEqCases specx x' t
          | Just Refl <- eqVar x x'
          , [Name y] <- Set.toList $ freeVarSet t
          , Result ctx <- toCtx y t =
              [SolverStage y [] (propagateSpec specx ctx)]
        termVarEqCases _ _ _ = []

-- =======================================================================================

-- | Functor like property for Specification, but instead of a Haskell function (a -> b),
--   it takes a function symbol (t '[a] b) from a to b.
--   Note, in this context, a function symbol is some constructor of a witnesstype.
--   Eg. ProdFstW, InjRightW, SingletonW, etc. NOT the lifted versions like fst_ singleton_,
--   which construct Terms. We had to wait until here to define this because it
--   depends on Semigroup property of Specification, and Asserting equality
mapSpec ::
  forall t a b.
  AppRequires t '[a] b =>
  t '[a] b ->
  Specification a ->
  Specification b
mapSpec f (ExplainSpec es s) = explainSpecOpt es (mapSpec f s)
mapSpec f TrueSpec = mapTypeSpec f (emptySpec @a)
mapSpec _ (ErrorSpec err) = ErrorSpec err
mapSpec f (MemberSpec as) = MemberSpec $ NE.nub $ fmap (semantics f) as
mapSpec f (SuspendedSpec x p) =
  constrained $ \x' ->
    Exists (\_ -> fatalError "mapSpec") (x :-> fold [Assert $ (x' ==. appTerm f (V x)), p])
mapSpec f (TypeSpec ts cant) = mapTypeSpec f ts <> notMemberSpec (map (semantics f) cant)

-- ==================================================================================================
-- TODO: generalize this to make it more flexible and extensible
--
-- The idea here is that we turn constraints into _extra_ constraints. C.f. the
-- `mapIsJust` example in `Constrained.Examples.Map`:

--    mapIsJust :: Specification BaseFn (Int, Int)
--    mapIsJust = constrained' $ \ [var| x |] [var| y |] ->
--      assert $ cJust_ x ==. lookup_ y (lit $ Map.fromList [(z, z) | z <- [100 .. 102]])

-- Without this code the example wouldn't work because `y` is completely unconstrained during
-- generation. With this code we essentially rewrite occurences of `cJust_ A == B` to
-- `[cJust A == B, case B of Nothing -> False; Just _ -> True]` to add extra information
-- about the variables in `B`. Consequently, `y` in the example above is
-- constrained to `MemberSpec [100 .. 102]` in the plan.
saturatePred :: Pred -> [Pred]
saturatePred p =
  -- [p]
  --  + ---- if there is an Explain, it is still on 'p' here
  --  |
  --  v
  p : case p of
    Explain _es x -> saturatePred x -- Note that the Explain is still on the original 'p', so it is not lost
    {- We want rules like this. But because the patterns can not be in scope, we implement these in
    -- the 'saturate' method of the Logic class
    Assert (Equal (FromGeneric (InjLeft _)) t) -> [toPreds t (SumSpec Nothing TrueSpec (ErrorSpec (pure "saturatePred")))]
    Assert (Equal (FromGeneric (InjRight _)) t) -> [toPreds t (SumSpec Nothing (ErrorSpec (pure "saturatePred")) TrueSpec)]
    Assert (Elem @Bool @a (FromGeneric (Product @n @m x y)) (Lit zs))
      | Just Refl <- eqT @a @(m, n) -> case zs of
          (w : ws) -> [ElemPred True x (fmap fst (w :| ws))]
          [] -> [FalsePred (pure $ "empty list, zs , in elem_ " ++ show (x, y) ++ " zs")]
    Assert (Elem x (Lit (y : ys))) -> [satisfies x (MemberSpec (y :| ys))]
    -- ElemPred True x ys -> [satisfies x (MemberSpec ys)]
    -}
    -- Note how the saturation is done by the 'saturate' method of the Logic class
    Assert ((App (sym :: t dom Bool) xs) :: Term Bool) -> saturate sym xs
    -- TODO: e.g. `elem (pair x y) (lit zs) -> elem x (lit $ map fst zs)` etc.
    _ -> []

-- ================================================================
-- HasSpec for Products
-- ================================================================

pairView :: forall a b. (HasSpec a, HasSpec b) => Term (Prod a b) -> Maybe (Term a, Term b)
pairView (App (sameFunSym $ ProdW @a @b -> Just (_, Refl, Refl, Refl)) (x :> y :> Nil)) = Just (x, y)
pairView _ = Nothing

cartesian ::
  forall a b.
  (HasSpec a, HasSpec b) =>
  Specification a ->
  Specification b ->
  Specification (Prod a b)
cartesian (ErrorSpec es) (ErrorSpec fs) = ErrorSpec (es <> fs)
cartesian (ErrorSpec es) _ = ErrorSpec (NE.cons "cartesian left" es)
cartesian _ (ErrorSpec es) = ErrorSpec (NE.cons "cartesian right" es)
cartesian s s' = typeSpec $ Cartesian s s'

data PairSpec a b = Cartesian (Specification a) (Specification b)

instance (Arbitrary (Specification a), Arbitrary (Specification b)) => Arbitrary (PairSpec a b) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary
  shrink (Cartesian a b) = uncurry Cartesian <$> shrink (a, b)

instance (HasSpec a, HasSpec b) => HasSpec (Prod a b) where
  type TypeSpec (Prod a b) = PairSpec a b

  type Prerequisites (Prod a b) = (HasSpec a, HasSpec b)

  emptySpec = Cartesian mempty mempty

  combineSpec (Cartesian a b) (Cartesian a' b') = cartesian (a <> a') (b <> b')

  conformsTo (Prod a b) (Cartesian sa sb) = conformsToSpec a sa && conformsToSpec b sb

  genFromTypeSpec (Cartesian sa sb) = Prod <$> genFromSpecT sa <*> genFromSpecT sb

  shrinkWithTypeSpec (Cartesian sa sb) (Prod a b) =
    [Prod a' b | a' <- shrinkWithSpec sa a]
      ++ [Prod a b' | b' <- shrinkWithSpec sb b]

  toPreds x (Cartesian sf ss) =
    satisfies (prodFst_ x) sf
      <> satisfies (prodSnd_ x) ss

  cardinalTypeSpec (Cartesian x y) = (cardinality x) + (cardinality y)

  typeSpecHasError (Cartesian x y) =
    case (isErrorLike x, isErrorLike y) of
      (False, False) -> Nothing
      (True, False) -> Just $ errorLikeMessage x
      (False, True) -> Just $ errorLikeMessage y
      (True, True) -> Just $ (errorLikeMessage x <> errorLikeMessage y)

  alternateShow (Cartesian left right@(TypeSpec r [])) =
    case alternateShow @b r of
      (BinaryShow "Cartesian" ps) -> BinaryShow "Cartesian" ("," <+> viaShow left : ps)
      (BinaryShow "SumSpec" ps) -> BinaryShow "Cartesian" ("," <+> viaShow left : ["SumSpec" /> vsep ps])
      _ -> BinaryShow "Cartesian" ["," <+> viaShow left, "," <+> viaShow right]
  alternateShow (Cartesian left right) = BinaryShow "Cartesian" ["," <+> viaShow left, "," <+> viaShow right]

instance (HasSpec a, HasSpec b) => Show (PairSpec a b) where
  show pair@(Cartesian l r) = case alternateShow @(Prod a b) pair of
    (BinaryShow "Cartesian" ps) -> show $ parens ("Cartesian" /> vsep ps)
    _ -> "(Cartesian " ++ "(" ++ show l ++ ") (" ++ show r ++ "))"

-- ==================================================
-- Logic instances for Prod
-- ==================================================

-- ========= ProdFstW

data ProdW :: [Type] -> Type -> Type where
  ProdW :: (HasSpec a, HasSpec b) => ProdW '[a, b] (Prod a b)
  ProdFstW :: (HasSpec a, HasSpec b) => ProdW '[Prod a b] a
  ProdSndW :: (HasSpec a, HasSpec b) => ProdW '[Prod a b] b
deriving instance Eq (ProdW as b)
deriving instance Show (ProdW as b)

instance Syntax ProdW
instance Semantics ProdW where
  semantics ProdW = Prod
  semantics ProdFstW = prodFst
  semantics ProdSndW = prodSnd

instance Logic ProdW where
  propagateTypeSpec ProdFstW (Unary HOLE) ts cant = cartesian (TypeSpec ts cant) TrueSpec
  propagateTypeSpec ProdSndW (Unary HOLE) ts cant =
    cartesian TrueSpec (TypeSpec ts cant)
  propagateTypeSpec ProdW (a :>: HOLE) sc@(Cartesian sa sb) cant
    | a `conformsToSpec` sa = sb <> foldMap notEqualSpec (sameFst a cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ " ++ show a ++ " HOLE) has conformance failure on a", show (TypeSpec sc cant)]
          )
  propagateTypeSpec ProdW (HOLE :<: b) sc@(Cartesian sa sb) cant
    | b `conformsToSpec` sb = sa <> foldMap notEqualSpec (sameSnd b cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ HOLE " ++ show b ++ ") has conformance failure on b", show (TypeSpec sc cant)]
          )

  propagateMemberSpec ProdFstW (Unary HOLE) es = cartesian (MemberSpec es) TrueSpec
  propagateMemberSpec ProdSndW (Unary HOLE) es = cartesian TrueSpec (MemberSpec es)
  propagateMemberSpec ProdW (a :>: HOLE) es =
    case (nub (sameFst a (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show a ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show a ++ " does not appear as the fst component of anything in the MemberSpec."
            ]
  propagateMemberSpec ProdW (HOLE :<: b) es =
    case (nub (sameSnd b (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show b ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show b ++ " does not appear as the snd component of anything in the MemberSpec."
            ]

  rewriteRules ProdFstW ((pairView -> Just (x, _)) :> Nil) Evidence = Just x
  rewriteRules ProdSndW ((pairView -> Just (_, y)) :> Nil) Evidence = Just y
  rewriteRules _ _ _ = Nothing

  mapTypeSpec ProdFstW (Cartesian s _) = s
  mapTypeSpec ProdSndW (Cartesian _ s) = s

prodFst_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term a
prodFst_ = appTerm ProdFstW

prodSnd_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term b
prodSnd_ = appTerm ProdSndW

-- ========= ProdW
sameFst :: Eq a1 => a1 -> [Prod a1 a2] -> [a2]
sameFst a ps = [b | Prod a' b <- ps, a == a']

sameSnd :: Eq a1 => a1 -> [Prod a2 a1] -> [a2]
sameSnd b ps = [a | Prod a b' <- ps, b == b']

prod_ :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (Prod a b)
prod_ = appTerm ProdW

-- ===============================================================================
-- Arbitrary instances
-- ===============================================================================

instance (HasSpec a, Arbitrary (TypeSpec a)) => Arbitrary (Specification a) where
  arbitrary = do
    baseSpec <-
      frequency
        [ (1, pure TrueSpec)
        ,
          ( 7
          , do
              zs <- nub <$> listOf1 (genFromSpec TrueSpec)
              pure
                ( memberSpecList
                    zs
                    ( NE.fromList
                        [ "In (Arbitrary Specification) this should never happen"
                        , "listOf1 generates empty list."
                        ]
                    )
                )
          )
        , (10, typeSpec <$> arbitrary)
        ,
          ( 1
          , do
              len <- choose (1, 5)
              TypeSpec <$> arbitrary <*> vectorOf len (genFromSpec TrueSpec)
          )
        , (1, ErrorSpec <$> arbitrary)
        , -- Recurse to make sure we apply the tricks for generating suspended specs multiple times
          (1, arbitrary)
        ]
    -- TODO: we probably want smarter ways of generating constraints
    frequency
      [ (1, pure $ constrained $ \x -> x `satisfies` baseSpec)
      , (1, ExplainSpec ["Arbitrary"] <$> arbitrary)
      ,
        ( 1
        , pure $ constrained $ \x -> exists (\eval -> pure $ eval x) $ \y ->
            [ assert $ x ==. y
            , y `satisfies` baseSpec
            ]
        )
      , (1, pure $ constrained $ \x -> letBind x $ \y -> y `satisfies` baseSpec)
      ,
        ( 1
        , pure $ constrained $ \x -> exists (\_ -> pure True) $ \b ->
            ifElse b (x `satisfies` baseSpec) (x `satisfies` baseSpec)
        )
      ,
        ( 1
        , pure $ constrained $ \x -> exists (\_ -> pure True) $ \b ->
            [ ifElse b True (x `satisfies` baseSpec)
            , x `satisfies` baseSpec
            ]
        )
      ,
        ( 1
        , pure $ constrained $ \x -> exists (\_ -> pure False) $ \b ->
            [ ifElse b (x `satisfies` baseSpec) True
            , x `satisfies` baseSpec
            ]
        )
      ,
        ( 1
        , pure $ constrained $ \x -> explanation (pure "its very subtle, you won't get it.") $ x `satisfies` baseSpec
        )
      , (10, pure baseSpec)
      ]

pattern Product ::
  forall c.
  () =>
  forall a b.
  ( c ~ Prod a b
  , AppRequires ProdW '[a, b] (Prod a b)
  ) =>
  Term a ->
  Term b ->
  Term c
pattern Product x y <- (App (getWitness -> Just ProdW) (x :> y :> Nil))

-- =================================================
-- CAN WE MOVE THIS OUT OF TheKnot?

data ElemW :: [Type] -> Type -> Type where
  ElemW :: HasSpec a => ElemW '[a, [a]] Bool

deriving instance Eq (ElemW dom rng)

instance Show (ElemW dom rng) where
  show ElemW = "elem_"

instance Syntax ElemW

instance Semantics ElemW where
  semantics ElemW = elem

instance Logic ElemW where
  propagate f ctxt (ExplainSpec es s) = explainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate ElemW (HOLE :<: (x :: [w])) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ElemW ((v' :: Term w) :> Lit x :> Nil)) (v :-> ps)
  propagate ElemW (x :>: HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ElemW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate ElemW (HOLE :<: es) spec =
    caseBoolSpec spec $ \case
      True -> memberSpecList (nub es) (pure "propagate on (elem_ x []), The empty list, [], has no solution")
      False -> notMemberSpec es
  propagate ElemW (e :>: HOLE) spec =
    caseBoolSpec spec $ \case
      True -> typeSpec (ListSpec Nothing [e] mempty mempty NoFold)
      False -> typeSpec (ListSpec Nothing mempty mempty (notEqualSpec e) NoFold)

  rewriteRules ElemW (_ :> Lit [] :> Nil) Evidence = Just $ Lit False
  rewriteRules ElemW (t :> Lit [a] :> Nil) Evidence = Just $ t ==. (Lit a)
  rewriteRules _ _ _ = Nothing

  saturate ElemW ((FromGeneric (Product (x :: Term a) (y :: Term b)) :: Term c) :> Lit zs :> Nil)
    | Just Refl <- eqT @c @(a, b) = case zs of
        (w : ws) -> [ElemPred True x (fmap fst (w :| ws))]
        [] -> [FalsePred (pure $ "empty list, zs , in elem_ " ++ show (x, y) ++ " zs")]
    | otherwise = []
  saturate ElemW (x :> Lit (y : ys) :> Nil) = [satisfies x (MemberSpec (y :| ys))]
  saturate _ _ = []

infix 4 `elem_`
elem_ :: (Sized [a], HasSpec a) => Term a -> Term [a] -> Term Bool
elem_ = appTerm ElemW

elemFn :: HasSpec a => Fun '[a, [a]] Bool
elemFn = Fun ElemW

pattern Elem ::
  forall b.
  () =>
  forall a.
  (b ~ Bool, Eq a, HasSpec a) =>
  Term a ->
  Term [a] ->
  Term b
pattern Elem x y <-
  ( App
      (getWitness -> Just ElemW)
      (x :> y :> Nil)
    )

-- ================================================================
-- The TypeSpec for List. Used in the HasSpec instance for Lists
-- ================================================================

data ListSpec a = ListSpec
  { listSpecHint :: Maybe Integer
  , listSpecMust :: [a]
  , listSpecSize :: Specification Integer
  , listSpecElem :: Specification a
  , listSpecFold :: FoldSpec a
  }

instance
  ( Arbitrary a
  , Arbitrary (FoldSpec a)
  , Arbitrary (TypeSpec a)
  , HasSpec a
  ) =>
  Arbitrary (ListSpec a)
  where
  arbitrary = ListSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (ListSpec a b c d e) = [ListSpec a' b' c' d' e' | (a', b', c', d', e') <- shrink (a, b, c, d, e)]

instance HasSpec a => Show (FoldSpec a) where
  showsPrec d = shows . prettyPrec d

instance HasSpec a => Pretty (WithPrec (FoldSpec a)) where
  pretty (WithPrec _ NoFold) = "NoFold"
  pretty (WithPrec d (FoldSpec fun s)) =
    parensIf (d > 10) $
      "FoldSpec"
        /> vsep'
          [ "fn   =" <+> viaShow fun
          , "spec =" <+> pretty s
          ]

instance HasSpec a => Pretty (FoldSpec a) where
  pretty = prettyPrec 0

instance HasSpec a => Show (ListSpec a) where
  showsPrec d = shows . prettyPrec d

instance
  HasSpec a =>
  Pretty (WithPrec (ListSpec a))
  where
  pretty (WithPrec d s) =
    parensIf (d > 10) $
      "ListSpec"
        /> vsep'
          [ "hint =" <+> viaShow (listSpecHint s)
          , "must =" <+> viaShow (listSpecMust s)
          , "size =" <+> pretty (listSpecSize s)
          , "elem =" <+> pretty (listSpecElem s)
          , "fold =" <+> pretty (listSpecFold s)
          ]

instance HasSpec a => Pretty (ListSpec a) where
  pretty = prettyPrec 0

guardListSpec :: HasSpec a => [String] -> ListSpec a -> Specification [a]
guardListSpec msg l@(ListSpec _hint must size elemS _fold)
  | ErrorSpec es <- size = ErrorSpec $ (NE.fromList ("Error in size of ListSpec" : msg)) <> es
  | Just u <- knownUpperBound size
  , u < 0 =
      ErrorSpec $ NE.fromList (["Negative size in guardListSpec", show size] ++ msg)
  | not (all (`conformsToSpec` elemS) must) =
      ErrorSpec $
        ( NE.fromList
            (["Some items in the must list do not conform to 'element' spec.", "   " ++ show elemS] ++ msg)
        )
  | otherwise = (typeSpec l)

instance (Sized [a], HasSpec a) => HasSpec [a] where
  type TypeSpec [a] = ListSpec a
  type Prerequisites [a] = HasSpec a
  emptySpec = ListSpec Nothing [] mempty mempty NoFold
  combineSpec l1@(ListSpec msz must size elemS foldS) l2@(ListSpec msz' must' size' elemS' foldS') =
    let must'' = nub $ must <> must'
        elemS'' = elemS <> elemS'
        size'' = size <> size'
        foldeither = combineFoldSpec foldS foldS'
        msg = ["Error in combineSpec for ListSpec", "1) " ++ show l1, "2) " ++ show l2]
     in case foldeither of
          Left foldmsg -> ErrorSpec (NE.fromList (msg ++ foldmsg))
          Right fold'' -> guardListSpec msg $ ListSpec (unionWithMaybe min msz msz') must'' size'' elemS'' fold''

  genFromTypeSpec (ListSpec _ must _ elemS _)
    | any (not . (`conformsToSpec` elemS)) must =
        genError "genTypeSpecSpec @ListSpec: some elements of mustSet do not conform to elemS"
  genFromTypeSpec (ListSpec msz must TrueSpec elemS NoFold) = do
    lst <- case msz of
      Nothing -> listOfT $ genFromSpecT elemS
      Just szHint -> do
        sz <- genFromSizeSpec (leqSpec szHint)
        listOfUntilLenT (genFromSpecT elemS) (fromIntegral sz) (const True)
    pureGen $ shuffle (must ++ lst)
  genFromTypeSpec (ListSpec msz must szSpec elemS NoFold) = do
    sz0 <- genFromSizeSpec (szSpec <> geqSpec (sizeOf must) <> maybe TrueSpec (leqSpec . max 0) msz)
    let sz = fromIntegral (sz0 - sizeOf must)
    lst <-
      listOfUntilLenT
        (genFromSpecT elemS)
        sz
        ((`conformsToSpec` szSpec) . (+ sizeOf must) . fromIntegral)
    pureGen $ shuffle (must ++ lst)
  genFromTypeSpec (ListSpec msz must szSpec elemS (FoldSpec f foldS)) = do
    let szSpec' = szSpec <> maybe TrueSpec (leqSpec . max 0) msz
    genFromFold must szSpec' elemS f foldS

  shrinkWithTypeSpec (ListSpec _ _ _ es _) as =
    shrinkList (shrinkWithSpec es) as

  cardinalTypeSpec _ = TrueSpec

  guardTypeSpec = guardListSpec

  conformsTo xs (ListSpec _ must size elemS foldS) =
    sizeOf xs
      `conformsToSpec` size
      && all (`elem` xs) must
      && all (`conformsToSpec` elemS) xs
      && xs
        `conformsToFoldSpec` foldS

  toPreds x (ListSpec msz must size elemS foldS) =
    (forAll x $ \x' -> satisfies x' elemS)
      <> (forAll (Lit must) $ \x' -> Assert (elem_ x' x))
      <> toPredsFoldSpec x foldS
      <> satisfies (sizeOf_ x) size
      <> maybe TruePred (flip genHint x) msz

sizeOf_ :: (HasSpec a, Sized a) => Term a -> Term Integer
sizeOf_ = curryList (App SizeOfW)

-- | Because Sizes should always be >= 0, We provide this alternate generator
--   that can be used to replace (genFromSpecT @Integer), to ensure this important property
genFromSizeSpec :: MonadGenError m => Specification Integer -> GenT m Integer
genFromSizeSpec integerSpec = genFromSpecT (integerSpec <> geqSpec 0)

instance (Sized [a], HasSpec a) => HasGenHint [a] where
  type Hint [a] = Integer
  giveHint szHint = typeSpec $ ListSpec (Just szHint) [] mempty mempty NoFold

instance Forallable [a] a where
  fromForAllSpec es = typeSpec (ListSpec Nothing [] mempty es NoFold)
  forAllToList = id

-- =====================================================================
-- Syntax, Semantics and Logic instances for function symbols on List

data ListW (args :: [Type]) (res :: Type) where
  FoldMapW :: forall a b. (Foldy b, HasSpec a) => Fun '[a] b -> ListW '[[a]] b
  SingletonListW :: HasSpec a => ListW '[a] [a]
  AppendW :: (HasSpec a, Typeable a, Show a) => ListW '[[a], [a]] [a]

instance Semantics ListW where
  semantics = listSem

instance Syntax ListW where
  prettyWit AppendW (Lit n :> y :> Nil) p = Just $ parensIf (p > 10) $ "append_" <+> short n <+> prettyPrec 10 y
  prettyWit AppendW (y :> Lit n :> Nil) p = Just $ parensIf (p > 10) $ "append_" <+> prettyPrec 10 y <+> short n
  prettyWit _ _ _ = Nothing

listSem :: ListW dom rng -> FunTy dom rng
listSem (FoldMapW (Fun f)) = adds . map (semantics f)
listSem SingletonListW = (: [])
listSem AppendW = (++)

instance Show (ListW d r) where
  show AppendW = "append_"
  show SingletonListW = "singletonList_"
  show (FoldMapW n) = "(FoldMapW  " ++ show n ++ ")"

deriving instance (Eq (ListW d r))

instance Logic ListW where
  propagateTypeSpec (FoldMapW f) (Unary HOLE) ts cant =
    typeSpec (ListSpec Nothing [] TrueSpec TrueSpec $ FoldSpec f (TypeSpec ts cant))
  propagateTypeSpec SingletonListW (Unary HOLE) (ListSpec _ m sz e f) cant
    | length m > 1 =
        ErrorSpec $
          NE.fromList
            [ "Too many required elements for SingletonListW : "
            , "  " ++ show m
            ]
    | not $ 1 `conformsToSpec` sz =
        ErrorSpec $ pure $ "Size spec requires too many elements for SingletonListW : " ++ show sz
    | bad@(_ : _) <- filter (not . (`conformsToSpec` e)) m =
        ErrorSpec $
          NE.fromList
            [ "The following elements of the must spec do not conforms to the elem spec:"
            , show bad
            ]
    -- There is precisely one required element in the final list, so the argument to singletonList_ has to
    -- be that element and we have to respect the cant and fold specs
    | [a] <- m = equalSpec a <> notMemberSpec [z | [z] <- cant] <> reverseFoldSpec f
    -- We have to respect the elem-spec, the can't spec, and the fold spec.
    | otherwise = e <> notMemberSpec [a | [a] <- cant] <> reverseFoldSpec f
  propagateTypeSpec AppendW ctx (ts@ListSpec {listSpecElem = e}) cant
    | (HOLE :? Value (ys :: [a]) :> Nil) <- ctx
    , Evidence <- prerequisites @[a]
    , all (`conformsToSpec` e) ys =
        TypeSpec (alreadyHave ys ts) (suffixedBy ys cant)
    | (Value (ys :: [a]) :! Unary HOLE) <- ctx
    , Evidence <- prerequisites @[a]
    , all (`conformsToSpec` e) ys =
        TypeSpec (alreadyHave ys ts) (prefixedBy ys cant)
    | otherwise = ErrorSpec $ pure "The spec given to propagate for AppendW is inconsistent!"

  propagateMemberSpec (FoldMapW f) (Unary HOLE) es =
    typeSpec (ListSpec Nothing [] TrueSpec TrueSpec $ FoldSpec f (MemberSpec es))
  propagateMemberSpec SingletonListW (Unary HOLE) xss =
    case [a | [a] <- NE.toList xss] of
      [] ->
        ErrorSpec $ (pure "PropagateSpec SingletonListW  with MemberSpec which has no lists of length 1")
      (x : xs) -> MemberSpec (x :| xs)
  propagateMemberSpec AppendW ctx xss
    | (HOLE :<: (ys :: [a])) <- ctx
    , Evidence <- prerequisites @[a] =
        -- Only keep the prefixes of the elements of xss that can
        -- give you the correct resulting list
        case suffixedBy ys (NE.toList xss) of
          [] ->
            ErrorSpec
              ( NE.fromList
                  [ "propagateSpecFun (append HOLE ys) with (MemberSpec xss)"
                  , "there are no elements in xss with suffix ys"
                  ]
              )
          (x : xs) -> MemberSpec (x :| xs)
    | ((ys :: [a]) :>: HOLE) <- ctx
    , Evidence <- prerequisites @[a] =
        -- Only keep the suffixes of the elements of xss that can
        -- give you the correct resulting list
        case prefixedBy ys (NE.toList xss) of
          [] ->
            ErrorSpec
              ( NE.fromList
                  [ "propagateSpecFun (append ys HOLE) with (MemberSpec xss)"
                  , "there are no elements in xss with prefix ys"
                  ]
              )
          (x : xs) -> MemberSpec (x :| xs)

  mapTypeSpec SingletonListW ts = typeSpec (ListSpec Nothing [] (equalSpec 1) (typeSpec ts) NoFold)
  mapTypeSpec (FoldMapW g) ts =
    constrained $ \x ->
      unsafeExists $ \x' ->
        Assert (x ==. appFun (foldMapFn g) x') <> toPreds x' ts

foldMap_ :: forall a b. (Foldy b, HasSpec a) => (Term a -> Term b) -> Term [a] -> Term b
foldMap_ f = appFun $ foldMapFn $ toFn $ f (V v)
  where
    v = Var (-1) "v" :: Var a
    -- Turn `f (V v) = fn (gn (hn v))` into `composeFn fn (composeFn gn hn)`
    -- Note: composeFn :: HasSpec b => Fun '[b] c -> Fun '[a] b -> Fun '[a] c
    toFn :: forall x. HasCallStack => Term x -> Fun '[a] x
    toFn (App fn (V v' :> Nil)) | Just Refl <- eqVar v v' = Fun fn
    toFn (App fn (t :> Nil)) = composeFn (Fun fn) (toFn t)
    toFn (V v') | Just Refl <- eqVar v v' = idFn
    toFn _ = error "foldMap_ has not been given a function of the form \\ x -> f (g ... (h x))"

-- function symbol definitions for List
sum_ ::
  Foldy a =>
  Term [a] ->
  Term a
sum_ = foldMap_ id

singletonList_ :: (Sized [a], HasSpec a) => Term a -> Term [a]
singletonList_ = appTerm SingletonListW

append_ :: (Sized [a], HasSpec a) => Term [a] -> Term [a] -> Term [a]
append_ = appTerm AppendW

-- Fun types for lists and their helper functions

appendFn :: forall a. (Sized [a], HasSpec a) => Fun '[[a], [a]] [a]
appendFn = Fun AppendW

singletonListFn :: forall a. HasSpec a => Fun '[a] [a]
singletonListFn = Fun SingletonListW

foldMapFn :: forall a b. (HasSpec a, Foldy b) => Fun '[a] b -> Fun '[[a]] b
foldMapFn f = Fun (FoldMapW f)

reverseFoldSpec :: FoldSpec a -> Specification a
reverseFoldSpec NoFold = TrueSpec
-- The single element list has to sum to something that obeys spec, i.e. `conformsToSpec (f a) spec`
reverseFoldSpec (FoldSpec (Fun fn) spec) = propagate fn (HOLE :? Nil) spec

-- ==============  Helper functions

prefixedBy :: Eq a => [a] -> [[a]] -> [[a]]
prefixedBy ys xss = [drop (length ys) xs | xs <- xss, ys `isPrefixOf` xs]

suffixedBy :: Eq a => [a] -> [[a]] -> [[a]]
suffixedBy ys xss = [take (length xs - length ys) xs | xs <- xss, ys `isSuffixOf` xs]

alreadyHave :: Eq a => [a] -> ListSpec a -> ListSpec a
alreadyHave ys (ListSpec h m sz e f) =
  ListSpec
    -- Reduce the hint
    (fmap (subtract (sizeOf ys)) h)
    -- The things in `ys` have already been added to the list, no need to
    -- require them too
    (m \\ ys)
    -- Reduce the required size
    (constrained $ \x -> (x + Lit (sizeOf ys)) `satisfies` sz)
    -- Nothing changes about what's a correct element
    e
    -- we have fewer things to sum now
    (alreadyHaveFold ys f)

alreadyHaveFold :: [a] -> FoldSpec a -> FoldSpec a
alreadyHaveFold _ NoFold = NoFold
alreadyHaveFold ys (FoldSpec fn spec) =
  FoldSpec
    fn
    (constrained $ \s -> appTerm theAddFn s (foldMap_ (appFun fn) (Lit ys)) `satisfies` spec)

-- | Used in the HasSpec [a] instance
toPredsFoldSpec :: HasSpec a => Term [a] -> FoldSpec a -> Pred
toPredsFoldSpec _ NoFold = TruePred
toPredsFoldSpec x (FoldSpec funAB sspec) =
  satisfies (appFun (foldMapFn funAB) x) sspec

-- =======================================================
-- FoldSpec is a Spec that appears inside of ListSpec

data FoldSpec a where
  NoFold :: FoldSpec a
  FoldSpec ::
    forall b a.
    ( HasSpec a
    , HasSpec b
    , Foldy b
    ) =>
    Fun '[a] b ->
    Specification b ->
    FoldSpec a

preMapFoldSpec :: HasSpec a => Fun '[a] b -> FoldSpec b -> FoldSpec a
preMapFoldSpec _ NoFold = NoFold
preMapFoldSpec f (FoldSpec g s) = FoldSpec (composeFn g f) s

composeFn :: (HasSpec b, HasSpec c) => Fun '[b] c -> Fun '[a] b -> Fun '[a] c
composeFn (Fun f) (Fun g) = (Fun (ComposeW f g))

idFn :: HasSpec a => Fun '[a] a
idFn = Fun IdW

combineFoldSpec :: FoldSpec a -> FoldSpec a -> Either [String] (FoldSpec a)
combineFoldSpec NoFold s = pure s
combineFoldSpec s NoFold = pure s
combineFoldSpec (FoldSpec (Fun f) s) (FoldSpec (Fun g) s') =
  case sameFunSym f g of
    Just (_h, Refl, Refl, Refl) -> pure $ FoldSpec (Fun f) (s <> s')
    Nothing -> Left ["Can't combine fold specs on different functions", "  " ++ show f, "  " ++ show g]

conformsToFoldSpec :: forall a. [a] -> FoldSpec a -> Bool
conformsToFoldSpec _ NoFold = True
conformsToFoldSpec xs (FoldSpec (Fun f) s) = adds (map (semantics f) xs) `conformsToSpec` s

class (HasSpec a, NumLike a, Logic IntW) => Foldy a where
  genList ::
    MonadGenError m => Specification a -> Specification a -> GenT m [a]
  theAddFn :: IntW '[a, a] a
  theAddFn = AddW
  theZero :: a
  theZero = 0
  genSizedList ::
    MonadGenError m =>
    Specification Integer ->
    Specification a ->
    Specification a ->
    GenT m [a]
  noNegativeValues :: Bool

-- ================
-- Sized
-- ================

type SizeSpec = NumSpec Integer

class Sized t where
  sizeOf :: t -> Integer
  default sizeOf :: (HasSimpleRep t, Sized (SimpleRep t)) => t -> Integer
  sizeOf = sizeOf . toSimpleRep

  liftSizeSpec :: HasSpec t => SizeSpec -> [Integer] -> Specification t
  default liftSizeSpec ::
    ( HasSpec t
    , HasSimpleRep t
    , Sized (SimpleRep t)
    , HasSpec (SimpleRep t)
    , TypeSpec t ~ TypeSpec (SimpleRep t)
    ) =>
    SizeSpec ->
    [Integer] ->
    Specification t
  liftSizeSpec sz cant = fromSimpleRepSpec $ liftSizeSpec sz cant

  liftMemberSpec :: HasSpec t => [Integer] -> Specification t
  default liftMemberSpec ::
    ( HasSpec t
    , HasSpec (SimpleRep t)
    , HasSimpleRep t
    , Sized (SimpleRep t)
    , TypeSpec t ~ TypeSpec (SimpleRep t)
    ) =>
    [Integer] ->
    Specification t
  liftMemberSpec = fromSimpleRepSpec . liftMemberSpec

  sizeOfTypeSpec :: HasSpec t => TypeSpec t -> Specification Integer
  default sizeOfTypeSpec ::
    ( HasSpec (SimpleRep t)
    , Sized (SimpleRep t)
    , TypeSpec t ~ TypeSpec (SimpleRep t)
    ) =>
    TypeSpec t ->
    Specification Integer
  sizeOfTypeSpec = sizeOfTypeSpec @(SimpleRep t)

adds :: Foldy a => [a] -> a
adds = foldr (semantics theAddFn) theZero

-- =============================================================
-- All Foldy class instances are over Numbers (so far).
-- Foldy class requires higher order functions, so here they are.
-- Note this is a new witness type, different from BaseW
-- but serving the same purpose. Note it can take Witnesses from
-- other classes as inputs. See FlipW amd ComposeW
-- ==============================================================

-- We need Arbitrary Specification to do this
instance {-# OVERLAPPABLE #-} (Arbitrary (Specification a {- Arbitrary (TypeSpec a), -}), Foldy a) => Arbitrary (FoldSpec a) where
  arbitrary = oneof [FoldSpec (Fun IdW) <$> arbitrary, pure NoFold]
  shrink NoFold = []
  shrink (FoldSpec (Fun wit) spec)
    | Just (idW, Refl, Refl, Refl) <- sameFunSym (IdW @a) wit = FoldSpec (Fun idW) <$> shrink spec
  shrink FoldSpec {} = [NoFold]

data FunW (dom :: [Type]) (rng :: Type) where
  IdW :: forall a. FunW '[a] a
  ComposeW ::
    forall b t1 t2 a r.
    ( AppRequires t1 '[b] r
    , AppRequires t2 '[a] b
    , HasSpec b
    ) =>
    t1 '[b] r ->
    t2 '[a] b ->
    FunW '[a] r
  FlipW ::
    forall t a b r.
    AppRequires t '[a, b] r =>
    t '[a, b] r ->
    FunW '[b, a] r

funSem :: FunW dom rng -> FunTy dom rng
funSem IdW = id
funSem (ComposeW f g) = (\a -> semantics f (semantics g a))
funSem (FlipW (f :: g d r)) = flip (semantics f)

instance Semantics FunW where
  semantics = funSem

instance Syntax FunW

instance Show (FunW dom rng) where
  show IdW = "id_"
  show (FlipW f) = "(flip_ " ++ show f ++ ")"
  show (ComposeW x y) = "(compose_ " ++ show x ++ " " ++ show y ++ ")"

instance Eq (FunW dom rng) where
  IdW == IdW = True
  FlipW t1 == FlipW t2 = compareWit t1 t2
  ComposeW f f' == ComposeW g g' = compareWit f g && compareWit f' g'
  _ == _ = False

compareWit ::
  forall t1 bs1 r1 t2 bs2 r2.
  (AppRequires t1 bs1 r1, AppRequires t2 bs2 r2) =>
  t1 bs1 r1 ->
  t2 bs2 r2 ->
  Bool
compareWit x y = case (eqT @t1 @t2, eqT @bs1 @bs2, eqT @r1 @r2) of
  (Just Refl, Just Refl, Just Refl) -> x == y
  _ -> False

-- ===================================
-- Logic instances for IdW, FlipW and ComposeW
-- Also their Haskell implementations id_ flip_ composeFn

instance Logic FunW where
  propagate IdW (Unary HOLE) = id
  propagate (FlipW f) ctx = propagate f (flipCtx ctx)
  propagate (ComposeW f g) (Unary HOLE) = propagate g (Unary HOLE) . propagate f (Unary HOLE)

  mapTypeSpec IdW ts = typeSpec ts
  mapTypeSpec (ComposeW g h) ts = mapSpec g . mapSpec h $ typeSpec ts

  -- Note we need the Evidence to apply App to f, and to apply App to g
  rewriteRules (ComposeW f g) (x :> Nil) Evidence = Just $ App f (App g (x :> Nil) :> Nil)
  rewriteRules IdW (x :> Nil) Evidence = Just x
  rewriteRules (FlipW f) (a@Lit {} :> b :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW f) (a :> b@Lit {} :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW {}) _ Evidence = Nothing

id_ :: forall a. HasSpec a => Term a -> Term a
id_ = appTerm IdW

--   -- Note we need Evidence to apply App to f

flip_ ::
  forall (t :: [Type] -> Type -> Type) a b r.
  (HasSpec b, HasSpec a, AppRequires t '[a, b] r) =>
  t '[a, b] r ->
  Term b ->
  Term a ->
  Term r
flip_ x = appTerm (FlipW x)

compose_ ::
  forall b t1 t2 a r.
  ( AppRequires t1 '[b] r
  , AppRequires t2 '[a] b
  ) =>
  t1 '[b] r ->
  t2 '[a] b ->
  Term a ->
  Term r
compose_ f g = appTerm $ ComposeW f g -- @b @c1 @c2 @s1 @s2 @t1 @t2 @a @r f g

-- =======================================================
-- The Foldy class instances for Numbers
-- =======================================================

instance Foldy Integer where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int8 where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int16 where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int32 where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Int64 where
  noNegativeValues = False
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Natural where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Word8 where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Word16 where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Word32 where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

instance Foldy Word64 where
  noNegativeValues = True
  genList = genNumList
  genSizedList = genListWithSize

genInverse ::
  ( MonadGenError m
  , HasSpec a
  , HasSpec b
  ) =>
  Fun '[a] b ->
  Specification a ->
  b ->
  GenT m a
genInverse (Fun f) argS x =
  let argSpec' = argS <> propagate f (HOLE :? Nil) (equalSpec x)
   in explainNE
        ( NE.fromList
            [ "genInverse"
            , "  f = " ++ show f
            , show $ "  argS =" <+> pretty argS
            , "  x = " ++ show x
            , show $ "  argSpec' =" <+> pretty argSpec'
            ]
        )
        $ genFromSpecT argSpec'

genFromFold ::
  forall m a b.
  ( MonadGenError m
  , Foldy b
  , HasSpec a
  ) =>
  [a] ->
  Specification Integer ->
  Specification a ->
  Fun '[a] b ->
  Specification b ->
  GenT m [a]
genFromFold must (simplifySpec -> size) elemS fun@(Fun fn) foldS
  | isErrorLike size =
      fatalErrorNE (NE.cons "genFromFold has ErrorLike sizeSpec" (errorLikeMessage size))
  | isErrorLike elemS =
      fatalErrorNE (NE.cons "genFromFold has ErrorLike elemSpec" (errorLikeMessage elemS))
  | isErrorLike foldS =
      fatalErrorNE (NE.cons "genFromFold has ErrorLike totalSpec" (errorLikeMessage foldS))
  | otherwise = ( explainNE
                    ( NE.fromList
                        [ "while calling genFromFold"
                        , "  must  = " ++ show must
                        , "  size  = " ++ show size
                        , "  elemS = " ++ show elemS
                        , "  fun   = " ++ show fun
                        , "  foldS = " ++ show foldS
                        ]
                    )
                )
      $ do
        let elemS' :: Specification b
            elemS' = mapSpec fn elemS
            mustVal = adds (map (semantics fn) must)
            foldS' :: Specification b
            foldS' = propagate theAddFn (HOLE :? Value mustVal :> Nil) foldS
            sizeSpec' :: Specification Integer
            sizeSpec' = propagate AddW (HOLE :? Value (sizeOf must) :> Nil) size
        when (isErrorLike sizeSpec') $ genError "Inconsistent size spec"
        results0 <- case sizeSpec' of
          TrueSpec -> genList (simplifySpec elemS') (simplifySpec foldS')
          _ -> genSizedList sizeSpec' (simplifySpec elemS') (simplifySpec foldS')
        results <-
          explainNE
            ( NE.fromList
                [ "genInverse"
                , "  fun = " ++ show fun
                , "  results0 = " ++ show results0
                , show $ "  elemS' =" <+> pretty elemS'
                ]
            )
            $ mapM (genInverse fun elemS) results0
        pureGen $ shuffle $ must ++ results

addFun :: NumLike n => Fun '[n, n] n
addFun = Fun AddW

-- ================================================
-- Sized instance for Lists

instance Sized [a] where
  sizeOf = toInteger . length
  liftSizeSpec spec cant = typeSpec (ListSpec Nothing mempty (TypeSpec spec cant) TrueSpec NoFold)
  liftMemberSpec xs = case NE.nonEmpty xs of
    Nothing -> ErrorSpec (pure ("In liftMemberSpec for (Sized List) instance, xs is the empty list"))
    Just zs -> typeSpec (ListSpec Nothing mempty (MemberSpec zs) TrueSpec NoFold)
  sizeOfTypeSpec (ListSpec _ _ _ ErrorSpec {} _) = equalSpec 0
  sizeOfTypeSpec (ListSpec _ must sizespec _ _) = sizespec <> geqSpec (sizeOf must)

-- ======================================================================
-- Size and its 'generic' operations over Sized types.
-- ======================================================================

data SizeW (dom :: [Type]) rng :: Type where
  SizeOfW :: (Sized n, HasSpec n) => SizeW '[n] Integer

deriving instance Eq (SizeW ds r)

instance Show (SizeW d r) where
  show SizeOfW = "sizeOf_"

instance Semantics SizeW where
  semantics SizeOfW = sizeOf -- From the Sized class.

instance Syntax SizeW

instance Logic SizeW where
  propagateTypeSpec SizeOfW (Unary HOLE) ts cant = liftSizeSpec ts cant

  propagateMemberSpec SizeOfW (Unary HOLE) es = liftMemberSpec (NE.toList es)

  mapTypeSpec (SizeOfW :: SizeW '[a] b) ts =
    constrained $ \x ->
      unsafeExists $ \x' -> Assert (x ==. sizeOf_ x') <> toPreds @a x' ts

sizeOfFn :: forall a. (HasSpec a, Sized a) => Fun '[a] Integer
sizeOfFn = Fun SizeOfW

-- ======================================

rangeSize :: Integer -> Integer -> SizeSpec
rangeSize a b | a < 0 || b < 0 = error ("Negative Int in call to rangeSize: " ++ show a ++ " " ++ show b)
rangeSize a b = NumSpecInterval (Just a) (Just b)

between :: (HasSpec a, TypeSpec a ~ NumSpec a) => a -> a -> Specification a
between lo hi = TypeSpec (NumSpecInterval (Just lo) (Just hi)) []

-- | The widest interval whose largest element is admitted by the original spec
maxSpec :: Specification Integer -> Specification Integer
maxSpec (ExplainSpec es s) = explainSpecOpt es (maxSpec s)
maxSpec TrueSpec = TrueSpec
maxSpec s@(SuspendedSpec _ _) =
  constrained $ \x -> unsafeExists $ \y -> [y `satisfies` s, Explain (pure "maxSpec on SuspendedSpec") $ Assert (x <=. y)]
maxSpec (ErrorSpec xs) = ErrorSpec xs
maxSpec (MemberSpec xs) = leqSpec (maximum xs)
maxSpec (TypeSpec (NumSpecInterval _ hi) bad) = TypeSpec (NumSpecInterval Nothing hi) bad

-- How to constrain the size of any type, with a Sized instance
hasSize :: (HasSpec t, Sized t) => SizeSpec -> Specification t
hasSize sz = liftSizeSpec sz []

-- =================================================
infix 4 +.
(+.) :: NumLike a => Term a -> Term a -> Term a
(+.) = addFn

negate_ :: NumLike a => Term a -> Term a
negate_ = negateFn

infix 4 -.
(-.) :: Numeric n => Term n -> Term n -> Term n
(-.) x y = addFn x (negateFn y)

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
