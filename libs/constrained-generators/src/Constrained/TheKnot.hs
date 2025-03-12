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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | All the things that are mutually recursive.
module Constrained.TheKnot where

import Constrained.Base
import Constrained.Conformance
import Constrained.Generic
import Constrained.NumSpec
import Constrained.Syntax

import Constrained.Core (
  Evidence (..),
  NonEmpty ((:|)),
  Var (..),
  eqVar,
  freshen,
  unValue,
  unionWithMaybe,
 )
import Constrained.Env
import Constrained.GenT
import Constrained.Graph hiding (dependency, irreflexiveDependencyOn, noDependencies)
import qualified Constrained.Graph as Graph
import Constrained.List
import Control.Monad
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable
import Data.Kind
import Data.List (nub, partition)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup (Any (..), getSum)
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Typeable
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
-- STUBS
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
    | emptyA, emptyB = genError $ pure ("genFromTypeSpec @SumSpec: empty")
    | emptyA = SumRight <$> genFromSpecT sb
    | emptyB = SumLeft <$> genFromSpecT sa
    | fA == 0, fB == 0 = genError $ pure ("All frequencies 0")
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

-- ===========================================================================
-- HasSpec Bool
-- ===========================================================================

instance HasSimpleRep Bool
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
data BoolW (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  NotW :: BoolW "not_" '[Bool] Bool
  OrW :: BoolW "or_" '[Bool, Bool] Bool

deriving instance Eq (BoolW s dom rng)

instance Show (BoolW s dom rng) where
  show NotW = "not_"
  show OrW = "or_"

boolSem :: BoolW sym dom rng -> FunTy dom rng
boolSem NotW = not
boolSem OrW = (||)

instance Semantics BoolW where
  semantics = boolSem

instance Syntax BoolW

-- ======= Logic instance NotW(not_)

instance (HasSpec Bool, TypeSpec Bool ~ SumSpec () ()) => Logic "not_" BoolW '[Bool] Bool where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context NotW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App NotW (v' :> Nil)) (v :-> ps)
  propagate (Context NotW (HOLE :<> End)) spec =
    caseBoolSpec spec (equalSpec . not)
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for NotW with wrong number of arguments. " ++ show ctx)

  mapTypeSpec NotW (SumSpec h a b) = typeSpec $ SumSpec h b a

not_ :: Term Bool -> Term Bool
not_ = appTerm NotW

-- ======= Logic instance OrW(or_)

instance HasSpec Bool => Logic "or_" BoolW '[Bool, Bool] Bool where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context OrW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App OrW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context OrW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App OrW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context OrW (HOLE :<> (s :: Bool) :<| End)) spec =
    caseBoolSpec spec (okOr s)
  propagate (Context OrW ((s :: Bool) :|> HOLE :<> End)) spec =
    caseBoolSpec spec (okOr s)
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for OrW with wrong number of arguments. " ++ show ctx)

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

-- ======= Logic instance EqualW(==.)

instance (HasSpec Bool, Eq a, Typeable a) => Logic "==." BaseW '[a, a] Bool where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context EqualW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App EqualW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context EqualW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App EqualW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context EqualW (HOLE :<> (s :: a) :<| End)) spec =
    caseBoolSpec spec $ \case True -> equalSpec s; False -> notEqualSpec s
  propagate (Context EqualW ((s :: a) :|> HOLE :<> End)) spec =
    caseBoolSpec spec $ \case True -> equalSpec s; False -> notEqualSpec s
  propagate ctx@(Context _ _) _ =
    ErrorSpec $ pure ("Logic instance for EqualW with wrong number of arguments. " ++ show ctx)

  rewriteRules EqualW (t :> t' :> Nil) Evidence
    | t == t' = Just $ lit True
    | otherwise = Nothing

  saturate EqualW (FromGeneric (InjLeft _) :> t :> Nil) = [toPreds t (SumSpec Nothing TrueSpec (ErrorSpec (pure "saturatePred")))]
  saturate EqualW (FromGeneric (InjRight _) :> t :> Nil) = [toPreds t (SumSpec Nothing (ErrorSpec (pure "saturatePred")) TrueSpec)]
  saturate _ _ = []

infix 4 ==.
(==.) :: HasSpec a => Term a -> Term a -> Term Bool
(==.) = appTerm EqualW

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

-- Logic instances for (<=.) and (<.)
instance
  OrdLike a =>
  Logic "<=." NumOrdW '[a, a] Bool
  where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context LessOrEqualW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App LessOrEqualW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context LessOrEqualW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App LessOrEqualW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context LessOrEqualW (HOLE :<> l :<| End)) spec =
    caseBoolSpec spec $ \case True -> leqSpec l; False -> gtSpec l
  propagate (Context LessOrEqualW (l :|> HOLE :<> End)) spec =
    caseBoolSpec spec $ \case True -> geqSpec l; False -> ltSpec l
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for LessOrEqualW with wrong number of arguments. " ++ show ctx)

infixr 4 <=.
(<=.) :: forall a. OrdLike a => Term a -> Term a -> Term Bool
(<=.) = appTerm LessOrEqualW

instance
  OrdLike a =>
  Logic "<." NumOrdW '[a, a] Bool
  where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context LessW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App LessW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context LessW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App LessW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context LessW (HOLE :<> l :<| End)) spec =
    caseBoolSpec spec $ \case True -> ltSpec l; False -> geqSpec l
  propagate (Context LessW (l :|> HOLE :<> End)) spec =
    caseBoolSpec spec $ \case True -> gtSpec l; False -> leqSpec l
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for LessW with wrong number of arguments. " ++ show ctx)

infixr 4 <.
(<.) :: forall a. OrdLike a => Term a -> Term a -> Term Bool
(<.) = appTerm LessW

-- See  https://www.mathsisfun.com/algebra/inequality-solving.html
instance
  OrdLike a =>
  Logic ">=." NumOrdW '[a, a] Bool
  where
  propagate (Context GreaterOrEqualW (HOLE :<> x :<| End)) spec =
    propagate (Context LessOrEqualW (x :|> HOLE :<> End)) spec
  propagate (Context GreaterOrEqualW (x :|> HOLE :<> End)) spec =
    propagate (Context LessOrEqualW (HOLE :<> x :<| End)) spec
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for GreaterOrEqualW with wrong number of arguments. " ++ show ctx)

infixr 4 >=.
(>=.) :: forall a. OrdLike a => Term a -> Term a -> Term Bool
(>=.) = appTerm GreaterOrEqualW

instance
  OrdLike a =>
  Logic ">." NumOrdW '[a, a] Bool
  where
  propagate (Context GreaterW (HOLE :<> x :<| End)) spec =
    propagate (Context LessW (x :|> HOLE :<> End)) spec
  propagate (Context GreaterW (x :|> HOLE :<> End)) spec =
    propagate (Context LessW (HOLE :<> x :<| End)) spec
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for GreaterW with wrong number of arguments. " ++ show ctx)

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
            (pure ("\nWhile calling simplifySpec on var " ++ show x ++ "\noptP=\n" ++ show optP ++ "\n"))
            (computeSpecSimplified x optP)
  MemberSpec xs -> MemberSpec xs
  ErrorSpec es -> ErrorSpec es
  TypeSpec ts cant -> TypeSpec ts cant
  TrueSpec -> TrueSpec
  ExplainSpec es s -> explainSpecOpt es (simplifySpec s)

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
  App (f :: t sym bs a) (mapList simplifyTerm -> ts)
    | Just vs <- fromLits ts -> Lit $ uncurryList_ unValue (semantics f) vs
    | Just t <- rewriteRules @sym @t @bs @a f ts (Evidence @(AppRequires sym t bs a)) -> simplifyTerm t
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
  FalsePred es -> genError es
  And ps -> do
    spec <- fold <$> mapM (computeSpecSimplified x) ps
    case spec of
      ExplainSpec es (SuspendedSpec y ps') -> pure $ explainSpecOpt es (SuspendedSpec y $ simplifyPred ps')
      SuspendedSpec y ps' -> pure $ SuspendedSpec y $ simplifyPred ps'
      s -> pure s
  Let t b -> pure $ SuspendedSpec x (Let t b)
  Exists k b -> pure $ SuspendedSpec x (Exists k b)
  Assert (Lit True) -> pure mempty
  Assert (Lit False) -> genError1 (show pred3)
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
    fatalError $ NE.fromList ["Dependency error in computeSpec: Reifies", "  " ++ show pred3]
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
    fatalError $
      NE.fromList
        [ "The impossible happened in computeSpec: DependsOn"
        , "  " ++ show x
        , show $ indent 2 (pretty pred3)
        ]
  Reifies {} ->
    fatalError $
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
      Maybe String -> List (Weighted Specification) as3 -> Specification (SumOver as3)
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
  MemberSpec as -> explain1 ("genFromSpecT on spec" ++ show spec) $ pureGen (elements (NE.toList as))
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
    explain
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
  ErrorSpec e -> genError e

-- | A version of `genFromSpecT` that simply errors if the generator fails
genFromSpec :: forall a. (HasCallStack, HasSpec a) => Specification a -> Gen a
genFromSpec spec = do
  res <- catchGen $ genFromSpecT @a @GE spec
  either (error . show . catMessages) pure res

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
        fatalError1 "Trying to shrink a bad value, don't do that!"
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
    explain
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
      fatalError1
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
            Just msgs -> genError msgs
      | otherwise =
          fatalError $
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
    $ explain1
      ( show
          ( "Computing specs for variable "
              <> pretty x /> vsep' (map pretty ps)
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
        explain1 (show $ "Stepping the plan:" /> vsep [pretty plan, pretty env]) $ stepPlan env plan
      go env' plan'

-- | Push as much information we can backwards through the plan.
backPropagation :: SolverPlan -> SolverPlan
-- backPropagation (SolverPlan _plan _graph) =
backPropagation (SolverPlan initplan graph) = SolverPlan (go [] (reverse initplan)) graph
  where
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
--   it takes a function symbol (t c s '[a] b) from a to b.
--   Note, in this context, a function symbol is some constructor of a witnesstype.
--   Eg. ProdFstW, InjRightW, SingletonW, etc. NOT the lifted versions like fst_ singleton_,
--   which construct Terms. We had to wait until here to define this because it
--   depends on Semigroup property of Specification, and Asserting equality
mapSpec ::
  forall s t a b.
  (Logic s t '[a] b, HasSpec a, HasSpec b) => t s '[a] b -> Specification a -> Specification b
mapSpec f (ExplainSpec es s) = explainSpecOpt es (mapSpec f s)
mapSpec f TrueSpec = mapTypeSpec @_ @_ @'[a] @b f (emptySpec @a)
mapSpec _ (ErrorSpec err) = ErrorSpec err
mapSpec f (MemberSpec as) = MemberSpec $ NE.nub $ fmap (semantics f) as
mapSpec f (SuspendedSpec x p) =
  constrained $ \x' ->
    Exists (\_ -> fatalError (pure "mapSpec")) (x :-> fold [Assert $ (x' ==. appTerm f (V x)), p])
mapSpec f (TypeSpec ts cant) = mapTypeSpec @s @t @'[a] @b f ts <> notMemberSpec (map (semantics f) cant)

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
    {-
    Assert (Equal (FromGeneric (InjLeft _)) t) -> [toPreds t (SumSpec Nothing TrueSpec (ErrorSpec (pure "saturatePred")))]
    Assert (Equal (FromGeneric (InjRight _)) t) -> [toPreds t (SumSpec Nothing (ErrorSpec (pure "saturatePred")) TrueSpec)]
    Assert (Elem @Bool @a (FromGeneric (Product @n @m x y)) (Lit zs))
      | Just Refl <- eqT @a @(m, n) -> case zs of
          (w : ws) -> [ElemPred True x (fmap fst (w :| ws))]
          [] -> [FalsePred (pure $ "empty list, zs , in elem_ " ++ show (x, y) ++ " zs")]
    Assert (Elem x (Lit (y : ys))) -> [satisfies x (MemberSpec (y :| ys))]
    -- ElemPred True x ys -> [satisfies x (MemberSpec ys)]
    -}
    Assert ((App (sym :: t s dom Bool) xs) :: Term Bool) -> saturate @s @t @dom @Bool sym xs
    -- TODO: e.g. `elem (pair x y) (lit zs) -> elem x (lit $ map fst zs)` etc.
    _ -> []

-- ================================================================
-- HasSpec for Products
-- ================================================================

pairView :: forall a b. (HasSpec a, HasSpec b) => Term (Prod a b) -> Maybe (Term a, Term b)
pairView (App (sameFunSym $ ProdW @a @b -> Just (_, Refl, Refl, Refl, Refl)) (x :> y :> Nil)) = Just (x, y)
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

instance (HasSpec a, HasSpec b) => Logic "prodFst_" BaseW '[Prod a b] a where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context ProdFstW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ProdFstW (v' :> Nil)) (v :-> ps)
  propagate (Context ProdFstW (HOLE :<> End)) (TypeSpec ts cant) =
    cartesian @a @b (TypeSpec ts cant) TrueSpec
  propagate (Context ProdFstW (HOLE :<> End)) (MemberSpec es) =
    cartesian @a @b (MemberSpec es) TrueSpec
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for ProdFstW with wrong number of arguments. " ++ show ctx)

  rewriteRules ProdFstW ((pairView -> Just (x, _)) :> Nil) Evidence = Just x
  rewriteRules _ _ _ = Nothing

  mapTypeSpec ProdFstW (Cartesian s _) = s

prodFst_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term a
prodFst_ = appTerm ProdFstW

-- ========= ProdSndW

instance (HasSpec a, HasSpec b) => Logic "prodSnd_" BaseW '[Prod a b] b where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context ProdSndW (HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ProdSndW (v' :> Nil)) (v :-> ps)
  propagate (Context ProdSndW (HOLE :<> End)) (TypeSpec ts cant) =
    cartesian @a @b TrueSpec (TypeSpec ts cant)
  propagate (Context ProdSndW (HOLE :<> End)) (MemberSpec es) =
    cartesian @a @b TrueSpec (MemberSpec es)
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for ProdSndW with wrong number of arguments. " ++ show ctx)

  rewriteRules ProdSndW ((pairView -> Just (_, y)) :> Nil) Evidence = Just y
  rewriteRules _ _ _ = Nothing

  mapTypeSpec ProdSndW (Cartesian _ s) = s

prodSnd_ :: (HasSpec a, HasSpec b) => Term (Prod a b) -> Term b
prodSnd_ = appTerm ProdSndW

-- ========= ProdW
sameFst :: Eq a1 => a1 -> [Prod a1 a2] -> [a2]
sameFst a ps = [b | Prod a' b <- ps, a == a']

sameSnd :: Eq a1 => a1 -> [Prod a2 a1] -> [a2]
sameSnd b ps = [a | Prod a b' <- ps, b == b']

instance (HasSpec a, HasSpec b) => Logic "prod_" BaseW '[a, b] (Prod a b) where
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context ProdW (HOLE :<> x :<| End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ProdW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context ProdW (x :|> HOLE :<> End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App ProdW (Lit x :> v' :> Nil)) (v :-> ps)
  propagate (Context ProdW (a :|> HOLE :<> End)) ts@(TypeSpec (Cartesian sa sb) cant)
    | a `conformsToSpec` sa = sb <> foldMap notEqualSpec (sameFst a cant)
    | otherwise =
        ErrorSpec
          (NE.fromList ["propagate (pair_ " ++ show a ++ " HOLE) has conformance failure on a", show ts])
  propagate (Context ProdW (HOLE :<> b :<| End)) ts@(TypeSpec (Cartesian sa sb) cant)
    | b `conformsToSpec` sb = sa <> foldMap notEqualSpec (sameSnd b cant)
    | otherwise =
        ErrorSpec
          (NE.fromList ["propagate (pair_ HOLE " ++ show b ++ ") has conformance failure on b", show ts])
  propagate (Context ProdW (a :|> HOLE :<> End)) (MemberSpec es) =
    case (nub (sameFst a (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show a ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show a ++ " does not appear as the fst component of anything in the MemberSpec."
            ]
  propagate (Context ProdW (HOLE :<> b :<| End)) (MemberSpec es) =
    case (nub (sameSnd b (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show b ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show b ++ " does not appear as the snd component of anything in the MemberSpec."
            ]
  propagate ctx _ =
    ErrorSpec $ pure ("Logic instance for ProdW with wrong number of arguments. " ++ show ctx)

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
