{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Constrained.SimplifyExperiment where

import Constrained.BaseExperiment
import Constrained.GenericExperiment
import Constrained.Core

import Control.Monad.Identity
import Data.Foldable
import Data.List (intersect, isPrefixOf, isSuffixOf, nub, partition, (\\))
import Data.Maybe
import Data.Semigroup (Any (..), Max (..), getAll, getMax, sconcat)
import Data.String(fromString)
import Data.Typeable
import GHC.Stack
import GHC.TypeLits
import GHC.TypeLits(pattern SSymbol)
import Prettyprinter hiding (cat)
import Test.QuickCheck hiding (Args, Fun, forAll, Witness)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Constrained.Env
import Constrained.GenT
import Constrained.List
import Constrained.Core(Var(..),unionWithMaybe,Rename(rename))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Type.Equality(TestEquality(..))
import Control.Monad.Writer (Writer, tell,runWriter)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Monoid qualified as Monoid
import qualified Data.Semigroup as Semigroup

import Constrained.BaseExperiment
import Constrained.SyntaxExperiment

-- ===================================
-- STUBS
elem_ :: Term e -> Term [e] -> Term Bool
elem_ = undefined

-- STUB  Add this to the FunctionSymbol class
{-
rewriteRules ::
    ( TypeList as
    , Typeable as
    , HasSpec b
    , All HasSpec as
    ) =>
    Witness ->
    List Term as ->
    Maybe (Term fn b)
-}    
rewriteRules _ _ = Nothing

conformsToSpec :: a -> Specification a -> Bool
conformsToSpec = undefined

satisfies :: Term a -> Specification a -> Pred
satisfies = undefined

-- =======================================================
-- helpers

ifElse :: (IsPred p, IsPred q) => Term Bool -> p -> q -> Pred 
ifElse b p q = whenTrue b p <> whenTrue (not_ b) q

whenTrue :: forall p. (IsPred p) => Term Bool -> p -> Pred
whenTrue (Lit True) (toPred -> p) = p
whenTrue (Lit False) _ = TruePred
whenTrue b (toPred -> FalsePred {}) = assert (not_ b)
whenTrue _ (toPred -> TruePred) = TruePred
whenTrue b (toPred  -> p) = When b p    

-- | Is the variable x pinned to some free term in p? (free term
-- meaning that all the variables in the term are free in p).
--
-- TODO: complete this with more cases!
pinnedBy :: forall a. (Eq a,Typeable a) => Var a -> Pred -> Maybe (Term a)
-- pinnedBy x (Assert (App (extractFn @EqFn @fn -> Just EqualW) (t :> t' :> Nil)))
pinnedBy x (Assert (EqualPat (t :: Term a) t'))
  | V x' <- t, Just Refl <- eqVar x x' = Just t'
  | V x' <- t', Just Refl <- eqVar x x' = Just t
pinnedBy x (And ps) = listToMaybe $ catMaybes $ map (pinnedBy x) ps
pinnedBy _ _ = Nothing

-- ===================================================================
-- Simplifying
-- ===================================================================

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
aggressiveInlining p 
  | inlined = aggressiveInlining pInlined
  | otherwise = p
  where
    (pInlined, Any inlined) = runWriter $ go (freeVars p) [] p

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

    go fvs sub p = case p of
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
        | otherwise -> pure p
      -- If the term turns into a literal, there is no more generation to do here
      -- so we can ignore the `GenHint`
      GenHint _ t
        | not (isLit t)
        , Lit {} <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure TruePred
        | otherwise -> pure p
      DependsOn t t'
        | not (isLit t)
        , Lit {} <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ TruePred
        | not (isLit t')
        , Lit {} <- substituteAndSimplifyTerm sub t' -> do
            tell $ Any True
            pure $ TruePred
        | otherwise -> pure p
      TruePred -> pure p
      FalsePred {} -> pure p
      Monitor {} -> pure p
      Explain es p -> Explain es <$> go fvs sub p

-- =======================================================================
-- Simplification for preds and terms -------------------------------------
-- =========================================================================

-- | Apply a substitution and simplify the resulting term if the 
-- substitution changed the term.
substituteAndSimplifyTerm :: Subst -> Term a -> Term a
substituteAndSimplifyTerm sub t =
  case runWriter $ substituteTerm' sub t of
    (t', Any b)
      | b -> simplifyTerm t'
      | otherwise -> t'

simplifyTerm :: forall a. Term a -> Term a
simplifyTerm = \case
  V v -> V v
  Lit l -> Lit l
  App f (mapList simplifyTerm -> ts)
    | Just vs <- fromLits ts -> Lit $ uncurryList_ unValue (semantics f) vs
    | Just t <- rewriteRules f ts -> simplifyTerm t
    | otherwise -> App f ts


simplifyPred :: Pred -> Pred 
simplifyPred = \case
  -- If the term simplifies away to a literal, that means there is no
  -- more generation to do so we can get rid of `GenHint`
  GenHint h t -> case simplifyTerm t of
    Lit {} -> TruePred
    t' -> GenHint h t'
  Subst x t p -> simplifyPred $ substitutePred x t p
  Assert t -> Assert $ simplifyTerm t
  Reifies t' t f -> case simplifyTerm t of
    Lit a -> Assert $ simplifyTerm t' ==. Lit (f a)
    t'' -> Reifies (simplifyTerm t') t'' f
  ForAll set b -> case simplifyTerm set of
    Lit as -> foldMap (`unBind` b) (forAllToList as)
    {-  FIX ME STUBB undefined
    App UnionW (xs :> ys :> Nil) ->
      let b' = simplifyBinder b
       in mkForAll xs b' <> mkForAll ys b' -}
    set' -> case simplifyBinder b of
      _ :-> TruePred -> TruePred
      b' -> ForAll set' b'
  DependsOn _ Lit {} -> TruePred
  DependsOn Lit {} _ -> TruePred
  DependsOn x y -> DependsOn x y
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

-- =================================================================

regularize :: HasVariables t => Var a -> t -> Var a
regularize v t =
  case [nameHint v' | Name v' <- Set.toList $ freeVarSet t, nameOf v' == nameOf v, nameHint v' /= "v"] of
    [] -> v
    nh : _ -> v {nameHint = nh}

regularizeBinder :: Binder a -> Binder a
regularizeBinder (x :-> p) = x' :-> substitutePred x (V x') (regularizeNamesPred p)
  where
    x' = regularize x p

regularizeNamesPred :: Pred -> Pred
regularizeNamesPred p = case p of
  Monitor {} -> p
  And ps -> And $ map regularizeNamesPred ps
  Exists k b -> Exists k (regularizeBinder b)
  Subst v t p -> regularizeNamesPred (substitutePred v t p)
  Let t b -> Let t (regularizeBinder b)
  Assert {} -> p
  Reifies {} -> p
  DependsOn {} -> p
  ForAll t b -> ForAll t (regularizeBinder b)
  Case t bs -> Case t (mapList (mapWeighted regularizeBinder) bs)
  When b p' -> When b (regularizeNamesPred p')
  GenHint {} -> p
  TruePred {} -> p
  FalsePred {} -> p
  Explain es p' -> Explain es (regularizeNamesPred p')

regularizeNames :: Specification a -> Specification a
regularizeNames (ExplainSpec es x) = explainSpecOpt es (regularizeNames x)
regularizeNames (SuspendedSpec x p) =
  SuspendedSpec x' p'
  where
    x' :-> p' = regularizeBinder (x :-> p)
regularizeNames spec = spec

-- ===================================================================
-- Simplifcation

simplifySpec :: HasSpec a => Specification a -> Specification a
simplifySpec spec = case regularizeNames spec of
  SuspendedSpec x p ->
    let optP = optimisePred p
     in fromGESpec $
          explain
            (pure ("\nWhile calling simplifySpec on var " ++ show x))
            (computeSpecSimplified x optP)
  MemberSpec xs -> MemberSpec xs
  ErrorSpec es -> ErrorSpec es
  TypeSpec ts cant -> TypeSpec ts cant
  TrueSpec -> TrueSpec
  ExplainSpec es s -> explainSpecOpt es (simplifySpec s)



-- | Precondition: the `Pred fn` defines the `Var a`

-- Runs in `GE` in order for us to have detailed context on failure.
computeSpecSimplified ::
  forall fn a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Specification a)
computeSpecSimplified x p = undefined {- localGESpec $ case p of
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
  Assert (Lit False) -> genError1 (show p)

  Assert (ElemPat _ (Lit [])) -> pure (ErrorSpec (pure (show p)))
  Assert (ElemPat t (Lit xs)) -> propagateSpec (MemberSpec (NE.fromList xs)) <$> toCtx x t

  Assert t -> undefined -- propagateSpec (equalSpec True) <$> toCtx x t
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
  When {} -> pure $ SuspendedSpec x p
  Reifies (Lit a) (Lit val) f
    | f val == a -> pure TrueSpec
    | otherwise ->
        pure $
          ErrorSpec (NE.fromList ["Value does not reify to literal: " ++ show val ++ " -/> " ++ show a])
  Reifies t' (Lit val) f ->
    propagateSpec (equalSpec (f val)) <$> toCtx x t'
  Reifies Lit {} _ _ ->
    fatalError $ NE.fromList ["Dependency error in computeSpec: Reifies", "  " ++ show p]
  Explain es p -> do
    -- In case things crash in here we want the explanation
    s <- pushGE (NE.toList es) (computeSpecSimplified x p)
    -- This is because while we do want to propagate `explanation`s into `SuspendedSpec`
    -- we probably don't want to propagate the full "currently simplifying xyz" explanation.
    case s of
      SuspendedSpec x p -> pure $ SuspendedSpec x (explanation es p)
      _ -> pure $ addToErrorSpec es s
  -- Impossible cases that should be ruled out by the dependency analysis and linearizer
  DependsOn {} ->
    fatalError $
      NE.fromList
        ["The impossible happened in computeSpec: DependsOn", "  " ++ show x, show $ indent 2 (pretty p)]
  Reifies {} ->
    fatalError $
      NE.fromList
        ["The impossible happened in computeSpec: Reifies", "  " ++ show x, show $ indent 2 (pretty p)]
  where
    -- We want `genError` to turn into `ErrorSpec` and we want `FatalError` to turn into `FatalError`
    localGESpec ge = case ge of
      (GenError xs) -> Result $ ErrorSpec (catMessageList xs)
      (FatalError es) -> FatalError es
      (Result x) -> Result x
-}
-- | Precondition: the `Pred fn` defines the `Var a`.
--
-- Runs in `GE` in order for us to have detailed context on failure.
computeSpec ::
  forall a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Specification a)
computeSpec x p = computeSpecSimplified x (simplifyPred p)

computeSpecBinder :: Binder a -> GE (Specification a)
computeSpecBinder (x :-> p) = computeSpec x p

computeSpecBinderSimplified :: Binder a -> GE (Specification a)
computeSpecBinderSimplified (x :-> p) = computeSpecSimplified x p



-- ===================================================================================
-- The type we will use for the SimpleRep of Sums

{- 
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
-}

{-
-- | Turn a list of branches into a SumSpec. If all the branches fail return an ErrorSpec.
caseSpec ::
  forall as.
  HasSpec (SumOver as) =>
  Maybe String ->
  List (Weighted Specification) as ->
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

    allBranchesFail :: forall as. List (Weighted Specification) as -> Bool
    allBranchesFail Nil = error "The impossible happened in allBranchesFail"
    allBranchesFail (Weighted _ s :> Nil) = isErrorLike s
    allBranchesFail (Weighted _ s :> ss@(_ :> _)) = isErrorLike s && allBranchesFail ss

    loop ::
      forall as.
      HasSpec (SumOver as) =>
      Maybe String -> List (Weighted Specification) as -> Specification (SumOver as)
    loop _ Nil = error "The impossible happened in caseSpec"
    loop _ (s :> Nil) = thing s
    loop mTypeString (s :> ss@(_ :> _))
      | Evidence <- prerequisites @(SumOver as) =
          (typeSpec $ SumSpecRaw mTypeString theWeights (thing s) (loop Nothing ss))
      where
        theWeights =
          case (weight s, totalWeight ss) of
            (Nothing, Nothing) -> Nothing
            (a, b) -> Just (fromMaybe 1 a, fromMaybe (lengthList ss) b)
-}

totalWeight :: List (Weighted f) as -> Maybe Int
totalWeight = fmap Semigroup.getSum . foldMapList (fmap Semigroup.Sum . weight)

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

type family CountCases a where
  CountCases (Sum a b) = 1 + CountCases b
  CountCases _ = 1

countCases :: forall a. KnownNat (CountCases a) => Int
countCases = fromIntegral (natVal @(CountCases a) Proxy)   

sumType :: (Maybe String) -> String
sumType Nothing = ""
sumType (Just x) = " type=" ++ x

sumWeightL, sumWeightR :: Maybe (Int, Int) -> Doc a
sumWeightL Nothing = "1"
sumWeightL (Just (x, _)) = fromString (show x)
sumWeightR Nothing = "1"
sumWeightR (Just (_, x)) = fromString (show x)

-- ====================================================================
-- Monoid and Semigroup, depends on conformance functions    
-- ====================================================================    

instance HasSpec a => Semigroup (Specification a) where
  ExplainSpec es x <> y = explainSpecOpt es (x <> y)
  x <> ExplainSpec es y = explainSpecOpt es (x <> y)
  TrueSpec <> s = s
  s <> TrueSpec = s
  ErrorSpec e <> ErrorSpec e' =
    ErrorSpec
      ( e
          <> pure ("------ spec <> spec ------ @" ++ showType @a)
          <> e'
      )
  ErrorSpec e <> _ = ErrorSpec e
  _ <> ErrorSpec e = ErrorSpec e
  MemberSpec as <> MemberSpec as' =
    addToErrorSpec
      ( NE.fromList
          ["Intersecting: ", "  MemberSpec " ++ show (NE.toList as), "  MemberSpec " ++ show (NE.toList as')]
      )
      ( memberSpecList
          (nub $ intersect (NE.toList as) (NE.toList as'))
          (pure "Empty intersection")
      )
  ms@(MemberSpec as) <> ts@TypeSpec {} =
    memberSpecList
      (nub $ NE.filter (`conformsToSpec` ts) as)
      ( NE.fromList
          [ "The two " ++ showType @a ++ " Specifications are inconsistent."
          , "  " ++ show ms
          , "  " ++ show ts
          ]
      )
  TypeSpec s cant <> MemberSpec as = MemberSpec as <> TypeSpec s cant
  SuspendedSpec v p <> SuspendedSpec v' p' = SuspendedSpec v (p <> rename v' v p')
  SuspendedSpec v ps <> s = SuspendedSpec v (ps <> satisfies (V v) s)
  s <> SuspendedSpec v ps = SuspendedSpec v (ps <> satisfies (V v) s)
  TypeSpec s cant <> TypeSpec s' cant' = case combineSpec s s' of
    -- NOTE: This might look like an unnecessary case, but doing
    -- it like this avoids looping.
    TypeSpec s'' cant'' -> TypeSpec s'' (cant <> cant' <> cant'')
    s'' -> s'' <> notMemberSpec (cant <> cant')

instance HasSpec a => Monoid (Specification a) where
  mempty = TrueSpec

