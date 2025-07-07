{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- Monoid Pred and Spec, Pretty and ReName instances
{-# OPTIONS_GHC -Wno-orphans #-}

-- Syntactic operations on types: Term, Pred, Spec, Ctx, etc.
module Test.Minimal.Syntax where

import Constrained.Core (
  Evidence (..),
  Rename (rename),
  Value (..),
  Var (..),
  eqVar,
  freshen,
  unValue,
 )
import Constrained.Env (Env)
import Constrained.Env qualified as Env
import Constrained.GenT
import Constrained.Graph
import Constrained.List hiding (ListCtx)
import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable qualified as Foldable (fold, toList)
import Data.Kind
import Data.List (intersect, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Monoid qualified as Monoid
import Data.Semigroup (Any (..), sconcat)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Typeable
import Prettyprinter
import Test.Minimal.Base

-- =======================================
-- Tools for building Spec

forAll :: (Container t a, HasSpec t, HasSpec a) => Term t -> (Term a -> Pred) -> Pred
forAll tm = mkForAll tm . bind

-- forSome :: (Container t a, HasSpec t, HasSpec a) => Term t -> (Term a -> Pred) -> Pred
-- forSome tm = mkForSome tm . bind

mkForAll ::
  (Container t a, HasSpec t, HasSpec a) =>
  Term t -> Binder a -> Pred
mkForAll (Lit (forAllToList -> [])) _ = TruePred
mkForAll _ (_ :-> TruePred) = TruePred
mkForAll tm binder = ForAll tm binder

exists ::
  forall a.
  HasSpec a =>
  ((forall b. Term b -> b) -> GE a) ->
  (Term a -> Pred) ->
  Pred
exists sem k =
  Exists sem $ bind k

unsafeExists ::
  forall a.
  HasSpec a =>
  (Term a -> Pred) ->
  Pred
unsafeExists = exists (\_ -> fatalError "unsafeExists")

notMemberSpec :: forall a f. (HasSpec a, Foldable f) => f a -> Spec a
notMemberSpec x = TypeSpec (anySpec @a) (Foldable.toList x)

isErrorLike :: forall a. Spec a -> Bool
isErrorLike spec = isJust (hasError spec)

hasError :: forall a. Spec a -> Maybe (NonEmpty String)
hasError (ErrorSpec ss) = Just ss
hasError (TypeSpec x _) =
  case guardTypeSpec @a x of
    ErrorSpec ss -> Just ss
    _ -> Nothing
hasError _ = Nothing

-- | Given two 'Spec', return an 'ErrorSpec' if one or more is an 'ErrorSpec'
--   If neither is an 'ErrorSpec' apply the continuation 'f'
handleErrors :: Spec a -> Spec b -> (Spec a -> Spec b -> Spec c) -> Spec c
handleErrors spec1 spec2 f = case (hasError spec1, hasError spec2) of
  (Just m1, Just m2) -> ErrorSpec (m1 <> m2)
  (Just m1, _) -> ErrorSpec m1
  (_, Just m2) -> ErrorSpec m2
  (Nothing, Nothing) -> f spec1 spec2

-- =========================================================
-- Conformance of Pred and Spec

-- | Add the explanations, if it's an ErrorSpec, else drop them
addToErrorSpec :: NE.NonEmpty String -> Spec a -> Spec a
addToErrorSpec es (ErrorSpec es') = ErrorSpec (es <> es')
addToErrorSpec _ s = s

-- | return a MemberSpec or ans ErrorSpec depending on if 'xs' the null list or not
memberSpec :: [a] -> NE.NonEmpty String -> Spec a
memberSpec xs messages =
  case NE.nonEmpty xs of
    Nothing -> ErrorSpec messages
    Just ys -> MemberSpec ys

satisfies :: forall a. HasSpec a => Term a -> Spec a -> Pred
satisfies _ TrueSpec = TruePred
satisfies e (MemberSpec nonempty) = ElemPred True e nonempty
satisfies t (SuspendedSpec x p) = Subst x t p
satisfies e (TypeSpec s cant) = case cant of
  [] -> toPreds e s
  (c : cs) -> ElemPred False e (c :| cs) <> toPreds e s
satisfies _ (ErrorSpec e) = FalsePred e

runTermE :: forall a. Env -> Term a -> Either (NE.NonEmpty String) a
runTermE env = \case
  Lit a -> Right a
  V v -> case Env.lookup env v of
    Just a -> Right a
    Nothing -> Left (pure ("Couldn't find " ++ show v ++ " in " ++ show env))
  App f (ts :: List Term dom) -> do
    vs <- mapMList (fmap Identity . runTermE env) ts
    pure $ uncurryList_ runIdentity (semantics f) vs

runTerm :: MonadGenError m => Env -> Term a -> m a
runTerm env x = case runTermE env x of
  Left msgs -> fatalErrorNE msgs
  Right val -> pure val

conformsToSpec :: forall a. HasSpec a => a -> Spec a -> Bool
conformsToSpec _ TrueSpec = True
conformsToSpec a (MemberSpec as) = elem a as
conformsToSpec a (TypeSpec s cant) = notElem a cant && conformsTo a s
conformsToSpec a (SuspendedSpec v ps) = case checkPredE (Env.singleton v a) (pure "checkPredE") ps of
  Nothing -> True
  Just _ -> False
conformsToSpec _ (ErrorSpec _) = False

checkPredE :: Env -> NonEmpty String -> Pred -> Maybe (NonEmpty String)
checkPredE env msgs = \case
  p@(ElemPred bool t xs) ->
    case runTermE env t of
      Left message -> Just (msgs <> message)
      Right v -> case (elem v xs, bool) of
        (True, True) -> Nothing
        (True, False) -> Just ("notElemPred reduces to True" :| [show p])
        (False, True) -> Just ("elemPred reduces to False" :| [show p])
        (False, False) -> Nothing
  Subst x t p -> checkPredE env msgs $ substitutePred x t p
  Assert t -> case runTermE env t of
    Right True -> Nothing
    Right False ->
      Just
        (msgs <> pure ("Assert " ++ show t ++ " returns False") <> pure ("\nenv=\n" ++ show (pretty env)))
    Left es -> Just (msgs <> es)
  ForAll t (x :-> p) -> case runTermE env t of
    Left es -> Just $ (msgs <> NE.fromList ["checkPredE: ForAll fails to run."] <> es)
    Right set ->
      let answers =
            catMaybes
              [ checkPredE env' (pure "Some items in ForAll fail") p
              | v <- forAllToList set
              , let env' = Env.extend x v env
              ]
       in case answers of
            [] -> Nothing
            (y : ys) -> Just (NE.nub (sconcat (y NE.:| ys)))
  Case t a b -> case runTermE env t of
    Right v -> runCaseOn v a b (\x val ps -> checkPredE (Env.extend x val env) msgs ps)
    Left es -> Just (msgs <> pure "checkPredE: Case fails" <> es)
  Let t (x :-> p) -> case runTermE env t of
    Right val -> checkPredE (Env.extend x val env) msgs p
    Left es -> Just (msgs <> pure "checkPredE: Let fails" <> es)
  DependsOn {} -> Nothing
  TruePred -> Nothing
  FalsePred es -> Just (msgs <> pure "checkPredE: FalsePred" <> es)
  And ps ->
    case catMaybes (fmap (checkPredE env (pure "Some items in And  fail")) ps) of
      [] -> Nothing
      (x : xs) -> Just (msgs <> NE.nub (sconcat (x NE.:| xs)))
  Exists k (x :-> p) ->
    let eval :: forall b. Term b -> b
        eval term = case runTermE env term of
          Right v -> v
          Left es -> error $ unlines $ NE.toList (msgs <> es)
     in case k eval of
          Result a -> checkPredE (Env.extend x a env) msgs p
          FatalError es -> Just (msgs <> catMessageList es)
          GenError es -> Just (msgs <> catMessageList es)

runCaseOn ::
  Either a b -> Binder a -> Binder b -> (forall x. HasSpec x => Var x -> x -> Pred -> r) -> r
runCaseOn (Left a) (x :-> xps) (_ :-> _) f = f x a xps
runCaseOn (Right b) (_ :-> _) (y :-> yps) f = f y b yps

-- | Like checkPred, But it takes [Pred] rather than a single Pred,
--   and it builds a much more involved explanation if it fails.
--   Does the Pred evaluate to True under the given Env?
--   If it doesn't, an involved explanation appears in the (Just message)
--   If it does, then it returns Nothing
checkPredsE ::
  NE.NonEmpty String ->
  Env ->
  [Pred] ->
  Maybe (NE.NonEmpty String)
checkPredsE msgs env ps =
  case catMaybes (fmap (checkPredE env msgs) ps) of
    [] -> Nothing
    (x : xs) -> Just (NE.nub (sconcat (x NE.:| xs)))

-- ==========================================================
-- Semigroup and Monoid instances for Syntax Pred and Spec

instance Semigroup Pred where
  FalsePred xs <> FalsePred ys = FalsePred (xs <> ys)
  FalsePred es <> _ = FalsePred es
  _ <> FalsePred es = FalsePred es
  TruePred <> p = p
  p <> TruePred = p
  p <> p' = And (unpackPred p ++ unpackPred p')
    where
      unpackPred (And ps) = ps
      unpackPred x = [x]

instance Monoid Pred where
  mempty = TruePred

-- Spec instance

instance HasSpec a => Semigroup (Spec a) where
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
      ( memberSpec
          (nub $ intersect (NE.toList as) (NE.toList as'))
          (pure "Empty intersection")
      )
  ms@(MemberSpec as) <> ts@TypeSpec {} =
    memberSpec
      (nub $ NE.filter (`conformsToSpec` ts) as)
      ( NE.fromList
          [ "The two " ++ showType @a ++ " Specs are inconsistent."
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

instance HasSpec a => Monoid (Spec a) where
  mempty = TrueSpec

-- ==================================================
-- Syntactic operation Renaming
-- ==================================================

-- Name

data Name where
  Name :: HasSpec a => Var a -> Name

deriving instance Show Name

instance Eq Name where
  Name v == Name v' = isJust $ eqVar v v'

-- Instances

instance Pretty (Var a) where
  pretty = fromString . show

instance Pretty Name where
  pretty (Name v) = pretty v

instance Ord Name where
  compare (Name v) (Name v') = compare (nameOf v, typeOf v) (nameOf v', typeOf v')

instance Rename Name where
  rename v v' (Name v'') = Name $ rename v v' v''

instance Rename (Term a) where
  rename v v'
    | v == v' = id
    | otherwise = \case
        Lit l -> Lit l
        V v'' -> V (rename v v' v'')
        App f a -> App f (rename v v' a)

instance Rename Pred where
  rename v v'
    | v == v' = id
    | otherwise = \case
        ElemPred bool t xs -> ElemPred bool (rename v v' t) xs
        Subst x t p -> rename v v' $ substitutePred x t p
        And ps -> And (rename v v' ps)
        Exists k b -> Exists (\eval -> k $ eval . rename v v') (rename v v' b)
        Let t b -> Let (rename v v' t) (rename v v' b)
        DependsOn x y -> DependsOn (rename v v' x) (rename v v' y)
        Assert t -> Assert (rename v v' t)
        ForAll set b -> ForAll (rename v v' set) (rename v v' b)
        Case t a b -> Case (rename v v' t) (rename v v' a) (rename v v' b)
        TruePred -> TruePred
        FalsePred es -> FalsePred es

instance Rename (Binder a) where
  rename v v' (va :-> psa) = va' :-> rename v v' psa'
    where
      (va', psa') = freshen va psa (Set.fromList [nameOf v, nameOf v'] <> Set.delete (nameOf va) (freeVarNames psa))

-- ============================================================
-- Syntactic operation: Free variables and variable names
-- ============================================================

freeVarNames :: forall t. HasVariables t => t -> Set Int
freeVarNames = Set.mapMonotonic (\(Name v) -> nameOf v) . freeVarSet

newtype FreeVars = FreeVars {unFreeVars :: Map Name Int}
  deriving (Show)

restrictedTo :: FreeVars -> Set Name -> FreeVars
restrictedTo (FreeVars m) nms = FreeVars $ Map.restrictKeys m nms

memberOf :: Name -> FreeVars -> Bool
memberOf n (FreeVars m) = Map.member n m

count :: Name -> FreeVars -> Int
count n (FreeVars m) = fromMaybe 0 $ Map.lookup n m

instance Semigroup FreeVars where
  FreeVars fv <> FreeVars fv' = FreeVars $ Map.unionWith (+) fv fv'

instance Monoid FreeVars where
  mempty = FreeVars mempty

freeVar :: Name -> FreeVars
freeVar n = singleton n 1

singleton :: Name -> Int -> FreeVars
singleton n k = FreeVars $ Map.singleton n k

without :: Foldable t => FreeVars -> t Name -> FreeVars
without (FreeVars m) remove = FreeVars $ foldr Map.delete m (Foldable.toList remove)

class HasVariables a where
  freeVars :: a -> FreeVars
  freeVarSet :: a -> Set Name
  freeVarSet = Map.keysSet . unFreeVars . freeVars
  countOf :: Name -> a -> Int
  countOf n = count n . freeVars
  appearsIn :: Name -> a -> Bool
  appearsIn n = (> 0) . count n . freeVars

instance (HasVariables a, HasVariables b) => HasVariables (a, b) where
  freeVars (a, b) = freeVars a <> freeVars b
  freeVarSet (a, b) = freeVarSet a <> freeVarSet b
  countOf n (a, b) = countOf n a + countOf n b
  appearsIn n (a, b) = appearsIn n a || appearsIn n b

instance HasVariables (List Term as) where
  freeVars Nil = mempty
  freeVars (x :> xs) = freeVars x <> freeVars xs
  freeVarSet Nil = mempty
  freeVarSet (x :> xs) = freeVarSet x <> freeVarSet xs
  countOf _ Nil = 0
  countOf n (x :> xs) = countOf n x + countOf n xs
  appearsIn _ Nil = False
  appearsIn n (x :> xs) = appearsIn n x || appearsIn n xs

instance HasVariables Name where
  freeVars = freeVar
  freeVarSet = Set.singleton
  countOf n n'
    | n == n' = 1
    | otherwise = 0
  appearsIn n n' = n == n'

instance HasVariables (Term a) where
  freeVars = \case
    Lit {} -> mempty
    V x -> freeVar (Name x)
    App _ ts -> freeVars ts
  freeVarSet = \case
    Lit {} -> mempty
    V x -> freeVarSet (Name x)
    App _ ts -> freeVarSet ts
  countOf n = \case
    Lit {} -> 0
    V x -> countOf n (Name x)
    App _ ts -> countOf n ts
  appearsIn n = \case
    Lit {} -> False
    V x -> appearsIn n (Name x)
    App _ ts -> appearsIn n ts

instance HasVariables Pred where
  freeVars = \case
    ElemPred _ t _ -> freeVars t
    -- GenHint _ t -> freeVars t
    Subst x t p -> freeVars t <> freeVars p `without` [Name x]
    And ps -> foldMap freeVars ps
    Exists _ b -> freeVars b
    Let t b -> freeVars t <> freeVars b
    -- Exists _ b -> freeVars b
    Assert t -> freeVars t
    -- Reifies t' t _ -> freeVars t' <> freeVars t
    DependsOn x y -> freeVars x <> freeVars y
    ForAll set b -> freeVars set <> freeVars b
    Case t as bs -> freeVars t <> freeVars as <> freeVars bs
    -- When b p -> freeVars b <> freeVars p
    TruePred -> mempty
    FalsePred _ -> mempty

  -- Monitor {} -> mempty
  -- Explain _ p -> freeVars p
  freeVarSet = \case
    ElemPred _ t _ -> freeVarSet t
    -- GenHint _ t -> freeVarSet t
    Subst x t p -> freeVarSet t <> Set.delete (Name x) (freeVarSet p)
    And ps -> foldMap freeVarSet ps
    Exists _ b -> freeVarSet b
    Let t b -> freeVarSet t <> freeVarSet b
    -- Exists _ b -> freeVarSet b
    Assert t -> freeVarSet t
    -- Reifies t' t _ -> freeVarSet t' <> freeVarSet t
    DependsOn x y -> freeVarSet x <> freeVarSet y
    ForAll set b -> freeVarSet set <> freeVarSet b
    Case t a b -> freeVarSet t <> freeVarSet a <> freeVarSet b
    -- When b p -> freeVarSet b <> freeVarSet p
    -- Explain _ p -> freeVarSet p
    TruePred -> mempty
    FalsePred _ -> mempty

  -- Monitor {} -> mempty
  countOf n = \case
    ElemPred _ t _ -> countOf n t
    -- GenHint _ t -> countOf n t
    Subst x t p
      | n == Name x -> countOf n t
      | otherwise -> countOf n t + countOf n p
    And ps -> sum $ map (countOf n) ps
    Let t b -> countOf n t + countOf n b
    Exists _ b -> countOf n b
    Assert t -> countOf n t
    -- Reifies t' t _ -> countOf n t' + countOf n t
    DependsOn x y -> countOf n x + countOf n y
    ForAll set b -> countOf n set + countOf n b
    Case t a b -> countOf n t + countOf n a + countOf n b
    -- When b p -> countOf n b + countOf n p
    -- Explain _ p -> countOf n p
    TruePred -> 0
    FalsePred _ -> 0

  -- Monitor {} -> 0
  appearsIn n = \case
    ElemPred _ t _ -> appearsIn n t
    -- GenHint _ t -> appearsIn n t
    Subst x t p
      | n == Name x -> appearsIn n t
      | otherwise -> appearsIn n t || appearsIn n p
    And ps -> any (appearsIn n) ps
    Let t b -> appearsIn n t || appearsIn n b
    Exists _ b -> appearsIn n b
    Assert t -> appearsIn n t
    -- Reifies t' t _ -> appearsIn n t' || appearsIn n t
    DependsOn x y -> appearsIn n x || appearsIn n y
    ForAll set b -> appearsIn n set || appearsIn n b
    Case t a b -> appearsIn n t || appearsIn n a || appearsIn n b
    -- When b p -> appearsIn n b || appearsIn n p
    -- Explain _ p -> appearsIn n p
    TruePred -> False
    FalsePred _ -> False

-- Monitor {} -> False

instance HasVariables (Binder a) where
  freeVars (x :-> p) = freeVars p `without` [Name x]
  freeVarSet (x :-> p) = Set.delete (Name x) (freeVarSet p)
  countOf n (x :-> p)
    | Name x == n = 0
    | otherwise = countOf n p
  appearsIn n (x :-> p)
    | Name x == n = False
    | otherwise = appearsIn n p

instance {-# OVERLAPPABLE #-} (Foldable t, HasVariables a) => HasVariables (t a) where
  freeVars = foldMap freeVars
  freeVarSet = foldMap freeVarSet
  countOf n = Monoid.getSum . foldMap (Monoid.Sum . countOf n)
  appearsIn n = any (appearsIn n)

instance HasVariables a => HasVariables (Set a) where
  freeVars = foldMap freeVars
  freeVarSet = foldMap freeVarSet
  countOf n = sum . Set.map (countOf n)
  appearsIn n = any (appearsIn n)

-- =========================================================
-- Helpers

fromLits :: List Term as -> Maybe (List Value as)
fromLits = mapMList fromLit

fromLit :: Term a -> Maybe (Value a)
fromLit (Lit l) = pure $ Value l
-- fromLit (To x) = (Value . toSimpleRep . unValue) <$> fromLit x -- MAYBE we don't want to do this?
-- fromLit (From x) = (Value . fromSimpleRep . unValue) <$> fromLit x -- Why not apply unary functions to Lit ?
fromLit _ = Nothing

isLit :: Term a -> Bool
isLit = isJust . fromLit

-- =================================================================
-- Syntactic operations Substitutions
-- ============================================================

type Subst = [SubstEntry]

data SubstEntry where
  (:=) :: HasSpec a => Var a -> Term a -> SubstEntry

backwardsSubstitution :: forall a. HasSpec a => Subst -> Term a -> Term a
backwardsSubstitution sub0 t =
  case findMatch sub0 t of
    -- TODO: what about multiple matches??
    Just x -> V x
    Nothing -> case t of
      Lit a -> Lit a
      V x -> V x
      App f ts -> App f (mapListC @HasSpec (backwardsSubstitution sub0) ts)
  where
    findMatch :: Subst -> Term a -> Maybe (Var a)
    findMatch [] _ = Nothing
    findMatch (x := t' : sub1) t1
      | fastInequality t1 t' = findMatch sub1 t1
      | Just (x', t'') <- cast (x, t')
      , t == t'' =
          Just x'
      | otherwise = findMatch sub1 t1

-- | Sound but not complete inequality on terms
fastInequality :: Term a -> Term b -> Bool
fastInequality (V (Var i _)) (V (Var j _)) = i /= j
fastInequality Lit {} Lit {} = False
fastInequality (App _ as) (App _ bs) = go as bs
  where
    go :: List Term as -> List Term bs -> Bool
    go Nil Nil = False
    go (a :> as') (b :> bs') = fastInequality a b || go as' bs'
    go _ _ = True
fastInequality _ _ = True

-- ===================================================================

substituteTerm :: forall a. Subst -> Term a -> Term a
substituteTerm sub = \case
  Lit a -> Lit a
  V x -> substVar sub x
  App f (mapList (substituteTerm sub) -> (ts :: List Term dom)) ->
    case fromLits ts of
      Just vs -> Lit (uncurryList_ unValue (semantics f) vs)
      _ -> App f ts
  where
    substVar :: HasSpec a => Subst -> Var a -> Term a
    substVar [] x = V x
    substVar (y := t : sub1) x
      | Just Refl <- eqVar x y = t
      | otherwise = substVar sub1 x

substituteTerm' :: forall a. Subst -> Term a -> Writer Any (Term a)
substituteTerm' sub = \case
  Lit a -> pure $ Lit a
  V x -> substVar sub x
  App f ts ->
    App f <$> mapMList (substituteTerm' sub) ts
  where
    substVar :: HasSpec a => Subst -> Var a -> Writer Any (Term a)
    substVar [] x = pure $ V x
    substVar (y := t : sub1) x
      | Just Refl <- eqVar x y = t <$ tell (Any True)
      | otherwise = substVar sub1 x

substituteBinder :: HasSpec a => Var a -> Term a -> Binder b -> Binder b
substituteBinder x tm (y :-> p) = y' :-> substitutePred x tm p'
  where
    (y', p') =
      freshen y p (Set.singleton (nameOf x) <> freeVarNames tm <> Set.delete (nameOf y) (freeVarNames p))

substitutePred :: HasSpec a => Var a -> Term a -> Pred -> Pred
substitutePred x tm = \case
  ElemPred bool t xs -> ElemPred bool (substituteTerm [x := tm] t) xs
  -- GenHint h t -> GenHint h (substituteTerm [x := tm] t)
  Subst x' t p -> substitutePred x tm $ substitutePred x' t p
  Assert t -> Assert (substituteTerm [x := tm] t)
  And ps -> Foldable.fold (substitutePred x tm <$> ps)
  Exists k b -> Exists (\eval -> k (eval . substituteTerm [x := tm])) (substituteBinder x tm b)
  Let t b -> Let (substituteTerm [x := tm] t) (substituteBinder x tm b)
  ForAll t b -> ForAll (substituteTerm [x := tm] t) (substituteBinder x tm b)
  Case t as bs -> Case (substituteTerm [x := tm] t) (substituteBinder x tm as) (substituteBinder x tm bs)
  -- When b p -> When (substituteTerm [x := tm] b) (substitutePred x tm p)
  -- Reifies t' t f -> Reifies (substituteTerm [x := tm] t') (substituteTerm [x := tm] t) f
  DependsOn t t' -> DependsOn (substituteTerm [x := tm] t) (substituteTerm [x := tm] t')
  TruePred -> TruePred
  FalsePred es -> FalsePred es

-- Monitor m -> Monitor (\eval -> m (eval . substituteTerm [x := tm]))
-- Explain es p -> Explain es $ substitutePred x tm p

-- =====================================================
-- Substituion under an Env, rather than a single Var
-- It takes Values in the Env, and makes them Literals in the Term.

substTerm :: Env -> Term a -> Term a
substTerm env = \case
  Lit a -> Lit a
  V v
    | Just a <- Env.lookup env v -> Lit a
    | otherwise -> V v
  App f (mapList (substTerm env) -> ts) ->
    case fromLits ts of
      Just vs -> Lit (uncurryList_ unValue (semantics f) vs)
      _ -> App f ts

substBinder :: Env -> Binder a -> Binder a
substBinder env (x :-> p) = x :-> substPred (Env.remove x env) p

substPred :: Env -> Pred -> Pred
substPred env = \case
  ElemPred bool t xs -> ElemPred bool (substTerm env t) xs
  -- GenHint h t -> GenHint h (substTerm env t)
  Subst x t p -> substPred env $ substitutePred x t p
  Assert t -> Assert (substTerm env t)
  -- Reifies t' t f -> Reifies (substTerm env t') (substTerm env t) f
  ForAll set b -> ForAll (substTerm env set) (substBinder env b)
  Case t as bs -> Case (substTerm env t) (substBinder env as) (substBinder env bs)
  -- When b p -> When (substTerm env b) (substPred env p)
  DependsOn x y -> DependsOn (substTerm env x) (substTerm env y)
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  And ps -> Foldable.fold (substPred env <$> ps)
  Exists k b -> Exists (\eval -> k $ eval . substTerm env) (substBinder env b)
  Let t b -> Let (substTerm env t) (substBinder env b)

substSpec :: Env -> Spec a -> Spec a
substSpec env (SuspendedSpec v p) = SuspendedSpec v (substPred env p)
substSpec _ spec = spec

substSolverStage :: Env -> SolverStage -> SolverStage
substSolverStage env (SolverStage var preds spec) = SolverStage var (map (substPred env) preds) (substSpec env spec)

substPlan :: Env -> SolverPlan -> SolverPlan
substPlan env (SolverPlan stages deps) = SolverPlan (map (substSolverStage env) stages) deps

-- Monitor m -> Monitor m
-- Explain es p -> Explain es $ substPred env p

unBind :: a -> Binder a -> Pred
unBind a (x :-> p) = substPred (Env.singleton x a) p

-- ==================================================
-- Syntactic operation Regularizing
-- ==================================================

liftNameHintToBinder :: HasVariables t => Var a -> t -> Var a
liftNameHintToBinder v t =
  case [nameHint v' | Name v' <- Set.toList $ freeVarSet t, nameOf v' == nameOf v, nameHint v' /= "v"] of
    [] -> v
    nh : _ -> v {nameHint = nh}

liftNameHintToBinderBinder :: Binder a -> Binder a
liftNameHintToBinderBinder (x :-> p) = x' :-> substitutePred x (V x') (applyNameHintsPred p)
  where
    x' = liftNameHintToBinder x p

applyNameHintsPred :: Pred -> Pred
applyNameHintsPred pred0 = case pred0 of
  ElemPred {} -> pred0
  And ps -> And $ map applyNameHintsPred ps
  Exists k b -> Exists k (liftNameHintToBinderBinder b)
  Subst v t p -> applyNameHintsPred (substitutePred v t p)
  Let t b -> Let t (liftNameHintToBinderBinder b)
  Assert {} -> pred0
  ForAll t b -> ForAll t (liftNameHintToBinderBinder b)
  Case t as bs -> Case t (liftNameHintToBinderBinder as) (liftNameHintToBinderBinder bs)
  TruePred {} -> pred0
  FalsePred {} -> pred0
  DependsOn {} -> pred0

-- Explain es p' -> Explain es (applyNameHintsPred p')

applyNameHints :: Spec a -> Spec a
applyNameHints (SuspendedSpec x p) = SuspendedSpec x' p'
  where
    x' :-> p' = liftNameHintToBinderBinder (x :-> p)
applyNameHints spec = spec

-- ======================================================
-- Simplify

-- | Apply a substitution and simplify the resulting term if the
-- substitution changed the term.
substituteAndSimplifyTerm :: Subst -> Term a -> Term a
substituteAndSimplifyTerm sub t =
  case runWriter $ substituteTerm' sub t of
    (t', Any b)
      | b -> simplifyTerm t'
      | otherwise -> t'

-- | Simplify a Term, if the Term is an 'App', apply the rewrite rules
--   chosen by the (Semantics t)  instance attached to the App
--   to the function witness 'f'
simplifyTerm :: forall a. Term a -> Term a
simplifyTerm = \case
  V v -> V v
  Lit l -> Lit l
  App (f :: t bs a) (mapList simplifyTerm -> ts)
    | Just vs <- fromLits ts -> Lit $ uncurryList_ unValue (semantics f) vs
    | Just t <- rewriteRules f ts (Evidence @(Typeable a, Eq a, Show a)) -> simplifyTerm t
    | otherwise -> App f ts

simplifyPred :: Pred -> Pred
simplifyPred = \case
  -- If the term simplifies away to a literal, that means there is no
  -- more generation to do so we can get rid of `GenHint`
  p@(ElemPred bool t xs) -> case simplifyTerm t of
    Lit x -> case (elem x xs, bool) of
      (True, True) -> TruePred
      (True, False) -> FalsePred ("notElemPred reduces to True" :| [show p])
      (False, True) -> FalsePred ("elemPred reduces to False" :| [show p])
      (False, False) -> TruePred
    t' -> ElemPred bool t' xs
  Subst x t p -> simplifyPred $ substitutePred x t p
  Assert t -> Assert $ simplifyTerm t
  ForAll (ts :: Term t) (b :: Binder a) -> case simplifyTerm ts of
    Lit as -> foldMap (`unBind` b) (forAllToList as)
    set' -> case simplifyBinder b of
      (_ :-> TruePred) -> TruePred
      b' -> ForAll set' b'
  Case t as@(_ :-> _) bs@(_ :-> _) -> mkCase (simplifyTerm t) (simplifyBinder as) (simplifyBinder bs)
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  And ps -> Foldable.fold (simplifyPreds ps)
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
  DependsOn _ Lit {} -> TruePred
  DependsOn Lit {} _ -> TruePred
  DependsOn x y -> DependsOn x y

mkCase ::
  HasSpec (Either a b) => Term (Either a b) -> Binder a -> Binder b -> Pred
mkCase tm as bs
  -- TODO: all equal maybe?
  | isTrueBinder as && isTrueBinder bs = TruePred
  | isFalseBinder as && isFalseBinder bs = FalsePred (pure "mkCase on all False")
  | Lit a <- tm = runCaseOn a as bs (\x val p -> substPred (Env.singleton x val) p)
  | otherwise = Case tm as bs
  where
    isTrueBinder (_ :-> TruePred) = True
    isTrueBinder (_ :-> _) = False

    isFalseBinder (_ :-> FalsePred {}) = True
    isFalseBinder (_ :-> _) = False

simplifyPreds :: [Pred] -> [Pred]
simplifyPreds = go [] . map simplifyPred
  where
    go acc [] = reverse acc
    go _ (FalsePred err : _) = [FalsePred err]
    go acc (TruePred : ps) = go acc ps
    go acc (p : ps) = go (p : acc) ps

simplifyBinder :: Binder a -> Binder a
simplifyBinder (x :-> p) = x :-> simplifyPred p

toPred :: Bool -> Pred
toPred True = TruePred
toPred False = FalsePred (pure "toPred False")

-- =================================================================

-- TODO: this can probably be cleaned up and generalized along with generalizing
-- to make sure we float lets in some missing cases.
letFloating :: Pred -> Pred
letFloating = Foldable.fold . go []
  where
    goBlock ctx ps = goBlock' (freeVarNames ctx <> freeVarNames ps) ctx ps

    goBlock' _ ctx [] = ctx
    goBlock' fvs ctx (Let t (x :-> p) : ps) =
      -- We can do `goBlock'` here because we've already done let floating
      -- on the inner `p`
      [Let t (x' :-> Foldable.fold (goBlock' (Set.insert (nameOf x') fvs) ctx (p' : ps)))]
      where
        (x', p') = freshen x p fvs
    goBlock' fvs ctx (And ps : ps') = goBlock' fvs ctx (ps ++ ps')
    goBlock' fvs ctx (p : ps) = goBlock' fvs (p : ctx) ps

    go ctx = \case
      ElemPred bool t xs -> ElemPred bool t xs : ctx
      And ps0 -> goBlock ctx (map letFloating ps0)
      Exists k (x :-> p) -> goExists ctx (Exists k) x (letFloating p)
      Let t (x :-> p) -> goBlock ctx [Let t (x :-> letFloating p)]
      Subst x t p -> go ctx (substitutePred x t p)
      ForAll t (x :-> p) -> ForAll t (x :-> letFloating p) : ctx
      Case t (x :-> px) (y :-> py) -> Case t (x :-> letFloating px) (y :-> letFloating py) : ctx
      Assert t -> Assert t : ctx
      TruePred -> TruePred : ctx
      FalsePred es -> FalsePred es : ctx
      DependsOn t t' -> DependsOn t t' : ctx

    goExists :: HasSpec a => [Pred] -> (Binder a -> Pred) -> Var a -> Pred -> [Pred]
    goExists ctx ex x (Let t (y :-> p))
      | not $ Name x `appearsIn` t =
          let (y', p') = freshen y p (Set.insert (nameOf x) $ freeVarNames p <> freeVarNames t)
           in go ctx (Let t (y' :-> ex (x :-> p')))
    goExists ctx ex x p = ex (x :-> p) : ctx

-- Common subexpression elimination but only on terms that are already let-bound.
letSubexpressionElimination :: HasSpec Bool => Pred -> Pred
letSubexpressionElimination = go []
  where
    adjustSub x sub =
      [ x' := t
      | x' := t <- sub
      , isNothing $ eqVar x x'
      , -- TODO: possibly freshen the binder where
      -- `x` appears instead?
      not $ Name x `appearsIn` t
      ]

    goBinder :: Subst -> Binder a -> Binder a
    goBinder sub (x :-> p) = x :-> go (adjustSub x sub) p

    go sub = \case
      ElemPred bool t xs -> ElemPred bool (backwardsSubstitution sub t) xs
      And ps -> And (go sub <$> ps)
      Exists k b -> Exists k (goBinder sub b)
      Let t (x :-> p) -> Let t' (x :-> go (x := t' : sub') p)
        where
          t' = backwardsSubstitution sub t
          sub' = adjustSub x sub
      Subst x t p -> go sub (substitutePred x t p)
      Assert t -> Assert (backwardsSubstitution sub t)
      ForAll t b -> ForAll (backwardsSubstitution sub t) (goBinder sub b)
      Case t as bs -> Case (backwardsSubstitution sub t) (goBinder sub as) (goBinder sub bs)
      TruePred -> TruePred
      FalsePred es -> FalsePred es
      DependsOn t t' -> DependsOn (backwardsSubstitution sub t) (backwardsSubstitution sub t')

-- ===============================================================================
-- Syntax for Solving : stages and plans
-- ===============================================================================

data SolverStage where
  SolverStage ::
    HasSpec a =>
    { stageVar :: Var a
    , stagePreds :: [Pred]
    , stageSpec :: Spec a
    } ->
    SolverStage

instance Pretty SolverStage where
  pretty SolverStage {..} =
    (viaShow stageVar <+> "<-")
      /> vsep'
        ( [pretty stageSpec | not $ isTrueSpec stageSpec]
            ++ (map pretty stagePreds)
            ++ ["---"]
        )

data SolverPlan = SolverPlan
  { solverPlan :: [SolverStage]
  , solverDependencies :: Graph Name
  }

instance Pretty SolverPlan where
  pretty SolverPlan {..} =
    "\nSolverPlan"
      /> vsep'
        [ -- "\nDependencies:" /> pretty solverDependencies, -- Might be usefull someday
          "Linearization:" /> prettyLinear solverPlan
        ]

isTrueSpec :: Spec a -> Bool
isTrueSpec TrueSpec = True
isTrueSpec _ = False

prettyLinear :: [SolverStage] -> Doc ann
prettyLinear = vsep' . map pretty

-- ==========================================================
-- The equality function symbol (==.)

data EqSym :: [Type] -> Type -> Type where
  EqualW :: (Eq a, HasSpec a) => EqSym '[a, a] Bool

deriving instance Eq (EqSym dom rng)

instance Show (EqSym d r) where
  show EqualW = "==."

instance Syntax EqSym where
  inFix EqualW = True
  name EqualW = "==."

instance Semantics EqSym where
  semantics EqualW = (==)

  rewriteRules EqualW (t :> t' :> Nil) Evidence
    | t == t' = Just $ Lit True
  rewriteRules t@EqualW l Evidence = Lit <$> (applyFunSym @EqSym (semantics t) l)

-- We don't need a HasSpec instance, since we can make equality specs at any type
-- using just MemberSpec and TypeSpec

equalSpec :: a -> Spec a
equalSpec = MemberSpec . pure

notEqualSpec :: forall a. HasSpec a => a -> Spec a
notEqualSpec n = TypeSpec (anySpec @a) [n]

caseBoolSpec :: (HasSpec Bool, HasSpec a) => Spec Bool -> (Bool -> Spec a) -> Spec a
caseBoolSpec spec cont = case possibleValues spec of
  [] -> ErrorSpec (NE.fromList ["No possible values in caseBoolSpec"])
  [b] -> cont b
  _ -> mempty
  where
    possibleValues s = filter (flip conformsToSpec s) [True, False]

instance Logic EqSym where
  propagate tag ctx spec = case (tag, ctx, spec) of
    (_, _, TrueSpec) -> TrueSpec
    (_, _, ErrorSpec msgs) -> ErrorSpec msgs
    (f, context, SuspendedSpec v ps) -> constrained $ \v' -> Let (App f (fromListCtx context v')) (v :-> ps)
    (EqualW, HOLE :<| s, bspec) -> caseBoolSpec bspec $ \case
      True -> equalSpec s
      False -> notEqualSpec s
    (EqualW, s :|> HOLE, bspec) -> caseBoolSpec bspec $ \case
      True -> equalSpec s
      False -> notEqualSpec s

infix 4 ==.

(==.) :: (HasSpec Bool, HasSpec a) => Term a -> Term a -> Term Bool
(==.) x y = App EqualW (x :> y :> Nil)

getWitness :: forall t t' d r. (AppRequires t d r, Typeable t') => t d r -> Maybe (t' d r)
getWitness = cast

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
