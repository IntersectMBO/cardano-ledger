{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- Base types: Term, Pred, Spec, Ctx, and classes: HasSpec, Syntax, Semantics, and Logic for the MinModel
module Constrained.MinBase where

import Constrained.Core (Evidence (..), Var (..), eqVar)
import Constrained.GenT
import Constrained.List hiding (ListCtx)
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.String (fromString)
import Data.Typeable
import GHC.Stack
import Prettyprinter

-- ===========================================
-- Terms
-- ===========================================

type AppRequires t dom rng =
  ( Logic t
  , TypeList dom
  , Eq (t dom rng)
  , Show (t dom rng)
  , Typeable dom
  , Typeable rng
  , All HasSpec dom
  , HasSpec rng
  )

data Term a where
  App ::
    forall t dom rng.
    AppRequires t dom rng =>
    t dom rng ->
    List Term dom ->
    Term rng
  Lit :: (Typeable a, Eq a, Show a) => a -> Term a
  V :: HasSpec a => Var a -> Term a

instance Eq (Term a) where
  V x == V x' = x == x'
  Lit a == Lit b = a == b
  App (w1 :: x1) (ts :: List Term dom1) == App (w2 :: x2) (ts' :: List Term dom2) =
    case (eqT @dom1 @dom2, eqT @x1 @x2) of
      (Just Refl, Just Refl) ->
        w1 == w2
          && sameTerms ts ts'
      _ -> False
  _ == _ = False

-- | If the list is composed solely of literals, apply the function to get a value
applyFunSym ::
  forall t ds rng.
  (Typeable rng, Eq rng, Show rng, Semantics t) => FunTy ds rng -> List Term ds -> Maybe rng
applyFunSym f Nil = Just f
applyFunSym f (Lit x :> xs) = applyFunSym @t (f x) xs
applyFunSym _ _ = Nothing

reducesToLit :: Term a -> Maybe a
reducesToLit (Lit n) = Just n
reducesToLit (V _) = Nothing
reducesToLit (App (f :: t ds r) xs) = applyFunSym @t (semantics f) xs

-- How to compare the args of two applications for equality
sameTerms :: All HasSpec as => List Term as -> List Term as -> Bool
sameTerms Nil Nil = True
sameTerms (x :> xs) (y :> ys) = x == y && sameTerms xs ys

-- ===========================================
-- Function Symbol Classes
-- ===========================================

-- | Syntactic operations are ones that have to do with the structure and appearence of the type.
class Syntax (t :: [Type] -> Type -> Type) where
  inFix :: forall dom rng. t dom rng -> Bool
  inFix _ = False
  name :: forall dom rng. t dom rng -> String

-- | Semantic operations are ones that give the function symbol, meaning as a function.
--   I.e. how to apply the function to a list of arguments and return a value,
--   or to apply meaning preserving rewrite rules.
class Syntax t => Semantics (t :: [Type] -> Type -> Type) where
  semantics :: forall d r. t d r -> FunTy d r -- e.g. FunTy '[a,Int] Bool == a -> Int -> Bool
  rewriteRules ::
    forall ds rng.
    (TypeList ds, Typeable ds, HasSpec rng, All HasSpec ds) =>
    t ds rng -> List Term ds -> Evidence (Typeable rng, Eq rng, Show rng) -> Maybe (Term rng)
  rewriteRules t l Evidence = Lit <$> (applyFunSym @t (semantics t) l)

-- | Logical operations are one that support reasoning about how a function symbol
--   relates to logical properties, that we call Spec's
class (Typeable t, Syntax t, Semantics t) => Logic (t :: [Type] -> Type -> Type) where
  {-# MINIMAL propagate | (propagateTypeSpec, propagateMemberSpec) #-}

  propagateTypeSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as (HOLE a) -> TypeSpec b -> [b] -> Spec a
  propagateTypeSpec f ctx ts cant = propagate f ctx (TypeSpec ts cant)

  propagateMemberSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as (HOLE a) -> NonEmpty b -> Spec a
  propagateMemberSpec f ctx xs = propagate f ctx (MemberSpec xs)

  propagate ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as (HOLE a) -> Spec b -> Spec a
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec es) = ErrorSpec es
  propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps)
  propagate f ctx (TypeSpec ts cant) = propagateTypeSpec f ctx ts cant
  propagate f ctx (MemberSpec xs) = propagateMemberSpec f ctx xs

-- ===========================
-- Pred
-- ===========================

data Pred where
  ElemPred :: forall a. HasSpec a => Bool -> Term a -> NonEmpty a -> Pred
  And :: [Pred] -> Pred
  Exists :: ((forall b. Term b -> b) -> GE a) -> Binder a -> Pred
  ForAll :: (Container t a, HasSpec t, HasSpec a) => Term t -> Binder a -> Pred
  DependsOn :: (HasSpec a, HasSpec b) => Term a -> Term b -> Pred
  Assert :: Term Bool -> Pred
  TruePred :: Pred
  FalsePred :: NonEmpty String -> Pred
  Case :: HasSpec (Either a b) => Term (Either a b) -> Binder a -> Binder b -> Pred
  Let :: Term a -> Binder a -> Pred
  Subst :: HasSpec a => Var a -> Term a -> Pred -> Pred

data Binder a where
  (:->) :: HasSpec a => Var a -> Pred -> Binder a

class Container t e | t -> e where
  fromForAllSpec :: (HasSpec t, HasSpec e) => Spec e -> Spec t
  forAllToList :: t -> [e]

data Binders as where
  Binds :: All HasSpec as => List Var as -> Pred -> Binders as

data Bind a where
  Bind :: HasSpec a => {varBind :: Var a, termBind :: Term a} -> Bind a

toBind :: All HasSpec as => List Term as -> List Var as -> List Bind as
toBind Nil Nil = Nil
toBind (t :> ts) (v :> vs) = (Bind v t :> toBind ts vs)

-- ================================
-- Spec
-- ================================

data Spec a where
  TrueSpec :: Spec a
  ErrorSpec :: NonEmpty String -> Spec a
  SuspendedSpec :: HasSpec a => Var a -> Pred -> Spec a -- Maybe we elide this at first
  MemberSpec :: NonEmpty a -> Spec a
  TypeSpec :: HasSpec a => TypeSpec a -> [a] -> Spec a

typeSpec :: HasSpec a => TypeSpec a -> Spec a
typeSpec ts = TypeSpec ts mempty

constrained :: forall a. HasSpec a => (Term a -> Pred) -> Spec a
constrained body =
  let x :-> p = bind body
   in SuspendedSpec x p

bind :: HasSpec a => (Term a -> Pred) -> Binder a
bind bodyf = newv :-> bodyPred
  where
    bodyPred = {- toPred -} body
    newv = Var (nextVar bodyPred) "v"
    body = bodyf (V newv)

    nextVar q = 1 + bound q

    boundBinder :: Binder a -> Int
    boundBinder (x :-> p) = max (nameOf x) (bound p)

    bound (ElemPred _ _ _) = -1
    bound (Subst x _ p) = max (nameOf x) (bound p)
    bound (And ps) = maximum $ (-1) : map bound ps -- (-1) as the default to get 0 as `nextVar p`
    bound (Let _ b) = boundBinder b
    bound (ForAll _ b) = boundBinder b
    bound (Exists _ b) = boundBinder b
    bound (Case _ ba bb) = max (boundBinder ba) (boundBinder bb)
    bound Assert {} = -1
    bound TruePred = -1
    bound FalsePred {} = -1
    bound DependsOn {} = -1

-- ========================================
-- HasSpec
-- ========================================

class (Typeable a, Eq a, Show a, Show (TypeSpec a), Typeable (TypeSpec a)) => HasSpec a where
  -- | The `TypeSpec a` is the type-specific `Spec a`.
  type TypeSpec a

  -- `TypeSpec` behaves sort-of like a monoid with a neutral
  -- element `emptySpec` and a `combineSpec` for combining
  -- two `TypeSpec a`. However, in order to provide flexibilty
  -- `combineSpec` takes two `TypeSpec` and constucts a `Spec`. This
  -- avoids e.g. having to have a separate implementation of `ErrorSpec`
  -- and `MemberSpec` in `TypeSpec`.

  emptySpec :: TypeSpec a
  combineSpec :: TypeSpec a -> TypeSpec a -> Spec a

  -- | Generate a value that satisfies the `TypeSpec`.
  -- The key property for this generator is soundness:
  --  ∀ a ∈ genFromTypeSpec spec. a `conformsTo` spec
  genFromTypeSpec :: (HasCallStack, MonadGenError m) => TypeSpec a -> GenT m a

  -- | Check conformance to the spec.
  conformsTo :: HasCallStack => a -> TypeSpec a -> Bool

  -- | Convert a spec to predicates:
  -- The key property here is:
  --   ∀ a. a `conformsTo` spec == a `conformsTo` constrained (\t -> toPreds t spec)
  toPreds :: Term a -> TypeSpec a -> Pred

  -- | This is used to detect self inconsistencies in a (TypeSpec t)
  --   guardTypeSpec message ty --> ErrorSpec message, if ty is inconsistent
  guardTypeSpec :: TypeSpec a -> Spec a
  guardTypeSpec ty = typeSpec ty

-- =========================================
-- Contexts
-- =========================================

data HOLE a b where
  HOLE :: HOLE a a

deriving instance Show (HOLE a b)

-- | Note the arrows (n :|> hole) and (hole :<| n) always point towards the term with
--   type `(c x)`, (i.e. `hole` in the picture above) where the target variable must occur.
data ListCtx (as :: [Type]) (c :: Type -> Type) where
  Unary :: c a -> ListCtx '[a] c
  (:<|) :: c b -> x -> ListCtx '[b, x] c
  (:|>) :: x -> c b -> ListCtx '[x, b] c

data Ctx v (a :: Type) where
  CtxHole :: HasSpec v => Ctx v v
  CtxApp ::
    ( AppRequires fn as b
    , HasSpec b
    , TypeList as
    , Typeable as
    , All HasSpec as
    , Logic fn
    ) =>
    fn as b -> ListCtx as (Ctx v) -> Ctx v b

ctxHasSpec :: Ctx v a -> Evidence (HasSpec a)
ctxHasSpec CtxHole = Evidence
ctxHasSpec CtxApp {} = Evidence

-- | From a ListCtx, build a (List Term as), to which the function symbol can be applied.
--   Hole becomes 't', values become `Lit` .
fromListCtx :: All HasSpec as => ListCtx as (HOLE a) -> Term a -> List Term as
fromListCtx (Unary HOLE) t = t :> Nil
fromListCtx (HOLE :<| y) t = t :> Lit y :> Nil
fromListCtx (x :|> HOLE) t = Lit x :> t :> Nil

--- | Consider the term `((size_ x +. Lit 3) <=. Lit 12)` with a bunch of nested function symbols, with just 1 variable 'x'
--    `(toCtx x term)` builds a context, with exactly one `CtxHole`, replacing the variable `x`
--    `CtxApp <=. (CtxApp +. (CtxApp size_ (Unary CtxHole) :<| 3) :<| 12)`
--    Working our way from outside in, we first propagate (<=.), then (+.), then (size_). This reduces in several steps
--    1) propagateSpec (CtxApp <=. (CtxApp +. (CtxApp size_ (Unary CtxHole) :<| 3) :<| 12)) $  spec
--    2) propagateSpec (CtxApp +. (CtxApp size_ (Unary CtxHole) :<| 3)) $  (propagate <=. (HOLE:<| 12))
--    3) propagateSpec (CtxApp size_ (Unary CtxHole)) $ (propagate +. (HOLE:<| 3) (propagate <=. (HOLE :<| 12)))
--    4) propagateSpec CtxHole $ (propagate size_ Hole (propagate +. (HOLE:<| 3) (propagate <=. (HOLE :<| 12))))
--    5) propagate size_ Hole (propagate +. (HOLE:<| 3) (propagate <=. (HOLE :<| 12)))
--    Note the pattern in the code below. The recursize call to 'propagateSpec' is on the pattern variable `ctx` which is the
--    part of the context pointed to by the arrows (:<|) and (:|>), and this recurive call to `propagateSpec` is
--    applied to a new spec computed by 'propagate', where the variable `ctx` is replaced by HOLE.
--    we end up on line 5), with three nested calls to `propagate`
propagateSpec ::
  forall v a.
  HasSpec v =>
  Ctx v a ->
  Spec a ->
  Spec v
propagateSpec context spec = case context of
  CtxHole -> spec
  CtxApp f (Unary ctx) | Evidence <- ctxHasSpec ctx -> propagateSpec ctx (propagate f (Unary HOLE) spec)
  CtxApp f (ctx :<| v) | Evidence <- ctxHasSpec ctx -> propagateSpec ctx (propagate f (HOLE :<| v) spec)
  CtxApp f (v :|> ctx) | Evidence <- ctxHasSpec ctx -> propagateSpec ctx (propagate f (v :|> HOLE) spec)

-- Construct a Ctx for a variable 'v', which should occur exactly once in the given Term.
toCtx ::
  forall m v a.
  (Typeable v, Show v, MonadGenError m, HasCallStack) =>
  Var v -> Term a -> m (Ctx v a)
toCtx v = go
  where
    go :: forall b. Term b -> m (Ctx v b)
    go (Lit i) =
      fatalErrorNE $
        NE.fromList
          [ "toCtx applied to literal: (Lit " ++ show i ++ ")"
          , "A context is always constructed from an (App f xs) term"
          , "with a single occurence of the target variable " ++ show v ++ "@(" ++ show (typeOf v) ++ ")"
          ]
    go t@(App f xs) = CtxApp f <$> toCtxList (show t) v xs
    go (V v')
      | Just Refl <- eqVar v v' = pure $ CtxHole
      | otherwise =
          fatalErrorNE $
            NE.fromList
              [ "A context is always constructed from an (App f xs) term with a single target variable"
              , "which in this case is: " ++ show v ++ " :: (" ++ show (typeOf v) ++ ")"
              , "Instead we found an unknown variable: " ++ show v' ++ " :: (" ++ show (typeOf v') ++ ")"
              ]

toCtxList ::
  forall m v as.
  (Show v, Typeable v, MonadGenError m, HasCallStack) =>
  String -> Var v -> List Term as -> m (ListCtx as (Ctx v))
toCtxList termName v Nil = fatalErrorNE $ ("toCtxList without hole, for variable " ++ show v) :| [termName]
toCtxList termName v (V v' :> Nil)
  | Just Refl <- eqVar v v' = pure $ Unary CtxHole
  | otherwise =
      fatalErrorNE $
        NE.fromList
          [ "A context is always constructed from an (App f xs) term,"
          , "with a single occurence of the target variable " ++ show v ++ "@(" ++ show (typeOf v) ++ ")"
          , "Instead we found an unknown variable " ++ show v' ++ "@(" ++ show (typeOf v') ++ ")"
          , "in an application: " ++ termName
          ]
toCtxList termName v (x :> y :> Nil)
  | Just i <- reducesToLit x = do hole <- toCtx v y; pure $ (i :|> hole)
  | Just i <- reducesToLit y = do hole <- toCtx v x; pure $ (hole :<| i)
  | otherwise =
      fatalErrorNE $
        "toCtx applied to an App with 2 parameters."
          :| [ termName
             , "The target variable we are searching for is " ++ show v
             , "One of these parameters must reduce to a literal, which is not the case."
             , "If both are non-literals, then term could have two variables, which is not allowed."
             ]
toCtxList termName v xs =
  fatalErrorNE $
    NE.fromList
      [ "toCtx applied to an App with more than 2 parameters"
      , termName
      , "The target variable we are searching for is " ++ show v
      , "All function symbols should have 1 or 2 parameters"
      , "This one appears to accept " ++ show (lengthList xs) ++ "."
      ]

-- ===================================================================
-- Pretty Printer Helper functions
-- ===================================================================

data WithPrec a = WithPrec Int a

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

prettyPrec :: Pretty (WithPrec a) => Int -> a -> Doc ann
prettyPrec p = pretty . WithPrec p

ppList ::
  forall f as ann.
  All HasSpec as => -- can we use something other than All HasSpec as here? We know Function Symbol HERE
  (forall a. HasSpec a => f a -> Doc ann) ->
  List f as ->
  [Doc ann]
ppList _ Nil = []
ppList pp (a :> as) = pp a : ppList pp as

ppList_ :: forall f as ann. (forall a. f a -> Doc ann) -> List f as -> [Doc ann]
ppList_ _ Nil = []
ppList_ pp (a :> as) = pp a : ppList_ pp as

prettyType :: forall t x. Typeable t => Doc x
prettyType = fromString $ show (typeRep (Proxy @t))

vsep' :: [Doc ann] -> Doc ann
vsep' = align . mconcat . punctuate hardline

(/>) :: Doc ann -> Doc ann -> Doc ann
h /> cont = hang 2 $ sep [h, align cont]

infixl 5 />

short :: forall a x. (Show a, Typeable a) => [a] -> Doc x
short [] = "[]"
short [x] =
  let raw = show x
      refined = if length raw <= 20 then raw else take 20 raw ++ " ... "
   in "[" <+> fromString refined <+> "]"
short xs =
  let raw = show xs
   in if length raw <= 50
        then fromString raw
        else "([" <+> viaShow (length xs) <+> "elements ...] @" <> prettyType @a <> ")"

showType :: forall t. Typeable t => String
showType = show (typeRep (Proxy @t))

-- ==========================================================================
-- Pretty and Show instances
-- ==========================================================================

-- ------------ Term -----------------
instance Pretty (WithPrec (Term a)) where
  pretty (WithPrec p t) = case t of
    Lit n -> fromString $ showsPrec p n ""
    V x -> viaShow x
    App x Nil -> viaShow x
    App f as
      | inFix f
      , a :> b :> Nil <- as ->
          parensIf (p > 9) $ prettyPrec 10 a <+> viaShow f <+> prettyPrec 10 b
      | otherwise -> parensIf (p > 10) $ viaShow f <+> align (fillSep (ppList (prettyPrec 11) as))

instance Pretty (Term a) where
  pretty = prettyPrec 0

instance Show (Term a) where
  showsPrec p t = shows $ pretty (WithPrec p t)

-- ------------ Pred -----------------

instance Pretty Pred where
  pretty = \case
    ElemPred True term vs ->
      align $
        sep
          [ "MemberPred"
          , parens (pretty term)
          , if length vs <= 2
              then brackets (fillSep (punctuate "," (map viaShow (NE.toList vs))))
              else "(" <> viaShow (length vs) <> " items)"
          ]
    ElemPred False term vs -> align $ sep ["notMemberPred", pretty term, fillSep (punctuate "," (map viaShow (NE.toList vs)))]
    -- Exists _ (x :-> p) -> align $ sep ["exists" <+> viaShow x <+> "in", pretty p]
    Let t (x :-> p) -> align $ sep ["let" <+> viaShow x <+> "=" /> pretty t <+> "in", pretty p]
    And ps -> braces $ vsep' $ map pretty ps
    Exists _ (x :-> p) -> align $ sep ["exists" <+> viaShow x <+> "in", pretty p]
    Assert t -> "assert $" <+> pretty t
    -- Reifies t' t _ -> "reifies" <+> pretty (WithPrec 11 t') <+> pretty (WithPrec 11 t)
    DependsOn a b -> pretty a <+> "<-" /> pretty b
    ForAll t (x :-> p) -> "forall" <+> viaShow x <+> "in" <+> pretty t <+> "$" /> pretty p
    Case t as bs -> "case" <+> pretty t <+> "of" /> vsep' [pretty as, pretty bs]
    -- When b p -> "whenTrue" <+> pretty (WithPrec 11 b) <+> "$" /> pretty p
    Subst x t p -> "[" <> pretty t <> "/" <> viaShow x <> "]" <> pretty p
    -- GenHint h t -> "genHint" <+> fromString (showsPrec 11 h "") <+> "$" <+> pretty t
    TruePred -> "True"
    FalsePred {} -> "False"

-- Monitor {} -> "monitor"
-- Explain es p -> "Explain" <+> viaShow (NE.toList es) <+> "$" /> pretty p

instance Show Pred where
  show = show . pretty

instance Pretty (Binder a) where
  pretty (x :-> p) = viaShow x <+> "->" <+> pretty p

-- ------------ Specifications -----------------

instance HasSpec a => Pretty (WithPrec (Spec a)) where
  pretty (WithPrec d s) = case s of
    ErrorSpec es -> "ErrorSpec" /> vsep' (map fromString (NE.toList es))
    TrueSpec -> fromString $ "TrueSpec @(" ++ showType @a ++ ")"
    MemberSpec xs -> "MemberSpec" <+> short (NE.toList xs)
    SuspendedSpec x p -> parensIf (d > 10) $ "constrained $ \\" <+> viaShow x <+> "->" /> pretty p
    -- TODO: require pretty for `TypeSpec` to make this much nicer
    TypeSpec ts cant ->
      parensIf (d > 10) $
        "TypeSpec"
          /> vsep
            [ fromString (showsPrec 11 ts "")
            , viaShow cant
            ]

instance HasSpec a => Pretty (Spec a) where
  pretty = pretty . WithPrec 0

instance HasSpec a => Show (Spec a) where
  showsPrec d = shows . pretty . WithPrec d
