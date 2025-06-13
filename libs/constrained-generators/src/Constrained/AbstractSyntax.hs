{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.AbstractSyntax where

import Constrained.Core
import Constrained.DependencyInjection
import Constrained.Env
import Constrained.FunctionSymbol
import Constrained.GenT
import Constrained.Generic
import Constrained.List
import Constrained.PrettyUtils
import Control.Monad.Identity
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.String
import Data.Typeable
import Prettyprinter hiding (cat)
import Test.QuickCheck

------------------------------------------------------------------------
-- The first-order term language
------------------------------------------------------------------------

-- See `Constrained.DependencyInjection` to better understand `deps` - it's a
-- pointer to postpone having to define `HasSpec` and friends here.
data TermD deps a where
  App ::
    AppRequiresD deps t dom rng =>
    t dom rng ->
    List (TermD deps) dom ->
    TermD deps rng
  Lit :: (Typeable a, Eq a, Show a) => a -> TermD deps a
  V :: (HasSpecD deps a, Typeable a) => Var a -> TermD deps a

type AppRequiresD deps (t :: [Type] -> Type -> Type) dom rng =
  ( LogicD deps t
  , Syntax t
  , Semantics t
  , TypeList dom
  , Eq (t dom rng)
  , Show (t dom rng)
  , Typeable t
  , All Typeable dom
  , Typeable dom
  , Typeable rng
  , All (HasSpecD deps) dom
  , All Show dom
  , HasSpecD deps rng
  , Show rng
  )

instance Eq (TermD deps a) where
  V x == V x' = x == x'
  Lit a == Lit b = a == b
  App (w1 :: x1) (ts :: List (TermD deps) dom1) == App (w2 :: x2) (ts' :: List (TermD deps) dom2) =
    case (eqT @dom1 @dom2, eqT @x1 @x2) of
      (Just Refl, Just Refl) ->
        w1 == w2
          && ts == ts'
      _ -> False
  _ == _ = False

-- Semantics --------------------------------------------------------------

runTermE :: forall a deps. Env -> TermD deps a -> Either (NE.NonEmpty String) a
runTermE env = \case
  Lit a -> Right a
  V v -> case lookupEnv env v of
    Just a -> Right a
    Nothing -> Left (pure ("Couldn't find " ++ show v ++ " in " ++ show env))
  App f ts -> do
    vs <- mapMList (fmap Identity . runTermE env) ts
    pure $ uncurryList_ runIdentity (semantics f) vs

runTerm :: MonadGenError m => Env -> TermD deps a -> m a
runTerm env x = case runTermE env x of
  Left msgs -> fatalErrorNE msgs
  Right val -> pure val

-- Utilities --------------------------------------------------------------

-- | Sound but not complete inequality on terms
fastInequality :: TermD deps a -> TermD deps b -> Bool
fastInequality (V (Var i _)) (V (Var j _)) = i /= j
fastInequality Lit {} Lit {} = False
fastInequality (App _ as) (App _ bs) = go as bs
  where
    go :: List (TermD deps) as -> List (TermD deps) bs -> Bool
    go Nil Nil = False
    go (a :> as') (b :> bs') = fastInequality a b || go as' bs'
    go _ _ = True
fastInequality _ _ = True

-- Pretty-printing --------------------------------------------------------

-- | Syntactic operations are ones that have to do with the structure and appearence of the type.
-- See `Constrained.DependencyInjection` to better understand `deps` - it's a
-- pointer to postpone having to define `HasSpec` and friends here.
class Syntax (t :: [Type] -> Type -> Type) where
  isInfix :: t dom rng -> Bool
  isInfix _ = False
  prettySymbol ::
    forall deps dom rng ann.
    t dom rng ->
    List (TermD deps) dom ->
    Int ->
    Maybe (Doc ann)
  prettySymbol _ _ _ = Nothing

instance Show a => Pretty (WithPrec (TermD deps a)) where
  pretty (WithPrec p t) = case t of
    Lit n -> fromString $ showsPrec p n ""
    V x -> viaShow x
    App x Nil -> viaShow x
    App f as
      | Just doc <- prettySymbol f as p -> doc -- Use Function Symbol specific pretty printers
    App f as
      | isInfix f
      , a :> b :> Nil <- as ->
          parensIf (p > 9) $ prettyPrec 10 a <+> viaShow f <+> prettyPrec 10 b
      | otherwise -> parensIf (p > 10) $ viaShow f <+> align (fillSep (ppListShow (prettyPrec 11) as))

instance Show a => Pretty (TermD deps a) where
  pretty = prettyPrec 0

instance Show a => Show (TermD deps a) where
  showsPrec p t = shows $ pretty (WithPrec p t)

------------------------------------------------------------------------
-- The language for predicates
------------------------------------------------------------------------

-- This is _essentially_ a first-order logic with some extra spicyness thrown
-- in to handle things like sum types and the specific problems you get into
-- when generating from constraints (mostly to do with choosing the order in
-- which to generate things).

-- See `Constrained.DependencyInjection` to better understand `deps` - it's a
-- pointer to postpone having to define `HasSpec` and friends here.
data PredD deps where
  ElemPred ::
    (HasSpecD deps a, Show a) =>
    Bool ->
    TermD deps a ->
    NonEmpty a ->
    PredD deps
  Monitor :: ((forall a. TermD deps a -> a) -> Property -> Property) -> PredD deps
  And :: [PredD deps] -> PredD deps
  Exists ::
    -- | Constructive recovery function for checking
    -- existential quantification
    ((forall b. TermD deps b -> b) -> GE a) ->
    BinderD deps a ->
    PredD deps
  -- This is here because we sometimes need to delay substitution until we're done building
  -- terms and predicates. This is because our surface syntax relies on names being "a bit"
  -- lazily bound to avoid infinite loops when trying to create new names.
  Subst ::
    ( HasSpecD deps a
    , Show a
    ) =>
    Var a ->
    TermD deps a ->
    PredD deps ->
    PredD deps
  Let ::
    TermD deps a ->
    BinderD deps a ->
    PredD deps
  Assert :: TermD deps Bool -> PredD deps
  Reifies ::
    ( HasSpecD deps a
    , HasSpecD deps b
    , Show a
    , Show b
    ) =>
    -- | This depends on the `a` term
    TermD deps b ->
    TermD deps a ->
    -- | Recover a useable value from the `a` term.
    (a -> b) ->
    PredD deps
  DependsOn ::
    ( HasSpecD deps a
    , HasSpecD deps b
    , Show a
    , Show b
    ) =>
    TermD deps a ->
    TermD deps b ->
    PredD deps
  ForAll ::
    ( ForallableD deps t e
    , HasSpecD deps t
    , HasSpecD deps e
    , Show t
    , Show e
    ) =>
    TermD deps t ->
    BinderD deps e ->
    PredD deps
  Case ::
    ( HasSpecD deps (SumOver as)
    , Show (SumOver as)
    ) =>
    TermD deps (SumOver as) ->
    -- | Each branch of the type is bound with
    -- only one variable because `as` are types.
    -- Constructors with multiple arguments are
    -- encoded with `ProdOver` (c.f. `Constrained.Univ`).
    List (Weighted (BinderD deps)) as ->
    PredD deps
  -- monadic-style `when` - if the first argument is False the second
  -- doesn't apply.
  When ::
    TermD deps Bool ->
    PredD deps ->
    PredD deps
  GenHint ::
    ( HasGenHintD deps a
    , Show a
    , Show (HintD deps a)
    ) =>
    HintD deps a ->
    TermD deps a ->
    PredD deps
  TruePred :: PredD deps
  FalsePred :: NE.NonEmpty String -> PredD deps
  Explain :: NE.NonEmpty String -> PredD deps -> PredD deps

data BinderD deps a where
  (:->) ::
    (HasSpecD deps a, Show a) =>
    Var a ->
    PredD deps ->
    BinderD deps a

deriving instance Show (BinderD deps a)

data Weighted f a = Weighted {weight :: Maybe Int, thing :: f a}
  deriving (Functor, Traversable, Foldable)

mapWeighted :: (f a -> g b) -> Weighted f a -> Weighted g b
mapWeighted f (Weighted w t) = Weighted w (f t)

traverseWeighted :: Applicative m => (f a -> m (g a)) -> Weighted f a -> m (Weighted g a)
traverseWeighted f (Weighted w t) = Weighted w <$> f t

instance Semigroup (PredD deps) where
  FalsePred xs <> FalsePred ys = FalsePred (xs <> ys)
  FalsePred es <> _ = FalsePred es
  _ <> FalsePred es = FalsePred es
  TruePred <> p = p
  p <> TruePred = p
  p <> p' = And (unpackPred p ++ unpackPred p')
    where
      unpackPred (And ps) = ps
      unpackPred x = [x]

instance Monoid (PredD deps) where
  mempty = TruePred

-- Pretty-printing --------------------------------------------------------

instance Pretty (PredD deps) where
  pretty = \case
    ElemPred True term vs ->
      align $
        sep
          [ "memberPred"
          , pretty term
          , "(" <> viaShow (length vs) <> " items)"
          , brackets (fillSep (punctuate "," (map viaShow (NE.toList vs))))
          ]
    ElemPred False term vs -> align $ sep ["notMemberPred", pretty term, fillSep (punctuate "," (map viaShow (NE.toList vs)))]
    Exists _ (x :-> p) -> align $ sep ["exists" <+> viaShow x <+> "in", pretty p]
    Let t (x :-> p) -> align $ sep ["let" <+> viaShow x <+> "=" /> pretty t <+> "in", pretty p]
    And ps -> braces $ vsep' $ map pretty ps
    Assert t -> "assert $" <+> pretty t
    Reifies t' t _ -> "reifies" <+> pretty (WithPrec 11 t') <+> pretty (WithPrec 11 t)
    DependsOn a b -> pretty a <+> "<-" /> pretty b
    ForAll t (x :-> p) -> "forall" <+> viaShow x <+> "in" <+> pretty t <+> "$" /> pretty p
    Case t bs -> "case" <+> pretty t <+> "of" /> vsep' (ppList_ pretty bs)
    When b p -> "whenTrue" <+> pretty (WithPrec 11 b) <+> "$" /> pretty p
    Subst x t p -> "[" <> pretty t <> "/" <> viaShow x <> "]" <> pretty p
    GenHint h t -> "genHint" <+> fromString (showsPrec 11 h "") <+> "$" <+> pretty t
    TruePred -> "True"
    FalsePred {} -> "False"
    Monitor {} -> "monitor"
    Explain es p -> "Explain" <+> viaShow (NE.toList es) <+> "$" /> pretty p

instance Show (PredD deps) where
  show = show . pretty

instance Pretty (f a) => Pretty (Weighted f a) where
  pretty (Weighted Nothing t) = pretty t
  pretty (Weighted (Just w) t) = viaShow w <> "~" <> pretty t

instance Pretty (BinderD deps a) where
  pretty (x :-> p) = viaShow x <+> "->" <+> pretty p

------------------------------------------------------------------------
-- The language of specifications
------------------------------------------------------------------------

-- | A `Specification a` denotes a set of `a`s
data SpecificationD deps a where
  -- | Explain a Specification
  ExplainSpec :: [String] -> SpecificationD deps a -> SpecificationD deps a
  -- | Elements of a known set
  MemberSpec ::
    -- | It must be an element of this OrdSet (List). Try hard not to put duplicates in the List.
    NE.NonEmpty a ->
    SpecificationD deps a
  -- | The empty set
  ErrorSpec ::
    NE.NonEmpty String ->
    SpecificationD deps a
  -- | The set described by some predicates
  -- over the bound variable.
  SuspendedSpec ::
    HasSpecD deps a =>
    -- | This variable ranges over values denoted by
    -- the spec
    Var a ->
    -- | And the variable is subject to these constraints
    PredD deps ->
    SpecificationD deps a
  -- | A type-specific spec
  TypeSpecD ::
    HasSpecD deps a =>
    TypeSpecD deps a ->
    -- | It can't be any of the elements of this set
    [a] ->
    SpecificationD deps a
  -- | Anything
  TrueSpec :: SpecificationD deps a

instance (Show a, Typeable a, Show (TypeSpecD deps a)) => Pretty (WithPrec (SpecificationD deps a)) where
  pretty (WithPrec d s) = case s of
    ExplainSpec es z -> "ExplainSpec" <+> viaShow es <+> "$" /> pretty z
    ErrorSpec es -> "ErrorSpec" /> vsep' (map fromString (NE.toList es))
    TrueSpec -> fromString $ "TrueSpec @(" ++ showType @a ++ ")"
    MemberSpec xs -> "MemberSpec" <+> short (NE.toList xs)
    SuspendedSpec x p -> parensIf (d > 10) $ "constrained $ \\" <+> viaShow x <+> "->" /> pretty p
    -- TODO: require pretty for `TypeSpec` to make this much nicer
    TypeSpecD ts cant ->
      parensIf (d > 10) $
        "TypeSpec"
          /> vsep
            [ fromString (showsPrec 11 ts "")
            , viaShow cant
            ]

instance (Show a, Typeable a, Show (TypeSpecD deps a)) => Pretty (SpecificationD deps a) where
  pretty = pretty . WithPrec 0

instance (Show a, Typeable a, Show (TypeSpecD deps a)) => Show (SpecificationD deps a) where
  showsPrec d = shows . pretty . WithPrec d
