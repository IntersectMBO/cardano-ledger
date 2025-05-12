{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | This module contains the most basic parts the implementation. Essentially
--   everything to define Specification, HasSpec, HasSimpleRep, Term, Pred, and the Syntax,
--   Semantics, and Logic class. It also has a few HasSpec, HasSimpleRep, and Logic
--   instances for basic types needed to define the default types and methods of HasSpec.
--   It also supplies Eq, Pretty, and Show instances on the syntax (Term, Pred, Binder etc.)
--   because many functions require these instances. It exports functions that define the
--   user interface to the domain embedded language (constrained, forall, exists etc.).
--   And, by design, nothing more.
module Constrained.Base where

import Constrained.AbstractSyntax
import Constrained.Core (
  Evidence (..),
  Value (..),
  Var (..),
  eqVar,
 )
import Constrained.DependencyInjection
import Constrained.FunctionSymbol
import Constrained.GenT (
  GE (..),
  GenT,
  MonadGenError (..),
  catMessageList,
  catMessages,
  fatalError,
 )
import Constrained.Generic (
  HasSimpleRep,
  SimpleRep,
  fromSimpleRep,
  toSimpleRep,
 )
import Constrained.List (
  All,
  FunTy,
  List (..),
  ListCtx (..),
  MapList,
  TypeList,
  curryList,
  fillListCtx,
  foldMapList,
  mapListCtxC,
  pattern ListCtx,
  pattern NilCtx,
 )
import Constrained.PrettyUtils
import Constrained.TypeErrors

import Control.Monad.Writer (
  Writer,
  tell,
 )
import Data.Foldable (
  toList,
 )
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Orphans ()
import Data.Semigroup (Max (..), getMax)
import Data.Typeable
import GHC.Stack
import Prettyprinter hiding (cat)

newtype TypeSpecF a = TypeSpecF (TypeSpec a)

instance Show (TypeSpec a) => Show (TypeSpecF a) where
  show (TypeSpecF ts) = show ts

newtype HintF a = HintF (Hint a)

instance Show (Hint a) => Show (HintF a) where
  show (HintF h) = show h

data Deps
instance Dependencies Deps where
  type HasSpecD Deps = HasSpec
  type TypeSpecD Deps = TypeSpecF
  type LogicD Deps = Logic
  type ForallableD Deps = Forallable
  type HasGenHintD Deps = HasGenHint
  type HintD Deps = HintF

type Binder = BinderD Deps
type AppRequires t as b = AppRequiresD Deps t as b
type Pred = PredD Deps
type Term = TermD Deps
type Specification = SpecificationD Deps

pattern TypeSpec :: () => HasSpec a => TypeSpec a -> [a] -> Specification a
pattern TypeSpec ts cant = TypeSpecD (TypeSpecF ts) cant

{-# COMPLETE ExplainSpec, MemberSpec, ErrorSpec, SuspendedSpec, TypeSpec, TrueSpec #-}

typeSpec :: HasSpec a => TypeSpec a -> Specification a
typeSpec ts = TypeSpec ts mempty

-- ====================================================================

-- | A First-order typed logic has 4 components
--     1. Terms        (Variables (x), Constants (5), and Applications (F x 5)
--        Applications, apply a function symbol to a list of arguments: (FunctionSymbol term1 .. termN)
--     2. Predicates   (Ordered, Odd, ...)
--     3. Connectives  (And, Or, Not, =>, ...)
--     4. Quantifiers  (Forall, Exists)
--
-- The Syntax, Semantics, and Logic classes implement new function symbols in
-- the first order logic. Note that a function symbol is first order
-- data, that uniquely identifies a higher order function. The three classes
-- supply varying levels of functionality, relating to the Syntax, Semantics, and
-- Logical operations of the function symbol.

-- | Logical operations are one that support reasoning about how a function symbol
--   relates to logical properties, that we call Specification's
class (Typeable t, Semantics t, Syntax t) => Logic t where
  {-# MINIMAL propagate | (propagateTypeSpec, propagateMemberSpec) #-}

  propagateTypeSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b ->
    ListCtx Value as (HOLE a) ->
    TypeSpec b ->
    [b] ->
    Specification a
  propagateTypeSpec f ctx ts cant = propagate f ctx (TypeSpec ts cant)

  propagateMemberSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b ->
    ListCtx Value as (HOLE a) ->
    NonEmpty b ->
    Specification a
  propagateMemberSpec f ctx xs = propagate f ctx (MemberSpec xs)

  propagate ::
    (AppRequires t as b, HasSpec a) =>
    t as b ->
    ListCtx Value as (HOLE a) ->
    Specification b ->
    Specification a
  propagate f ctx (ExplainSpec es s) = explainSpec es (propagate f ctx s)
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec es) = ErrorSpec es
  propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps) :: Pred
  propagate f ctx (TypeSpec ts cant) = propagateTypeSpec f ctx ts cant
  propagate f ctx (MemberSpec xs) = propagateMemberSpec f ctx xs

  rewriteRules ::
    (TypeList dom, Typeable dom, HasSpec rng, All HasSpec dom) =>
    t dom rng ->
    List Term dom ->
    Evidence (AppRequires t dom rng) ->
    Maybe (Term rng)
  rewriteRules _ _ _ = Nothing

  mapTypeSpec ::
    forall a b.
    (HasSpec a, HasSpec b) =>
    t '[a] b ->
    TypeSpec a ->
    Specification b
  mapTypeSpec _ts _spec = TrueSpec

  saturate :: t dom Bool -> List Term dom -> [Pred]
  saturate _symbol _ = []

-- This is where the logical properties of a function symbol are applied to transform one spec into another
-- Note if there is a bunch of functions nested together, like (sizeOf_ (elems_ (snd_ x)))
-- we propagate each of those nested function symbols over the current spec, one at a time.
-- The result of this propagation is then made the current spec in the recusive calls to 'propagateSpec'
propagateSpec ::
  forall v a.
  HasSpec v =>
  Specification a ->
  Ctx v a ->
  Specification v
propagateSpec spec = \case
  CtxHOLE -> spec
  CtxApp f (ListCtx pre c suf)
    | Evidence <- ctxHasSpec c -> propagateSpec (propagate f (ListCtx pre HOLE suf) spec) c

ctxHasSpec :: Ctx v a -> Evidence (HasSpec a)
ctxHasSpec CtxHOLE = Evidence
ctxHasSpec CtxApp {} = Evidence

-- | Contexts for Terms, basically a term with a _single_ HOLE
-- instead of a variable. This is used to traverse the defining
-- constraints for a variable and turn them into a spec. Each
-- subterm `f vs Ctx vs'` for lists of values `vs` and `vs'`
-- gets given to the `propagateSpecFun` for `f` as  `(f vs HOLE vs')`.
data Ctx v a where
  -- | A single hole of type `v`. Note ctxHOLE is a nullary constructor, where the `a` type index is the same as the `v` type index.
  CtxHOLE ::
    HasSpec v =>
    Ctx v v
  -- | The application `f vs Ctx vs'`
  CtxApp ::
    ( AppRequires fn as b
    , HasSpec b
    , TypeList as
    , Typeable as
    , All HasSpec as
    , Logic fn
    ) =>
    fn as b ->
    -- This is basically a `List` where
    -- everything is `Value` except for
    -- one entry which is `Ctx fn v`.
    ListCtx Value as (Ctx v) ->
    Ctx v b

-- | This is used together with `ListCtx` to form
-- just the arguments to `f vs Ctx vs'` - replacing
-- `Ctx` with `HOLE`, to get a `ListCtx Value as (HOLE a)` which then can be used as an input to `propagate`.
data HOLE a b where
  HOLE :: HOLE a a

toCtx ::
  forall m v a.
  ( Typeable v
  , Show v
  , MonadGenError m
  , HasCallStack
  ) =>
  Var v ->
  Term a ->
  m (Ctx v a)
toCtx v = go
  where
    go :: forall b. Term b -> m (Ctx v b)
    go (Lit i) =
      fatalErrorNE $
        NE.fromList
          [ "toCtx applied to literal: (Lit " ++ show i ++ ")"
          , "A context is always constructed from an (App f xs) term."
          ]
    go (App f as) = CtxApp f <$> toCtxList v as
    go (V v')
      | Just Refl <- eqVar v v' = pure $ CtxHOLE
      | otherwise =
          fatalErrorNE $
            NE.fromList
              [ "A context is always constructed from an (App f xs) term,"
              , "with a single occurence of the variable " ++ show v ++ "@(" ++ show (typeOf v) ++ ")"
              , "Instead we found an unknown variable " ++ show v' ++ "@(" ++ show (typeOf v') ++ ")"
              ]

toCtxList ::
  forall m v as.
  (Show v, Typeable v, MonadGenError m, HasCallStack) =>
  Var v ->
  List Term as ->
  m (ListCtx Value as (Ctx v))
toCtxList v xs = prefix xs
  where
    prefix :: forall as'. HasCallStack => List Term as' -> m (ListCtx Value as' (Ctx v))
    prefix Nil = fatalError ("toCtxList without hole, for variable " ++ show v)
    prefix (Lit l :> ts) = do
      ctx <- prefix ts
      pure $ Value l :! ctx
    prefix (t :> ts) = do
      hole <- toCtx v t
      suf <- suffix ts
      pure $ hole :? suf

    suffix :: forall as'. List Term as' -> m (List Value as')
    suffix Nil = pure Nil
    suffix (Lit l :> ts) = (Value l :>) <$> suffix ts
    suffix (_ :> _) = fatalErrorNE $ NE.fromList ["toCtxList with too many holes, for variable " ++ show v]

-- | A Convenient pattern for singleton contexts
pattern Unary :: HOLE a' a -> ListCtx f '[a] (HOLE a')
pattern Unary h = NilCtx h

{-# COMPLETE Unary #-}

-- | Convenient patterns for binary contexts (the arrow :<: points towards the hole)
pattern (:<:) :: (Typeable b, Show b) => HOLE c a -> b -> ListCtx Value '[a, b] (HOLE c)
pattern h :<: a = h :? Value a :> Nil

-- | Convenient patterns for binary contexts (the arrow :>: points towards the hole)
pattern (:>:) :: (Typeable a, Show a) => a -> HOLE c b -> ListCtx Value '[a, b] (HOLE c)
pattern a :>: h = Value a :! NilCtx h

{-# COMPLETE (:<:), (:>:) #-}

flipCtx ::
  (Typeable a, Show a, Typeable b, Show b) =>
  ListCtx Value '[a, b] (HOLE c) -> ListCtx Value '[b, a] (HOLE c)
flipCtx (HOLE :<: x) = x :>: HOLE
flipCtx (x :>: HOLE) = HOLE :<: x

-- | From a ListCtx, build a (List Term as), to which the function symbol can be applied.
fromListCtx :: All HasSpec as => ListCtx Value as (HOLE a) -> Term a -> List Term as
fromListCtx ctx t = fillListCtx (mapListCtxC @HasSpec (\(Value a) -> Lit a) ctx) (\HOLE -> t)

-- =================================================================
-- The class (HasSpec a) tells us what operations type 'a' must
-- support to add it to the constraint solver and generator
-- Writing HasSpec instances gives the system the power to grow
-- Don't be afraid of all the methods. Most have default implementations.
-- =================================================================

type GenericallyInstantiated a =
  ( AssertComputes
      (SimpleRep a)
      ( Text "Trying to use a generic instantiation of "
          :<>: ShowType a
          :<>: Text ", likely in a HasSpec instance."
          :$$: Text
                 "However, the type has no definition of SimpleRep, likely because of a missing instance of HasSimpleRep."
      )
  , HasSimpleRep a
  , HasSpec (SimpleRep a)
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  )

type TypeSpecEqShow a =
  ( AssertComputes
      (TypeSpec a)
      ( Text "Can't compute "
          :<>: ShowType (TypeSpec a)
          :$$: Text "Either because of a missing definition of TypeSpec or a missing instance of HasSimpleRep."
      )
  , Show (TypeSpec a)
  , Typeable (TypeSpec a)
  )

{- NOTE: type errors in constrained-generators
    It's easy to make a mistake like this:
      data Bad = Bad | Worse deriving (Eq, Show)
      instance HasSpec Bad
    Missing that this requires an instance of HasSimpleRep for Bad to work.
    The two `AssertComputes` uses above are here to give you better error messages when you make this mistake,
    e.g. giving you something like this:
      src/Constrained/Examples/Basic.hs:327:10: error: [GHC-64725]
          • Can't compute TypeSpec (SimpleRep Bad)
            Either because of a missing definition of TypeSpec or a missing instance of HasSimpleRep.
          • In the instance declaration for ‘HasSpec Bad’
          |
      327 | instance HasSpec Bad
          |          ^^^^^^^^^^^

      src/Constrained/Examples/Basic.hs:327:10: error: [GHC-64725]
          • Trying to use a generic instantiation of Bad, likely in a HasSpec instance.
            However, the type has no definition of SimpleRep, likely because of a missing instance of HasSimpleRep.
          • In the expression: Constrained.Base.$dmemptySpec @(Bad)
            In an equation for ‘emptySpec’:
                emptySpec = Constrained.Base.$dmemptySpec @(Bad)
            In the instance declaration for ‘HasSpec Bad’
          |
      327 | instance HasSpec Bad
          |          ^^^^^^^^^^^
-}

class
  ( Typeable a
  , Eq a
  , Show a
  , TypeSpecEqShow a
  ) =>
  HasSpec a
  where
  -- | The `TypeSpec a` is the type-specific `Specification a`.
  type TypeSpec a

  type TypeSpec a = TypeSpec (SimpleRep a)

  -- `TypeSpec` behaves sort-of like a monoid with a neutral
  -- element `emptySpec` and a `combineSpec` for combining
  -- two `TypeSpec a`. However, in order to provide flexibilty
  -- `combineSpec` takes two `TypeSpec` and constucts a `Specification`. This
  -- avoids e.g. having to have a separate implementation of `ErrorSpec`
  -- and `MemberSpec` in `TypeSpec`.

  emptySpec :: TypeSpec a
  combineSpec :: TypeSpec a -> TypeSpec a -> Specification a

  -- | Generate a value that satisfies the `TypeSpec`.
  -- The key property for this generator is soundness:
  --  ∀ a ∈ genFromTypeSpec spec. a `conformsTo` spec
  genFromTypeSpec :: (HasCallStack, MonadGenError m) => TypeSpec a -> GenT m a

  -- | Check conformance to the spec.
  conformsTo :: HasCallStack => a -> TypeSpec a -> Bool

  -- | Shrink an `a` with the aide of a `TypeSpec`
  shrinkWithTypeSpec :: TypeSpec a -> a -> [a]

  -- | Convert a spec to predicates:
  -- The key property here is:
  --   ∀ a. a `conformsTo` spec == a `conformsTo` constrained (\t -> toPreds t spec)
  toPreds :: Term a -> TypeSpec a -> Pred

  -- | Compute an upper and lower bound on the number of solutions genFromTypeSpec might return
  cardinalTypeSpec :: TypeSpec a -> Specification Integer

  -- | A bound on the number of solutions `genFromTypeSpec TrueSpec` can produce.
  --   For a type with finite elements, we can get a much more accurate
  --   answer than TrueSpec
  cardinalTrueSpec :: Specification Integer
  cardinalTrueSpec = TrueSpec

  -- Each instance can decide if a TypeSpec has an Error, and what String
  -- to pass to ErrorSpec to create an ErrorSpec value. Particulary
  -- useful for type Sum and Prod. The default instance uses guardTypeSpec,
  -- which also has a default value, and if that defualt value is used, typeSpecHasError will
  -- return Nothing. Both 'typeSpecHasError' and 'guardTypeSpec' can be set individually.
  -- If you're only writing one of these non default values, give it to 'guardTypeSpec'
  typeSpecHasError :: TypeSpec a -> Maybe (NE.NonEmpty String)
  typeSpecHasError tspec = case guardTypeSpec @a [] tspec of
    ErrorSpec msgs -> Just msgs
    _ -> Nothing

  -- Some binary TypeSpecs, which nest to the right
  -- e.g. something like this (X a (TypeSpec (X b (TypeSpec (X c w))))))
  -- An would look better in Vertical mode as (X [a,b,c] m).
  -- This lets each HasSpec instance decide. Particulary useful for type Sum and Prod
  alternateShow :: TypeSpec a -> BinaryShow
  alternateShow _ = NonBinary

  monadConformsTo :: a -> TypeSpec a -> Writer [String] Bool
  monadConformsTo x spec =
    if conformsTo @a x spec
      then pure True
      else tell ["Fails by " ++ show spec] >> pure False

  -- | For some types (especially finite ones) there may be much better ways to construct
  --   a Specification than the default method of just adding a large 'bad' list to TypSpec. This
  --   function allows each HasSpec instance to decide.
  typeSpecOpt :: TypeSpec a -> [a] -> Specification a
  typeSpecOpt tySpec bad = TypeSpec tySpec bad

  -- | This can be used to detect self inconsistencies in a (TypeSpec t)
  --   Note this is similar to 'typeSpecHasError', and the default
  --   value for 'typeSpecHasError' is written in terms of 'guardTypeSpec'
  --   Both 'typeSpecHasError' and 'guardTypeSpec' can be set individually.
  guardTypeSpec :: [String] -> TypeSpec a -> Specification a
  guardTypeSpec _ ty = typeSpec ty

  -- | Prerequisites for the instance that are sometimes necessary
  -- when working with e.g. `Specification`s or functions in the universe.
  type Prerequisites a :: Constraint

  type Prerequisites a = ()

  -- | Materialize the `Prerequisites` dictionary. It should not be necessary to
  -- implement this function manually.
  prerequisites :: Evidence (Prerequisites a)
  default prerequisites :: Prerequisites a => Evidence (Prerequisites a)
  prerequisites = Evidence

  {- NOTE: Below follows default implementations for the functions in this
     class based on Generics.  They are meant to provide an implementation of
     `HasSpec a` when `HasSimpleRep a` and `HasSpec (SimpleRep a)`. For example,
     for a newtype wrapper like `newtype Foo = Foo Word64` we can define `SimpleRep
     Foo = Word64` with the requisite instance for `HasSimpleRep` (all of which
     is derived from `Generic Foo`) and the instance for `HasSpec Foo` is
     essentially the same as the instance for `Word64`. This is achieved by
     ensuring that `TypeSpec Foo = TypeSpec Word64` (c.f. the default
     implementation of `TypeSpec` above). To this end, the implementations
     below simply convert the relevant things between `SimpleRep a` and `a`.
     For example, in the implementation of `combineSpec s s'` we treat `s` and
     `s'` (which have type `TypeSpec a`) as `TypeSpec (SimpleRep a)`,
     combine them, and go from the resulting `Specification (SimpleRep a)` to `Specification
     a` using `fromSimpleRepSpec`.
   -}

  default emptySpec :: GenericallyInstantiated a => TypeSpec a
  emptySpec = emptySpec @(SimpleRep a)

  default combineSpec ::
    GenericallyInstantiated a =>
    TypeSpec a ->
    TypeSpec a ->
    Specification a
  combineSpec s s' = fromSimpleRepSpec $ combineSpec @(SimpleRep a) s s'

  default genFromTypeSpec ::
    (GenericallyInstantiated a, HasCallStack, MonadGenError m) =>
    TypeSpec a ->
    GenT m a
  genFromTypeSpec s = fromSimpleRep <$> genFromTypeSpec s

  default conformsTo ::
    (GenericallyInstantiated a, HasCallStack) =>
    a ->
    TypeSpec a ->
    Bool
  a `conformsTo` s = conformsTo (toSimpleRep a) s

  default toPreds ::
    GenericallyInstantiated a =>
    Term a ->
    TypeSpec a ->
    Pred
  toPreds v s = toPreds (toGeneric_ v) s

  default shrinkWithTypeSpec ::
    GenericallyInstantiated a =>
    TypeSpec a ->
    a ->
    [a]
  shrinkWithTypeSpec spec a = map fromSimpleRep $ shrinkWithTypeSpec spec (toSimpleRep a)

  default cardinalTypeSpec ::
    GenericallyInstantiated a =>
    TypeSpec a ->
    Specification Integer
  cardinalTypeSpec = cardinalTypeSpec @(SimpleRep a)

-- ===================================================================
-- toGeneric and fromGeneric as Function Symbols
-- That means they can be used inside (Term a)
-- ===================================================================

-- The things you need to know to work with the generics which translates things
-- into their SimpleRep, made of Sum and Prod
type GenericRequires a =
  ( HasSpec a -- This gives Show, Eq, and Typeable instances
  , GenericallyInstantiated a
  )

-- The constructors of BaseW, are first order data (i.e Function Symbols) that describe functions.
-- The Base functions are just the functions neccessary to define Specification, and the classes
-- HasSimpleRep, HasSpec, Syntax, Semantics, and Logic. We call BaseW a 'witness type', and use
-- the convention that all witness types (and their constructors) have "W" as thrit last character.
data BaseW (dom :: [Type]) (rng :: Type) where
  ToGenericW :: GenericRequires a => BaseW '[a] (SimpleRep a)
  FromGenericW :: GenericRequires a => BaseW '[SimpleRep a] a
deriving instance Eq (BaseW dom rng)

instance Show (BaseW d r) where
  show ToGenericW = "toSimpleRep"
  show FromGenericW = "fromSimpleRep"

instance Syntax BaseW where
  prettySymbol ToGenericW (x :> Nil) p = Just $ "to" <+> pretty (WithPrec p x)
  prettySymbol FromGenericW (x :> Nil) p = Just $ "from" <+> pretty (WithPrec p x)

instance Semantics BaseW where
  semantics FromGenericW = fromSimpleRep
  semantics ToGenericW = toSimpleRep

-- -- ============== ToGenericW Logic instance

instance Logic BaseW where
  propagateTypeSpec ToGenericW (Unary HOLE) s cant = TypeSpec s (fromSimpleRep <$> cant)
  propagateTypeSpec FromGenericW (Unary HOLE) s cant = TypeSpec s (toSimpleRep <$> cant)

  propagateMemberSpec ToGenericW (Unary HOLE) es = MemberSpec (fmap fromSimpleRep es)
  propagateMemberSpec FromGenericW (Unary HOLE) es = MemberSpec (fmap toSimpleRep es)

  mapTypeSpec ToGenericW ts = typeSpec ts
  mapTypeSpec FromGenericW ts = typeSpec ts

  rewriteRules ToGenericW (FromGeneric x :> Nil) Evidence = Just x
  rewriteRules (FromGenericW :: BaseW dom rng) (ToGeneric (x :: Term a) :> Nil) Evidence
    | Just Refl <- eqT @rng @a = Just x
  rewriteRules _ _ _ = Nothing

toGeneric_ ::
  forall a.
  GenericRequires a =>
  Term a ->
  Term (SimpleRep a)
toGeneric_ = appTerm ToGenericW

fromGeneric_ ::
  forall a.
  (GenericRequires a, AppRequires BaseW '[SimpleRep a] a) =>
  Term (SimpleRep a) ->
  Term a
fromGeneric_ = appTerm FromGenericW

-- ====================================================================
-- Generic Transformers
-- Using Generics to transform from ordinary (Specifications a) to
-- Specifications over 'a's SimpleRep (Specification (SimpleRep a))
-- ====================================================================

fromSimpleRepSpec ::
  GenericRequires a =>
  Specification (SimpleRep a) ->
  Specification a
fromSimpleRepSpec = \case
  ExplainSpec es s -> explainSpecOpt es (fromSimpleRepSpec s)
  TrueSpec -> TrueSpec
  ErrorSpec e -> ErrorSpec e
  TypeSpec s'' cant -> TypeSpec s'' $ map fromSimpleRep cant
  MemberSpec elems -> MemberSpec $ NE.nub (fmap fromSimpleRep elems)
  SuspendedSpec x p ->
    constrained $ \x' ->
      Let (toGeneric_ x') (x :-> p) :: Pred

toSimpleRepSpec ::
  forall a.
  GenericRequires a =>
  Specification a ->
  Specification (SimpleRep a)
toSimpleRepSpec = \case
  ExplainSpec es s -> explainSpecOpt es (toSimpleRepSpec s)
  TrueSpec -> TrueSpec
  ErrorSpec e -> ErrorSpec e
  TypeSpec s'' cant -> TypeSpec s'' $ map toSimpleRep cant
  MemberSpec elems -> MemberSpec $ NE.nub $ fmap toSimpleRep elems
  SuspendedSpec x p ->
    constrained $ \x' ->
      Let (fromGeneric_ x') (x :-> p) :: Pred

-- =====================================================================
-- Now the supporting operations and types.
-- =====================================================================

-- Used to show binary operators like SumSpec and PairSpec
data BinaryShow where
  BinaryShow :: forall a. String -> [Doc a] -> BinaryShow
  NonBinary :: BinaryShow

-- =================================================
-- Term

-- | Recall function symbols are objects that you can use to build applications
--   They carry information about both its semantic and logical properties.
--   Usually the Haskel name ends in '_', for example consider: not_, subset_ ,lookup_, toGeneric_
--   Infix function symbols names end in '.', for example: ==. , <=.
--   E.g  appTerm ToGenericW  :: Term a -> Term(SimpleRep a)
--        (appTerm ToGenericW  (lit True)) builds the Term  (toGeneric_ True)
--   Note the witness (ToGenericW ) must have a Logic instance like:
--   instance Logic      BaseW          '[a]           (SimpleRep a) where ...
--        type of ToGenericW ^    arg types^            result type^
--   The Logic instance does not demand any of these things have any properties at all.
--   It is here, where we actually build the App node, that we demand the properties App terms require.
--   App :: AppRequires s t ds r => t s ds r -> List Term dom -> Term rng
appSym ::
  forall t as b.
  AppRequires t as b =>
  t as b ->
  List Term as ->
  Term b
appSym w xs = App w xs

-- Like 'appSym' but builds functions over terms, rather that just one App term.
appTerm ::
  forall t ds r.
  AppRequires t ds r =>
  t ds r ->
  FunTy (MapList Term ds) (Term r)
appTerm sym = curryList @ds (App @Deps @t @ds @r sym)

name :: String -> Term a -> Term a
name nh (V (Var i _)) = V (Var i nh)
name _ _ = error "applying name to non-var thing! Shame on you!"

-- | Give a Term a nameHint, if its a Var, and doesn't already have one,
--  otherwise return the Term unchanged.
named :: String -> Term a -> Term a
named nh t@(V (Var i x)) = if x /= "v" then t else V (Var i nh)
named _ t = t

bind :: (HasSpec a, IsPred p) => (Term a -> p) -> Binder a
bind bodyf = newv :-> bodyPred
  where
    bodyPred = toPred body
    newv = Var (nextVar bodyPred) "v"
    body = bodyf (V newv)

    nextVar q = 1 + bound q

    boundBinder :: Binder a -> Int
    boundBinder (x :-> p) = max (nameOf x) (bound p)

    bound (ElemPred _ _ _) = -1
    bound (Explain _ p) = bound p
    bound (Subst x _ p) = max (nameOf x) (bound p)
    bound (And ps) = maximum $ (-1) : map bound ps -- (-1) as the default to get 0 as `nextVar p`
    bound (Exists _ b) = boundBinder b
    bound (Let _ b) = boundBinder b
    bound (ForAll _ b) = boundBinder b
    bound (Case _ cs) = getMax $ foldMapList (Max . boundBinder . thing) cs
    bound (When _ p) = bound p
    bound Reifies {} = -1
    bound GenHint {} = -1
    bound Assert {} = -1
    bound DependsOn {} = -1
    bound TruePred = -1
    bound FalsePred {} = -1
    bound Monitor {} = -1

-- ==================================================
-- Pred

class Forallable t e | t -> e where
  fromForAllSpec ::
    (HasSpec t, HasSpec e) => Specification e -> Specification t
  default fromForAllSpec ::
    ( HasSpec e
    , Forallable (SimpleRep t) e
    , GenericRequires t
    ) =>
    Specification e ->
    Specification t
  fromForAllSpec es = fromSimpleRepSpec $ fromForAllSpec @(SimpleRep t) @e es

  forAllToList :: t -> [e]
  default forAllToList ::
    ( HasSimpleRep t
    , Forallable (SimpleRep t) e
    ) =>
    t ->
    [e]
  forAllToList t = forAllToList (toSimpleRep t)

-- ===========================================
-- IsPred

class Show p => IsPred p where
  toPred :: p -> Pred

instance IsPred Pred where
  toPred (Assert (Lit False)) = FalsePred (pure "toPred(Lit False)")
  toPred (Assert (Lit True)) = TruePred
  toPred (Explain xs p) = Explain xs (toPred p)
  toPred (And ps) = And (map toPred ps)
  toPred x = x

instance IsPred p => IsPred [p] where
  toPred xs = And (map toPred xs)

instance IsPred Bool where
  toPred True = TruePred
  toPred False = FalsePred (pure "toPred False")

instance IsPred (Term Bool) where
  toPred (Lit b) = toPred b
  toPred term = Assert term

-- ============================================================
-- Simple Widely used operations on Specification

-- | return a MemberSpec or ans ErrorSpec depending on if 'xs' the null list or not
memberSpecList :: [a] -> NE.NonEmpty String -> Specification a
memberSpecList xs messages =
  case NE.nonEmpty xs of
    Nothing -> ErrorSpec messages
    Just ys -> MemberSpec ys

explainSpec :: [String] -> Specification a -> Specification a
explainSpec [] x = x
explainSpec es spec = ExplainSpec es spec

explainSpecOpt :: [String] -> Specification a -> Specification a
explainSpecOpt [] x = x
explainSpecOpt es1 (ExplainSpec es2 x) = explainSpecOpt (es1 ++ es2) x
explainSpecOpt es spec = ExplainSpec es spec

equalSpec :: a -> Specification a
equalSpec = MemberSpec . pure

notEqualSpec :: forall a. HasSpec a => a -> Specification a
notEqualSpec = TypeSpec (emptySpec @a) . pure

notMemberSpec :: forall a f. (HasSpec a, Foldable f) => f a -> Specification a
notMemberSpec = typeSpecOpt (emptySpec @a) . toList

constrained ::
  forall a p.
  (IsPred p, HasSpec a) =>
  (Term a -> p) ->
  Specification a
constrained body =
  let x :-> p = bind body
   in SuspendedSpec x p

isErrorLike :: forall a. Specification a -> Bool
isErrorLike (ExplainSpec _ s) = isErrorLike s
isErrorLike ErrorSpec {} = True
isErrorLike (TypeSpec x _) =
  case typeSpecHasError @a x of
    Nothing -> False
    Just _ -> True
isErrorLike _ = False

errorLikeMessage :: forall a. Specification a -> NE.NonEmpty String
errorLikeMessage (ErrorSpec es) = es
errorLikeMessage (TypeSpec x _) =
  case typeSpecHasError @a x of
    Nothing -> pure ("Bad call to errorLikeMessage case 1, not guarded by isErrorLike")
    Just xs -> xs
errorLikeMessage _ = pure ("Bad call to errorLikeMessage, case 2, not guarded by isErrorLike")

fromGESpec :: HasCallStack => GE (Specification a) -> Specification a
fromGESpec ge = case ge of
  Result s -> s
  GenError xs -> ErrorSpec (catMessageList xs)
  FatalError es -> error $ catMessages es

-- | Add the explanations, if it's an ErrorSpec, else drop them
addToErrorSpec :: NE.NonEmpty String -> Specification a -> Specification a
addToErrorSpec es (ExplainSpec [] x) = addToErrorSpec es x
addToErrorSpec es (ExplainSpec es2 x) = ExplainSpec es2 (addToErrorSpec es x)
addToErrorSpec es (ErrorSpec es') = ErrorSpec (es <> es')
addToErrorSpec _ s = s

-- ==========================================================================
-- Pretty and Show instances
-- ==========================================================================

-- ====================================================
-- The Fun type encapuslates a Logic instance to hide
-- everything but the domain and range. This is a way
-- to pass around functions without pain. Usefull in the
-- ListFoldy implementaion that deals with higher order functions.

data Fun dom rng where
  Fun ::
    forall t dom rng.
    AppRequires t dom rng =>
    t dom rng ->
    Fun dom rng

instance Show (Fun dom r) where
  show (Fun (f :: t dom rng)) = "(Fun " ++ show f ++ ")"

extractf :: Typeable t => Fun d r -> Maybe (t d r)
extractf (Fun f) = cast f

appFun :: Fun '[x] b -> Term x -> Term b
appFun (Fun f) x = App f (x :> Nil)

sameFun :: Fun d1 r1 -> Fun d2 r2 -> Bool
sameFun (Fun f) (Fun g) = case cast f of
  Just f' -> f' == g
  Nothing -> False

instance Eq (Fun d r) where
  (==) = sameFun

-- =================================================================
-- A simple but important HasSpec instances. The  other
-- instances usually come in a file of their own.

instance HasSpec () where
  type TypeSpec () = ()
  emptySpec = ()
  combineSpec _ _ = typeSpec ()
  _ `conformsTo` _ = True
  shrinkWithTypeSpec _ _ = []
  genFromTypeSpec _ = pure ()
  toPreds _ _ = TruePred
  cardinalTypeSpec _ = MemberSpec (pure 1)
  cardinalTrueSpec = equalSpec 1 -- there is exactly one, ()
  typeSpecOpt _ [] = TrueSpec
  typeSpecOpt _ (_ : _) = ErrorSpec (pure "Non null 'cant' set in typeSpecOpt @()")

-- ========================================================================
-- Uni-directional, Match only patterns, for the Function Symbols in BaseW.
-- We do not defined Constructor patterns. Use the xxx_ functions instead.

pattern FromGeneric ::
  forall rng.
  () =>
  forall a.
  (rng ~ a, GenericRequires a, HasSpec a, AppRequires BaseW '[SimpleRep a] rng) =>
  Term (SimpleRep a) ->
  Term rng
pattern FromGeneric x <-
  (App (getWitness -> Just FromGenericW) (x :> Nil))

pattern ToGeneric ::
  forall rng.
  () =>
  forall a.
  (rng ~ SimpleRep a, GenericRequires a, HasSpec a, AppRequires BaseW '[a] rng) =>
  Term a ->
  Term rng
pattern ToGeneric x <- (App (getWitness -> Just ToGenericW) (x :> Nil))

-- | Hints are things that only affect generation, and not validation. For instance, parameters to
--   control distribution of generated values.
class (HasSpec a, Show (Hint a)) => HasGenHint a where
  type Hint a
  giveHint :: Hint a -> Specification a
