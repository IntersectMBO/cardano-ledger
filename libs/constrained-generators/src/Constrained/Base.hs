{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
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

-- | This module contains most of the implementation
-- of the framework.
--
-- NOTE: This is a very big module. Splitting it up would
-- be a nice thing to do but it's not very easy. The problem
-- is that a lot of the things in here depend on each other
-- via a cycle like `Pred` depends on `Term` which depend on
-- `HasSpec` which depends on `Specification` and `Generic` and `Specification`
-- depends in turn on `Pred` and so on.
module Constrained.Base where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable
import Data.Kind
import Data.List (intersect, nub, partition, (\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid qualified as Monoid
import Data.Semigroup (Any (..), Max (..), getAll, getMax)
import Data.Semigroup qualified as Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Typeable
import Data.Word
import GHC.Generics
import GHC.Int
import GHC.Natural
import GHC.Real
import GHC.Stack
import GHC.TypeLits
import Prettyprinter
import System.Random
import System.Random.Stateful
import Test.QuickCheck hiding (Args, Fun, forAll)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import Constrained.Core
import Constrained.Env
import Constrained.GenT
import Constrained.Graph hiding (dependency, irreflexiveDependencyOn, noDependencies)
import Constrained.Graph qualified as Graph
import Constrained.List
import Constrained.Univ

{- NOTE [High level overview of generation from predicates]:

-- The overall strategy ---------------------------------------------------

The strategy for generating things from `Pred`s is relatively straightforward
and relies on one key fact: any constraint that has only one free variable `x`
and where `x` occurs only once can be turned into a `Specification` for `x`.

We say that such constraints _define_ `x` and given a set of constraints `ps`
and a variable `x` we can split `ps` into the constraints that define `x` and
any constraints that don't. We can then generate a value from `x` by computing
a spec for each defining constraint in `ps` and using the `Semigroup` structure
of `Specification`s to combine them and give them to `genFromSpec`. Once we obtain a
value for `x` we can substitute this value in all other constraints and pick
another variable to solve.

For example, given the following constraints on integers `x` and `y`

  x < 10
  3 <= x
  y < x

we see that `x < 10` and `3 <= x` are defining constraints for `x` and there
are no definining constraints for `y`. We compute a `Specification` for `x` for each
constraint, in this case `x < 10` turns into something like `(-∞,10)` and
`3 <= x` turns into `[3, ∞)`. We combine the specs to form `[3, 10)` from which we
can generate a value, e.g. 4 (chosen by fair dice roll). We then substitute
`[x := 4]` in the remaining constraints and obtain `y < 4`, giving us a defining
constraint for `y`.

-- How to pick the variable order -----------------------------------------

At this point it should be relatively clear that the order we pick for the
variables matters a great deal. If we choose to generate `y` before `x` in our
example we will have no defining constraints for `y` and so we pick a value for
it freely. But that renders `x` unsolveable if `y > 9` - which will result in
the generator failing to generate a value (one could consider backtracking, but
that is very computationally expensive so _relying_ on it would probably not be
wise).

Computing a good choice of variable order that leaves the least room for error
is obviously undecidable and difficult and we choose instead an explicit
syntax-directed variable order. Specifically, variable dependency in terms is
_left-to-right_, meaning that the variables in `x + y < z` will be solved in
the order `z -> y -> x`. On top of that there is a constraint `dependsOn y x`
that allows you to overwrite the order of two variables. Consequently, the
following constraints will be solved in the order `z -> x -> y`:

  x + y < z
  y `dependsOn` x

A consequence of this is that it is possible to form dependency loops by
specifying multiple constraints, e.g. in:

  x < y
  y < x + 10

However, this situation can be addressed by the introduction of `dependsOn` to
settle the order.  It is worth noting that the choice of order in `dependsOn`
is important as it affects the solveability of the constraints (as we saw
above). We leave the choice of `dependsOn` in the example below as an exercise
for the reader.

  x < y
  y < x + 10
  0 < x
  ? `dependsOn` ?

-- The total definition requirement ---------------------------------------

For the sake of efficiency we require that all constraints are dispatched as
definining constraints for a variable before we begin solving. We call this the
total definition requirement. This requirement is necessary because a set of
constraints with left over constraints are unlikely to be solveable.

Consider the following example for `p :: (Int, Int)`

fst p < snd p

in which there is no defining constraint for `p`, which would lead us to
compute the spec `mempty` for `p` during solving - meaning we would pick an
arbitrary `p` that is irrespective of the constraints. This is problematic as
the probability of picking `p = (x, y)` such that `x < y` is roughly `1/2`, as
you add more constraints things get much worse.

The principal problem above is that information that is present in the
constraints is lost, which would force us to rely on a `suchThat` approach to
generation - which will become very slow as constraint systems grow.

-- Let binders ------------------------------------------------------------

A solution to the total definition requirement is to introduce more variables.
We can rewrite the problematic `fst p < snd p` example below as:

fst p = x
snd p = y
x < y

The dependency graph for these constraints will be the following:

x `dependsOn` y
p `dependsOn` x

This configuration is solveable, one picks `y` first, then picks `x < y`
and finally constructs `p = (x, y)`.

Note that (1) we introduced more variables than were initially in the
constraints - these need to be bound somewhere - and (2) the order of
`fst p = x` is important - `p` depends on `x` and not the other way
around.

To do both of these things at the same time we introduce the `letBind` construct
to the language:

letBind tm $ \ x -> preds

Which is semantically equivalent to:

exists $ \ x ->
  tm == x
  preds

-- Reifies ----------------------------------------------------------------

Sometimes it's important to be able to perform complex calculations on data
to obtain values that further constrain later variables. For this purpose
the language contains the the `reify` construct:

reify :: IsPred p fn
      => Term fn a
      -> (a -> b)
      -> (Term fn b -> p)
      -> Pred fn

The important thing about `reify` is that because everything in the term
being reified needs to be solved before the body can be solved, all
variables in the body depend on the term being reified.

-}

------------------------------------------------------------------------
-- Terms and Predicates
------------------------------------------------------------------------

-- | Typed first order terms with function symbols from `fn`.
data Term (fn :: [Type] -> Type -> Type) a where
  App ::
    ( Typeable as
    , TypeList as
    , All (HasSpec fn) as
    , HasSpec fn b
    , BaseUniverse fn
    ) =>
    fn as b ->
    List (Term fn) as ->
    Term fn b
  Lit ::
    Show a =>
    a ->
    Term fn a
  V ::
    HasSpec fn a =>
    Var a ->
    Term fn a

instance HasSpec fn a => Eq (Term fn a) where
  V x == V x' = x == x'
  Lit a == Lit b = a == b
  App (f :: fn as b) ts == App (f' :: fn as' b') ts'
    | Just Refl <- eqT @as @as'
    , f == f' =
        mapListC @(HasSpec fn) (WithHasSpec @fn) ts
          == mapListC @(HasSpec fn) (WithHasSpec @fn) ts'
  _ == _ = False

-- NOTE: Fourmolu made me do this - it happily breaks the code unless you
-- make this a standalone type synonym.
type HasSpecImpliesEq fn a f = HasSpec fn a => Eq (f a) :: Constraint
deriving instance HasSpecImpliesEq fn a f => Eq (WithHasSpec fn f a)

instance (Ord a, HasSpec fn (Set a)) => Semigroup (Term fn (Set a)) where
  (<>) = app unionFn

instance (Ord a, HasSpec fn (Set a)) => Monoid (Term fn (Set a)) where
  mempty = Lit mempty

data Binder fn a where
  (:->) ::
    HasSpec fn a =>
    Var a ->
    Pred fn ->
    Binder fn a

deriving instance Show (Binder fn a)

data Pred (fn :: [Type] -> Type -> Type) where
  Monitor :: ((forall a. Term fn a -> a) -> Property -> Property) -> Pred fn
  Block ::
    [Pred fn] ->
    Pred fn
  Exists ::
    -- | Constructive recovery function for checking
    -- existential quantification
    ((forall b. Term fn b -> b) -> GE a) ->
    Binder fn a ->
    Pred fn
  Subst ::
    HasSpec fn a =>
    Var a ->
    Term fn a ->
    Pred fn ->
    Pred fn
  Let ::
    Term fn a ->
    Binder fn a ->
    Pred fn
  Assert ::
    BaseUniverse fn =>
    [String] ->
    Term fn Bool ->
    Pred fn
  Reifies ::
    ( HasSpec fn a
    , HasSpec fn b
    ) =>
    -- | This depends on the `a` term
    Term fn b ->
    Term fn a ->
    -- | Recover a useable value from the `a` term.
    (a -> b) ->
    Pred fn
  -- TODO: there is good cause for not limiting this to `Term fn a` and `Term fn b`.
  -- However, changing it requires re-working quite a lot of code.
  DependsOn ::
    ( HasSpec fn a
    , HasSpec fn b
    ) =>
    Term fn a ->
    Term fn b ->
    Pred fn
  ForAll ::
    ( Forallable t a
    , HasSpec fn t
    , HasSpec fn a
    ) =>
    Term fn t ->
    Binder fn a ->
    Pred fn
  Case ::
    HasSpec fn (SumOver as) =>
    Term fn (SumOver as) ->
    -- | Each branch of the type is bound with
    -- only one variable because `as` are types.
    -- Constructors with multiple arguments are
    -- encoded with `ProdOver` (c.f. `Constrained.Univ`).
    List (Binder fn) as ->
    Pred fn
  -- if-then-else where the branches depend on the scrutinee.
  IfElse ::
    HasSpec fn Bool =>
    Term fn Bool ->
    Pred fn ->
    Pred fn ->
    Pred fn
  GenHint ::
    HasGenHint fn a =>
    Hint a ->
    Term fn a ->
    Pred fn
  TruePred :: Pred fn
  FalsePred :: [String] -> Pred fn

instance BaseUniverse fn => Semigroup (Pred fn) where
  FalsePred es <> _ = FalsePred es
  _ <> FalsePred es = FalsePred es
  TruePred <> p = p
  p <> TruePred = p
  p <> p' = Block (unpackPred p ++ unpackPred p')
    where
      unpackPred (Block ps) = ps
      unpackPred p = [p]

instance BaseUniverse fn => Monoid (Pred fn) where
  mempty = TruePred

-- | Contexts for Terms, basically a term with a _single_ HOLE
-- instead of a variable. This is used to traverse the defining
-- constraints for a variable and turn them into a spec. Each
-- subterm `f vs Ctx vs'` for lists of values `vs` and `vs'`
-- gets given to the `propagateSpecFun` for `f` as
-- `f vs HOLE vs'`.
data Ctx (fn :: [Type] -> Type -> Type) v a where
  -- | A single hole of type `v`
  CtxHOLE ::
    HasSpec fn v =>
    Ctx fn v v
  -- | The application `f vs Ctx vs'`
  CtxApp ::
    ( HasSpec fn b
    , TypeList as
    , Typeable as
    , All (HasSpec fn) as
    ) =>
    fn as b ->
    -- This is basically a `List` where
    -- everything is `Value` except for
    -- one entry which is `Ctx fn v`.
    ListCtx Value as (Ctx fn v) ->
    Ctx fn v b

-- | This is used together with `ListCtx` to form
-- just the arguments to `f vs Ctx vs'` - replacing
-- `Ctx` with `HOLE` - to provide to `propagateSpecFun`.
data HOLE a b where
  HOLE :: HOLE a a

toCtx ::
  forall m fn v a.
  ( BaseUniverse fn
  , Typeable v
  , MonadGenError m
  , HasCallStack
  ) =>
  Var v ->
  Term fn a ->
  m (Ctx fn v a)
toCtx v = go -- . simplifyTerm
  where
    go :: forall b. Term fn b -> m (Ctx fn v b)
    go (Lit i) = fatalError ["toCtx (Lit " ++ show i ++ ")"]
    go (App f as) = CtxApp f <$> toCtxList v as
    go (V v')
      | Just Refl <- eqVar v v' = pure $ CtxHOLE
      | otherwise = fatalError ["toCtx " ++ show v ++ " (V " ++ show v' ++ ")"]

toCtxList ::
  forall m fn v as.
  (BaseUniverse fn, Typeable v, MonadGenError m, HasCallStack) =>
  Var v ->
  List (Term fn) as ->
  m (ListCtx Value as (Ctx fn v))
toCtxList v = prefix
  where
    prefix :: forall as'. HasCallStack => List (Term fn) as' -> m (ListCtx Value as' (Ctx fn v))
    prefix Nil = fatalError ["toCtxList without hole"]
    prefix (Lit l :> ts) = do
      ctx <- prefix ts
      pure $ Value l :! ctx
    prefix (t :> ts) = do
      hole <- toCtx v t
      suf <- suffix ts
      pure $ hole :? suf

    suffix :: forall as'. List (Term fn) as' -> m (List Value as')
    suffix Nil = pure Nil
    suffix (Lit l :> ts) = (Value l :>) <$> suffix ts
    suffix (_ :> _) = fatalError ["toCtxList with too many holes"]

------------------------------------------------------------------------
-- Semantics of `Term` and `Pred`
------------------------------------------------------------------------

runTerm :: MonadGenError m => Env -> Term fn a -> m a
runTerm env = \case
  Lit a -> pure a
  V v -> findEnv env v
  App f ts -> do
    vs <- mapMList (fmap Identity . runTerm env) ts
    pure $ uncurryList_ runIdentity (sem f) vs

-- | Collect the 'monitor' calls from a specification instantiated to the given value. Typically,
--
--   >>> quickCheck $ forAll (genFromSpec_ spec) $ \ x -> monitorSpec spec x $ ...
monitorSpec :: (FunctionLike fn, Testable p) => Specification fn a -> a -> p -> Property
monitorSpec (SuspendedSpec x p) a =
  errorGE (monitorPred (singletonEnv x a) p) . property
monitorSpec _ _ = property

monitorPred ::
  forall fn m. (FunctionLike fn, MonadGenError m) => Env -> Pred fn -> m (Property -> Property)
monitorPred env = \case
  Monitor m -> pure (m $ errorGE . explain ["monitorPred: Monitor"] . runTerm env)
  Subst x t p -> monitorPred env $ substitutePred x t p
  Assert {} -> pure id
  GenHint {} -> pure id
  Reifies {} -> pure id
  ForAll t (x :-> p) -> do
    set <- runTerm env t
    foldr (.) id
      <$> sequence
        [ monitorPred env' p
        | v <- forAllToList set
        , let env' = extendEnv x v env
        ]
  Case t bs -> do
    v <- runTerm env t
    runCaseOn v bs (\x val ps -> monitorPred (extendEnv x val env) ps)
  IfElse b p q -> do
    v <- runTerm env b
    if v then monitorPred env p else monitorPred env q
  TruePred -> pure id
  FalsePred {} -> pure id
  DependsOn {} -> pure id
  Block ps -> foldr (.) id <$> mapM (monitorPred env) ps
  Let t (x :-> p) -> do
    val <- runTerm env t
    monitorPred (extendEnv x val env) p
  Exists k (x :-> p) -> do
    case k (errorGE . explain ["monitorPred: Exists"] . runTerm env) of
      Result _ a -> monitorPred (extendEnv x a env) p
      _ -> pure id

checkPred :: forall fn m. (FunctionLike fn, MonadGenError m) => Env -> Pred fn -> m Bool
checkPred env = \case
  Monitor {} -> pure True
  Subst x t p -> checkPred env $ substitutePred x t p
  Assert [] t -> runTerm env t
  Assert es t -> explain ("assert:" : map ("  " ++) es) $ runTerm env t
  GenHint {} -> pure True
  p@(Reifies t' t f) -> do
    val <- runTerm env t
    val' <- runTerm env t'
    explain ["Reification:", "  " ++ show p] $ pure (f val == val')
  ForAll t (x :-> p) -> do
    set <- runTerm env t
    and
      <$> sequence
        [ checkPred env' p
        | v <- forAllToList set
        , let env' = extendEnv x v env
        ]
  Case t bs -> do
    v <- runTerm env t
    runCaseOn v bs (\x val ps -> checkPred (extendEnv x val env) ps)
  IfElse bt p q -> do
    b <- runTerm env bt
    if b then checkPred env p else checkPred env q
  TruePred -> pure True
  FalsePred es -> explain es $ pure False
  DependsOn {} -> pure True
  Block ps -> checkPreds env ps
  Let t (x :-> p) -> do
    val <- runTerm env t
    checkPred (extendEnv x val env) p
  Exists k (x :-> p) -> do
    a <- runGE $ k (errorGE . explain ["checkPred: Exists"] . runTerm env)
    checkPred (extendEnv x a env) p

checkPreds :: (MonadGenError m, Traversable t, FunctionLike fn) => Env -> t (Pred fn) -> m Bool
checkPreds env ps = and <$> mapM (checkPred env) ps

checkPredPure :: FunctionLike fn => Env -> Pred fn -> Bool
checkPredPure env p = fromGE (const False) $ checkPred env p

runCaseOn ::
  SumOver as ->
  List (Binder fn) as ->
  (forall a. HasSpec fn a => Var a -> a -> Pred fn -> r) ->
  r
runCaseOn _ Nil _ = error "The impossible happened in runCaseOn"
runCaseOn a ((x :-> ps) :> Nil) f = f x a ps
runCaseOn s ((x :-> ps) :> bs@(_ :> _)) f = case s of
  SumLeft a -> f x a ps
  SumRight a -> runCaseOn a bs f

------------------------------------------------------------------------
-- Specs
------------------------------------------------------------------------

-- NOTE: in the future one might consider using sets when a type
-- has `Ord` here. Beware, this means that one needs to have a check
-- for instances of Ord at runtime!
type OrdSet a = [a]

-- | A `Specification fn a` denotes a set of `a`s
data Specification fn a where
  -- | Elements of a known set
  MemberSpec ::
    -- | It must be an element of this set
    OrdSet a ->
    Specification fn a
  -- | The empty set
  ErrorSpec ::
    [String] ->
    Specification fn a
  -- | The set described by some predicates
  -- over the bound variable.
  --
  -- TODO: possibly we want to use a `Binder` here
  SuspendedSpec ::
    HasSpec fn a =>
    -- | This variable ranges over values denoted by
    -- the spec
    Var a ->
    -- | And the variable is subject to these constraints
    Pred fn ->
    Specification fn a
  -- | A type-specific spec
  TypeSpec ::
    HasSpec fn a =>
    TypeSpec fn a ->
    -- | It can't be any of the elements of this set
    OrdSet a ->
    Specification fn a
  -- | Anything
  TrueSpec :: Specification fn a

instance HasSpec fn a => Semigroup (Specification fn a) where
  TrueSpec <> s = s
  s <> TrueSpec = s
  ErrorSpec e <> ErrorSpec e' = ErrorSpec (e ++ "" : (replicate 20 '-') : "" : e')
  ErrorSpec e <> _ = ErrorSpec e
  _ <> ErrorSpec e = ErrorSpec e
  MemberSpec as <> MemberSpec as' =
    explainSpec ["Intersecting: ", "  MemberSpec " ++ show as, "  MemberSpec " ++ show as'] $
      MemberSpec $
        intersect as as'
  MemberSpec as <> TypeSpec s cant =
    MemberSpec $
      filter
        (flip (conformsTo @fn) s)
        (filter (`notElem` cant) as)
  TypeSpec s cant <> MemberSpec as = MemberSpec as <> TypeSpec s cant
  SuspendedSpec v p <> SuspendedSpec v' p' = SuspendedSpec v (p <> rename v' v p')
  SuspendedSpec v ps <> s = SuspendedSpec v (ps <> satisfies (V v) s)
  s <> SuspendedSpec v ps = SuspendedSpec v (ps <> satisfies (V v) s)
  TypeSpec s cant <> TypeSpec s' cant' = case combineSpec s s' of
    -- NOTE: This might look like an unnecessary case, but doing
    -- it like this avoids looping.
    TypeSpec s'' cant'' -> TypeSpec s'' (cant <> cant' <> cant'')
    s'' -> s'' <> notMemberSpec (cant <> cant')

instance HasSpec fn a => Monoid (Specification fn a) where
  mempty = TrueSpec

equalSpec :: a -> Specification fn a
equalSpec = MemberSpec . pure

notEqualSpec :: forall fn a. HasSpec fn a => a -> Specification fn a
notEqualSpec = TypeSpec (emptySpec @fn @a) . pure

notMemberSpec :: forall fn a f. (HasSpec fn a, Foldable f) => f a -> Specification fn a
notMemberSpec = typeSpecOpt (emptySpec @fn @a) . toList

typeSpec :: HasSpec fn a => TypeSpec fn a -> Specification fn a
typeSpec ts = TypeSpec ts mempty

-- The HasSpec Class ------------------------------------------------------

-- | This class provides the interface that allows you to extend the language
-- to handle a new type. In the case of types that have a generic representation
-- (via `HasSimpleRep`) that already has an instance of `HasSpec` it is sufficient
-- to provide an empty instance. However, for types that are used together with
-- specific functions in the function universe `fn` it may be necessary to provide
-- a specific implementation of `HasSpec`. This is typically necessary when the `TypeSpec`
-- for the generic representation does not permit an implementation of `propagateSpecFun`
-- for some function.
--
-- The basic types provided in the language, `Set`, `[]`, `Map`, `Int`, `Word64`,
-- `(,)`, `Either`, etc. have instances of this class (technically `(,)` and `Either` have
-- instances derived from the instances for their generic `Prod` and `Sum` implementations).
class
  ( Typeable a
  , Eq a
  , Show a
  , Show (TypeSpec fn a)
  , BaseUniverse fn
  ) =>
  HasSpec fn a
  where
  -- | The `TypeSpec fn a` is the type-specific `Specification fn a`.
  type TypeSpec (fn :: [Type] -> Type -> Type) a

  type TypeSpec fn a = TypeSpec fn (SimpleRep a)

  -- `TypeSpec` behaves sort-of like a monoid with a neutral
  -- enement `emptySpec` and a `combineSpec` for combining
  -- two `TypeSpec fn a`. However, in order to provide flexibilty
  -- `combineSpec` takes two `TypeSpec` and provide a `Specification`. This
  -- avoids e.g. having to have a separate implementation of `ErrorSpec`
  -- and `MemberSpec` in `TypeSpec`.

  emptySpec :: TypeSpec fn a
  combineSpec :: TypeSpec fn a -> TypeSpec fn a -> Specification fn a

  -- | Generate a value that satisfies the `TypeSpec`.
  -- The key property for this generator is soundness:
  --  ∀ a ∈ genFromTypeSpec spec. a `conformsTo` spec
  genFromTypeSpec :: (HasCallStack, MonadGenError m) => TypeSpec fn a -> GenT m a

  -- | Check conformance to the spec.
  conformsTo :: HasCallStack => a -> TypeSpec fn a -> Bool

  -- | Shrink an `a` with the aide of a `TypeSpec`
  shrinkWithTypeSpec :: TypeSpec fn a -> a -> [a]

  -- | Convert a spec to predicates:
  -- The key property here is:
  --   ∀ a. a `conformsTo` spec == a `conformsTo` constrained (\t -> toPreds t spec)
  toPreds :: Term fn a -> TypeSpec fn a -> Pred fn

  -- | Compute an upper and lower bound on the number of solutions genFromTypeSpec might return
  cardinalTypeSpec :: TypeSpec fn a -> Specification fn Integer
  cardinalTypeSpec _ = TrueSpec

  -- | A bound on the number of solutions `genFromTypeSpec TrueSpec` can produce.
  --   For a type with finite elements, we can get a much more accurate
  --   answer than TrueSpec
  cardinalTrueSpec :: Specification fn Integer
  cardinalTrueSpec = TrueSpec

  -- | For some types (especially finite ones) there may be much better ways to construct
  --   a Specification than the default method of just adding a large 'bad' list to TypSpec. This
  --   function allows each HasSpec instance to decide.
  typeSpecOpt :: TypeSpec fn a -> [a] -> Specification fn a
  typeSpecOpt tySpec bad = TypeSpec tySpec bad

  -- | Prerequisites for the instance that are sometimes necessary
  -- when working with e.g. `Specification`s or functions in the universe.
  type Prerequisites fn a :: Constraint

  type Prerequisites fn a = ()

  -- | Materialize the `Prerequisites` dictionary. It should not be necessary to
  -- implement this function manually.
  prerequisites :: Evidence (Prerequisites fn a)
  default prerequisites :: Prerequisites fn a => Evidence (Prerequisites fn a)
  prerequisites = Evidence

  {- NOTE: Below follows default implementations for the the functions in this
     class.  They are meant to provide an implementation of `HasSpec fn a` when
     `HasSimpleRep a` and `HasSpec fn (SimpleRep a)`. For example, for a
     newtype wrapper like `newtype Foo = Foo Word64` we can define `SimpleRep
     Foo = Word64` with the requisite instance for `HasSimpleRep` (all of which
     is derived from `Generic Foo`) and the instance for `HasSpec fn Foo` is
     essentially the same as the instance for `Word64`. This is achieved by
     ensuring that `TypeSpec fn Foo = TypeSpec fn Word64` (c.f. the default
     implementation of `TypeSpec` above). To this end, the implementations
     below simply convert the relevant things between `SimpleRep a` and `a`.
     For example, in the implementation of `combineSpec s s'` we treat `s` and
     `s'` (which have type `TypeSpec fn a`) as `TypeSpec fn (SimpleRep a)`,
     combine them, and go from the resulting `Specification fn (SimpleRep a)` to `Specification
     fn a` using `fromSimpleRepSpec`.
   -}

  default emptySpec ::
    (HasSpec fn (SimpleRep a), TypeSpec fn a ~ TypeSpec fn (SimpleRep a)) => TypeSpec fn a
  emptySpec = emptySpec @fn @(SimpleRep a)

  default combineSpec ::
    ( HasSimpleRep a
    , HasSpec fn (SimpleRep a)
    , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    ) =>
    TypeSpec fn a ->
    TypeSpec fn a ->
    Specification fn a
  combineSpec s s' = fromSimpleRepSpec $ combineSpec @fn @(SimpleRep a) s s'

  default genFromTypeSpec ::
    ( HasSimpleRep a
    , HasSpec fn (SimpleRep a)
    , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    ) =>
    (HasCallStack, MonadGenError m) =>
    TypeSpec fn a ->
    GenT m a
  genFromTypeSpec s = fromSimpleRep <$> genFromTypeSpec @fn s

  default conformsTo ::
    ( HasSimpleRep a
    , HasSpec fn (SimpleRep a)
    , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    ) =>
    HasCallStack =>
    a ->
    TypeSpec fn a ->
    Bool
  a `conformsTo` s = conformsTo @fn (toSimpleRep a) s

  default toPreds ::
    ( HasSpec fn (SimpleRep a)
    , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    ) =>
    Term fn a ->
    TypeSpec fn a ->
    Pred fn
  toPreds v s = toPreds (toGeneric_ v) s

  default shrinkWithTypeSpec ::
    ( HasSpec fn (SimpleRep a)
    , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    ) =>
    TypeSpec fn a ->
    a ->
    [a]
  shrinkWithTypeSpec spec a = map fromSimpleRep $ shrinkWithTypeSpec @fn spec (toSimpleRep a)

data WithHasSpec fn f a where
  WithHasSpec :: HasSpec fn a => f a -> WithHasSpec fn f a

-- The Forallable class ---------------------------------------------------

class Forallable t e | t -> e where
  fromForAllSpec ::
    (HasSpec fn t, HasSpec fn e, BaseUniverse fn) => Specification fn e -> Specification fn t
  default fromForAllSpec ::
    ( HasSpec fn t
    , HasSpec fn e
    , HasSimpleRep t
    , TypeSpec fn t ~ TypeSpec fn (SimpleRep t)
    , Forallable (SimpleRep t) e
    , HasSpec fn (SimpleRep t)
    ) =>
    Specification fn e ->
    Specification fn t
  fromForAllSpec es = fromSimpleRepSpec $ fromForAllSpec @(SimpleRep t) @e es

  forAllToList :: t -> [e]
  default forAllToList ::
    ( HasSimpleRep t
    , Forallable (SimpleRep t) e
    ) =>
    t ->
    [e]
  forAllToList t = forAllToList (toSimpleRep t)

-- The HasGenHint class ---------------------------------------------------

-- | Hints are things that only affect generation, and not validation. For instance, parameters to
--   control distribution of generated values.
class (HasSpec fn a, Show (Hint a)) => HasGenHint fn a where
  type Hint a
  giveHint :: Hint a -> Specification fn a

-- Semantics of specs -----------------------------------------------------

conformsToSpecM ::
  forall fn a m. (HasSpec fn a, MonadGenError m, Alternative m) => a -> Specification fn a -> m ()
conformsToSpecM _ TrueSpec = pure ()
conformsToSpecM a (MemberSpec as) = explain [show a ++ " not an element of " ++ show as] $ guard $ elem a as
conformsToSpecM a (TypeSpec s cant) = guard $ notElem a cant && conformsTo @fn a s
conformsToSpecM a (SuspendedSpec v ps) = guard =<< checkPred (singletonEnv v a) ps
conformsToSpecM _ (ErrorSpec es) = explain es $ guard False

conformsToSpecProp :: forall fn a. HasSpec fn a => a -> Specification fn a -> Property
conformsToSpecProp a s = fromGEProp $ conformsToSpecM a (simplifySpec s)

conformsToSpec :: forall fn a. HasSpec fn a => a -> Specification fn a -> Bool
conformsToSpec a s = isOk $ conformsToSpecM a s

satisfies :: forall fn a. HasSpec fn a => Term fn a -> Specification fn a -> Pred fn
satisfies _ TrueSpec = TruePred
satisfies e (MemberSpec as) = Assert [show e ++ " `elem` " ++ show as] $ elem_ e (Lit as)
satisfies t (SuspendedSpec x p) = Subst x t p
satisfies e (TypeSpec s cant)
  | null cant = toPreds e s
  | otherwise =
      Assert [show e ++ " `notElem` " ++ show cant] (not_ (elem_ e $ Lit cant)) <> toPreds e s
satisfies _ (ErrorSpec e) = FalsePred e

------------------------------------------------------------------------
-- Generating things
------------------------------------------------------------------------

-- Generating things from specs -------------------------------------------

-- | Generate a value that satisfies the spec. This function can fail if the
-- spec is inconsistent, there is a dependency error, or if the underlying
-- generators are not flexible enough.
genFromSpec ::
  forall fn a m. (HasCallStack, HasSpec fn a, MonadGenError m) => Specification fn a -> GenT m a
genFromSpec (simplifySpec -> spec) = case spec of
  TrueSpec -> genFromSpec @fn (typeSpec $ emptySpec @fn @a)
  MemberSpec as
    | null as -> genError ["MemberSpec {}"]
    | otherwise -> explain ["genFromSpec " ++ show spec] $ pureGen (elements as)
  SuspendedSpec x p
    -- NOTE: If `x` isn't free in `p` we still have to try to generate things
    -- from `p` to make sure `p` is sat and then we can throw it away. A better
    -- approach would be to only do this in the case where we don't know if `p`
    -- is sat. The proper way to implement such a sat check is to remove
    -- sat-but-unnecessary variables in the optimiser.
    | not $ Name x `appearsIn` p -> do
        _ <- genFromPreds p
        genFromSpec @fn TrueSpec
    | otherwise -> do
        env <- genFromPreds p
        findEnv env x
  TypeSpec s cant ->
    explain ["", "genFromSpec", "    " ++ show (typeRep cant), "    " ++ show spec] $
      -- TODO: we could consider giving `cant` as an argument to `genFromTypeSpec` if this
      -- starts giving us trouble.
      genFromTypeSpec @fn s `suchThatT` (`notElem` cant)
  ErrorSpec e -> genError e

shrinkWithSpec :: forall fn a. HasSpec fn a => Specification fn a -> a -> [a]
-- TODO: possibly allow for ignoring the `conformsToSpec` check in the `TypeSpec`
-- case when you know what you're doing
shrinkWithSpec (simplifySpec -> spec) a = filter (`conformsToSpec` spec) $ case spec of
  -- TODO: filter on can't if we have a known to be sound shrinker
  TypeSpec s _ -> shrinkWithTypeSpec @fn s a
  -- TODO: The better way of doing this is to compute the dependency graph,
  -- shrink one variable at a time, and fixup the rest of the variables
  SuspendedSpec {} -> shr a
  MemberSpec {} -> shr a
  TrueSpec -> shr a
  ErrorSpec {} -> []
  where
    shr = shrinkWithTypeSpec @fn (emptySpec @fn @a)

-- | A version of `genFromSpec` that simply errors if the generator fails
genFromSpec_ :: forall fn a. (HasCallStack, HasSpec fn a) => Specification fn a -> Gen a
genFromSpec_ spec = do
  res <- strictGen $ genFromSpec spec
  errorGE $ fmap pure res

-- | A version of `genFromSpec` that takes a seed and a size and gives you a result
genFromSpecWithSeed ::
  forall fn a. (HasCallStack, HasSpec fn a) => Int -> Int -> Specification fn a -> a
genFromSpecWithSeed seed size spec = unGen (genFromSpec_ spec) (mkQCGen seed) size

genInverse ::
  ( MonadGenError m
  , HasSpec fn a
  , Show b
  , Functions fn fn
  ) =>
  fn '[a] b ->
  Specification fn a ->
  b ->
  GenT m a
genInverse f argS x =
  let argSpec' = argS <> propagateSpecFun f (NilCtx HOLE) (equalSpec x)
   in explain
        [ "genInverse"
        , "  f = " ++ show f
        , show $ "  argS =" <+> pretty argS
        , "  x = " ++ show x
        , show $ "  argSpec' =" <+> pretty argSpec'
        ]
        $ genFromSpec argSpec'

-- Generating things from predicates --------------------------------------

-- | Flatten nested `Let`, `Exists`, and `Block` in a `Pred fn`. `Let` and
-- `Exists` bound variables become free in the result.
flattenPred :: forall fn. BaseUniverse fn => Pred fn -> [Pred fn]
flattenPred pIn = go (freeVarNames pIn) [pIn]
  where
    go _ [] = []
    go fvs (p : ps) = case p of
      Block ps' -> go fvs (ps' ++ ps)
      -- NOTE: the order of the arguments to `==.` here are important.
      -- The whole point of `Let` is that it allows us to solve all of `t`
      -- before we solve the variables in `t`.
      Let t b -> goBinder fvs b ps (\x -> (assert (t ==. V x) :))
      Exists _ b -> goBinder fvs b ps (const id)
      _ -> p : go fvs ps

    goBinder ::
      Set Int ->
      Binder fn a ->
      [Pred fn] ->
      (HasSpec fn a => Var a -> [Pred fn] -> [Pred fn]) ->
      [Pred fn]
    goBinder fvs (x :-> p) ps k = k x' $ go (Set.insert (nameOf x') fvs) (p' : ps)
      where
        (x', p') = freshen x p fvs

computeDependencies :: Pred fn -> DependGraph fn
computeDependencies = \case
  Monitor {} -> mempty
  Subst x t p -> computeDependencies (substitutePred x t p)
  Assert _ t -> computeTermDependencies t
  Reifies t' t _ -> t' `irreflexiveDependencyOn` t
  ForAll set b ->
    let innerG = computeBinderDependencies b
     in innerG <> set `irreflexiveDependencyOn` nodes innerG
  x `DependsOn` y -> x `irreflexiveDependencyOn` y
  Case t bs ->
    let innerG = foldMapList (computeBinderDependencies) bs
     in innerG <> t `irreflexiveDependencyOn` nodes innerG
  IfElse b p q ->
    let pG = computeDependencies p
        qG = computeDependencies q
     in (nodes pG <> nodes qG) `irreflexiveDependencyOn` b <> pG <> qG
  TruePred -> mempty
  FalsePred {} -> mempty
  Block ps -> foldMap computeDependencies ps
  Exists _ b -> computeBinderDependencies b
  Let t b -> noDependencies t <> computeBinderDependencies b
  GenHint _ t -> noDependencies t

-- | Linearize a predicate, turning it into a list of variables to solve and
-- their defining constraints such that each variable can be solved independently.
prepareLinearization :: BaseUniverse fn => Pred fn -> GE [(Name fn, [Pred fn])]
prepareLinearization p = do
  let preds = flattenPred p
      hints = computeHints preds
      graph = transitiveClosure $ hints <> respecting hints (foldMap computeDependencies preds)
  linearize preds graph

-- | Generate a satisfying `Env` for a `p : Pred fn`. The `Env` contains values for
-- all the free variables in `flattenPred p`.
genFromPreds :: (MonadGenError m, BaseUniverse fn) => Pred fn -> GenT m Env
genFromPreds (optimisePred -> preds) = do
  -- NOTE: this is just lazy enough that the work of flattening, computing dependencies,
  -- and linearizing is memoized in properties that use `genFromPreds`.
  linear <- runGE $ prepareLinearization preds
  go mempty linear
  where
    go :: (MonadGenError m, BaseUniverse fn) => Env -> [(Name fn, [Pred fn])] -> GenT m Env
    go env [] = pure env
    go env ((Name v, ps) : nps) = do
      let ps' = substPred env <$> ps
          spec = explainSpec ("Computing specs for" : map show ps') $ foldMap (computeSpec v) ps'
      val <- genFromSpec spec
      go (extendEnv v val env) nps

-- TODO: here we can compute both the explicit hints (i.e. constraints that
-- define the order of two variables) and any whole-program smarts.
computeHints :: forall fn. [Pred fn] -> Hints fn
computeHints ps =
  transitiveClosure $ fold [x `irreflexiveDependencyOn` y | DependsOn x y <- ps]

computeBinderDependencies :: Binder fn a -> DependGraph fn
computeBinderDependencies (x :-> p) =
  deleteNode (Name x) $ computeDependencies p

computeTermDependencies :: Term fn a -> DependGraph fn
computeTermDependencies = fst . computeTermDependencies'

computeTermDependencies' :: Term fn a -> (DependGraph fn, Set (Name fn))
computeTermDependencies' (App _ args) = go args
  where
    go :: List (Term fn) as -> (DependGraph fn, Set (Name fn))
    go Nil = (mempty, mempty)
    go (t :> ts) =
      let (gr, ngr) = go ts
          (tgr, ntgr) = computeTermDependencies' t
       in (ntgr `irreflexiveDependencyOn` ngr <> tgr <> gr, ngr <> ntgr)
computeTermDependencies' Lit {} = (mempty, mempty)
computeTermDependencies' (V x) = (noDependencies (Name x), Set.singleton (Name x))

-- Consider: A + B = C + D
-- We want to fail if A and B are independent.
-- Consider: A + B = A + C, A <- B
-- Here we want to consider this constraint defining for A
linearize ::
  (MonadGenError m, BaseUniverse fn) => [Pred fn] -> DependGraph fn -> m [(Name fn, [Pred fn])]
linearize preds graph = do
  sorted <- case topsort graph of
    Left cycle ->
      fatalError $
        [ "linearize: Dependency cycle in graph: "
        , "  cycle: " ++ show cycle
        , "  preds:"
        ]
          ++ [show $ indent 4 (pretty p) | p <- preds]
          ++ [ "  graph: " ++ show graph
             ]
    Right sorted -> pure sorted
  go sorted [(freeVarSet ps, ps) | ps <- filter isRelevantPred preds]
  where
    isRelevantPred TruePred = False
    isRelevantPred DependsOn {} = False
    isRelevantPred (Assert _ (Lit True)) = False
    isRelevantPred _ = True

    go [] [] = pure []
    go [] ps
      | null $ foldMap fst ps = do
          res <- checkPreds mempty (map snd ps)
          if res
            then pure []
            else genError ["Linearize const False"]
      | otherwise =
          fatalError $
            [ "Dependency error in `linearize`: "
            , "  graph: " ++ show graph
            , "  the following left-over constraints are not defining constraints for a unique variable: "
            ]
              ++ [show $ indent 4 (pretty p) | (_, p) <- ps]
    go (n : ns) ps = do
      let (nps, ops) = partition (isLastVariable n . fst) ps
      ((n, map snd nps) :) <$> go ns ops

    isLastVariable n set = n `Set.member` set && solvableFrom n (Set.delete n set) graph

------------------------------------------------------------------------
-- Computing specs
------------------------------------------------------------------------

fromGESpec :: GE (Specification fn a) -> Specification fn a
fromGESpec ge = case ge of
  Result es s -> explainSpec (concatMap (++ [""]) es) s
  _ -> fromGE ErrorSpec ge

explainSpec :: [String] -> Specification fn a -> Specification fn a
explainSpec es (ErrorSpec es') = ErrorSpec (es ++ es')
explainSpec es (MemberSpec []) = ErrorSpec (es ++ ["MemberSpec []"])
explainSpec _ s = s

simplifySpec :: HasSpec fn a => Specification fn a -> Specification fn a
simplifySpec spec = case spec of
  SuspendedSpec x p -> explainSpec [show $ "Simplifying" /> pretty spec] $ computeSpecSimplified x (optimisePred p)
  MemberSpec xs -> MemberSpec (nub xs)
  ErrorSpec es -> ErrorSpec es
  TypeSpec ts cant -> TypeSpec ts (nub cant)
  TrueSpec -> TrueSpec

-- | Precondition: the `Pred fn` defines the `Var a`
computeSpecSimplified ::
  forall fn a. (HasSpec fn a, HasCallStack) => Var a -> Pred fn -> Specification fn a
computeSpecSimplified x p = fromGESpec $ case p of
  Monitor {} -> pure mempty
  GenHint h t -> propagateSpec (giveHint h) <$> toCtx x t -- NOTE: this implies you do need to actually propagate hints, e.g. propagate depth control in a `tail` or `cdr` like function
  Subst x' t p' -> pure $ computeSpec x (substitutePred x' t p') -- NOTE: this is impossible as it should have gone away already
  TruePred -> pure mempty
  FalsePred es -> genError es
  Block ps -> pure $ foldMap (computeSpecSimplified x) ps
  Let t b -> pure $ SuspendedSpec x (Let t b)
  Exists k b -> pure $ SuspendedSpec x (Exists k b)
  Assert _ (Lit True) -> pure mempty
  Assert _ (Lit False) -> genError [show p]
  Assert _ t -> explain [show p] $ propagateSpec (equalSpec True) <$> toCtx x t
  ForAll (Lit s) b -> pure $ foldMap (\val -> computeSpec x $ unBind val b) (forAllToList s)
  ForAll t b -> propagateSpec (fromForAllSpec $ computeSpecBinderSimplified b) <$> toCtx x t
  Case (Lit val) bs -> pure $ runCaseOn val bs $ \va vaVal psa -> computeSpec x (substPred (singletonEnv va vaVal) psa)
  Case t branches ->
    let branchSpecs = mapList computeSpecBinderSimplified branches
     in propagateSpec (caseSpec branchSpecs) <$> toCtx x t
  IfElse (Lit b) tp fp -> if b then pure $ computeSpecSimplified x tp else pure $ computeSpecSimplified x fp
  -- TODO: Fix this by having a pass that figures out if `p` or `q` are trivially true or false (thus constraining
  -- the scrutinee of the ifElse)
  IfElse {} ->
    fatalError ["Dependency error in computeSpec: IfElse", "  " ++ show x, show $ indent 2 (pretty p)]
  Reifies (Lit a) (Lit val) f
    | f val == a -> pure TrueSpec
    | otherwise -> genError ["Value does not reify to literal: " ++ show val ++ " -/> " ++ show a]
  Reifies t' (Lit val) f ->
    propagateSpec (equalSpec (f val)) <$> toCtx x t'
  Reifies Lit {} _ _ -> pure TrueSpec
  -- Impossible cases that should be ruled out by the dependency analysis and linearizer
  DependsOn {} ->
    fatalError
      ["The impossible happened in computeSpec: DependsOn", "  " ++ show x, show $ indent 2 (pretty p)]
  Reifies {} ->
    fatalError
      ["The impossible happened in computeSpec: Reifies", "  " ++ show x, show $ indent 2 (pretty p)]

-- | Precondition: the `Pred fn` defines the `Var a`
computeSpec :: forall fn a. (HasSpec fn a, HasCallStack) => Var a -> Pred fn -> Specification fn a
computeSpec x p = computeSpecSimplified x (simplifyPred p)

computeSpecBinder :: Binder fn a -> Specification fn a
computeSpecBinder (x :-> p) = computeSpec x p

computeSpecBinderSimplified :: Binder fn a -> Specification fn a
computeSpecBinderSimplified (x :-> p) = computeSpecSimplified x p

caseSpec ::
  forall fn as.
  HasSpec fn (SumOver as) =>
  List (Specification fn) as ->
  Specification fn (SumOver as)
caseSpec Nil = error "The impossible happened in caseSpec"
caseSpec (s :> Nil) = s
caseSpec (s :> ss@(_ :> _))
  | Evidence <- prerequisites @fn @(SumOver as) = typeSpec $ SumSpec s (caseSpec ss)

propagateSpec ::
  forall fn v a.
  HasSpec fn v =>
  Specification fn a ->
  Ctx fn v a ->
  Specification fn v
propagateSpec spec = \case
  CtxHOLE -> spec
  CtxApp f (ListCtx pre c suf)
    | Evidence <- ctxHasSpec c -> propagateSpec (propagateSpecFun f (ListCtx pre HOLE suf) spec) c

ctxHasSpec :: Ctx fn v a -> Evidence (HasSpec fn a)
ctxHasSpec CtxHOLE = Evidence
ctxHasSpec CtxApp {} = Evidence

class
  ( forall as b. Show (f as b)
  , forall as b. Eq (f as b)
  , Typeable f
  , FunctionLike f
  ) =>
  Functions f fn
  where
  propagateSpecFun ::
    ( TypeList as
    , Typeable as
    , HasSpec fn a
    , All (HasSpec fn) as
    ) =>
    f as b ->
    ListCtx Value as (HOLE a) ->
    Specification fn b ->
    Specification fn a

  rewriteRules ::
    ( TypeList as
    , Typeable as
    , HasSpec fn b
    , All (HasSpec fn) as
    ) =>
    f as b ->
    List (Term fn) as ->
    Maybe (Term fn b)
  rewriteRules _ _ = Nothing

  mapTypeSpec ::
    ( HasSpec fn a
    , HasSpec fn b
    ) =>
    f '[a] b ->
    TypeSpec fn a ->
    Specification fn b

mapSpec ::
  forall fn a b.
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  fn '[a] b ->
  Specification fn a ->
  Specification fn b
mapSpec f TrueSpec = mapTypeSpec f (emptySpec @fn @a)
mapSpec _ (ErrorSpec err) = ErrorSpec err
mapSpec f (MemberSpec as) = MemberSpec $ map (sem f) as
mapSpec f (SuspendedSpec x p) =
  constrained $ \x' ->
    Exists (\_ -> fatalError ["mapSpec"]) (x :-> fold [assert $ x' ==. app f (V x), p])
mapSpec f (TypeSpec ts cant) = mapTypeSpec f ts <> notMemberSpec (map (sem f) cant)

-- | If the `Specification fn Bool` doesn't constrain the boolean you will get a `TrueSpec` out.
caseBoolSpec ::
  HasSpec fn a => Specification fn Bool -> (Bool -> Specification fn a) -> Specification fn a
caseBoolSpec spec cont = case possibleValues spec of
  [] -> ErrorSpec ["No possible values in caseBoolSpec"]
  [b] -> cont b
  _ -> mempty
  where
    possibleValues s = filter (flip conformsToSpec (simplifySpec s)) [True, False]

isErrorLike :: Specification fn a -> Bool
isErrorLike ErrorSpec {} = True
isErrorLike (MemberSpec []) = True
isErrorLike _ = False

------------------------------------------------------------------------
-- Dependency Graphs
------------------------------------------------------------------------

type DependGraph fn = Graph.Graph (Name fn)

dependency :: HasVariables fn t => Name fn -> t -> DependGraph fn
dependency x (freeVarSet -> xs) = Graph.dependency x xs

irreflexiveDependencyOn ::
  forall fn t t'. (HasVariables fn t, HasVariables fn t') => t -> t' -> DependGraph fn
irreflexiveDependencyOn (freeVarSet -> xs) (freeVarSet -> ys) = Graph.irreflexiveDependencyOn xs ys

noDependencies :: HasVariables fn t => t -> DependGraph fn
noDependencies (freeVarSet -> xs) = Graph.noDependencies xs

type Hints fn = DependGraph fn

respecting :: Hints f -> DependGraph f -> DependGraph f
respecting hints g = g `subtractGraph` opGraph hints

solvableFrom :: Name f -> Set (Name f) -> DependGraph f -> Bool
solvableFrom x s g =
  let less = dependencies x g
   in s `Set.isSubsetOf` less && not (x `Set.member` less)

------------------------------------------------------------------------
-- Free variables and variable names
------------------------------------------------------------------------

freeVarNames :: forall fn t. HasVariables fn t => t -> Set Int
freeVarNames = Set.mapMonotonic (\(Name v) -> nameOf v) . freeVarSet

data Name fn where
  Name :: HasSpec fn a => Var a -> Name fn

deriving instance Show (Name fn)

instance Eq (Name fn) where
  Name (Var i) == Name (Var j) = i == j

instance Ord (Name fn) where
  compare (Name (Var i)) (Name (Var j)) = compare i j

newtype FreeVars fn = FreeVars {unFreeVars :: Map (Name fn) Int}
  deriving (Show)

restrictedTo :: FreeVars fn -> Set (Name fn) -> FreeVars fn
restrictedTo (FreeVars m) nms = FreeVars $ Map.restrictKeys m nms

memberOf :: Name fn -> FreeVars fn -> Bool
memberOf n (FreeVars m) = Map.member n m

count :: Name fn -> FreeVars fn -> Int
count n (FreeVars m) = fromMaybe 0 $ Map.lookup n m

instance Semigroup (FreeVars fn) where
  FreeVars fv <> FreeVars fv' = FreeVars $ Map.unionWith (+) fv fv'

instance Monoid (FreeVars fn) where
  mempty = FreeVars mempty

freeVar :: Name fn -> FreeVars fn
freeVar n = singleton n 1

singleton :: Name fn -> Int -> FreeVars fn
singleton n k = FreeVars $ Map.singleton n k

without :: Foldable t => FreeVars fn -> t (Name fn) -> FreeVars fn
without (FreeVars m) remove = FreeVars $ foldr Map.delete m (toList remove)

class HasVariables fn a | a -> fn where
  freeVars :: a -> FreeVars fn
  freeVarSet :: a -> Set (Name fn)
  freeVarSet = Map.keysSet . unFreeVars . freeVars
  countOf :: Name fn -> a -> Int
  countOf n = count n . freeVars
  appearsIn :: Name fn -> a -> Bool
  appearsIn n = (> 0) . count n . freeVars

instance (HasVariables f a, HasVariables f b) => HasVariables f (a, b) where
  freeVars (a, b) = freeVars a <> freeVars b
  freeVarSet (a, b) = freeVarSet a <> freeVarSet b
  countOf n (a, b) = countOf n a + countOf n b
  appearsIn n (a, b) = appearsIn n a || appearsIn n b

instance HasVariables fn (List (Term fn) as) where
  freeVars Nil = mempty
  freeVars (x :> xs) = freeVars x <> freeVars xs
  freeVarSet Nil = mempty
  freeVarSet (x :> xs) = freeVarSet x <> freeVarSet xs
  countOf _ Nil = 0
  countOf n (x :> xs) = countOf n x + countOf n xs
  appearsIn _ Nil = False
  appearsIn n (x :> xs) = appearsIn n x || appearsIn n xs

instance HasVariables f (Name f) where
  freeVars = freeVar
  freeVarSet = Set.singleton
  countOf n n'
    | n == n' = 1
    | otherwise = 0
  appearsIn n n' = n == n'

instance HasVariables fn (Term fn a) where
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

instance HasVariables fn (Pred fn) where
  freeVars = \case
    GenHint _ t -> freeVars t
    Subst x t p -> freeVars t <> freeVars p `without` [Name x]
    Block ps -> foldMap freeVars ps
    Let t b -> freeVars t <> freeVars b
    Exists _ b -> freeVars b
    Assert _ t -> freeVars t
    Reifies t' t _ -> freeVars t' <> freeVars t
    DependsOn x y -> freeVars x <> freeVars y
    ForAll set b -> freeVars set <> freeVars b
    Case t bs -> freeVars t <> freeVars bs
    IfElse b p q -> freeVars b <> freeVars p <> freeVars q
    TruePred -> mempty
    FalsePred _ -> mempty
    Monitor {} -> mempty
  freeVarSet = \case
    GenHint _ t -> freeVarSet t
    Subst x t p -> freeVarSet t <> Set.delete (Name x) (freeVarSet p)
    Block ps -> foldMap freeVarSet ps
    Let t b -> freeVarSet t <> freeVarSet b
    Exists _ b -> freeVarSet b
    Assert _ t -> freeVarSet t
    Reifies t' t _ -> freeVarSet t' <> freeVarSet t
    DependsOn x y -> freeVarSet x <> freeVarSet y
    ForAll set b -> freeVarSet set <> freeVarSet b
    Case t bs -> freeVarSet t <> freeVarSet bs
    IfElse b p q -> freeVarSet b <> freeVarSet p <> freeVarSet q
    TruePred -> mempty
    FalsePred _ -> mempty
    Monitor {} -> mempty
  countOf n = \case
    GenHint _ t -> countOf n t
    Subst x t p
      | n == Name x -> countOf n t
      | otherwise -> countOf n t + countOf n p
    Block ps -> sum $ map (countOf n) ps
    Let t b -> countOf n t + countOf n b
    Exists _ b -> countOf n b
    Assert _ t -> countOf n t
    Reifies t' t _ -> countOf n t' + countOf n t
    DependsOn x y -> countOf n x + countOf n y
    ForAll set b -> countOf n set + countOf n b
    Case t bs -> countOf n t + countOf n bs
    IfElse b p q -> countOf n b + countOf n p + countOf n q
    TruePred -> 0
    FalsePred _ -> 0
    Monitor {} -> 0
  appearsIn n = \case
    GenHint _ t -> appearsIn n t
    Subst x t p
      | n == Name x -> appearsIn n t
      | otherwise -> appearsIn n t || appearsIn n p
    Block ps -> any (appearsIn n) ps
    Let t b -> appearsIn n t || appearsIn n b
    Exists _ b -> appearsIn n b
    Assert _ t -> appearsIn n t
    Reifies t' t _ -> appearsIn n t' || appearsIn n t
    DependsOn x y -> appearsIn n x || appearsIn n y
    ForAll set b -> appearsIn n set || appearsIn n b
    Case t bs -> appearsIn n t || appearsIn n bs
    IfElse b p q -> appearsIn n b || appearsIn n p || appearsIn n q
    TruePred -> False
    FalsePred _ -> False
    Monitor {} -> False

instance HasVariables fn (Binder fn a) where
  freeVars (x :-> p) = freeVars p `without` [Name x]
  freeVarSet (x :-> p) = Set.delete (Name x) (freeVarSet p)
  countOf n (x :-> p)
    | Name x == n = 0
    | otherwise = countOf n p
  appearsIn n (x :-> p)
    | Name x == n = False
    | otherwise = appearsIn n p

instance HasVariables fn (List (Binder fn) as) where
  freeVars Nil = mempty
  freeVars (a :> as) = freeVars a <> freeVars as
  freeVarSet Nil = mempty
  freeVarSet (a :> as) = freeVarSet a <> freeVarSet as
  countOf _ Nil = 0
  countOf n (x :> xs) = countOf n x + countOf n xs
  appearsIn _ Nil = False
  appearsIn n (x :> xs) = appearsIn n x || appearsIn n xs

instance {-# OVERLAPPABLE #-} (Foldable t, HasVariables f a) => HasVariables f (t a) where
  freeVars = foldMap freeVars
  freeVarSet = foldMap freeVarSet
  countOf n = Monoid.getSum . foldMap (Monoid.Sum . countOf n)
  appearsIn n = any (appearsIn n)

instance HasVariables f a => HasVariables f (Set a) where
  freeVars = foldMap freeVars
  freeVarSet = foldMap freeVarSet
  countOf n = sum . Set.map (countOf n)
  appearsIn n = any (appearsIn n)

------------------------------------------------------------------------
-- Substitutions
------------------------------------------------------------------------

type Subst fn = [SubstEntry fn]

data SubstEntry fn where
  (:=) :: HasSpec fn a => Var a -> Term fn a -> SubstEntry fn

backwardsSubstitution :: forall fn a. HasSpec fn a => Subst fn -> Term fn a -> Term fn a
backwardsSubstitution sub t =
  case findMatch sub t of
    -- TODO: what about multiple matches??
    Just x -> V x
    Nothing -> case t of
      Lit a -> Lit a
      V x -> V x
      App f ts -> App f (mapListC @(HasSpec fn) (backwardsSubstitution sub) ts)
  where
    findMatch :: Subst fn -> Term fn a -> Maybe (Var a)
    findMatch [] _ = Nothing
    findMatch (x := t' : sub) t
      | fastInequality t t' = findMatch sub t
      | Just (x', t'') <- cast (x, t')
      , t == t'' =
          Just x'
      | otherwise = findMatch sub t

-- | Sound but not complete inequality on terms
fastInequality :: Term fn a -> Term fn b -> Bool
fastInequality (V (Var i)) (V (Var j)) = i /= j
fastInequality Lit {} Lit {} = False
fastInequality (App _ as) (App _ bs) = go as bs
  where
    go :: List (Term fn) as -> List (Term fn) bs -> Bool
    go Nil Nil = False
    go (a :> as) (b :> bs) = fastInequality a b || go as bs
    go _ _ = True
fastInequality _ _ = True

substituteTerm :: forall fn a. Subst fn -> Term fn a -> Term fn a
substituteTerm sub = \case
  Lit a -> Lit a
  V x -> substVar sub x
  App f (mapList (substituteTerm sub) -> ts) ->
    case fromLits ts of
      Just vs -> Lit (uncurryList_ unValue (sem f) vs)
      _ -> App f ts
  where
    substVar :: HasSpec fn a => Subst fn -> Var a -> Term fn a
    substVar [] x = V x
    substVar (y := t : sub) x
      | Just Refl <- eqVar x y = t
      | otherwise = substVar sub x

substituteTerm' :: forall fn a. Subst fn -> Term fn a -> Writer Any (Term fn a)
substituteTerm' sub = \case
  Lit a -> pure $ Lit a
  V x -> substVar sub x
  App f ts ->
    App f <$> mapMList (substituteTerm' sub) ts
  where
    substVar :: HasSpec fn a => Subst fn -> Var a -> Writer Any (Term fn a)
    substVar [] x = pure $ V x
    substVar (y := t : sub) x
      | Just Refl <- eqVar x y = t <$ tell (Any True)
      | otherwise = substVar sub x

substituteBinder :: HasSpec fn a => Var a -> Term fn a -> Binder fn b -> Binder fn b
substituteBinder x tm (y :-> p) = y' :-> substitutePred x tm p'
  where
    (y', p') =
      freshen y p (Set.singleton (nameOf x) <> freeVarNames tm <> Set.delete (nameOf y) (freeVarNames p))

substitutePred :: HasSpec fn a => Var a -> Term fn a -> Pred fn -> Pred fn
substitutePred x tm = \case
  GenHint h t -> GenHint h (substituteTerm [x := tm] t)
  Subst x' t p -> substitutePred x tm $ substitutePred x' t p
  Assert es t -> Assert es (substituteTerm [x := tm] t)
  Block ps -> fold (substitutePred x tm <$> ps)
  Exists k b -> Exists (\eval -> k (eval . substituteTerm [x := tm])) (substituteBinder x tm b)
  Let t b -> Let (substituteTerm [x := tm] t) (substituteBinder x tm b)
  ForAll t b -> ForAll (substituteTerm [x := tm] t) (substituteBinder x tm b)
  Case t bs -> Case (substituteTerm [x := tm] t) (mapList (substituteBinder x tm) bs)
  IfElse b p q -> IfElse (substituteTerm [x := tm] b) (substitutePred x tm p) (substitutePred x tm q)
  Reifies t' t f -> Reifies (substituteTerm [x := tm] t') (substituteTerm [x := tm] t) f
  DependsOn t t' -> DependsOn (substituteTerm [x := tm] t) (substituteTerm [x := tm] t')
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  Monitor m -> Monitor m

instance Rename (Name f) where
  rename v v' (Name v'') = Name $ rename v v' v''

instance Rename (Term fn a) where
  rename v v'
    | v == v' = id
    | otherwise = \case
        Lit l -> Lit l
        V v'' -> V (rename v v' v'')
        App f a -> App f (rename v v' a)

instance Rename (Pred fn) where
  rename v v'
    | v == v' = id
    | otherwise = \case
        GenHint h t -> GenHint h (rename v v' t)
        Subst x t p -> rename v v' $ substitutePred x t p
        Block ps -> Block (rename v v' ps)
        Exists k b -> Exists (\eval -> k $ eval . rename v v') (rename v v' b)
        Let t b -> Let (rename v v' t) (rename v v' b)
        Reifies t' t f -> Reifies (rename v v' t') (rename v v' t) f
        Assert es t -> Assert es (rename v v' t)
        DependsOn x y -> DependsOn (rename v v' x) (rename v v' y)
        ForAll set b -> ForAll (rename v v' set) (rename v v' b)
        Case t bs -> Case (rename v v' t) (rename v v' bs)
        IfElse b p q -> IfElse (rename v v' b) (rename v v' p) (rename v v' q)
        TruePred -> TruePred
        FalsePred es -> FalsePred es
        Monitor m -> Monitor m

instance Rename (Binder fn a) where
  rename v v' (va :-> psa) = va' :-> rename v v' psa'
    where
      (va', psa') = freshen va psa (Set.fromList [nameOf v, nameOf v'] <> Set.delete (nameOf va) (freeVarNames psa))

substTerm :: Env -> Term fn a -> Term fn a
substTerm env = \case
  Lit a -> Lit a
  V v
    | Just a <- lookupEnv env v -> Lit a
    | otherwise -> V v
  App f (mapList (substTerm env) -> ts) ->
    case fromLits ts of
      Just vs -> Lit (uncurryList_ unValue (sem f) vs)
      _ -> App f ts

substBinder :: Env -> Binder fn a -> Binder fn a
substBinder env (x :-> p) = x :-> substPred (removeVar x env) p

substPred :: BaseUniverse fn => Env -> Pred fn -> Pred fn
substPred env = \case
  GenHint h t -> GenHint h (substTerm env t)
  Subst x t p -> substPred env $ substitutePred x t p
  Assert es t -> assertExplain es (substTerm env t)
  Reifies t' t f -> Reifies (substTerm env t') (substTerm env t) f
  ForAll set b -> ForAll (substTerm env set) (substBinder env b)
  Case t bs -> Case (substTerm env t) (mapList (substBinder env) bs)
  IfElse b p q -> IfElse (substTerm env b) (substPred env p) (substPred env q)
  DependsOn x y -> DependsOn (substTerm env x) (substTerm env y)
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  Block ps -> fold (substPred env <$> ps)
  Exists k b -> Exists (\eval -> k $ eval . substTerm env) (substBinder env b)
  Let t b -> Let (substTerm env t) (substBinder env b)
  Monitor m -> Monitor m

unBind :: a -> Binder fn a -> Pred fn
unBind a (x :-> p) = substPred (singletonEnv x a) p

------------------------------------------------------------------------
-- Rewrite rules and simplification
------------------------------------------------------------------------

-- Simplification for preds and terms -------------------------------------

simplifyTerm :: forall fn a. BaseUniverse fn => Term fn a -> Term fn a
simplifyTerm = \case
  V v -> V v
  Lit l -> Lit l
  App f (mapList simplifyTerm -> ts)
    | Just vs <- fromLits ts -> Lit $ uncurryList_ unValue (sem f) vs
    | Just t <- rewriteRules f ts -> simplifyTerm t
    | otherwise -> App f ts

fromLits :: List (Term fn) as -> Maybe (List Value as)
fromLits = mapMList fromLit

fromLit :: Term fn a -> Maybe (Value a)
fromLit (Lit l) = pure $ Value l
fromLit _ = Nothing

isLit :: Term fn a -> Bool
isLit = isJust . fromLit

simplifyPred :: BaseUniverse fn => Pred fn -> Pred fn
simplifyPred = \case
  -- If the term simplifies away to a literal, that means there is no
  -- more generation to do so we can get rid of `GenHint`
  GenHint h t -> case simplifyTerm t of
    Lit {} -> TruePred
    t' -> GenHint h t'
  Subst x t p -> simplifyPred $ substitutePred x t p
  Assert es t -> assertExplain es (simplifyTerm t)
  Reifies t' t f -> Reifies (simplifyTerm t') (simplifyTerm t) f
  ForAll set b -> case simplifyTerm set of
    Lit as -> foldMap (`unBind` b) (forAllToList as)
    set' -> ForAll set' (simplifyBinder b)
  DependsOn x y -> DependsOn x y
  Case t bs -> mkCase (simplifyTerm t) (mapList simplifyBinder bs)
  IfElse b p q -> ifElse (simplifyTerm b) (simplifyPred p) (simplifyPred q)
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  Block ps -> fold (simplifyPreds ps)
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

simplifyPreds :: BaseUniverse fn => [Pred fn] -> [Pred fn]
simplifyPreds = go [] . map simplifyPred
  where
    go acc [] = reverse acc
    go _ (FalsePred err : _) = [FalsePred err]
    go acc (TruePred : ps) = go acc ps
    go acc (p : ps) = go (p : acc) ps

simplifyBinder :: Binder fn a -> Binder fn a
simplifyBinder (x :-> p) = x :-> simplifyPred p

-- Arcane magic -----------------------------------------------------------

-- | Is the variable x pinned to some free term in p? (free term
-- meaning that all the variables in the term are free in p).
--
-- TODO: complete this with more cases!
pinnedBy :: forall fn a. (BaseUniverse fn, Typeable a) => Var a -> Pred fn -> Maybe (Term fn a)
pinnedBy x (Assert _ (App (extractFn @(EqFn fn) @fn -> Just Equal) (t :> t' :> Nil)))
  | V x' <- t, Just Refl <- eqVar x x' = Just t'
  | V x' <- t', Just Refl <- eqVar x x' = Just t
pinnedBy x (Block ps) = listToMaybe $ catMaybes $ map (pinnedBy x) ps
pinnedBy _ _ = Nothing

-- TODO: it might be necessary to run aggressiveInlining again after the let floating etc.
optimisePred :: BaseUniverse fn => Pred fn -> Pred fn
optimisePred p =
  simplifyPred
    . letSubexpressionElimination
    . letFloating
    . aggressiveInlining
    . simplifyPred
    $ p

-- Common subexpression elimination but only on terms that are already let-bound.
letSubexpressionElimination :: BaseUniverse fn => Pred fn -> Pred fn
letSubexpressionElimination = go []
  where
    adjustSub x sub =
      [ x' := t | x' := t <- sub, isNothing $ eqVar x x',
      -- TODO: possibly freshen the binder where
      -- `x` appears instead?
      not $ Name x `appearsIn` t
      ]

    goBinder :: Subst fn -> Binder fn a -> Binder fn a
    goBinder sub (x :-> p) = x :-> go (adjustSub x sub) p

    go sub = \case
      GenHint h t -> GenHint h (backwardsSubstitution sub t)
      Block ps -> Block (go sub <$> ps)
      Let t (x :-> p) -> Let t' (x :-> go (x := t' : sub') p)
        where
          t' = backwardsSubstitution sub t
          sub' = adjustSub x sub
      Exists k b -> Exists k (goBinder sub b)
      Subst x t p -> go sub (substitutePred x t p)
      Assert es t -> Assert es (backwardsSubstitution sub t)
      Reifies t' t f -> Reifies (backwardsSubstitution sub t') (backwardsSubstitution sub t) f
      -- NOTE: this is a tricky case. One possible thing to do here is to keep the old `DependsOn t t'`
      -- and have the new DependsOn if `backwardsSubstitution` changed something. With this semantics you
      -- risk running into unintuitive behaviour if you have something like:
      -- ```
      -- let x = y + z in
      --  {y + z `dependsOn` w
      --   assert $ w <. y + 2
      --   ...}
      -- ```
      -- This will be rewritten as:
      -- ```
      -- let x = y + z in
      --  {z `dependsOn` w
      --   assert $ w <. y + 2
      --   ...}
      -- ```
      -- which changes the dependency order of `w` and `y`. However, fixing
      -- this behaviour in turn makes it more difficult to detect when
      -- variables are no longer used after being substituted away - which
      -- blocks some other optimizations. As we strongly encourage users not to
      -- use `letBind` in their own code most users will never encounter this issue
      -- so the tradeoff is probably worth it.
      DependsOn t t' -> DependsOn (backwardsSubstitution sub t) (backwardsSubstitution sub t')
      ForAll t b -> ForAll (backwardsSubstitution sub t) (goBinder sub b)
      Case t bs -> Case (backwardsSubstitution sub t) (mapList (goBinder sub) bs)
      IfElse b p q -> IfElse (backwardsSubstitution sub b) (go sub p) (go sub q)
      TruePred -> TruePred
      FalsePred es -> FalsePred es
      Monitor m -> Monitor m

-- TODO: this can probably be cleaned up and generalized along with generalizing
-- to make sure we float lets in some missing cases.
letFloating :: BaseUniverse fn => Pred fn -> Pred fn
letFloating = fold . go []
  where
    goBlock ctx ps = goBlock' (freeVarNames ctx <> freeVarNames ps) ctx ps

    goBlock' _ ctx [] = ctx
    goBlock' fvs ctx (Let t (x :-> p) : ps) =
      -- We can do `goBlock'` here because we've already done let floating
      -- on the inner `p`
      [Let t (x' :-> fold (goBlock' (Set.insert (nameOf x') fvs) ctx (p' : ps)))]
      where
        (x', p') = freshen x p fvs
    goBlock' fvs ctx (Block ps : ps') = goBlock' fvs ctx (ps ++ ps')
    goBlock' fvs ctx (p : ps) = goBlock' fvs (p : ctx) ps

    goExists ::
      HasSpec fn a =>
      [Pred fn] ->
      (Binder fn a -> Pred fn) ->
      Var a ->
      Pred fn ->
      [Pred fn]
    goExists ctx ex x (Let t (y :-> p))
      | not $ Name x `appearsIn` t =
          let (y', p') = freshen y p (Set.insert (nameOf x) $ freeVarNames p <> freeVarNames t)
           in go ctx (Let t (y' :-> ex (x :-> p')))
    goExists ctx ex x p = ex (x :-> p) : ctx

    go ctx = \case
      Block ps0 -> goBlock ctx (map letFloating ps0)
      Let t (x :-> p) -> goBlock ctx [Let t (x :-> letFloating p)]
      Exists k (x :-> p) -> goExists ctx (Exists k) x (letFloating p)
      Subst x t p -> go ctx (substitutePred x t p)
      Reifies t' t f -> Reifies t' t f : ctx
      -- TODO: float let through forall if possible
      ForAll t (x :-> p) -> ForAll t (x :-> letFloating p) : ctx
      -- TODO: float let through the cases if possible
      Case t bs -> Case t (mapList (\(x :-> p) -> x :-> letFloating p) bs) : ctx
      -- TODO: float let through if possible
      IfElse b p q -> IfElse b (letFloating p) (letFloating q) : ctx
      -- Boring cases
      Assert es t -> Assert es t : ctx
      GenHint h t -> GenHint h t : ctx
      DependsOn t t' -> DependsOn t t' : ctx
      TruePred -> TruePred : ctx
      FalsePred es -> FalsePred es : ctx
      Monitor m -> Monitor m : ctx

aggressiveInlining :: BaseUniverse fn => Pred fn -> Pred fn
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
    goBinder :: FreeVars fn -> Subst fn -> Binder fn a -> Writer Any (Binder fn a)
    goBinder fvs sub (x :-> p) = (x :->) <$> go (underBinder fvs x p) (underBinderSub sub x) p

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
            pure $ runCaseOn a bs $ \x v p -> substPred (singletonEnv x v) p
        | (x :-> p :> Nil) <- bs -> do
            let t' = substituteAndSimplifyTerm sub t
            p' <- go (underBinder fvs x p) (x := t' : sub) p
            pure $ Case t (x :-> p' :> Nil)
        | otherwise -> Case t <$> mapMList (goBinder fvs sub) bs
      IfElse b tp fp
        | not (isLit b)
        , Lit a <- substituteAndSimplifyTerm sub b -> do
            tell $ Any True
            pure $ if a then tp else fp
        | otherwise -> ifElse b <$> go fvs sub tp <*> go fvs sub fp
      Let t (x :-> p)
        | all (\n -> count n fvs <= 1) (freeVarSet t) -> do
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
      Block ps -> fold <$> mapM (go fvs sub) ps
      Assert es t
        | not (isLit t)
        , Lit b <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ assertExplain es b
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

-- | Apply a substitution and simplify the resulting term if the substitution changed the
-- term.
substituteAndSimplifyTerm :: BaseUniverse fn => Subst fn -> Term fn a -> Term fn a
substituteAndSimplifyTerm sub t =
  case runWriter $ substituteTerm' sub t of
    (t', Any b)
      | b -> simplifyTerm t'
      | otherwise -> t'

------------------------------------------------------------------------
-- Generics
------------------------------------------------------------------------

{-
`HasSimpleRep` and `GenericsFn` are meant to allow you to express that a
type is isomorphic to some other type that has a `HasSpec` instance.

The trick is that the default instance of `HasSpec fn a` assumes
`HasSimpleRep a` and defines `TypeSpec fn a = TypeSpec fn (SimpleRep a)`.

From this it's possible to work with things of type `a` in constraints by
treating them like things of type `SimpleRep a`. This allows us to do case
matching etc. on `a` when `SimpleRep a` is a `Sum` type, for example.

Or alternatively it allows us to treat `a` as a newtype over `SimpleRep a`
when using `match`.

-}

class HasSimpleRep a where
  type SimpleRep a
  type TheSop a :: [Type]
  toSimpleRep :: a -> SimpleRep a
  fromSimpleRep :: SimpleRep a -> a

  type TheSop a = SOPOf (Rep a)
  type SimpleRep a = SOP (TheSop a)

  default toSimpleRep ::
    ( Generic a
    , SimpleGeneric (Rep a)
    , SimpleRep a ~ SimplifyRep (Rep a)
    ) =>
    a ->
    SimpleRep a
  toSimpleRep = toSimpleRep' . from

  default fromSimpleRep ::
    ( Generic a
    , SimpleGeneric (Rep a)
    , SimpleRep a ~ SimplifyRep (Rep a)
    ) =>
    SimpleRep a ->
    a
  fromSimpleRep = to . fromSimpleRep'

type family SimplifyRep f where
  SimplifyRep f = SOP (SOPOf f)

toGenericFn ::
  forall fn a.
  ( HasSpec fn (SimpleRep a)
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , HasSimpleRep a
  ) =>
  fn '[a] (SimpleRep a)
toGenericFn = injectFn $ ToGeneric @fn

fromGenericFn ::
  forall fn a.
  ( HasSpec fn (SimpleRep a)
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  , HasSimpleRep a
  ) =>
  fn '[SimpleRep a] a
fromGenericFn = injectFn $ FromGeneric @fn

data GenericsFn fn args res where
  ToGeneric ::
    ( HasSpec fn (SimpleRep a)
    , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    ) =>
    GenericsFn fn '[a] (SimpleRep a)
  FromGeneric ::
    ( HasSpec fn (SimpleRep a)
    , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    ) =>
    GenericsFn fn '[SimpleRep a] a

deriving instance Show (GenericsFn fn as b)
deriving instance Eq (GenericsFn fn args res)

fromSimpleRepSpec ::
  forall a fn.
  (HasSpec fn a, HasSimpleRep a, TypeSpec fn a ~ TypeSpec fn (SimpleRep a)) =>
  Specification fn (SimpleRep a) ->
  Specification fn a
fromSimpleRepSpec = \case
  TrueSpec -> TrueSpec
  ErrorSpec e -> ErrorSpec e
  TypeSpec s'' cant -> TypeSpec s'' $ map fromSimpleRep cant
  MemberSpec elems -> MemberSpec (map fromSimpleRep elems)
  SuspendedSpec x p ->
    constrained $ \x' ->
      Let (toGeneric_ x') (x :-> p)

toSimpleRepSpec ::
  forall a fn.
  (HasSpec fn a, HasSpec fn (SimpleRep a), HasSimpleRep a, TypeSpec fn a ~ TypeSpec fn (SimpleRep a)) =>
  Specification fn a ->
  Specification fn (SimpleRep a)
toSimpleRepSpec = \case
  TrueSpec -> TrueSpec
  ErrorSpec e -> ErrorSpec e
  TypeSpec s'' cant -> TypeSpec s'' $ map toSimpleRep cant
  MemberSpec elems -> MemberSpec (map toSimpleRep elems)
  SuspendedSpec x p ->
    constrained $ \x' ->
      Let (fromGeneric_ x') (x :-> p)

{- This part of the code base is responsible for implementing the conversion
   from a `Generic` type to a `Sum` over `Prod` representation that automatically
   gives you an instance of `HasSpec`. The user has three options for building their
   own instances of `HasSpec`, either they hand-roll an instance, they go with the
   entirely `Generic` instance, or they provide their own `SimpleRep` for their type.

   The latter may be appropriate when the type is an optimized representation:

   ```
   newtype Foo = Foo { unFoo :: MemoBytes ActualFoo }

   instance HasSimpleRep Foo where
     type SimpleRep Foo = ActualFoo
     toSimpleRep = unMemoBytes . unFoo
     fromSimpleRep = Foo . memoBytes
   ```

   This would then allow for `Foo` to be treated as a simple `newtype` over `ActualFoo`
   in constraints:

   ```
   fooSpec :: Specification fn Foo
   fooSpec = constrained $ \ foo ->
     match foo $ \ actualFoo -> ...
   ```
-}

-- Building a SOP type (Sum Of Prod) --------------------------------------

-- | A constructor name with the types of its arguments
data (c :: Symbol) ::: (ts :: [Type])

-- | Turn a `Rep` into a list that flattens the sum
-- structre and gives the constructors names:
--   Maybe Int -> '["Nothing" ::: '[()], "Just" ::: '[Int]]
--   Either Int Bool -> '["Left" ::: '[Int], "Right" ::: '[Bool]]
--   data Foo = Foo Int Bool | Bar Double -> '["Foo" ::: '[Int, Bool], "Bar" ::: '[Double]]
type family SOPOf f where
  SOPOf (D1 _ f) = SOPOf f
  SOPOf (f :+: g) = Append (SOPOf f) (SOPOf g)
  SOPOf (C1 ('MetaCons constr _ _) f) = '[constr ::: Constr f]

-- | Flatten a single constructor
type family Constr f where
  -- TODO: Here we should put in the selector names
  -- so that they can be re-used to create selectors more
  -- easily than the current disgusting `Fst . Snd . Snd . Snd ...`
  -- method.
  Constr (S1 _ f) = Constr f
  Constr (K1 _ k) = '[k]
  Constr U1 = '[()]
  Constr (f :*: g) = Append (Constr f) (Constr g)

-- | Turn a list from `SOPOf` into a `Sum` over
-- `Prod` representation.
type family SOP constrs where
  SOP '[c ::: prod] = ProdOver prod
  SOP ((c ::: prod) : constrs) = Sum (ProdOver prod) (SOP constrs)

-- Constructing a SOP -----------------------------------------------------

type family ConstrOf c sop where
  ConstrOf c (c ::: constr : sop) = constr
  ConstrOf c (_ : sop) = ConstrOf c sop

class Inject c constrs r where
  inject' :: (SOP constrs -> r) -> FunTy (ConstrOf c constrs) r

instance TypeList prod => Inject c '[c ::: prod] r where
  inject' k = curryList_ @prod Identity (listToProd k)

instance TypeList prod => Inject c ((c ::: prod) : prod' : constrs) r where
  inject' k = curryList_ @prod Identity (listToProd (k . SumLeft @_ @(SOP (prod' : constrs))))

instance
  {-# OVERLAPPABLE #-}
  ( FunTy (ConstrOf c ((c' ::: prod) : con : constrs)) r ~ FunTy (ConstrOf c (con : constrs)) r
  , -- \^ An unfortunately roundabout way of saying `c !~ c'`
    Inject c (con : constrs) r
  ) =>
  Inject c ((c' ::: prod) : con : constrs) r
  where
  inject' k = inject' @c @(con : constrs) (k . SumRight)

inject ::
  forall c constrs. Inject c constrs (SOP constrs) => FunTy (ConstrOf c constrs) (SOP constrs)
inject = inject' @c @constrs id

-- Deconstructing a SOP ---------------------------------------------------

-- | An `ALG constrs r` is a function that takes a way to turn every
-- constructor into an `r`:
-- ```
-- ALG (SOPOf (Rep (Either Int Bool))) r = (Int -> r) -> (Bool -> r) -> r
-- ```
type family ALG constrs r where
  ALG '[c ::: prod] r = FunTy prod r -> r
  ALG ((c ::: prod) : constrs) r = FunTy prod r -> ALG constrs r

class SOPLike constrs r where
  -- | Run a `SOP`
  algebra :: SOP constrs -> ALG constrs r

  -- | Ignore everything in the `SOP`
  consts :: r -> ALG constrs r

instance TypeList prod => SOPLike '[c ::: prod] r where
  algebra prod f = uncurryList_ @prod runIdentity f $ prodToList prod
  consts r _ = r

instance (TypeList prod, SOPLike (con : cases) r) => SOPLike ((c ::: prod) : con : cases) r where
  algebra (SumLeft prod) f = consts @(con : cases) @r (algebra @'[c ::: prod] prod f)
  algebra (SumRight rest) _ = algebra @(con : cases) @r rest

  consts r _ = consts @(con : cases) r

-- The individual constructor level ---------------------------------------

class SimpleConstructor rep where
  toSimpleCon' :: rep p -> ProdOver (Constr rep)
  fromSimpleCon' :: ProdOver (Constr rep) -> rep p

instance
  ( SimpleConstructor f
  , SimpleConstructor g
  , TypeList (Constr f)
  , TypeList (Constr g)
  ) =>
  SimpleConstructor (f :*: g)
  where
  toSimpleCon' (a :*: b) = appendProd @(Constr f) @(Constr g) (toSimpleCon' a) (toSimpleCon' b)
  fromSimpleCon' constr =
    let Prod a b = splitProd @(Constr f) @(Constr g) constr
     in (fromSimpleCon' a :*: fromSimpleCon' b)

instance SimpleConstructor f => SimpleConstructor (S1 s f) where
  toSimpleCon' (M1 f) = toSimpleCon' f
  fromSimpleCon' a = M1 (fromSimpleCon' a)

instance SimpleConstructor (K1 i k) where
  toSimpleCon' (K1 k) = k
  fromSimpleCon' k = K1 k

instance SimpleConstructor U1 where
  toSimpleCon' U1 = ()
  fromSimpleCon' _ = U1

-- The sum type level -----------------------------------------------------

-- | Construct and deconstruct cases in a `SOP`
class SopList xs ys where
  injectSOPLeft :: SOP xs -> SOP (Append xs ys)
  injectSOPRight :: SOP ys -> SOP (Append xs ys)
  caseSOP :: SOP (Append xs ys) -> Sum (SOP xs) (SOP ys)

instance SopList '[c ::: x] (y : ys) where
  injectSOPLeft = SumLeft
  injectSOPRight = SumRight
  caseSOP = id

instance SopList (x' : xs) (y : ys) => SopList (c ::: x : x' : xs) (y : ys) where
  injectSOPLeft (SumLeft a) = SumLeft a
  injectSOPLeft (SumRight b) = SumRight (injectSOPLeft @(x' : xs) @(y : ys) b)

  injectSOPRight a = SumRight (injectSOPRight @(x' : xs) @(y : ys) a)

  caseSOP (SumLeft a) = SumLeft (SumLeft a)
  caseSOP (SumRight b) = case caseSOP @(x' : xs) @(y : ys) b of
    SumLeft b' -> SumLeft (SumRight b')
    SumRight b' -> SumRight b'

class SimpleGeneric rep where
  toSimpleRep' :: rep p -> SimplifyRep rep
  fromSimpleRep' :: SimplifyRep rep -> rep p

instance SimpleGeneric f => SimpleGeneric (D1 d f) where
  toSimpleRep' (M1 f) = toSimpleRep' f
  fromSimpleRep' a = M1 (fromSimpleRep' a)

instance
  ( SimpleGeneric f
  , SimpleGeneric g
  , SopList (SOPOf f) (SOPOf g)
  ) =>
  SimpleGeneric (f :+: g)
  where
  toSimpleRep' (L1 f) = injectSOPLeft @(SOPOf f) @(SOPOf g) $ toSimpleRep' f
  toSimpleRep' (R1 g) = injectSOPRight @(SOPOf f) @(SOPOf g) $ toSimpleRep' g
  fromSimpleRep' sop =
    case caseSOP @(SOPOf f) @(SOPOf g) sop of
      SumLeft l -> L1 (fromSimpleRep' l)
      SumRight r -> R1 (fromSimpleRep' r)

instance SimpleConstructor f => SimpleGeneric (C1 ('MetaCons c a b) f) where
  toSimpleRep' (M1 f) = toSimpleCon' f
  fromSimpleRep' a = M1 (fromSimpleCon' a)

------------------------------------------------------------------------
-- Sums and folds
------------------------------------------------------------------------

class HasSpec fn a => Foldy fn a where
  genList ::
    (BaseUniverse fn, MonadGenError m) => Specification fn a -> Specification fn a -> GenT m [a]
  theAddFn :: fn '[a, a] a
  theZero :: a

adds :: forall fn a. Foldy fn a => [a] -> a
adds = foldr (sem $ theAddFn @fn) (theZero @fn)

data FoldSpec (fn :: [Type] -> Type -> Type) a where
  NoFold :: FoldSpec fn a
  FoldSpec ::
    forall b fn a.
    ( HasSpec fn a
    , HasSpec fn b
    , Foldy fn b
    , Member (ListFn fn) fn
    , BaseUniverse fn
    ) =>
    fn '[a] b ->
    Specification fn b ->
    FoldSpec fn a

combineFoldSpec :: MonadGenError m => FoldSpec fn a -> FoldSpec fn a -> m (FoldSpec fn a)
combineFoldSpec NoFold s = pure s
combineFoldSpec s NoFold = pure s
combineFoldSpec (FoldSpec (f :: fn as b) s) (FoldSpec (f' :: fn' as' b') s')
  | Just Refl <- eqT @b @b'
  , Just Refl <- eqT @fn @fn'
  , f == f' =
      pure $ FoldSpec f (s <> s')
  | otherwise =
      genError ["Can't combine fold specs on different functions", "  " ++ show f, "  " ++ show f']

conformsToFoldSpec :: forall fn a. [a] -> FoldSpec fn a -> Bool
conformsToFoldSpec _ NoFold = True
conformsToFoldSpec xs (FoldSpec f s) = adds @fn (map (sem f) xs) `conformsToSpec` s

toPredsFoldSpec :: forall fn a. BaseUniverse fn => Term fn [a] -> FoldSpec fn a -> Pred fn
toPredsFoldSpec _ NoFold = TruePred
toPredsFoldSpec x (FoldSpec fn sspec) =
  satisfies (app (foldMapFn fn) x) sspec

-- | Note: potentially infinite list
enumerateInterval :: (Enum a, Num a, Ord a, MaybeBounded a) => NumSpec a -> [a]
enumerateInterval (NumSpecInterval lo hi) =
  case (lo <|> lowerBound, hi <|> upperBound) of
    (Nothing, Nothing) -> interleave [0 ..] [-1, -2 ..]
    (Nothing, Just b) -> [b, b - 1 ..]
    (Just a, Nothing) -> [a ..]
    (Just a, Just b) -> [a .. b]
  where
    interleave [] ys = ys
    interleave (x : xs) ys = x : interleave ys xs

isEmptyNumSpec ::
  (TypeSpec fn a ~ NumSpec a, Ord a, Enum a, Num a, MaybeBounded a) => Specification fn a -> Bool
isEmptyNumSpec = \case
  ErrorSpec {} -> True
  TrueSpec -> False
  MemberSpec as -> null as
  SuspendedSpec {} -> False
  TypeSpec i cant -> null $ enumerateInterval i \\ cant

knownUpperBound ::
  (TypeSpec fn a ~ NumSpec a, Ord a, Enum a, Num a, MaybeBounded a) => Specification fn a -> Maybe a
knownUpperBound TrueSpec = upperBound
knownUpperBound (MemberSpec []) = Nothing
knownUpperBound (MemberSpec as) = Just $ maximum as
knownUpperBound ErrorSpec {} = Nothing
knownUpperBound SuspendedSpec {} = upperBound
knownUpperBound (TypeSpec (NumSpecInterval lo hi) cant) = upper (lo <|> lowerBound) (hi <|> upperBound)
  where
    upper _ Nothing = Nothing
    upper Nothing (Just b) = listToMaybe $ [b, b - 1 ..] \\ cant
    upper (Just a) (Just b)
      | a == b = a <$ guard (a `notElem` cant)
      | otherwise = listToMaybe $ [b, b - 1 .. a] \\ cant

knownLowerBound ::
  (TypeSpec fn a ~ NumSpec a, Ord a, Enum a, Num a, MaybeBounded a) => Specification fn a -> Maybe a
knownLowerBound TrueSpec = lowerBound
knownLowerBound (MemberSpec []) = Nothing
knownLowerBound (MemberSpec as) = Just $ minimum as
knownLowerBound ErrorSpec {} = Nothing
knownLowerBound SuspendedSpec {} = lowerBound
knownLowerBound (TypeSpec (NumSpecInterval lo hi) cant) =
  lower (lo <|> lowerBound) (hi <|> upperBound)
  where
    lower Nothing _ = Nothing
    lower (Just a) Nothing = listToMaybe $ [a, a + 1 ..] \\ cant
    lower (Just a) (Just b)
      | a == b = a <$ guard (a `notElem` cant)
      | otherwise = listToMaybe $ [a, a + 1 .. b] \\ cant

narrowByFuelAndSize ::
  forall a fn.
  ( BaseUniverse fn
  , TypeSpec fn a ~ NumSpec a
  , HasSpec fn a
  , Arbitrary a
  , Integral a
  , Ord a
  , Random a
  , MaybeBounded a
  ) =>
  -- | Fuel
  a ->
  -- | Integer
  Int ->
  (Specification fn a, Specification fn a) ->
  (Specification fn a, Specification fn a)
narrowByFuelAndSize fuel size specs =
  loop (1000 :: Int) (narrowFoldSpecs specs)
  where
    loop 0 specs =
      error $
        unlines
          [ "narrowByFuelAndSize loops:"
          , "  fuel = " ++ show fuel
          , "  size = " ++ show size
          , "  specs = " ++ show specs
          , "  narrowFoldSpecs spec = " ++ show (narrowFoldSpecs specs)
          , "  go (narrowFoldSpecs specs) = " ++ show (go (narrowFoldSpecs specs))
          ]
    loop n specs = case go specs of
      Nothing -> specs
      Just specs' -> loop (n - 1) (narrowFoldSpecs specs')

    canReach _ 0 s = s == 0
    canReach e fuel s
      -- You can reach it in one step
      | s <= e = 0 < fuel
      | otherwise = canReach e (fuel - 1) (s - e)

    -- Precondition:
    --   a is negative
    --   the type has more negative numbers than positive ones
    safeNegate a
      | Just u <- upperBound
      , a < negate u =
          u
      | otherwise = negate a

    divCeil a b
      | b * d < a = d + 1
      | otherwise = d
      where
        d = a `div` b

    go (elemS, foldS)
      -- There is nothing we can do
      | fuel == 0 = Nothing
      -- Give up as early as possible
      | Just 0 <- knownUpperBound elemS
      , Just 0 <- knownLowerBound elemS
      , not $ 0 `conformsToSpec` foldS =
          Just (ErrorSpec ["only 0 left"], foldS)
      -- Make sure we try to generate the smallest possible list
      -- that gives you the right result - don't put a bunch of zeroes in
      -- a _small_ (size 0) list.
      | size == 0
      , 0 `conformsToSpec` elemS =
          Just (elemS <> notEqualSpec 0, foldS)
      -- Member specs with non-zero elements, TODO: explain
      | MemberSpec xs <- elemS
      , Just u <- knownUpperBound foldS
      , all (0 <=) xs
      , any (0 <) xs
      , let xMinP = minimum $ filter (0 <) xs
            possible x = x == u || xMinP <= u - x
            xs' = filter possible xs
      , xs' /= xs =
          Just (MemberSpec xs', foldS)
      -- The lower bound on the number of elements is too low
      | Just e <- knownLowerBound elemS
      , e > 0
      , Just s <- knownLowerBound foldS
      , s > 0
      , let c = divCeil s fuel
      , e < c =
          Just (elemS <> geqSpec @fn c, foldS)
      -- The upper bound on the number of elements is too high
      | Just e <- knownUpperBound elemS
      , e < 0
      , Just s <- knownUpperBound foldS
      , s < 0
      , let c = divCeil (safeNegate s) fuel
      , negate c < e =
          Just (elemS <> leqSpec @fn c, foldS)
      -- It's time to stop generating negative numbers
      | Just s <- knownLowerBound foldS
      , s > 0
      , Just e <- knownUpperBound elemS
      , e > 0
      , not $ canReach e (fuel `div` 2) s
      , maybe True (<= 0) (knownLowerBound elemS) =
          Just (elemS <> gtSpec @fn 0, foldS)
      -- It's time to stop generating positive numbers
      | Just s <- knownUpperBound foldS
      , s < 0
      , Just e <- knownLowerBound elemS
      , e < 0
      , not $ canReach (safeNegate e) (fuel `div` 2) (safeNegate s)
      , maybe True (<= 0) (knownLowerBound elemS) =
          Just (elemS <> gtSpec @fn 0, foldS)
      -- We HAVE to set the lower bound to the lower
      -- bound on the sum
      | Just s <- knownLowerBound foldS
      , fuel == 1
      , s `conformsToSpec` elemS
      , maybe True (< s) (knownLowerBound elemS) =
          Just (elemS <> geqSpec @fn s, foldS)
      -- There is nothing we need to do
      | otherwise = Nothing

narrowFoldSpecs ::
  forall a fn.
  ( BaseUniverse fn
  , TypeSpec fn a ~ NumSpec a
  , HasSpec fn a
  , Arbitrary a
  , Integral a
  , Ord a
  , Random a
  , MaybeBounded a
  ) =>
  (Specification fn a, Specification fn a) ->
  (Specification fn a, Specification fn a)
narrowFoldSpecs specs = maybe specs narrowFoldSpecs (go specs)
  where
    -- Note: make sure there is some progress when returning Just or this will loop forever
    go (elemS, foldS) = case (elemS, foldS) of
      -- Empty foldSpec
      (_, ErrorSpec {}) -> Nothing
      _ | isEmptyNumSpec foldS -> Just (elemS, ErrorSpec ["Empty foldSpec:", show foldS])
      -- Empty elemSpec
      (ErrorSpec {}, MemberSpec [0]) -> Nothing
      (ErrorSpec {}, _)
        | 0 `conformsToSpec` foldS -> Just (elemS, MemberSpec [0])
        | otherwise ->
            Just
              ( elemS
              , ErrorSpec
                  [ "Empty elemSpec and non-zero foldSpec"
                  , show $ indent 2 $ "elemSpec =" /> pretty elemS
                  , show $ indent 2 $ "foldSpec =" /> pretty foldS
                  ]
              )
      -- We can reduce the size of the `elemS` interval when it is
      -- `[l, u]` or `[l, ∞)` given that `0 <= l` and we have
      -- an upper bound on the sum - we can't pick things bigger than the
      -- upper bound.
      _
        | Just lo <- knownLowerBound elemS
        , 0 <= lo
        , Just hi <- knownUpperBound foldS
        , -- Check that we will actually be making the set smaller
          fromMaybe True ((hi <) <$> knownUpperBound elemS) ->
            Just (elemS <> typeSpec (NumSpecInterval (Just lo) (Just hi)), foldS)
      -- We can reduce the size of the foldS set by bumping the lower bound when
      -- there is a positive lower bound on the elemS, we can't generate things smaller
      -- than the lower bound on `elemS`.
      _
        | Just lo <- knownLowerBound elemS
        , 0 <= lo
        , not $ 0 `conformsToSpec` foldS
        , -- Check that we will actually be making the set smaller
          fromMaybe True ((lo >) <$> knownLowerBound foldS) ->
            Just (elemS, foldS <> typeSpec (NumSpecInterval (Just lo) Nothing))
      -- NOTE: this is far from sufficient, but it's good enough of an approximation
      -- to avoid the worst failures.
      _
        | Just lo <- knownLowerBound elemS
        , Just loS <- knownLowerBound foldS
        , Just hi <- knownUpperBound elemS
        , Just hiS <- knownUpperBound foldS
        , hi < loS
        , lo > hiS - lo ->
            Just
              (ErrorSpec ["Can't solve diophantine equation"], ErrorSpec ["Can't solve diophantine equation"])
      _ -> Nothing

genNumList ::
  forall a fn m.
  ( BaseUniverse fn
  , MonadGenError m
  , TypeSpec fn a ~ NumSpec a
  , HasSpec fn a
  , Arbitrary a
  , Integral a
  , Ord a
  , Random a
  , MaybeBounded a
  , Foldy fn a
  ) =>
  Specification fn a ->
  Specification fn a ->
  GenT m [a]
genNumList elemSIn foldSIn = do
  let extraElemConstraints
        | Just l <- knownLowerBound elemSIn
        , 0 <= l
        , Just u <- knownUpperBound foldSIn =
            leqSpec u
        | otherwise = TrueSpec
      elemSIn' = elemSIn <> extraElemConstraints
  normElemS <- normalize elemSIn'
  normFoldS <- normalize foldSIn
  let narrowedSpecs = narrowFoldSpecs (normElemS, normFoldS)
  explain
    [ "Can't generate list of ints with fold constraint"
    , "  elemSpec = " ++ show elemSIn
    , "  normElemSpec = " ++ show normElemS
    , "  foldSpec = " ++ show foldSIn
    ]
    $ gen narrowedSpecs 50 [] >>= pureGen . shuffle
  where
    normalize spec@SuspendedSpec {} = do
      sz <- sizeT
      spec' <- buildMemberSpec sz (100 :: Int) mempty spec
      normalize $ spec'
    normalize spec =
      pure $
        maybe mempty (geqSpec @fn) lowerBound
          <> maybe mempty (leqSpec @fn) upperBound
          <> spec

    buildMemberSpec _ 0 es _ = pure (MemberSpec $ Set.toList es)
    buildMemberSpec sz fuel es spec = do
      me <- scaleT (const sz) $ tryGen (genFromSpec spec)
      let sz'
            | sz > 100 = sz
            | isNothing me = 2 * sz + 1
            | otherwise = sz
      buildMemberSpec
        sz'
        (fuel - 1)
        (maybe es (flip Set.insert es) me)
        (spec <> maybe mempty notEqualSpec me)

    gen ::
      forall m'. MonadGenError m' => (Specification fn a, Specification fn a) -> Int -> [a] -> GenT m' [a]
    gen (elemS, foldS) fuel lst
      | fuel <= 0
      , not $ 0 `conformsToSpec` foldS =
          genError
            [ "Ran out of fuel in genNumList"
            , "  elemSpec =" ++ show elemSIn
            , "  foldSpec = " ++ show foldSIn
            , "  lst = " ++ show (reverse lst)
            ]
      | ErrorSpec err <- foldS = genError err
      | ErrorSpec {} <- elemS = pure lst -- At this point we know that foldS admits 0 (also this should be redundant)
      | 0 `conformsToSpec` foldS = oneofT [pure lst, nonemptyList @GE] -- TODO: distribution
      | otherwise = nonemptyList
      where
        isUnsat (elemS, foldS) = isEmptyNumSpec foldS || not (0 `conformsToSpec` foldS) && isEmptyNumSpec elemS
        nonemptyList :: forall m''. MonadGenError m'' => GenT m'' [a]
        nonemptyList = do
          (x, specs') <-
            explain
              [ "Generating an element:"
              , "  elemS = " ++ show elemS
              , "  foldS = " ++ show foldS
              , "  fuel  = " ++ show fuel
              , "  lst   = " ++ show (reverse lst)
              ]
              $ do
                sz <- sizeT
                x <- genFromSpec elemS
                let foldS' = propagateSpecFun (theAddFn @fn) (HOLE :? Value x :> Nil) foldS
                    specs' = narrowByFuelAndSize (fromIntegral $ fuel - 1) sz (elemS, foldS')
                pure (x, specs')
                `suchThatT` not
                . isUnsat
                . snd
          gen specs' (fuel - 1) (x : lst)

instance BaseUniverse fn => Foldy fn Int where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Integer where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Int8 where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Int16 where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Int32 where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Int64 where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Word8 where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Word16 where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Word32 where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Word64 where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

instance BaseUniverse fn => Foldy fn Natural where
  genList = genNumList
  theAddFn = injectFn (Add @fn)
  theZero = 0

genFromFold ::
  forall m fn a b.
  ( MonadGenError m
  , Foldy fn b
  , HasSpec fn a
  ) =>
  [a] ->
  Specification fn Integer ->
  Specification fn a ->
  fn '[a] b ->
  Specification fn b ->
  GenT m [a]
genFromFold must size elemS fn foldS = do
  let elemS' = mapSpec fn elemS
      mustVal = adds @fn (map (sem fn) must)
      foldS' = propagateSpecFun (theAddFn @fn) (HOLE :? Value mustVal :> Nil) foldS
  results <-
    genList (simplifySpec elemS') (simplifySpec foldS')
      `suchThatT` (\xs -> sizeOf xs `conformsToSpec` size)
  explain
    [ "genInverse"
    , "  fn = " ++ show fn
    , "  results = " ++ show results
    , show $ "  elemS =" <+> pretty elemS
    ]
    $ mapM (genInverse fn elemS) results

------------------------------------------------------------------------
-- Instances of HasSpec
------------------------------------------------------------------------

-- () ---------------------------------------------------------------------

instance BaseUniverse fn => HasSpec fn () where
  type TypeSpec fn () = ()
  emptySpec = ()
  combineSpec _ _ = typeSpec ()
  _ `conformsTo` _ = True
  shrinkWithTypeSpec _ _ = []
  genFromTypeSpec _ = pure ()
  toPreds _ _ = TruePred
  cardinalTypeSpec _ = MemberSpec [1]
  cardinalTrueSpec = exactSizeSpec 1 -- there is exactly one, ()
  typeSpecOpt _ [] = TrueSpec
  typeSpecOpt _ (_ : _) = MemberSpec []

-- Bool -------------------------------------------------------------------

instance HasSimpleRep Bool
instance (BaseUniverse fn, HasSpec fn ()) => HasSpec fn Bool where
  shrinkWithTypeSpec _ = shrink

-- Sum --------------------------------------------------------------------

guardSumSpec :: (HasSpec fn a, HasSpec fn b) => SumSpec fn a b -> Specification fn (Sum a b)
guardSumSpec s@(SumSpec sa sb)
  | isErrorLike sa, isErrorLike sb = ErrorSpec ["empty SumSpec"]
  | otherwise = typeSpec s

data SumSpec fn a b = SumSpec (Specification fn a) (Specification fn b)

instance (HasSpec fn a, HasSpec fn b) => Semigroup (SumSpec fn a b) where
  SumSpec sa sb <> SumSpec sa' sb' = SumSpec (sa <> sa') (sb <> sb')

instance (HasSpec fn a, HasSpec fn b) => Monoid (SumSpec fn a b) where
  mempty = SumSpec mempty mempty

instance (HasSpec fn a, HasSpec fn b) => HasSpec fn (Sum a b) where
  type TypeSpec fn (Sum a b) = SumSpec fn a b

  type Prerequisites fn (Sum a b) = (HasSpec fn a, HasSpec fn b)

  emptySpec = mempty

  combineSpec s s' = guardSumSpec $ s <> s'

  conformsTo (SumLeft a) (SumSpec sa _) = conformsToSpec a sa
  conformsTo (SumRight b) (SumSpec _ sb) = conformsToSpec b sb

  genFromTypeSpec (SumSpec sa sb)
    | emptyA, emptyB = genError ["genFromTypeSpec @SumSpec: empty"]
    | emptyA = SumRight <$> genFromSpec sb
    | emptyB = SumLeft <$> genFromSpec sa
    | otherwise =
        oneofT
          [ SumLeft <$> genFromSpec sa
          , SumRight <$> genFromSpec sb
          ]
    where
      emptyA = isErrorLike sa
      emptyB = isErrorLike sb

  shrinkWithTypeSpec (SumSpec sa _) (SumLeft a) = SumLeft <$> shrinkWithSpec sa a
  shrinkWithTypeSpec (SumSpec _ sb) (SumRight b) = SumRight <$> shrinkWithSpec sb b

  toPreds ct (SumSpec sa sb) =
    Case
      ct
      ( (bind $ \a -> satisfies a sa)
          :> (bind $ \b -> satisfies b sb)
          :> Nil
      )

  cardinalTypeSpec (SumSpec leftspec rightspec) = addSpecInt (cardinality leftspec) (cardinality rightspec)

deriving instance (HasSpec fn a, HasSpec fn b) => Show (SumSpec fn a b)

-- Sets -------------------------------------------------------------------

data SetSpec fn a = SetSpec (Set a) (Specification fn a) (Specification fn Integer)

instance (Ord a, HasSpec fn a) => Semigroup (SetSpec fn a) where
  SetSpec must es size <> SetSpec must' es' size' =
    SetSpec (must <> must') (es <> es') (size <> size')

instance (Ord a, HasSpec fn a) => Monoid (SetSpec fn a) where
  mempty = SetSpec mempty mempty TrueSpec

instance (Ord a, HasSpec fn a) => HasSpec fn (Set a) where
  type TypeSpec fn (Set a) = SetSpec fn a

  type Prerequisites fn (Set a) = HasSpec fn a

  emptySpec = mempty

  -- TODO: we need to check conformsTo for musts and elem specs
  combineSpec s s' = typeSpec $ s <> s'

  conformsTo s (SetSpec must es size) =
    and
      [ sizeOf s `conformsToSpec` size
      , must `Set.isSubsetOf` s
      , all (`conformsToSpec` es) s
      ]

  genFromTypeSpec (SetSpec must e TrueSpec) = (must <>) . Set.fromList <$> listOfT (genFromSpec e)
  genFromTypeSpec (SetSpec must elemS szSpec) = do
    n <-
      explain ["Choose a possible size Bounds for the Sets to be generated"] $
        genFromSizeSpec (szSpec <> geqSpec @fn (sizeOf must) <> maxSpec (cardinality elemS))
    go (n - sizeOf must) must
    where
      go 0 s = pure s
      go n s = do
        e <-
          explain ["generate set member"] $
            withMode Strict $
              genFromSpec elemS `suchThatT` (`Set.notMember` s)
        go (n - 1) (Set.insert e s)

  shrinkWithTypeSpec (SetSpec _ es _) as = map Set.fromList $ shrinkList (shrinkWithSpec es) (Set.toList as)

  toPreds s (SetSpec m es size) =
    (Assert ["Subset of " ++ show m] $ subset_ (Lit m) s)
      <> forAll s (\e -> satisfies e es)
      <> satisfies (size_ s) size

instance Ord a => Forallable (Set a) a where
  fromForAllSpec (e :: Specification fn a)
    | Evidence <- prerequisites @fn @(Set a) = typeSpec $ SetSpec mempty e TrueSpec
  forAllToList = Set.toList

deriving instance HasSpec fn a => Show (SetSpec fn a)

instance BaseUniverse fn => Functions (SetFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn ctx spec = case fn of
    _
      | SuspendedSpec x p <- spec
      , ListCtx pre HOLE suf <- ctx ->
          constrained $ \x' ->
            let args =
                  appendList
                    (mapList (\(Value a) -> Lit a) pre)
                    (x' :> mapList (\(Value a) -> Lit a) suf)
             in Let (App (injectFn fn) args) (x :-> p)
    Singleton
      | NilCtx HOLE <- ctx ->
          let singletons = filter ((1 ==) . Set.size)
           in case spec of
                TypeSpec (SetSpec must es size) cant
                  | [a] <- Set.toList must
                  , a `conformsToSpec` es
                  , 1 `conformsToSpec` size
                  , Set.singleton a `notElem` cant ->
                      equalSpec a
                  | null must -> es <> notMemberSpec (Set.toList $ fold $ singletons cant)
                  | otherwise -> ErrorSpec ["propagateSpecFun Singleton with `must` of size > 1"]
                MemberSpec es -> MemberSpec (Set.toList $ fold $ singletons es)
    Union
      | Value s :! NilCtx HOLE <- ctx ->
          propagateSpecFun @(SetFn fn) @fn Union (HOLE :? Value s :> Nil) spec
      | HOLE :? Value (s :: Set a) :> Nil <- ctx
      , Evidence <- prerequisites @fn @(Set a) ->
          case spec of
            TypeSpec (SetSpec must es size) cant
              | not $ all (`conformsToSpec` es) s ->
                  ErrorSpec
                    [ "Elements in union argument does not conform to elem spec"
                    , "  spec: " ++ show es
                    , "  elems: " ++ show (filter (not . (`conformsToSpec` es)) (Set.toList s))
                    ]
              | not $ null cant -> ErrorSpec ["propagateSpecFun Union TypeSpec, not (null cant)"]
              | TrueSpec <- size -> typeSpec $ SetSpec (Set.difference must s) es TrueSpec
              | otherwise -> constrained $ \x ->
                  exists (\eval -> pure $ Set.intersection (eval x) s) $ \overlap ->
                    exists (\eval -> pure $ Set.difference (eval x) s) $ \disjoint ->
                      [ assert $ overlap `subset_` Lit s
                      , assert $ disjoint `disjoint_` Lit s
                      , satisfies (size_ disjoint + Lit (sizeOf s)) size
                      , assert $ x ==. overlap <> disjoint
                      ]
            -- TODO: shortcut more cases?
            --    Lower bound and |s| >= bound: mempty
            --    Upper bound and |s| == bound: X `subset` s
            MemberSpec [e] -> typeSpec (SetSpec (Set.difference e s) (MemberSpec $ Set.toList e) mempty)
            -- This risks blowing up too much, don't build sets of sets
            MemberSpec {} -> ErrorSpec ["propagateSpecFun Union MemberSpec"]
    Subset
      | HOLE :? Value (s :: Set a) :> Nil <- ctx
      , Evidence <- prerequisites @fn @(Set a) -> caseBoolSpec spec $ \case
          True ->
            if Set.null s
              then MemberSpec [Set.empty]
              else typeSpec $ SetSpec mempty (MemberSpec $ Set.toList s) mempty
          False -> constrained $ \set ->
            exists (\eval -> headGE $ Set.difference (eval set) s) $ \e ->
              [ set `DependsOn` e
              , assert $ not_ $ member_ e (Lit s)
              , assert $ member_ e set
              ]
      | Value (s :: Set a) :! NilCtx HOLE <- ctx
      , Evidence <- prerequisites @fn @(Set a) -> caseBoolSpec spec $ \case
          True -> typeSpec $ SetSpec s TrueSpec mempty
          False -> constrained $ \set ->
            exists (\eval -> headGE $ Set.difference (eval set) s) $ \e ->
              [ set `DependsOn` e
              , assert $ member_ e (Lit s)
              , assert $ not_ $ member_ e set
              ]
    Member
      | HOLE :? Value s :> Nil <- ctx -> caseBoolSpec spec $ \case
          True -> MemberSpec $ Set.toList s
          False -> notMemberSpec s
      | Value e :! NilCtx HOLE <- ctx -> caseBoolSpec spec $ \case
          True -> typeSpec $ SetSpec (Set.singleton e) mempty mempty
          False -> typeSpec $ SetSpec mempty (notEqualSpec e) mempty
    Elem
      | HOLE :? Value es :> Nil <- ctx -> caseBoolSpec spec $ \case
          True -> MemberSpec es
          False -> notMemberSpec es
      | Value e :! NilCtx HOLE <- ctx -> caseBoolSpec spec $ \case
          True -> typeSpec $ ListSpec Nothing [e] mempty mempty NoFold
          False -> typeSpec $ ListSpec Nothing mempty mempty (notEqualSpec e) NoFold
    Disjoint
      | HOLE :? Value (s :: Set a) :> Nil <- ctx ->
          propagateSpecFun @(SetFn fn) @fn Disjoint (Value s :! NilCtx HOLE) spec
      | Value (s :: Set a) :! NilCtx HOLE <- ctx
      , Evidence <- prerequisites @fn @(Set a) -> caseBoolSpec spec $ \case
          True -> typeSpec $ SetSpec mempty (notMemberSpec s) mempty
          False -> constrained $ \set ->
            exists (\eval -> headGE (Set.intersection (eval set) s)) $ \e ->
              [ set `DependsOn` e
              , assert $ member_ e (Lit s)
              , assert $ member_ e set
              ]

  rewriteRules Elem (_ :> Lit [] :> Nil) = Just $ Lit False
  rewriteRules Elem (t :> Lit [a] :> Nil) = Just $ t ==. lit a
  rewriteRules Member (t :> Lit s :> Nil)
    | null s = Just $ Lit False
    | [a] <- Set.toList s = Just $ t ==. lit a
  rewriteRules Union (x :> Lit s :> Nil) | null s = Just x
  rewriteRules Union (Lit s :> x :> Nil) | null s = Just x
  rewriteRules Subset (Lit s :> _ :> Nil) | null s = Just $ Lit True
  rewriteRules Subset (x :> Lit s :> Nil) | null s = Just $ x ==. mempty
  rewriteRules _ _ = Nothing

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    Singleton ->
      constrained $ \x ->
        unsafeExists $ \x' ->
          assert (x ==. singleton_ x') <> toPreds x' ts

-- List -------------------------------------------------------------------

data ListSpec fn a
  = ListSpec (Maybe Integer) [a] (Specification fn Integer) (Specification fn a) (FoldSpec fn a)

deriving instance Show (FoldSpec fn a)
deriving instance HasSpec fn a => Show (ListSpec fn a)

instance HasSpec fn a => HasSpec fn [a] where
  type TypeSpec fn [a] = ListSpec fn a
  emptySpec = ListSpec Nothing [] mempty mempty NoFold
  combineSpec (ListSpec msz must size elemS foldS) (ListSpec msz' must' size' elemS' foldS') = fromGESpec $ do
    let must'' = nub $ must <> must'
        elemS'' = elemS <> elemS'
        size'' = size <> size'
    unless (all (`conformsToSpec` elemS'') must'') $
      genError ["combineSpec ListSpec failed with <REASON>"]
    typeSpec . ListSpec (unionWithMaybe min msz msz') must'' size'' elemS''
      <$> combineFoldSpec foldS foldS'

  genFromTypeSpec (ListSpec msz must TrueSpec elemS NoFold) = do
    lst <- case msz of
      Nothing -> listOfT $ genFromSpec elemS
      Just szHint -> do
        sz <- genFromSizeSpec (leqSpec @fn szHint)
        listOfUntilLenT (genFromSpec elemS) (fromIntegral sz) (const True)
    pureGen $ shuffle (must ++ lst)
  genFromTypeSpec (ListSpec msz must szSpec elemS NoFold) = do
    sz0 <- genFromSizeSpec (szSpec <> geqSpec @fn (sizeOf must) <> maybe TrueSpec leqSpec msz)
    let sz = fromIntegral (sz0 - sizeOf must)
    lst <-
      listOfUntilLenT
        (genFromSpec elemS)
        sz
        ((`conformsToSpec` szSpec) . (+ sizeOf must) . fromIntegral)
    pureGen $ shuffle (must ++ lst)
  genFromTypeSpec (ListSpec msz must size elemS (FoldSpec f foldS)) = do
    genFromFold must (size <> maybe TrueSpec leqSpec msz) elemS f foldS

  shrinkWithTypeSpec (ListSpec _ _ _ es _) as =
    shrinkList (shrinkWithSpec es) as

  conformsTo xs (ListSpec _ must size elemS foldS) =
    sizeOf xs `conformsToSpec` size
      && all (`elem` xs) must
      && all (`conformsToSpec` elemS) xs
      && xs `conformsToFoldSpec` foldS

  toPreds x (ListSpec msz must size elemS foldS) =
    (forAll x $ \x' -> satisfies x' elemS)
      <> (forAll (lit must) $ \x' -> assert (elem_ x' x))
      <> toPredsFoldSpec x foldS
      <> satisfies (sizeOf_ x) size
      <> maybe TruePred (flip genHint x) msz

instance HasSpec fn a => HasGenHint fn [a] where
  type Hint [a] = Integer
  giveHint szHint = typeSpec $ ListSpec (Just szHint) [] mempty mempty NoFold

instance Forallable [a] a where
  fromForAllSpec es = typeSpec (ListSpec Nothing [] mempty es NoFold)
  forAllToList = id

-- Numbers ----------------------------------------------------------------

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

data NumSpec n = NumSpecInterval (Maybe n) (Maybe n)
  deriving (Ord, Eq)

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

emptyNumSpec :: Ord a => NumSpec a
emptyNumSpec = mempty

combineNumSpec ::
  (HasSpec fn n, Ord n, TypeSpec fn n ~ NumSpec n) => NumSpec n -> NumSpec n -> Specification fn n
combineNumSpec s s' = case s <> s' of
  s''@(NumSpecInterval (Just a) (Just b)) | a > b -> ErrorSpec [show s'']
  s'' -> typeSpec s''

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
      | l > u -> genError ["bad interval: " ++ show l ++ " " ++ show u]
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
  , OrdLike fn n
  ) =>
  Term fn n ->
  NumSpec n ->
  Pred fn
toPredsNumSpec v (NumSpecInterval ml mu) =
  fold $
    [assert $ Lit l <=. v | l <- maybeToList ml]
      ++ [assert $ v <=. Lit u | u <- maybeToList mu]

instance BaseUniverse fn => HasSpec fn Int where
  type TypeSpec fn Int = NumSpec Int
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec

instance BaseUniverse fn => HasSpec fn Integer where
  type TypeSpec fn Integer = NumSpec Integer
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalSizeSpec

instance BaseUniverse fn => HasSpec fn (Ratio Integer) where
  type TypeSpec fn (Ratio Integer) = NumSpec (Ratio Integer)
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec

instance BaseUniverse fn => HasSpec fn Natural where
  type TypeSpec fn Natural = NumSpec Natural
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec (NumSpecInterval (Just lo) (Just hi)) =
    if hi >= lo then MemberSpec [fromIntegral @Natural @Integer (hi - lo + 1)] else MemberSpec [0]
  cardinalTypeSpec (NumSpecInterval Nothing (Just hi)) =
    MemberSpec [fromIntegral @Natural @Integer hi + 1]
  cardinalTypeSpec _ = TrueSpec

instance BaseUniverse fn => HasSpec fn Word8 where
  type TypeSpec fn Word8 = NumSpec Word8
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalSizeSpec
  typeSpecOpt = notInNumSpec

instance BaseUniverse fn => HasSpec fn Word16 where
  type TypeSpec fn Word16 = NumSpec Word16
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalSizeSpec

instance BaseUniverse fn => HasSpec fn Word32 where
  type TypeSpec fn Word32 = NumSpec Word32
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalSizeSpec

instance BaseUniverse fn => HasSpec fn Word64 where
  type TypeSpec fn Word64 = NumSpec Word64
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec

instance BaseUniverse fn => HasSpec fn Int8 where
  type TypeSpec fn Int8 = NumSpec Int8
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalSizeSpec

instance BaseUniverse fn => HasSpec fn Int16 where
  type TypeSpec fn Int16 = NumSpec Int16
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalSizeSpec

instance BaseUniverse fn => HasSpec fn Int32 where
  type TypeSpec fn Int32 = NumSpec Int32
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalSizeSpec

instance BaseUniverse fn => HasSpec fn Int64 where
  type TypeSpec fn Int64 = NumSpec Int64
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalSizeSpec

instance BaseUniverse fn => HasSpec fn Float where
  type TypeSpec fn Float = NumSpec Float
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec

-----------------------------------------------------------------------------------
-- The Base Function universe defines operations on TypeSpec defined in the Base.
-- See module  Test.Cardano.Ledger.Constrained.V2.Conway for a Universe
-- that is an extension of the Base Universe.
-----------------------------------------------------------------------------------

type BaseFns =
  '[EqFn, SetFn, BoolFn, PairFn, IntFn, OrdFn, GenericsFn, ListFn, SumFn, MapFn, FunFn, SizeFn]
type BaseFn = Fix (OneofL BaseFns)

-- | A minimal Universe contains functions for a bunch of different things.
type BaseUniverse fn =
  ( Functions fn fn
  , Member (EqFn fn) fn
  , Member (SetFn fn) fn
  , Member (BoolFn fn) fn
  , Member (PairFn fn) fn
  , Member (IntFn fn) fn
  , Member (OrdFn fn) fn
  , Member (GenericsFn fn) fn
  , Member (ListFn fn) fn
  , Member (SumFn fn) fn
  , Member (MapFn fn) fn
  , Member (FunFn fn) fn
  , Member (SizeFn fn) fn
  )

-- Higher order functions -------------------------------------------------

idFn :: forall fn a. Member (FunFn fn) fn => fn '[a] a
idFn = injectFn $ Id @fn

composeFn ::
  ( Member (FunFn fn) fn
  , HasSpec fn b
  , Show (fn '[a] b)
  , Show (fn '[b] c)
  , Eq (fn '[a] b)
  , Eq (fn '[b] c)
  ) =>
  fn '[b] c ->
  fn '[a] b ->
  fn '[a] c
composeFn f g = injectFn $ Compose f g

data FunFn fn args res where
  Id :: FunFn fn '[a] a
  Compose ::
    ( Typeable b
    , HasSpec fn b
    , Show (fn '[a] b)
    , Show (fn '[b] c)
    , Eq (fn '[a] b)
    , Eq (fn '[b] c)
    ) =>
    fn '[b] c ->
    fn '[a] b ->
    FunFn fn '[a] c

deriving instance Show (FunFn fn args res)

instance Typeable fn => Eq (FunFn fn args res) where
  Compose (f :: fn '[b] c) f' == Compose (g :: fn '[b'] c') g'
    | Just Refl <- eqT @b @b' = f == g && f' == g'
  Compose {} == _ = False
  Id == Id = True
  Id == _ = False

instance FunctionLike fn => FunctionLike (FunFn fn) where
  sem = \case
    Id -> id
    Compose f g -> sem f . sem g

instance (BaseUniverse fn, Member (FunFn fn) fn) => Functions (FunFn fn) fn where
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn ctx spec = case fn of
    Id | NilCtx HOLE <- ctx -> spec
    Compose f g | NilCtx HOLE <- ctx -> propagateSpecFun g (NilCtx HOLE) $ propagateSpecFun f (NilCtx HOLE) spec

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    Id -> typeSpec ts
    Compose g h -> mapSpec g . mapSpec h $ typeSpec ts

  rewriteRules Id (x :> Nil) = Just x
  rewriteRules (Compose f g) (x :> Nil) = Just $ app f (app g x)

-- Ord functions ----------------------------------------------------------

lessOrEqualFn :: forall fn a. (Ord a, OrdLike fn a) => fn '[a, a] Bool
lessOrEqualFn = injectFn (LessOrEqual @_ @fn)

lessFn :: forall fn a. (Ord a, OrdLike fn a) => fn '[a, a] Bool
lessFn = injectFn (Less @_ @fn)

data OrdFn (fn :: [Type] -> Type -> Type) as b where
  LessOrEqual :: (Ord a, OrdLike fn a) => OrdFn fn '[a, a] Bool
  Less :: (Ord a, OrdLike fn a) => OrdFn fn '[a, a] Bool

class HasSpec fn a => OrdLike fn a where
  leqSpec :: a -> Specification fn a
  default leqSpec ::
    ( TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    , OrdLike fn (SimpleRep a)
    ) =>
    a ->
    Specification fn a
  leqSpec = fromSimpleRepSpec . leqSpec @fn . toSimpleRep

  ltSpec :: a -> Specification fn a
  default ltSpec ::
    ( TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    , OrdLike fn (SimpleRep a)
    ) =>
    a ->
    Specification fn a
  ltSpec = fromSimpleRepSpec . ltSpec @fn . toSimpleRep

  geqSpec :: a -> Specification fn a
  default geqSpec ::
    ( TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    , OrdLike fn (SimpleRep a)
    ) =>
    a ->
    Specification fn a
  geqSpec = fromSimpleRepSpec . geqSpec @fn . toSimpleRep

  gtSpec :: a -> Specification fn a
  default gtSpec ::
    ( TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    , OrdLike fn (SimpleRep a)
    ) =>
    a ->
    Specification fn a
  gtSpec = fromSimpleRepSpec . gtSpec @fn . toSimpleRep

instance {-# OVERLAPPABLE #-} (HasSpec fn a, MaybeBounded a, Num a, TypeSpec fn a ~ NumSpec a) => OrdLike fn a where
  leqSpec l = typeSpec $ NumSpecInterval Nothing (Just l)
  ltSpec l
    | Just b <- lowerBound
    , l == b =
        ErrorSpec ["ltSpec @" ++ show (typeOf l) ++ " " ++ show l]
    | otherwise = typeSpec $ NumSpecInterval Nothing (Just (l - 1))
  geqSpec l = typeSpec $ NumSpecInterval (Just l) Nothing
  gtSpec l
    | Just b <- upperBound
    , l == b =
        ErrorSpec ["gtSpec @" ++ show (typeOf l) ++ " " ++ show l]
    | otherwise = typeSpec $ NumSpecInterval (Just (l + 1)) Nothing

deriving instance Eq (OrdFn fn as b)
deriving instance Show (OrdFn fn as b)

instance FunctionLike (OrdFn fn) where
  sem LessOrEqual = (<=)
  sem Less = (<)

instance BaseUniverse fn => Functions (OrdFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn (ListCtx pre HOLE suf) (SuspendedSpec x p) =
    constrained $ \x' ->
      let args = appendList (mapList (\(Value a) -> Lit a) pre) (x' :> mapList (\(Value a) -> Lit a) suf)
       in Let (App (injectFn fn) args) (x :-> p)
  propagateSpecFun LessOrEqual ctx spec
    | HOLE :? Value l :> Nil <- ctx = caseBoolSpec spec $ \case
        True -> leqSpec @fn l
        False -> gtSpec @fn l
    | Value l :! NilCtx HOLE <- ctx = caseBoolSpec spec $ \case
        True -> geqSpec @fn l
        False -> ltSpec @fn l
  propagateSpecFun Less ctx spec
    | HOLE :? Value l :> Nil <- ctx = caseBoolSpec spec $ \case
        True -> ltSpec @fn l
        False -> geqSpec @fn l
    | Value l :! NilCtx HOLE <- ctx = caseBoolSpec spec $ \case
        True -> gtSpec @fn l
        False -> leqSpec @fn l

  mapTypeSpec = error "No cases"

-- List functions ---------------------------------------------------------

data ListFn fn args res where
  FoldMap ::
    ( HasSpec fn a
    , Foldy fn b
    , Show (fn '[a] b)
    , Eq (fn '[a] b)
    ) =>
    fn '[a] b ->
    ListFn fn '[[a]] b

{- TODO add these?
  AppendFn :: ListFn fn '[[a],[a]] [a]
  ConsFn :: ListFn fn '[a.[a]] [a]
-}

deriving instance Show (ListFn fn args res)

instance Typeable fn => Eq (ListFn fn args res) where
  FoldMap f == FoldMap g = cast f == Just g

instance FunctionLike fn => FunctionLike (ListFn fn) where
  sem = \case
    FoldMap f -> adds @fn . map (sem f)

instance BaseUniverse fn => Functions (ListFn fn) fn where
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn ctx spec = case fn of
    _
      | SuspendedSpec x p <- spec
      , ListCtx pre HOLE suf <- ctx ->
          constrained $ \v' ->
            let args = appendList (mapList (\(Value a) -> Lit a) pre) (v' :> mapList (\(Value a) -> Lit a) suf)
             in Let (App (injectFn fn) args) (x :-> p)
    FoldMap f | NilCtx HOLE <- ctx -> typeSpec (ListSpec Nothing [] TrueSpec TrueSpec $ FoldSpec f spec)

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    FoldMap g ->
      constrained $ \x ->
        unsafeExists $ \x' ->
          assert (x ==. app (foldMapFn g) x') <> toPreds x' ts

foldMapFn ::
  forall fn a b.
  ( HasSpec fn a
  , Foldy fn b
  , Show (fn '[a] b)
  , Eq (fn '[a] b)
  ) =>
  fn '[a] b ->
  fn '[[a]] b
foldMapFn f = injectFn $ FoldMap @fn f

-- Number functions -------------------------------------------------------

addFn :: forall fn a. NumLike fn a => fn '[a, a] a
addFn = injectFn (Add @fn)

negateFn :: forall fn a. NumLike fn a => fn '[a] a
negateFn = injectFn (Negate @fn)

data IntFn (fn :: [Type] -> Type -> Type) as b where
  Add :: NumLike fn a => IntFn fn '[a, a] a
  Negate :: NumLike fn a => IntFn fn '[a] a

deriving instance Eq (IntFn fn as b)
deriving instance Show (IntFn fn as b)

instance FunctionLike (IntFn fn) where
  sem Add = (+)
  sem Negate = negate

class (Num a, HasSpec fn a) => NumLike fn a where
  subtractSpec :: a -> TypeSpec fn a -> Specification fn a
  default subtractSpec ::
    ( TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    , NumLike fn (SimpleRep a)
    ) =>
    a ->
    TypeSpec fn a ->
    Specification fn a
  subtractSpec a ts = fromSimpleRepSpec $ subtractSpec @fn (toSimpleRep a) ts

  negateSpec :: TypeSpec fn a -> Specification fn a
  default negateSpec ::
    ( TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
    , HasSimpleRep a
    , NumLike fn (SimpleRep a)
    ) =>
    TypeSpec fn a ->
    Specification fn a
  negateSpec = fromSimpleRepSpec . negateSpec @fn @(SimpleRep a)

  safeSubtract :: a -> a -> Maybe a
  default safeSubtract ::
    (HasSimpleRep a, NumLike fn (SimpleRep a)) =>
    a ->
    a ->
    Maybe a
  safeSubtract a b = fromSimpleRep <$> safeSubtract @fn @(SimpleRep a) (toSimpleRep a) (toSimpleRep b)

instance NumLike fn a => Num (Term fn a) where
  (+) = app addFn
  negate = app negateFn
  fromInteger = Lit . fromInteger
  (*) = error "(*) not implemented for Term Fn Int"
  abs = error "abs not implemented for Term Fn Int"
  signum = error "signum not implemented for Term Fn Int"

instance {-# OVERLAPPABLE #-} (HasSpec fn a, Ord a, Num a, TypeSpec fn a ~ NumSpec a, MaybeBounded a) => NumLike fn a where
  subtractSpec a ts@(NumSpecInterval ml mu)
    | Just u <- mu
    , a > 0
    , Nothing <- safeSubtract @fn a u =
        ErrorSpec
          [ "Underflow in subtractSpec (" ++ show (typeOf a) ++ "):"
          , "  a = " ++ show a
          , "  ts = " ++ show ts
          ]
    | Just l <- ml
    , a < 0
    , Nothing <- safeSubtract @fn a l =
        ErrorSpec
          [ "Overflow in subtractSpec (" ++ show (typeOf a) ++ "):"
          , "  a = " ++ show a
          , "  ts = " ++ show ts
          ]
    | otherwise = typeSpec $ NumSpecInterval (safeSub a <$> ml) (safeSub a <$> mu)
    where
      safeSub :: a -> a -> a
      safeSub a x
        | Just r <- safeSubtract @fn a x = r
        | a < 0 = fromJust upperBound
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

instance BaseUniverse fn => Functions (IntFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn (ListCtx pre HOLE suf) (SuspendedSpec x p) =
    constrained $ \x' ->
      let args = appendList (mapList (\(Value a) -> Lit a) pre) (x' :> mapList (\(Value a) -> Lit a) suf)
       in Let (App (injectFn fn) args) (x :-> p)
  propagateSpecFun Add ctx spec
    | Value i :! NilCtx HOLE <- ctx = propagateSpecFun @(IntFn fn) @fn Add (HOLE :? Value i :> Nil) spec
    | HOLE :? Value i :> Nil <- ctx =
        case spec of
          TypeSpec ts cant ->
            subtractSpec @fn i ts <> notMemberSpec (catMaybes $ map (safeSubtract @fn i) cant)
          MemberSpec es -> MemberSpec $ catMaybes (map (safeSubtract @fn i) es)
  propagateSpecFun Negate (NilCtx HOLE) spec = case spec of
    TypeSpec ts (cant :: OrdSet a) ->
      negateSpec @fn @a ts <> notMemberSpec (map negate cant)
    MemberSpec es -> MemberSpec $ map negate es

  mapTypeSpec Negate (ts :: TypeSpec fn a) =
    negateSpec @fn @a ts

------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------

-- Functions on terms -----------------------------------------------------

toGeneric_ ::
  forall a fn.
  ( HasSpec fn a
  , HasSpec fn (SimpleRep a)
  , HasSimpleRep a
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  ) =>
  Term fn a ->
  Term fn (SimpleRep a)
toGeneric_ = app toGenericFn

fromGeneric_ ::
  forall a fn.
  ( HasSpec fn a
  , HasSpec fn (SimpleRep a)
  , HasSimpleRep a
  , TypeSpec fn a ~ TypeSpec fn (SimpleRep a)
  ) =>
  Term fn (SimpleRep a) ->
  Term fn a
fromGeneric_ = app fromGenericFn

not_ ::
  BaseUniverse fn =>
  Term fn Bool ->
  Term fn Bool
not_ = app notFn

infixr 2 ||.
(||.) ::
  BaseUniverse fn =>
  Term fn Bool ->
  Term fn Bool ->
  Term fn Bool
(||.) = app orFn

elem_ ::
  forall a fn.
  HasSpec fn a =>
  Term fn a ->
  Term fn [a] ->
  Term fn Bool
elem_ = app elemFn

member_ ::
  forall a fn.
  ( HasSpec fn a
  , Ord a
  ) =>
  Term fn a ->
  Term fn (Set a) ->
  Term fn Bool
member_ = app memberFn

subset_ ::
  ( HasSpec fn a
  , Ord a
  ) =>
  Term fn (Set a) ->
  Term fn (Set a) ->
  Term fn Bool
subset_ = app subsetFn

disjoint_ ::
  ( HasSpec fn a
  , Ord a
  ) =>
  Term fn (Set a) ->
  Term fn (Set a) ->
  Term fn Bool
disjoint_ = app disjointFn

singleton_ ::
  ( HasSpec fn a
  , Ord a
  ) =>
  Term fn a ->
  Term fn (Set a)
singleton_ = app singletonFn

union_ ::
  forall a fn.
  ( HasSpec fn a
  , Ord a
  ) =>
  Term fn (Set a) ->
  Term fn (Set a) ->
  Term fn (Set a)
union_ = app unionFn

sizeOf_ ::
  forall a fn.
  (HasSpec fn a, Sized a) =>
  Term fn a ->
  Term fn Integer
sizeOf_ = app sizeOfFn

-- | special instance of sizeOf (for Sets) for backward compatibility
size_ ::
  forall a fn.
  (HasSpec fn (Set a), Ord a) =>
  Term fn (Set a) ->
  Term fn Integer
size_ = app sizeOfFn

-- | special instance of sizeOf (for Lists) for backward compatibility
length_ ::
  forall a fn.
  HasSpec fn [a] =>
  Term fn [a] ->
  Term fn Integer
length_ = app sizeOfFn

null_ :: (HasSpec fn a, Sized a) => Term fn a -> Term fn Bool
null_ xs = sizeOf_ xs ==. 0

-- #####

infix 4 <=., <., ==., /=.

(<=.) ::
  ( Ord a
  , OrdLike fn a
  ) =>
  Term fn a ->
  Term fn a ->
  Term fn Bool
(<=.) = app lessOrEqualFn

(<.) ::
  ( Ord a
  , OrdLike fn a
  ) =>
  Term fn a ->
  Term fn a ->
  Term fn Bool
(<.) = app lessFn

(==.) ::
  HasSpec fn a =>
  Term fn a ->
  Term fn a ->
  Term fn Bool
(==.) = app equalFn

(/=.) ::
  HasSpec fn a =>
  Term fn a ->
  Term fn a ->
  Term fn Bool
a /=. b = not_ (a ==. b)

sum_ ::
  ( BaseUniverse fn
  , Member (FunFn fn) fn
  , Foldy fn a
  ) =>
  Term fn [a] ->
  Term fn a
sum_ = foldMap_ id

foldMap_ ::
  forall fn a b.
  ( BaseUniverse fn
  , Foldy fn b
  , HasSpec fn a
  ) =>
  (Term fn a -> Term fn b) ->
  Term fn [a] ->
  Term fn b
foldMap_ f = app $ foldMapFn $ toFn $ f (V v)
  where
    v = Var (-1) :: Var a
    -- Turn `f (V v) = fn (gn (hn v))` into `composeFn fn (composeFn gn hn)`
    toFn :: forall b. HasCallStack => Term fn b -> fn '[a] b
    toFn (App fn (V v' :> Nil)) | Just Refl <- eqVar v v' = fn
    toFn (App fn (t :> Nil)) = injectFn $ Compose fn (toFn t)
    toFn (V v') | Just Refl <- eqVar v v' = idFn
    toFn _ = error "foldMap_ has not been given a function of the form \\ x -> f (g ... (h x))"

-- Language constructs ----------------------------------------------------

constrained ::
  forall a fn p.
  (IsPred p fn, HasSpec fn a) =>
  (Term fn a -> p) ->
  Specification fn a
constrained body =
  let x :-> p = bind body
   in SuspendedSpec x p

assertExplain ::
  (BaseUniverse fn, IsPred p fn) =>
  [String] ->
  p ->
  Pred fn
assertExplain = toPredExplain

assert ::
  (BaseUniverse fn, IsPred p fn) =>
  p ->
  Pred fn
assert = toPred

forAll ::
  ( Forallable t a
  , HasSpec fn t
  , HasSpec fn a
  , IsPred p fn
  ) =>
  Term fn t ->
  (Term fn a -> p) ->
  Pred fn
forAll tm body = ForAll tm (bind body)

exists ::
  forall a p fn.
  (HasSpec fn a, IsPred p fn) =>
  ((forall b. Term fn b -> b) -> GE a) ->
  (Term fn a -> p) ->
  Pred fn
exists sem k =
  Exists sem $ bind k

unsafeExists ::
  forall a p fn.
  (HasSpec fn a, IsPred p fn) =>
  (Term fn a -> p) ->
  Pred fn
unsafeExists = exists (\_ -> fatalError ["unsafeExists"])

letBind ::
  ( HasSpec fn a
  , IsPred p fn
  ) =>
  Term fn a ->
  (Term fn a -> p) ->
  Pred fn
letBind tm@V {} body = toPred $ body tm
letBind tm body = Let tm (bind body)

reify ::
  ( HasSpec fn a
  , HasSpec fn b
  , IsPred p fn
  ) =>
  Term fn a ->
  (a -> b) ->
  (Term fn b -> p) ->
  Pred fn
reify t f body =
  exists (\eval -> pure $ f (eval t)) $ \x ->
    [ reifies x t f
    , toPred $ body x
    ]

-- | Add QuickCheck monitoring (e.g. 'Test.QuickCheck.collect' or 'Test.QuickCheck.counterexample')
--   to a predicate. To use the monitoring in a property call 'monitorSpec' on the 'Specification'
--   containing the monitoring and a value generated from the specification.
monitor :: ((forall a. Term fn a -> a) -> Property -> Property) -> Pred fn
monitor = Monitor

assertReified :: HasSpec fn a => Term fn a -> (a -> Bool) -> Pred fn
-- Note, it is necessary to introduce the extra variable from the `exists` here
-- to make things like `assertRealMultiple` work, if you don't have it then the
-- `reifies` isn't a defining constraint for anything any more.
assertReified t f =
  reify t f assert

reifies :: (HasSpec fn a, HasSpec fn b) => Term fn b -> Term fn a -> (a -> b) -> Pred fn
reifies = Reifies

dependsOn :: (HasSpec fn a, HasSpec fn b) => Term fn a -> Term fn b -> Pred fn
dependsOn = DependsOn

lit :: Show a => a -> Term fn a
lit = Lit

ifElse :: (BaseUniverse fn, IsPred p fn, IsPred q fn) => Term fn Bool -> p -> q -> Pred fn
ifElse (Lit True) (toPred -> p) _ = p
ifElse (Lit False) _ (toPred -> q) = q
ifElse b (toPred -> p) (toPred -> q)
  | FalsePred _ <- p = assert (not_ b) <> q
  | FalsePred _ <- q = assert b <> p
  | otherwise = IfElse b p q

genHint :: forall fn t. HasGenHint fn t => Hint t -> Term fn t -> Pred fn
genHint = GenHint

-- Internals --------------------------------------------------------------

app ::
  ( HasSpec fn b
  , Typeable as
  , TypeList as
  , All (HasSpec fn) as
  ) =>
  fn as b ->
  FunTy (MapList (Term fn) as) (Term fn b)
app fn = curryList (App fn)

bind :: (HasSpec fn a, IsPred p fn) => (Term fn a -> p) -> Binder fn a
bind body = x :-> p
  where
    p = toPred $ body (V x)
    x = Var (nextVar p)

    nextVar p = 1 + bound p

    boundBinder :: Binder fn a -> Int
    boundBinder (x :-> p) = max (nameOf x) (bound p)

    bound (Subst x _ p) = max (nameOf x) (bound p)
    bound (Block ps) = maximum $ map bound ps
    bound (Exists _ b) = boundBinder b
    bound (Let _ b) = boundBinder b
    bound (ForAll _ b) = boundBinder b
    bound (Case _ cs) = getMax $ foldMapList (Max . boundBinder) cs
    bound (IfElse _ p q) = max (bound p) (bound q)
    bound Reifies {} = -1
    bound GenHint {} = -1
    bound Assert {} = -1
    bound DependsOn {} = -1
    bound TruePred = -1
    bound FalsePred {} = -1
    bound Monitor {} = -1

mkCase :: HasSpec fn (SumOver as) => Term fn (SumOver as) -> List (Binder fn) as -> Pred fn
mkCase tm cs
  | x :-> p :> Nil <- cs = Subst x tm p
  | getAll $ foldMapList isTrueBinder cs = TruePred
  | getAll $ foldMapList isFalseBinder cs = FalsePred ["mkCase on all False"]
  | Lit a <- tm = runCaseOn a cs (\x val p -> substPred (singletonEnv x val) p)
  | otherwise = Case tm cs
  where
    isTrueBinder (_ :-> TruePred) = Semigroup.All True
    isTrueBinder _ = Semigroup.All False

    isFalseBinder (_ :-> FalsePred {}) = Semigroup.All True
    isFalseBinder _ = Semigroup.All False

-- Liberal syntax for what is a predicate ---------------------------------

type IsPred p fn = (PredLike p, UnivConstr p fn)

class PredLike p where
  type UnivConstr p (fn :: [Type] -> Type -> Type) :: Constraint
  toPredExplain :: (BaseUniverse fn, UnivConstr p fn) => [String] -> p -> Pred fn

toPred :: (BaseUniverse fn, IsPred p fn) => p -> Pred fn
toPred = toPredExplain []

instance PredLike (Pred fn) where
  type UnivConstr (Pred fn) fn' = fn ~ fn'
  toPredExplain _ = id

instance PredLike p => PredLike [p] where
  type UnivConstr [p] fn = UnivConstr p fn
  toPredExplain _ = fold . map toPred

instance PredLike Bool where
  type UnivConstr Bool fn = ()
  toPredExplain _ True = TruePred
  toPredExplain es False = FalsePred es

instance BaseUniverse fn => PredLike (Term fn Bool) where
  type UnivConstr (Term fn Bool) fn' = fn ~ fn'
  toPredExplain es (Lit b) = toPredExplain es b
  toPredExplain es tm = Assert es tm

------------------------------------------------------------------------
-- Pretty printing
------------------------------------------------------------------------

data WithPrec a = WithPrec Int a

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

prettyPrec :: Pretty (WithPrec a) => Int -> a -> Doc ann
prettyPrec p = pretty . WithPrec p

ppList ::
  forall fn f as ann.
  All (HasSpec fn) as =>
  (forall a. HasSpec fn a => f a -> Doc ann) ->
  List f as ->
  [Doc ann]
ppList _ Nil = []
ppList pp (a :> as) = pp a : ppList @fn pp as

ppList_ :: forall f as ann. (forall a. f a -> Doc ann) -> List f as -> [Doc ann]
ppList_ _ Nil = []
ppList_ pp (a :> as) = pp a : ppList_ pp as

instance HasSpec fn a => Pretty (WithPrec (Term fn a)) where
  pretty (WithPrec p t) = case t of
    Lit n -> fromString $ showsPrec p n ""
    V x -> viaShow x
    App f Nil -> viaShow f
    App f as -> parensIf (p > 10) $ viaShow f <+> align (fillSep (ppList @fn (prettyPrec 11) as))

instance HasSpec fn a => Pretty (Term fn a) where
  pretty = prettyPrec 0

vsep' :: [Doc ann] -> Doc ann
vsep' = align . mconcat . punctuate hardline

(/>) :: Doc ann -> Doc ann -> Doc ann
h /> cont = hang 2 $ sep [h, align cont]

infixl 5 />

instance Pretty (Pred fn) where
  pretty = \case
    Exists _ (x :-> p) -> align $ sep ["exists" <+> viaShow x <+> "in", pretty p]
    Let t (x :-> p) -> align $ sep ["let" <+> viaShow x <+> "=" /> pretty t <+> "in", pretty p]
    Block ps -> braces $ vsep' $ map pretty ps
    Assert _err t -> "assert $" <+> pretty t
    Reifies t' t _ -> "reifies" <+> pretty (WithPrec 11 t') <+> pretty (WithPrec 11 t)
    DependsOn a b -> pretty a <+> "<-" /> pretty b
    ForAll t (x :-> p) -> "forall" <+> viaShow x <+> "in" <+> pretty t <+> "$" /> pretty p
    Case t bs -> "case" <+> pretty t <+> "of" /> vsep' (ppList_ pretty bs)
    IfElse b p q ->
      vsep
        [ "if" <+> pretty b <+> "then"
        , indent 2 (pretty p)
        , "else"
        , indent 2 (pretty q)
        ]
    Subst x t p -> "[" <> pretty t <> "/" <> viaShow x <> "]" <> pretty p
    GenHint h t -> "genHint" <+> fromString (showsPrec 11 h "") <+> "$" <+> pretty t
    TruePred -> "True"
    FalsePred {} -> "False"
    Monitor {} -> "monitor"

instance Pretty (Binder fn a) where
  pretty (x :-> p) = viaShow x <+> "->" <+> pretty p

instance HasSpec fn a => Show (Term fn a) where
  showsPrec p t = shows $ pretty (WithPrec p t)

instance Show (Pred fn) where
  show = show . pretty

instance HasSpec fn a => Pretty (WithPrec (Specification fn a)) where
  pretty (WithPrec d s) = case s of
    ErrorSpec es -> "ErrorSpec" /> vsep' (map fromString es)
    TrueSpec -> "TrueSpec"
    MemberSpec xs -> parensIf (d > 10) $ "MemberSpec" <+> viaShow xs
    SuspendedSpec x p -> parensIf (d > 10) $ "constrained $ \\" <+> viaShow x <+> "->" /> pretty p
    -- TODO: require pretty for `TypeSpec` to make this much nicer
    TypeSpec ts cant ->
      parensIf (d > 10) $
        "TypeSpec"
          <+> fromString (showsPrec 11 ts "")
          <+> viaShow cant

instance HasSpec fn a => Pretty (Specification fn a) where
  pretty = pretty . WithPrec 0

instance HasSpec fn a => Show (Specification fn a) where
  showsPrec d = shows . pretty . WithPrec d

-- ======================================================================
-- Size and its 'generic' operations over Sized types.
-- ======================================================================

-- type Size = Integer

-- | Because Sizes should always be >= 0, We provide this alternate generator
--   that can be used to replace (genFromSpec @Integer), to ensure this important property
genFromSizeSpec :: (BaseUniverse fn, MonadGenError m) => Specification fn Integer -> GenT m Integer
genFromSizeSpec integerSpec = genFromSpec (integerSpec <> geqSpec 0)

data SizeFn (fn :: [Type] -> Type -> Type) as b where
  SizeOf :: forall fn a. (Sized a, HasSpec fn a) => SizeFn fn '[a] Integer

deriving instance Eq (SizeFn fn as b)
deriving instance Show (SizeFn fn as b)

instance FunctionLike (SizeFn fn) where
  sem SizeOf = sizeOf -- From the Sized class

sizeOfFn :: forall fn a. (HasSpec fn a, Member (SizeFn fn) fn, Sized a) => fn '[a] Integer
sizeOfFn = injectFn $ SizeOf @fn @a

-- Operations on Size (specified in SizeFn) by the Functions instance

instance (BaseUniverse fn, HasSpec fn Integer) => Functions (SizeFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn (ListCtx pre HOLE suf) (SuspendedSpec x p) =
    constrained $ \x' ->
      let args = appendList (mapList (\(Value a) -> Lit a) pre) (x' :> mapList (\(Value a) -> Lit a) suf)
       in Let (App (injectFn fn) args) (x :-> p)
  propagateSpecFun SizeOf (NilCtx HOLE) (TypeSpec x _) = liftSizeSpec x
  propagateSpecFun SizeOf (NilCtx HOLE) (MemberSpec xs) = liftMemberSpec xs

  mapTypeSpec f ts = mapTypeSpecSize f ts

mapTypeSpecSize :: forall fn a b f. f ~ SizeFn fn => f '[a] b -> TypeSpec fn a -> Specification fn b
mapTypeSpecSize f ts = case f of
  SizeOf ->
    constrained $ \x ->
      unsafeExists $ \x' ->
        assert (x ==. sizeOf_ x') <> toPreds @fn @a x' ts

-- ======================================
type SizeSpec = NumSpec Integer

-- Operations that build SizeSpec from Integer, but raise an error if called on a negative numbers
exactSize :: Integer -> SizeSpec
exactSize a
  | a >= 0 = rangeSize a a
  | otherwise = error ("Negative Int in call to exactSize: " ++ show a)

rangeSize :: Integer -> Integer -> SizeSpec
rangeSize a b | a < 0 || b < 0 = error ("Negative Int in call to rangeSize: " ++ show a ++ " " ++ show b)
rangeSize a b = NumSpecInterval (Just a) (Just b)

atLeastSize :: Integer -> SizeSpec
atLeastSize a
  | a < 0 = error ("Negative Int in call to atLeastSize: " ++ show a)
  | otherwise = NumSpecInterval (Just a) Nothing

atMostSize :: Integer -> SizeSpec
atMostSize a
  | a < 0 = error ("Negative Int in call to atMostSize: " ++ show a)
  | otherwise = NumSpecInterval Nothing (Just a)

exactSizeSpec :: BaseUniverse fn => Integer -> Specification fn Integer
exactSizeSpec a = MemberSpec [a]

-- | The widest interval whose largest element is admitted by the original spec
maxSpec :: BaseUniverse fn => Specification fn Integer -> Specification fn Integer
maxSpec TrueSpec = TrueSpec
maxSpec s@(SuspendedSpec _ _) =
  constrained $ \x -> unsafeExists $ \y -> [y `satisfies` s, Assert ["maxSpec on SuspendedSpec"] (x <=. y)]
maxSpec (ErrorSpec xs) = ErrorSpec xs
maxSpec (MemberSpec []) = ErrorSpec ["empty MemberSec in maxSpec."]
maxSpec (MemberSpec xs) = typeSpec (atMostSize (maximum xs))
maxSpec (TypeSpec (NumSpecInterval _ hi) bad) = TypeSpec (NumSpecInterval Nothing hi) bad

-- ================
-- Sized
-- ================

class Sized t where
  sizeOf :: t -> Integer
  liftSizeSpec :: HasSpec fn t => SizeSpec -> Specification fn t
  liftMemberSpec :: HasSpec fn t => OrdSet Integer -> Specification fn t
  sizeOfTypeSpec :: HasSpec fn t => TypeSpec fn t -> Specification fn Integer

instance Ord a => Sized (Set.Set a) where
  sizeOf = toInteger . Set.size
  liftSizeSpec spec = typeSpec (SetSpec mempty TrueSpec (typeSpec spec))
  liftMemberSpec xs = typeSpec (SetSpec mempty TrueSpec (MemberSpec xs))
  sizeOfTypeSpec (SetSpec must _ sz) = sz <> (TypeSpec (atLeastSize (sizeOf must)) [])

instance Sized [a] where
  sizeOf = toInteger . length
  liftSizeSpec spec = typeSpec (ListSpec Nothing mempty (typeSpec spec) TrueSpec NoFold)
  liftMemberSpec xs = typeSpec (ListSpec Nothing mempty (MemberSpec xs) TrueSpec NoFold)
  sizeOfTypeSpec (ListSpec _ _ _ ErrorSpec {} _) = equalSpec 0
  sizeOfTypeSpec (ListSpec _ must sizespec _ _) = sizespec <> (TypeSpec (atLeastSize (sizeOf must)) [])

-- How to constrain the size of any type, with a Sized instance
hasSize :: (HasSpec fn t, Sized t) => SizeSpec -> Specification fn t
hasSize sz = liftSizeSpec sz

-- ==================================================================================
-- (NumSpec Integer) can support interval arithmetic, so we can make a (Num (NumSpec fn Integer)) instance
-- Given operator ☉, then (a,b) ☉ (c,d) = (minimum s, maximum s) where s = [a ☉ c, a ☉ d, b ☉ c, b ☉ d]
-- There are simpler rules for (+) and (-), but for (*) we need to use the general rule.

addNumSpec :: NumSpec Integer -> NumSpec Integer -> NumSpec Integer
addNumSpec (NumSpecInterval x y) (NumSpecInterval a b) = NumSpecInterval ((+) <$> x <*> a) ((+) <$> y <*> b)

subNumSpec :: NumSpec Integer -> NumSpec Integer -> NumSpec Integer
subNumSpec (NumSpecInterval x y) (NumSpecInterval a b) = NumSpecInterval ((-) <$> x <*> b) ((-) <$> y <*> a)

multNumSpec :: NumSpec Integer -> NumSpec Integer -> NumSpec Integer
multNumSpec (NumSpecInterval a b) (NumSpecInterval c d) = NumSpecInterval (unT (minimum s)) (unT (maximum s))
  where
    s = [multT (neg a) (neg c), multT (neg a) (pos d), multT (pos b) (neg c), multT (pos b) (pos d)]

negNumSpec :: NumSpec Integer -> NumSpec Integer
negNumSpec (NumSpecInterval lo hi) = NumSpecInterval (negate <$> hi) (negate <$> lo)

instance Num (NumSpec Integer) where
  (+) = addNumSpec
  (-) = subNumSpec
  (*) = multNumSpec
  negate = negNumSpec
  fromInteger n = NumSpecInterval (Just (fromInteger n)) (Just (fromInteger n))
  abs = error "No abs in the Num (NumSpec Integer) instance"
  signum = error "No signum in the Num (NumSpec Integer) instance"

-- ========================================================================
-- To implement the (HasSpec fn t) method: cardinalTypeSpec :: HasSpec fn a => TypeSpec fn a -> Specification fn Integer
-- We are going to need some arithmetic-like operations on (Specification fn Integer)
-- We will instance equations like these in some HasSpec instances
--
-- cardinalTypeSpec (Cartesian x y) = 'multSpecInt' (cardinality x) (cardinality y)
--
-- cardinalTypeSpec (SumSpec leftspec rightspec) = 'addSpecInt' (cardinality leftspec) (cardinality rightspec)
--
-- To get those functions, we are going to have to lift opertions on (TypeSpec fn Integer) to (Specification fn Integer)

addSpecInt ::
  BaseUniverse fn => Specification fn Integer -> Specification fn Integer -> Specification fn Integer
addSpecInt x y = operateSpec (+) (+) x y

subSpecInt ::
  BaseUniverse fn => Specification fn Integer -> Specification fn Integer -> Specification fn Integer
subSpecInt x y = operateSpec (-) (-) x y

multSpecInt ::
  BaseUniverse fn => Specification fn Integer -> Specification fn Integer -> Specification fn Integer
multSpecInt x y = operateSpec (*) (*) x y

-- | let 'n' be some numeric type, and 'f' and 'ft' be operations on 'n' and (TypeSpec fn n)
--   Then lift these operations from (TypeSpec fn n) to (Specification fn n)
--   Normally 'f' will be a (Num n) instance method (+,-,*) on n,
--   and 'ft' will be a a (Num (TypeSpec fn n)) instance method (+,-,*) on (TypeSpec fn n)
--   But this will work for any operations 'f' and 'ft' with the right types
operateSpec ::
  (TypeSpec fn n ~ NumSpec n, Enum n, Ord n) =>
  (n -> n -> n) ->
  (TypeSpec fn n -> TypeSpec fn n -> TypeSpec fn n) ->
  Specification fn n ->
  Specification fn n ->
  Specification fn n
operateSpec f ft x y = case (x, y) of
  (ErrorSpec xs, ErrorSpec ys) -> ErrorSpec (xs ++ ys)
  (ErrorSpec xs, _) -> ErrorSpec xs
  (_, ErrorSpec ys) -> ErrorSpec ys
  (TrueSpec, _) -> TrueSpec
  (_, TrueSpec) -> TrueSpec
  (_, SuspendedSpec _ _) -> TrueSpec
  (SuspendedSpec _ _, _) -> TrueSpec
  (TypeSpec x bad1, TypeSpec y bad2) -> TypeSpec (ft x y) [f b1 b2 | b1 <- bad1, b2 <- bad2]
  (MemberSpec [], _) -> ErrorSpec ["Null MemberSpec"]
  (_, MemberSpec []) -> ErrorSpec ["Null MemberSpec"]
  (MemberSpec xs, MemberSpec ys) -> MemberSpec [f x y | x <- xs, y <- ys]
  -- This block is all (MemberSpec{}, TypeSpec{}) with MemberSpec on the left
  (MemberSpec xs, TypeSpec (NumSpecInterval (Just i) (Just j)) bad) ->
    MemberSpec [f x y | x <- xs, y <- [i .. j], not (elem y bad)]
  -- Somewhat loose spec here, but more accurate then TrueSpec, it is exact if 'xs' has one element (i.e. 'xs' = [i])
  (MemberSpec xs, TypeSpec (NumSpecInterval lo hi) bads) ->
    -- We use the specialized version of 'TypeSpec' 'typeSpecOpt'
    typeSpecOpt
      (NumSpecInterval (f (minimum xs) <$> lo) (f (maximum xs) <$> hi))
      [f x b | x <- xs, b <- bads]
  -- we flip the arguments, so we need to flip the functions as well
  (x, y) -> operateSpec (\a b -> f b a) (\u v -> ft v u) y x

-- =================================
-- Cardinality

-- | Put some (admittedly loose bounds) on the number of solutions that
--   'genFromTypeSpec' might return. For lots of types, there is no way to be very accurate.
--   Here we lift the HasSpec methods 'cardinalTrueSpec' and 'cardinalTypeSpec'
--   from (TypeSpec fn Integer) to (Specification fn Integer)
cardinality ::
  forall fn a. (Eq a, BaseUniverse fn, HasSpec fn a) => Specification fn a -> Specification fn Integer
cardinality TrueSpec = cardinalTrueSpec @fn @a
cardinality (MemberSpec es) = exactSizeSpec (sizeOf (nub es))
cardinality ErrorSpec {} = equalSpec 0
cardinality (TypeSpec s cant) =
  subSpecInt
    (cardinalTypeSpec @fn @a s)
    (exactSizeSpec (sizeOf (filter (\c -> conformsTo @fn @a c s) cant)))
cardinality SuspendedSpec {} = cardinalTrueSpec @fn @a

-- | A generic function to use as an instance for the HasSpec method
--   cardinalTypeSpec :: HasSpec fn a => TypeSpec fn a -> Specification fn Integer
--   for types 'n' such that (TypeSpec n ~ NumSpec n)
cardinalSizeSpec ::
  forall n fn. (Integral n, Num n, MaybeBounded n) => NumSpec n -> Specification fn Integer
cardinalSizeSpec (NumSpecInterval (Just lo) (Just hi)) =
  if hi >= lo then MemberSpec [toInteger (hi - lo)] else MemberSpec [0]
cardinalSizeSpec (NumSpecInterval Nothing (Just hi)) =
  case lowerBound @n of
    Just lo -> MemberSpec [toInteger (hi - lo)]
    Nothing -> TrueSpec
cardinalSizeSpec (NumSpecInterval (Just lo) Nothing) =
  case upperBound @n of
    Just hi -> MemberSpec [toInteger (hi - lo)]
    Nothing -> TrueSpec
cardinalSizeSpec (NumSpecInterval Nothing Nothing) = TrueSpec

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

-- | This is an optimizing version of  TypeSpec :: TypeSpec fn n -> [n] -> Specification fn n
--   for Bounded NumSpecs.
--                    notInNumSpec :: Bounded n => TypeSpec fn n -> [n] -> Specification fn n
--   We use this function to specialize the (HasSpec fn t) method 'typeSpecOpt' for Bounded n.
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
  forall fn n.
  ( Functions fn fn
  , BaseUniverse fn
  , HasSpec fn n
  , TypeSpec fn n ~ NumSpec n
  , Bounded n
  , Integral n
  ) =>
  NumSpec n ->
  [n] ->
  Specification fn n
notInNumSpec ns@(NumSpecInterval a b) bad
  | toInteger (length bad) > (finiteSize @n `div` 2) || countSpec ns < toInteger (length bad) =
      MemberSpec [x | x <- [lowBound a .. highBound b], notElem x bad]
  | otherwise = TypeSpec @fn @n ns bad

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

-- ====================================================================================
-- Generally useful functions

-- | sizeOfSpec generalizes the method 'sizeOfTypeSpec'
--   From (sizeOfTypeSpec :: TypeSpec fn t -> Specification fn Integer)
--   To   (sizeOfSpec     :: Specification fn t     -> Specification fn Integer)
--   It is not unusual for instances (HasSpec fn t) to define sizeOfTypeSpec with calls to sizeOfSpec,
--   Because many (TypeSpec fn t)'s contain (Specification fn s), for types 's' different from 't'
sizeOfSpec ::
  forall fn t. (BaseUniverse fn, Sized t) => Specification fn t -> Specification fn Integer
sizeOfSpec TrueSpec = TrueSpec
sizeOfSpec (MemberSpec xs) = MemberSpec (map sizeOf xs)
sizeOfSpec (ErrorSpec xs) = ErrorSpec xs
sizeOfSpec (SuspendedSpec x p) =
  constrained $ \len ->
    Exists
      (\_ -> fatalError ["sizeOfSpec: Exists"])
      (x :-> (Assert [] (len ==. sizeOf_ (V x)) <> p))
sizeOfSpec (TypeSpec x _) = sizeOfTypeSpec @t @fn x

-- | Turn a Size spec into an ErrorSpec if it has negative numbers.
checkForNegativeSize :: Specification fn Integer -> Specification fn Integer
checkForNegativeSize spec@(TypeSpec (NumSpecInterval x y) _) =
  case (x, y) of
    (Just lo, _) | lo < 0 -> ErrorSpec ["Negative low bound in conversion to SizeSpec: " ++ show spec]
    (_, Just hi) | hi < 0 -> ErrorSpec ["Negative high bound in conversion to SizeSpec: " ++ show spec]
    (Just lo, Just hi) | lo > hi -> ErrorSpec ["lo(" ++ show lo ++ ") > hi(" ++ show hi ++ ") in conversion to SizeSpec"]
    (_, _) -> spec
checkForNegativeSize (MemberSpec xs) | any (< 0) xs = ErrorSpec ["Negative Size in MemberSpec " ++ show xs]
checkForNegativeSize spec = spec
