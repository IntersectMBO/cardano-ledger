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

-- | This module contains the most basic parts the implementation. Essentially
--   everything to define Specification, HasSpec, HasSimpleRep, Term, Pred,
--   and the FunSym class. It also has a few HasSpec, HasSimpleRep, and FunSym
--   instances for basic types needed to define the default types and methods of HasSpec.
--   It also supplies Eq, Pretty, and Show instances on the syntax (Term, Pred, Binder etc.)
--   because many functions require these instances. It exports functions that define the
--   user interface to the domain embedded language (constrained, forall, exists etc.).
--   And, by design, nothing more.
module Constrained.Experiment.Base where

import Constrained.Experiment.Generic
import Constrained.Experiment.Witness

import Constrained.Core (Evidence (..), Var (..))
import Constrained.GenT (
  GE (..),
  GenT,
  MonadGenError (..),
  catMessageList,
  catMessages,
 )
import Constrained.List
import Control.Monad.Writer (Writer, tell)
import Data.Foldable (toList)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Orphans ()
import Data.Semigroup (Max (..), getMax)
import Data.String (fromString)
import Data.Typeable (Proxy (..), Typeable, eqT, typeRep, (:~:) (Refl))
import GHC.Stack
import GHC.TypeLits hiding (Text)
import Prettyprinter hiding (cat)
import Test.QuickCheck hiding (Args, Fun, Witness, forAll, witness)

-- ====================================================================
-- The FunSym class for implementing new function symbols in
-- the first order logic. Note that a function symbol is first order
-- data, that uniquely identifies a higher order function (the 'semantics' method)
-- Sort of a combination of the FunctionLike and Functions classes
-- An instance "assigns" several functions to each Symbol 's'
-- =====================================================================

-- The kind of a type, that is a candidate for a FunSym instance
type FSType = Constraint -> Symbol -> [Type] -> Type -> Type

-- What properties we need to be a function symbpl
type FSPre c s (t :: FSType) dom rng =
  ( Typeable t
  , Typeable c
  , Typeable s
  , Typeable dom
  , Typeable rng
  , Witness t
  , TypeList dom
  , All Typeable dom
  , Show (t c s dom rng)
  , Eq (t c s dom rng)
  )

class FSPre c s t dom rng => FunSym c s t dom rng | s -> t where
  {-# MINIMAL witness, (propagate | simplepropagate) #-}
  witness :: String -- For documentation about what constructor of 't' implements 's'

  -- 'propagate' handles all the obvious default cases. So if this is what you want
  -- then define simplepropagate instead. If you need special instructions
  -- for ExplainSpec, ErrorSpec, SuspendedSpec, or TrueSpec, then write your own propagate.
  propagate :: Context c s t dom rng hole -> Specification rng -> Specification hole
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  -- this is only good for unary functions
  -- Needed evidence--V             to build the App node.
  propagate (Context Evidence witW (HOLE End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App witW (v' :> Nil)) (v :-> ps)
  -- this is only good for binary functions
  propagate (Context Evidence witW (HOLE (x :<| End))) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App witW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context Evidence witW (x :>| (HOLE End))) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App witW (Lit x :> v' :> Nil)) (v :-> ps)
  -- Handle the important cases
  propagate ctxt spec = case simplepropagate ctxt spec of
    Left xs -> ErrorSpec xs
    Right spec2 -> spec2

  simplepropagate ::
    Context c s t dom rng hole -> Specification rng -> Either (NonEmpty String) (Specification hole)
  simplepropagate ctxt spec = Right $ propagate ctxt spec

  rewriteRules ::
    (TypeList as, Typeable as, HasSpec b, All HasSpec as) =>
    t c sym as b -> List Term as -> Maybe (Term b)
  rewriteRules _ _ = Nothing

  mapTypeSpec :: (HasSpec a, HasSpec b) => t c s '[a] b -> TypeSpec a -> Specification b
  mapTypeSpec _ts _spec = TrueSpec

-- This is where the logical properties of FunSym, are applied to transform one spec into another
propagateSpec ::
  forall v a.
  Specification a ->
  Ctx v a ->
  Specification v
propagateSpec spec (Ctx context) = propagate context spec

-- ========================================================================

-- | Designed to make it easy to write patterns that match an App node with
--   particular function symbol only.
--   To be used like this
--   pattern (Pat x y) <- a@(App (matchT a (EqualW @Int) -> Just(Evidence,Refl,Refl,Refl,Refl,Refl)) (x :> y :> Nil)
--   This will only match App nodes with the EqualW function symbol, and only at the Int type.
--   It recovers all the complicated type information from the 'a' which names the app node:  a@(App f xs)
matchT ::
  forall c1 s1 (t1 :: FSType) d1 r1 c2 s2 (t2 :: FSType) d2 r2.
  (FunSym c1 s1 t1 d1 r1, FunSym c2 s2 t2 d2 r2) =>
  Term r2 ->
  t1 c1 s1 d1 r1 ->
  t2 c2 s2 d2 r2 ->
  Maybe (Evidence (All HasSpec d2), c1 :~: c2, s1 :~: s2, t1 :~: t2, d1 :~: d2, r1 :~: r2)
matchT (App (_fs :: t c s d r) (_xs :: List Term d)) (_x :: t' c' s' d' r') _ = do
  c@Refl <- eqT @c @c'
  s@Refl <- eqT @s @s'
  t@Refl <- eqT @t @t'
  d@Refl <- eqT @d @d'
  r@Refl <- eqT @r @r'
  Refl <- eqT @c2 @c'
  Refl <- eqT @s2 @s'
  Refl <- eqT @t2 @t'
  Refl <- eqT @d2 @d'
  Refl <- eqT @r2 @r'
  Just (Evidence, c, s, t, d, r)
matchT _ _ _ = Nothing -- A function symbol call can only match against an App

-- ========================================================
-- A Specification is tells us what constraints must hold
-- ========================================================

-- | A `Specification a` denotes a set of `a`s
data Specification a where
  -- | Explain a Specification
  ExplainSpec :: [String] -> Specification a -> Specification a
  -- | Elements of a known set
  MemberSpec ::
    -- | It must be an element of this OrdSet (List). Try hard not to put duplicates in the List.
    NE.NonEmpty a ->
    Specification a
  -- | The empty set
  ErrorSpec ::
    NE.NonEmpty String ->
    Specification a
  -- | The set described by some predicates
  -- over the bound variable.
  SuspendedSpec ::
    HasSpec a =>
    -- | This variable ranges over values denoted by
    -- the spec
    Var a ->
    -- | And the variable is subject to these constraints
    Pred ->
    Specification a
  -- | A type-specific spec
  TypeSpec ::
    HasSpec a =>
    TypeSpec a ->
    -- | It can't be any of the elements of this set
    [a] ->
    Specification a
  -- | Anything
  TrueSpec :: Specification a

typeSpec :: HasSpec a => TypeSpec a -> Specification a
typeSpec ts = TypeSpec ts mempty

-- =================================================================
-- The class (HasSpec a) tells us what operations type 'a' must
-- support to add it to the constraint solver and generator
-- Writing HasSpec instances gives the system the power to grow
-- Don't be afraid of all the methods. Most have default implementations.
-- =================================================================

class (Typeable a, Eq a, Show a, Show (TypeSpec a), Typeable (TypeSpec a)) => HasSpec a where
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

  -- | For some types an Equality like (a ==. b) can be expanded by adding additional Pred. For example:
  --   `(cJust_ A) ==. B` can be expanded by adding [case B of Nothing -> False; Just _ -> True]
  --   This additional information is conjoined with the original equality , which should make it
  --   easier to solve. The simplify mechanism will pass both sides of the equality to 'saturate'
  saturate :: Term a -> Term a -> [Pred]
  saturate _ _ = []

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

  default emptySpec ::
    (HasSpec (SimpleRep a), TypeSpec a ~ TypeSpec (SimpleRep a)) => TypeSpec a
  emptySpec = emptySpec @(SimpleRep a)

  default combineSpec ::
    ( HasSimpleRep a
    , HasSpec (SimpleRep a)
    , TypeSpec a ~ TypeSpec (SimpleRep a)
    ) =>
    TypeSpec a ->
    TypeSpec a ->
    Specification a
  combineSpec s s' = fromSimpleRepSpec $ combineSpec @(SimpleRep a) s s'

  default genFromTypeSpec ::
    ( HasSimpleRep a
    , HasSpec (SimpleRep a)
    , TypeSpec a ~ TypeSpec (SimpleRep a)
    ) =>
    (HasCallStack, MonadGenError m) =>
    TypeSpec a ->
    GenT m a
  genFromTypeSpec s = fromSimpleRep <$> genFromTypeSpec s

  default conformsTo ::
    ( HasSimpleRep a
    , HasSpec (SimpleRep a)
    , TypeSpec a ~ TypeSpec (SimpleRep a)
    ) =>
    HasCallStack =>
    a ->
    TypeSpec a ->
    Bool
  a `conformsTo` s = conformsTo (toSimpleRep a) s

  default toPreds ::
    ( HasSpec (SimpleRep a)
    , TypeSpec a ~ TypeSpec (SimpleRep a)
    , HasSimpleRep a
    ) =>
    Term a ->
    TypeSpec a ->
    Pred
  toPreds v s = toPreds (toGeneric_ v) s

  default shrinkWithTypeSpec ::
    ( HasSpec (SimpleRep a)
    , TypeSpec a ~ TypeSpec (SimpleRep a)
    , HasSimpleRep a
    ) =>
    TypeSpec a ->
    a ->
    [a]
  shrinkWithTypeSpec spec a = map fromSimpleRep $ shrinkWithTypeSpec spec (toSimpleRep a)

  default cardinalTypeSpec ::
    (HasSpec (SimpleRep a), TypeSpec a ~ TypeSpec (SimpleRep a)) =>
    TypeSpec a ->
    Specification Integer
  cardinalTypeSpec = cardinalTypeSpec @(SimpleRep a)

-- ====================================================================
-- The Equality Function Symbol
-- This is given a FunSym instance in TheKnot, because all the things
-- that equality depends on, are not defined until that point
-- Note that we can still build equalities using the Equal Term constructor
-- ====================================================================

data EqW (c :: Constraint) (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  EqualW :: forall a. Typeable a => EqW (Eq a) "==." '[a, a] Bool

deriving instance Eq (EqW c s dom rng)

instance Show (EqW c s dom rng) where
  show EqualW = "==."

instance Witness EqW where
  semantics = eqSem

eqSem :: EqW c sym dom rng -> forall. c => FunTy dom rng -- Requires PolyKinds, RankNTypes
eqSem EqualW = (==)

-- | Note we inject this into the Term languge using the special 'Equal' construtor of Term
--   The FunSym instance appears in Constrained.Experiment.TheKnot where caseBoolSpec is finally defined
(==.) :: forall a. HasSpec a => Term a -> Term a -> Term Bool
(==.) = Equal

-- ===================================================================
-- toGeneric and fromGeneric as Function Symbols
-- That means they can be used inside (Term a)
-- ===================================================================
-- TODO cast this all in terms of PreG

data GenericsW constraint symbol args res where
  ToGenericW ::
    GenericsW
      (HasSimpleRep a, HasSpec (SimpleRep a), TypeSpec a ~ TypeSpec (SimpleRep a))
      "toGenericFn"
      '[a]
      (SimpleRep a)
  FromGenericW ::
    GenericsW
      ( Typeable (SimpleRep a)
      , Typeable (TypeSpec a)
      , HasSimpleRep a
      , HasSpec (SimpleRep a)
      , TypeSpec a ~ TypeSpec (SimpleRep a)
      )
      "fromGenericFn"
      '[SimpleRep a]
      a

deriving instance Eq (GenericsW c s dom rng)

instance Show (GenericsW constraint symbol args res) where
  show ToGenericW = "toGenericFn"
  show FromGenericW = "fromGenericFn"

instance Witness GenericsW where
  semantics FromGenericW = fromSimpleRep
  semantics ToGenericW = toSimpleRep

instance
  ( HasSimpleRep a
  , HasSpec (SimpleRep a)
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , Typeable (TypeSpec (SimpleRep a))
  , Typeable (SimpleRep a)
  , HasSpec a
  , typespecA ~ TypeSpec a
  , typespecsimplerep ~ TypeSpec (SimpleRep a)
  , simplerepA ~ SimpleRep a
  ) =>
  FunSym
    (HasSimpleRep a, HasSpec simplerepA, typespecA ~ typespecsimplerep)
    "toGenericFn"
    GenericsW
    '[a]
    simplerepA
  where
  witness = "ToGenericW[toGenericFn]"

  simplepropagate (Context _ ToGenericW (HOLE End)) (TypeSpec s cant) = Right $ TypeSpec s (fromSimpleRep <$> cant)
  simplepropagate (Context _ ToGenericW (HOLE End)) (MemberSpec es) = Right $ MemberSpec (fmap fromSimpleRep es)
  simplepropagate ctx _ = Left (NE.fromList ["ToGenericW (toGenericFn)", "Unreachable context, too many args", show ctx])

toGeneric_ ::
  forall a.
  ( TypeSpec a ~ TypeSpec (SimpleRep a)
  , -- , Typeable (TypeSpec (SimpleRep a))
    HasSimpleRep a
  , HasSpec a
  , HasSpec (SimpleRep a)
  ) =>
  Term a ->
  Term (SimpleRep a)
toGeneric_ = appTerm ToGenericW

instance
  ( HasSimpleRep a
  , HasSpec a
  , HasSpec (SimpleRep a)
  , HasSpec dom
  , dom ~ SimpleRep a
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , typespecA ~ TypeSpec a
  , typespecDOM ~ TypeSpec dom
  , Typeable typespecA
  -- , Typeable typespecDOM
  ) =>
  FunSym
    (Typeable dom, Typeable typespecA, HasSimpleRep a, HasSpec dom, typespecA ~ typespecDOM)
    "fromGenericFn"
    GenericsW
    '[dom]
    a
  where
  witness = "FromGenericW[fromGenericFn]"

  simplepropagate (Context _ FromGenericW (HOLE End)) (TypeSpec s cant) = Right $ TypeSpec s (toSimpleRep <$> cant)
  simplepropagate (Context _ FromGenericW (HOLE End)) (MemberSpec es) = Right $ MemberSpec (fmap toSimpleRep es)
  simplepropagate ctx _ =
    Left (NE.fromList ["FromGenericW (fromGenericFn)", "Unreachable context, too many args", show ctx])

fromGeneric_ ::
  forall a.
  ( TypeSpec a ~ TypeSpec (SimpleRep a)
  , HasSimpleRep a
  , HasSpec a
  , HasSpec (SimpleRep a)
  ) =>
  Term (SimpleRep a) ->
  Term a
fromGeneric_ = appTerm FromGenericW

-- ====================================================================
-- Generic Transformers
-- Using Generics to transform from ordinary (Specifications a) to
-- Specifications over 'a's SimpleRep (Specification (SimpleRep a))
-- ====================================================================

fromSimpleRepSpec ::
  forall a.
  (HasSpec a, HasSimpleRep a, TypeSpec a ~ TypeSpec (SimpleRep a)) =>
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
      Let (toGeneric_ x') (x :-> p)

toSimpleRepSpec ::
  forall a.
  ( HasSpec (SimpleRep a)
  , HasSimpleRep a
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , FunSym (HasSimpleRep a) "fromGenericFn" BaseW '[SimpleRep a] a
  ) =>
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
      Let (fromGeneric_ x') (x :-> p)

-- =====================================================================
-- Now the supporting operations and types.
-- =====================================================================

-- Used to show binary operators like SumSpec and PairSpec
data BinaryShow where
  BinaryShow :: forall a. String -> [Doc a] -> BinaryShow
  NonBinary :: BinaryShow

-- =================================================
-- Term

-- | Properties needed by objects to appear in the system,
--  if they have no semantic or logical requirements.
--  Mostly used for Lit terms, which are self evaluating
--  But such things also occurr in Contexts.
type Literal a = (Typeable a, Eq a, Show a)

data Term a where
  App ::
    forall c sym t dom rng.
    (FunSym c sym t dom rng, c, All HasSpec dom, HasSpec rng) =>
    t c sym dom rng -> List Term dom -> Term rng
  Lit :: Literal a => a -> Term a
  V :: HasSpec a => Var a -> Term a
  To :: GenericC a => Term a -> Term (SimpleRep a)
  From :: GenericC a => Term (SimpleRep a) -> Term a
  Equal :: HasSpec a => Term a -> Term a -> Term Bool

-- The things you need to know to lift toSimpleRep and fromSimpleRep
-- to the Term level. I think they are all reasonable requirements
type GenericC a =
  ( Typeable a
  , HasSpec a
  , HasSimpleRep a
  , Typeable (SimpleRep a)
  , HasSpec (SimpleRep a)
  , Show (SimpleRep a)
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  )

instance Eq (Term a) where
  V x == V x' = x == x'
  Lit a == Lit b = a == b
  App (w1 :: x1) (ts :: List Term dom1) == App (w2 :: x2) (ts' :: List Term dom2) =
    case (eqT @dom1 @dom2, eqT @x1 @x2) of
      (Just Refl, Just Refl) ->
        w1 == w2
          && sameTerms ts ts'
      _ -> False
  To (x :: b) == To (y :: c) = case eqT @b @c of Just Refl -> x == y; Nothing -> False
  From (x :: Term b) == From (y :: Term c) = case eqT @b @c of Just Refl -> x == y; Nothing -> False
  (Equal @t1 x y) == (Equal @t2 m n) =
    case eqT @t1 @t2 of
      Just Refl -> x == m && y == n
      Nothing -> False
  _ == _ = False

-- How to compare the args of two applications for equality
sameTerms :: All HasSpec as => List Term as -> List Term as -> Bool
sameTerms Nil Nil = True
sameTerms (x :> xs) (y :> ys) = x == y && sameTerms xs ys

-- Building App terms

-- | Recall FunSyms are objects that you can use to build applications
--   They carry information about both its semantic and logical properties.
--   Usually the Haskel name ends in '_', for example consider: not_, subset_ ,lookup_
--   Infix FunSyms names end in '.', for example: ==. , <=.
--   E.g  appTerm NotW :: Term Bool -> Term Bool
--        (appTerm NotW (lit False)) builds the Term  (not_ False)
--   Note the witness (NotW) must have a FunSym instance like:
--   instance FunSym "not_"            BaseW       '[Bool]         Bool where ...
--             Name in Haskell^    type of NotW^   arg types^   result type^
--   The FunSym instance does not demand any of these things have any properties at all.
--   It is here, where we actually build the App node, that we demand the properties App terms require.
--   App :: (FunSym c s t ds r, c, All HasSpec ds, HasSpec r) => t c s ds r -> List Term dom -> Term rng
--           ^--logic-and-semantics     ^--^---------------^----Required-properties
appSym ::
  forall c sym t as b.
  (FunSym c sym t as b, c, All HasSpec as, HasSpec b) =>
  t c sym as b -> List Term as -> Term b
appSym w xs = App w xs

-- All the same properties as 'aaSym' but builds functions over terms, rather that just one App term.
appTerm ::
  forall c sym t ds r.
  (FunSym c sym t ds r, c, All HasSpec ds, HasSpec r) =>
  t c sym ds r -> FunTy (MapList Term ds) (Term r)
appTerm sym = curryList @ds (App @c @sym @t @ds @r sym)

-- ===========================================
-- Binder

data Binder a where
  (:->) ::
    HasSpec a =>
    Var a ->
    Pred ->
    Binder a

deriving instance Show (Binder a)

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

-- =======================================================
-- Weighted

data Weighted f a = Weighted {weight :: Maybe Int, thing :: f a}
  deriving (Functor, Traversable, Foldable)

mapWeighted :: (f a -> g b) -> Weighted f a -> Weighted g b
mapWeighted f (Weighted w t) = Weighted w (f t)

traverseWeighted :: Applicative m => (f a -> m (g a)) -> Weighted f a -> m (Weighted g a)
traverseWeighted f (Weighted w t) = Weighted w <$> f t

-- ==================================================
-- Pred

data Pred where
  ElemPred :: forall a. HasSpec a => Bool -> Term a -> NonEmpty a -> Pred
  Monitor :: ((forall a. Term a -> a) -> Property -> Property) -> Pred
  And :: [Pred] -> Pred
  Exists ::
    -- | Constructive recovery function for checking
    -- existential quantification
    ((forall b. Term b -> b) -> GE a) ->
    Binder a ->
    Pred
  Subst :: HasSpec a => Var a -> Term a -> Pred -> Pred
  Let :: Term a -> Binder a -> Pred
  Assert :: Term Bool -> Pred
  Reifies ::
    ( HasSpec a
    , HasSpec b
    ) =>
    -- | This depends on the `a` term
    Term b ->
    Term a ->
    -- | Recover a useable value from the `a` term.
    (a -> b) ->
    Pred
  -- TODO: there is good cause for not limiting this to `Term a` and `Term b`.
  -- However, changing it requires re-working quite a lot of code.
  DependsOn ::
    ( HasSpec a
    , HasSpec b
    ) =>
    Term a ->
    Term b ->
    Pred
  ForAll ::
    ( Forallable t a
    , HasSpec t
    , HasSpec a
    ) =>
    Term t ->
    Binder a ->
    Pred
  Case ::
    HasSpec (SumOver as) =>
    Term (SumOver as) ->
    -- | Each branch of the type is bound with
    -- only one variable because `as` are types.
    -- Constructors with multiple arguments are
    -- encoded with `ProdOver` (c.f. `Constrained.Univ`).
    List (Weighted Binder) as ->
    Pred
  -- monadic-style `when` - if the first argument is False the second
  -- doesn't apply.
  When ::
    HasSpec Bool =>
    Term Bool ->
    Pred ->
    Pred
  GenHint ::
    HasGenHint a =>
    Hint a ->
    Term a ->
    Pred
  TruePred :: Pred
  FalsePred :: NE.NonEmpty String -> Pred
  Explain :: NE.NonEmpty String -> Pred -> Pred

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

class Forallable t e | t -> e where
  fromForAllSpec ::
    (HasSpec t, HasSpec e) => Specification e -> Specification t
  default fromForAllSpec ::
    ( HasSpec t
    , HasSpec e
    , HasSimpleRep t
    , TypeSpec t ~ TypeSpec (SimpleRep t)
    , Forallable (SimpleRep t) e
    , HasSpec (SimpleRep t)
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

-- | Hints are things that only affect generation, and not validation. For instance, parameters to
--   control distribution of generated values.
class (HasSpec a, Show (Hint a)) => HasGenHint a where
  type Hint a
  giveHint :: Hint a -> Specification a

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
short xs = "([" <+> viaShow (length xs) <+> "elements ...] @" <> prettyType @a <> ")"

-- ==========================================================================
-- Pretty and Show instances
-- ==========================================================================

-- ------------ Term -----------------
instance Show a => Pretty (WithPrec (Term a)) where
  pretty (WithPrec p t) = case t of
    Lit n -> fromString $ showsPrec p n ""
    V x -> viaShow x
    {-
    -- These pattern matches, using patterns SubsetPat, DisjointPat, ...
    -- elide large literal terms, to make the error messages easier to follow.
    SubsetPat (Lit n) y -> parensIf (p > 10) $ "subset_" <+> short (Set.toList n) <+> prettyPrec 10 y
    SubsetPat y (Lit n) -> parensIf (p > 10) $ "subset_" <+> prettyPrec 10 y <+> short (Set.toList n)
    DisjointPat (Lit n) y -> parensIf (p > 10) $ "disjoint_" <+> short (Set.toList n) <+> prettyPrec 10 y
    DisjointPat y (Lit n) -> parensIf (p > 10) $ "disjoint_" <+> prettyPrec 10 y <+> short (Set.toList n)
    UnionPat (Lit n) y -> parensIf (p > 10) $ "union_" <+> short (Set.toList n) <+> prettyPrec 10 y
    UnionPat y (Lit n) -> parensIf (p > 10) $ "union_" <+> prettyPrec 10 y <+> short (Set.toList n)
    MemberPat y (Lit n) -> parensIf (p > 10) $ "member_" <+> prettyPrec 10 y <+> short (Set.toList n)
    ElemPat y (Lit n) -> parensIf (p > 10) $ "elem_" <+> prettyPrec 10 y <+> short n
    AppendPat (Lit n) y -> parensIf (p > 10) $ "append_" <+> short n <+> prettyPrec 10 y
    AppendPat y (Lit n) -> parensIf (p > 10) $ "append_" <+> prettyPrec 10 y <+> short n
    -}
    App x Nil -> viaShow x
    {-
    App f as
      | Just Equal <- extractFn @(EqFn fn) f
      , a :> b :> _ <- as ->
          parensIf (p > 9) $ prettyPrec 10 a <+> "==." <+> prettyPrec 10 b
      | Just ToGeneric <- extractFn @(GenericsFn fn) f
      , a :> _ <- as ->
          prettyPrec p a
      | Just FromGeneric <- extractFn @(GenericsFn fn) f
      , a :> _ <- as ->
          prettyPrec p a
    -}
    -- App ToGenericW (x :> Nil) -> prettyPrec p x
    -- App FromGenericW (x :> Nil) -> prettyPrec p x

    App f as -> parensIf (p > 10) $ viaShow f <+> align (fillSep (ppList (prettyPrec 11) as))
    To x -> parensIf (p > 10) $ "toSimpleRep_" <+> align (prettyPrec 11 x)
    From x -> parensIf (p > 10) $ "fromSimpleRep_" <+> align (prettyPrec 11 x)
    Equal x y -> parensIf (p > 10) $ prettyPrec 11 x <+> "==." <+> prettyPrec 11 y

instance Show a => Pretty (Term a) where
  pretty = prettyPrec 0

instance Show a => Show (Term a) where
  showsPrec p t = shows $ pretty (WithPrec p t)

-- ------------ Pred -----------------

instance Pretty Pred where
  pretty = \case
    ElemPred True term vs -> align $ sep ["memberPred", pretty term, fillSep (punctuate "," (map viaShow (NE.toList vs)))]
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

instance Show Pred where
  show = show . pretty

-- TODO: make nicer
instance Pretty (f a) => Pretty (Weighted f a) where
  pretty (Weighted Nothing t) = pretty t
  pretty (Weighted (Just w) t) = viaShow w <> "~" <> pretty t

instance Pretty (Binder a) where
  pretty (x :-> p) = viaShow x <+> "->" <+> pretty p

-- ------------ Specification -----------------

instance HasSpec a => Pretty (WithPrec (Specification a)) where
  pretty (WithPrec d s) = case s of
    ExplainSpec es z -> "ExplainSpec" <+> viaShow es <+> "$" /> pretty z
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

instance HasSpec a => Pretty (Specification a) where
  pretty = pretty . WithPrec 0

instance HasSpec a => Show (Specification a) where
  showsPrec d = shows . pretty . WithPrec d

-- ==========================================================
-- Contexts
-- ==========================================================

-- | A Context identifies an App Term with exactly 1 variable, and all other args Literals.
--   Occaisionally we need to Build a new Term when propagating a SuspendedSpec
--   in which case we need evidence that the constraint 'c' holds. When we are
--   building the Context in 'toCtx' that evidence is attached to the App constructor
--   App :: (FunSym c s t d r, c, All HasSpec d, HasSpec r) => t c s d r -> List Term dom -> Term rng
--                                     ^ here
--   So we stuff its Evidence in the Context, to use later, if we need it.
data
  Context
    (c :: Constraint)
    (s :: Symbol)
    (t :: Constraint -> Symbol -> [Type] -> Type -> Type)
    (dom :: [Type])
    rng
    hole
  where
  Context ::
    (All HasSpec dom, HasSpec rng) =>
    Evidence c -> t c s dom rng -> CtxtList Pre dom hole -> Context c s t dom rng hole

deriving instance
  (Show (t c s dom rng), All Show dom, Typeable c) => Show (Context c s t dom rng hole)

data Ctx hole rng where
  Ctx :: (HasSpec hole, FunSym c s t dom rng) => Context c s t dom rng hole -> Ctx hole rng

data Mode = Pre | Post

-- Note arrows point towards the HOLE
infixr 5 :<|
infixr 1 :>|

data CtxtList (x :: Mode) (as :: [Type]) (hole :: Type) where
  End :: CtxtList Post '[] h
  (:<|) :: Literal a => a -> CtxtList Post as h -> CtxtList Post (a : as) h
  HOLE :: HasSpec b => CtxtList Post as i -> CtxtList Pre (b : as) b
  (:>|) :: Literal a => a -> CtxtList Pre as h -> CtxtList Pre (a : as) h

-- Here is an Example
c6 :: HasSpec b => CtxtList Pre [Bool, String, b, Bool, ()] b
c6 = True :>| ("abc" :: String) :>| (HOLE $ True :<| () :<| End)

--                      rightmost ^   ^--parens here ------------^
-- We need to put parenthesis around the segment from (Hole $ .... End),
-- before the rightmost (:>|), to make the fixity work.

val6 :: List [] [Bool, String, Int, Bool, ()]
val6 = [] :> ["abc"] :> [67, 12] :> [True, False] :> [()] :> Nil

-- Show instance puts parens in the output correctly to
-- remind us where the parens go.
instance All Show as => Show (CtxtList m as h) where
  show End = "End)"
  show (x :<| xs) = show x ++ " :<| " ++ show xs
  show (HOLE xs) = "(HOLE $ " ++ show xs
  show (x :>| xs) = show x ++ " :>| " ++ show xs

-- =================================================================
-- A simple but important HasSpec instances. The  other
-- instance usually come in a file of their own.

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

constrained ::
  forall a p.
  (IsPred p, HasSpec a) =>
  (Term a -> p) ->
  Specification a
constrained body =
  let x :-> p = bind body
   in SuspendedSpec x p
