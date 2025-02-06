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
--   and the FunctionSymbol class. It also has a few HasSpec, HasSimpleRep, and FunctionSymbol
--   instances for basic types needed to define the default types and methods of HasSpec.
--   It also supplies Eq, Pretty, and Show instances on the syntax (Term, Pred, Binder etc.)
--   because many functions require these instances. It exports functions that define the
--   user interface to the domain embedded language (constrained, forall, exists etc.).
--   And, by design, nothing more.
module Constrained.Experiment.Base where

import Constrained.Experiment.Generic
import Constrained.Experiment.Witness

import Constrained.Core (Evidence (..), Var (..), eqVar)
import Constrained.GenT (
  GE (..),
  GenT,
  MonadGenError (..),
  catMessageList,
  catMessages,
  fatalError1,
 )
import Constrained.List
import Control.Monad.Identity
import Control.Monad.Writer (Writer, tell)

-- instances on Symbol
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

-- import Type.Reflection(TypeRep)

-- ====================================================================
-- The FunctionSymbol class for implementing new function symbols in
-- the first order logic. Note that a function symbol is first order
-- data, that uniquely identifies a higher order function (the 'semantics' method)
-- Sort of a combination of the FunctionLike and Functions classes
-- An instance "assigns" several functions to each Symbol 's'
-- =====================================================================

class
  ( Typeable c
  , Typeable s
  , Typeable t
  , Witness t
  , TypeList dom
  , All Typeable dom
  , Typeable dom
  , All HasSpec dom
  , HasSpec rng
  , Show (t c s dom rng)
  , Eq (t c s dom rng)
  , c
  ) =>
  FunctionSymbol
    (c :: Constraint)
    (s :: Symbol)
    (t :: Constraint -> Symbol -> [Type] -> Type -> Type)
    (dom :: [Type])
    rng
    | s -> t
  where
  {-# MINIMAL witness, (propagate | simplepropagate) #-}
  evidence :: t c s dom rng -> Evidence c
  evidence _ = Evidence
  witness :: String -- For documentation about what constructor of 't' implements 's'
  -- Handles all the obvious default cases. So if this is what you want
  -- then define simplepropagate instead. If you need special instructions
  -- for ExplainSpec, ErrorSpec, SuspendedSpec, or TrueSpec, then write your own.

  propagate :: Context c s t dom rng hole -> Specification rng -> Specification hole
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate _ TrueSpec = TrueSpec
  propagate _ (ErrorSpec msgs) = ErrorSpec msgs
  -- this is only good for unary functions
  propagate (Context witW (HOLE End)) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App witW (v' :> Nil)) (v :-> ps)
  -- this is only good for binary functions
  propagate (Context witW (HOLE (x :<| End))) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App witW (v' :> Lit x :> Nil)) (v :-> ps)
  propagate (Context witW (x :>| (HOLE End))) (SuspendedSpec v ps) =
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

propagateSpecFun :: Ctx hole rng -> Specification rng -> Specification hole
propagateSpecFun (Ctx text@(Context _ _)) = propagate text

-- \^ The pattern match brings the FunctionSymbol constraint into scope
propagateSpec ::
  forall v a.
  Specification a ->
  Ctx v a ->
  Specification v
propagateSpec spec (Ctx context) = propagate context spec

-- ========================================================================
extractDom ::
  forall rng (t :: Constraint -> Symbol -> [Type] -> Type -> Type) c s dom t' c' s' dom' rng'.
  ( Typeable c
  , Typeable t
  , Typeable dom
  , Typeable rng
  , KnownSymbol s
  , FunctionSymbol c' s' t' dom' rng'
  ) =>
  t c s dom rng -> t' c' s' dom' rng' -> Maybe (dom :~: dom')
extractDom t1 t2 =
  case (eqT @t @t', eqT @c @c', eqT @s @s', eqT @dom @dom', eqT @rng @rng') of
    (Just Refl, Just Refl, Just Refl, p@(Just Refl), (Just Refl)) ->
      if t1 == t2 then p else Nothing
    _ -> Nothing

-- | Use this as a view pattern to match against the Term App constructor
--   test :: Term a -> String
--   test x = case x of
--     (App (extractAll NotW -> Just(NotW,Evidence,Refl,Refl,Refl)) xs) -> "NotW"
--     (App (extractAll (EqualW @Integer) -> Just(EqualW,Evidence,Refl,Refl,Refl)) xs) -> "EqualW Integer"
--     (App (extractAll (EqualW @Int) -> Just(EqualW,Evidence,Refl,Refl,Refl)) xs) -> "EqualW Int"
--     (App (extractAll (EqualW @Bool) -> Just (EqualW, Evidence, Refl, Refl,Refl)) _) -> "EqualW Bool"
--     (App (extractAll (AddW @Integer) -> Just(AddW,Evidence,Refl,Refl,Refl)) xs) -> "AddW Integer"
--     (App (extractAll (NegateW @Integer) -> Just(NegateW, Evidence,Refl, Refl,Refl)) _) -> "NegateW Integer"
--     _ -> "All branches fail"
--  Just(constr, constraints, domain equality, range equality)
extractAll ::
  forall
    (f :: Type)
    (t' :: Constraint -> Symbol -> [Type] -> Type -> Type)
    (c' :: Constraint)
    (s' :: Symbol)
    (d' :: [Type])
    r'
    (t :: Constraint -> Symbol -> [Type] -> Type -> Type)
    c
    s
    d
    r.
  ( FunctionSymbol c s t d r
  , Typeable t'
  , Typeable c'
  , Typeable s'
  , Typeable d'
  , Typeable r'
  , f ~ t' c' s' d' r'
  ) =>
  f -> t c s d r -> Maybe (t' c' s' d' r', Evidence c', d :~: d', r :~: r', f :~: t' c' s' d' r')
extractAll _f hidden =
  case (eqT @t @t', eqT @c @c', eqT @s @s', eqT @d @d', eqT @r @r', eqT @f @(t' c' s' d' r')) of
    (Just Refl, Just Refl, Just Refl, Just dp@Refl, Just rp@Refl, Just fp@Refl) ->
      Just (hidden, evidence hidden, dp, rp, fp)
    _ -> Nothing

type FSType = Constraint -> Symbol -> [Type] -> Type -> Type

matchFS ::
  forall
    (k :: FSType)
    (c1 :: Constraint)
    (s1 :: Symbol)
    (d1 :: [Type])
    r1
    f
    (c2 :: Constraint)
    (s2 :: Symbol)
    (d2 :: [Type])
    r2.
  ( Typeable k
  , Typeable c1
  , Typeable s1
  , Typeable d1
  , Typeable r1
  , FunctionSymbol c2 s2 f d2 r2
  ) =>
  k c1 s1 d1 r1 ->
  f c2 s2 d2 r2 ->
  Maybe (k c1 s1 d1 r1, Evidence c1, f :~: k, s1 :~: s2, d1 :~: d2, r1 :~: r2)
matchFS t x =
  case (eqT @f @k, eqT @c1 @c2, eqT @s1 @s2, eqT @d1 @d2, eqT @r1 @r2) of
    (Just pT@Refl, Just Refl, Just p2@Refl, Just p3@Refl, Just p4@Refl) ->
      if t == x then Just (t, evidence x, pT, p2, p3, p4) else Nothing
    _ -> Nothing

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
--   The FunctionSymbol instance appears in Constrained.Experiment.TheKnot where caseBoolSpec is finally defined
(==.) :: forall a. HasSpec a => Term a -> Term a -> Term Bool
(==.) = Equal

-- ===================================================================
-- toGeneric and fromGeneric as Function Symbols
-- That means they can be used inside (Term a)
-- ===================================================================

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

-- (HasSimpleRep a, HasSpec (SimpleRep a), TypeSpec a ~ TypeSpec (SimpleRep a)
--      , Typeable (TypeSpec (SimpleRep a)),Typeable (SimpleRep a))

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
  FunctionSymbol
    (HasSimpleRep a, HasSpec simplerepA, typespecA ~ typespecsimplerep)
    "toGenericFn"
    GenericsW
    '[a]
    simplerepA
  where
  witness = "ToGenericW[toGenericFn]"

  simplepropagate (Context ToGenericW (HOLE End)) (TypeSpec s cant) = Right $ TypeSpec s (fromSimpleRep <$> cant)
  simplepropagate (Context ToGenericW (HOLE End)) (MemberSpec es) = Right $ MemberSpec (fmap fromSimpleRep es)
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
  FunctionSymbol
    (Typeable dom, Typeable typespecA, HasSimpleRep a, HasSpec dom, typespecA ~ typespecDOM)
    "fromGenericFn"
    GenericsW
    '[dom]
    a
  where
  witness = "FromGenericW[fromGenericFn]"

  simplepropagate (Context FromGenericW (HOLE End)) (TypeSpec s cant) = Right $ TypeSpec s (toSimpleRep <$> cant)
  simplepropagate (Context FromGenericW (HOLE End)) (MemberSpec es) = Right $ MemberSpec (fmap toSimpleRep es)
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
  ( -- HasSimpleRep a
    HasSpec (SimpleRep a)
  , TypeSpec a ~ TypeSpec (SimpleRep a)
  , FunctionSymbol (HasSimpleRep a) "fromGenericFn" BaseW '[SimpleRep a] a
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

-- =============================================
-- Patterns over Function Symbols
-- =============================================

-- | When writing patterns over (App witness arglist), we have to obtain proof that 'witness'
--   is some particular constructor of BaseW (or some other witness type). For example
--   suppose we were writing a pattern for (PairPat x y) <- (App (x::(f s d r)) (x :> y :> Nil))
--   we want proof that (x:: f s d r) is (PairW :: BaseW "pair_" '[a, b] (Prod a b))
--   So we test 4 things
--   (1) f :~: BaseW
--   (2) s :~: "pair_"
--   (3) d :~: '[a,b]
--   (4) r :~: (Prod a b)
isBaseWit ::
  forall (m :: Constraint) (a :: Symbol) (b :: [Type]) c f (n :: Constraint) (x :: Symbol) (y :: [Type]) z.
  ( Typeable a
  , Typeable b
  , Typeable c
  , Typeable f
  , Typeable x
  , Typeable y
  , Typeable z
  , Typeable m
  , Typeable n
  ) =>
  BaseW m a b c ->
  f n x y z ->
  ( Maybe (BaseW n x y z)
  , Maybe (f :~: BaseW)
  , Maybe (m :~: n)
  , Maybe (a :~: x)
  , Maybe (b :~: y)
  , Maybe (c :~: z)
  )
isBaseWit t x =
  case (eqT @f @BaseW, eqT @m @n, eqT @a @x, eqT @b @y, eqT @c @z) of
    (p1@(Just Refl), pcon@(Just Refl), p2@(Just Refl), p3@(Just Refl), p4@(Just Refl)) ->
      if t == x then (Just t, p1, pcon, p2, p3, p4) else (Nothing, p1, pcon, p2, p3, p4)
    _ -> (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

-- ========== "elem_"
pattern ElemPat ::
  forall a rng.
  (Eq a, Typeable a) =>
  forall.
  (rng ~ Bool, FunctionSymbol (Eq a) "elem_" BaseW '[a, [a]] Bool) =>
  Term a -> Term [a] -> Term rng
pattern ElemPat x y <-
  App
    (isBaseWit (ElemW @a) -> (Just ElemW, Just Refl, Just Refl, Just Refl, Just Refl, Just Refl))
    (x :> y :> Nil)
  where
    ElemPat x y = App ElemW (x :> y :> Nil)

-- ===========================================================================
-- This is quite usefull, as long as we have actual Function Symbol instances
extractW ::
  forall t2 c2 s2 d2 a2 t c s d a.
  ( Typeable t2
  , Typeable s2
  , Typeable d2
  , Typeable a2
  , Typeable s
  , Typeable c2
  , Typeable a
  , Typeable c
  , Typeable d
  , Typeable t
  , Eq (t c s d a)
  ) =>
  t2 c2 s2 d2 a2 -> t c s d a -> Maybe (t :~: t2)
extractW known hidden = case (eqT @t @t2, eqT @c @c2, eqT @s @s2, eqT @d @d2, eqT @a @a2) of
  (Just p@Refl, Just Refl, Just Refl, Just Refl, Just Refl) -> if known == hidden then (Just p) else Nothing
  _bad -> Nothing

{-
  where
    _ = semantics known -- These force the FunctionSymbol and
    _ = App hidden undefined -- Witness constraints to be not redundant
    -- We should never reach here
-}
-- =============================================================================================

{-
These are out of date See EqualPat and ElemPat to see why

-- UnionW :: (Ord a, Show a, Typeable a) => BaseW "union_" '[Set a, Set a] (Set a)
-- ============= "union_"
pattern UnionPat ::
  forall a rng . (Typeable a,Ord a) =>
  forall        . (rng ~ Set a,FunctionSymbol (Ord a) "union_" BaseW '[Set a,Set a] (Set a))
                 => Term (Set a) -> Term (Set a) -> Term rng
pattern UnionPat x y <- App (isBaseWit (UnionW @a) -> (Just UnionW, Just Refl,Just Refl,Just Refl,Just Refl)) (x :> y :> Nil)
   where UnionPat x y = App UnionW (x :> y :> Nil)

-- ========== "pair_"
pattern PairPat ::
  forall a b rng . (Typeable a,Typeable b) =>
  forall         . (rng ~ Prod a b,FunctionSymbol "pair_" BaseW '[a,b] (Prod a b))
                 => Term a -> Term b -> Term rng
pattern PairPat x y <- App (isBaseWit (PairW @a @b) -> (Just PairW, Just Refl, Just Refl,Just Refl,Just Refl)) (x :> y :> Nil)
   where PairPat x y = App PairW (x :> y :> Nil)
   -- You can't actually apply PairPat, because when this pattern was written,
   -- there was no instance (FunctionSymbol "pair_" BaseW '[a,b] (Prod a b)) yet!

pattern InjLeftPat ::
  forall a b rng . (Typeable a, Typeable b) =>
  forall         . (rng ~ Sum a b,FunctionSymbol "injectLeft_" BaseW '[a] (Sum a b))
                 => Term a -> Term rng
pattern InjLeftPat x <- App (isBaseWit (InjLeftW @a @b) -> (Just InjLeftW, Just Refl, Just Refl,Just Refl,Just Refl)) (x :> Nil)
   where InjLeftPat x = App InjLeftW (x :> Nil)

pattern InjRightPat ::
  forall a b rng . (Typeable a,Typeable b) =>
  forall         . (rng ~ Sum a b,FunctionSymbol "injectRight_" BaseW '[b] (Sum a b))
                 => Term b -> Term rng
pattern InjRightPat x <- App (isBaseWit (InjRightW @a @b) -> (Just InjRightW, Just Refl, Just Refl,Just Refl,Just Refl)) (x :> Nil)
   where InjRightPat x = App InjRightW (x :> Nil)

-- ============= fromGenericFn
pattern FromGenericPat ::
  forall a rng . (HasSimpleRep a,Typeable a,Typeable (SimpleRep a)) =>
  forall       . (rng ~ a,FunctionSymbol "fromGenericFn" BaseW '[SimpleRep a] a)
              => Term (SimpleRep a) -> Term rng
pattern FromGenericPat x <- App (isBaseWit (FromGenericW @a) -> (Just FromGenericW, Just Refl, Just Refl,Just Refl,Just Refl)) (x :> Nil)
   where FromGenericPat x = App FromGenericW (x :> Nil)

-- ============= toGenericFn
pattern ToGenericPat ::
  forall a rng . (Typeable a,Typeable (SimpleRep a),HasSimpleRep a) =>
  forall       . (rng ~ SimpleRep a,FunctionSymbol "toGenericFn" BaseW '[a] (SimpleRep a))
              => Term a -> Term rng
pattern ToGenericPat x <- App (isBaseWit (ToGenericW @a) -> (Just ToGenericW, Just Refl, Just Refl,Just Refl,Just Refl)) (x :> Nil)
   -- where ToGenericPat x = App ToGenericW (x :> Nil)
-}

-- =====================================================================
-- Now the supporting operations and types.
-- =====================================================================

-- Used to show binary operators like SumSpec and PairSpec
data BinaryShow where
  BinaryShow :: forall a. String -> [Doc a] -> BinaryShow
  NonBinary :: BinaryShow

-- =================================================
-- Term
{-
data Term a where
  App ::
    forall c sym t dom rng.
    (FunctionSymbol c sym t dom rng, c, All HasSpec dom, HasSpec rng) =>
    t c sym dom rng -> List Term dom -> Term rng
  Lit :: (Typeable a, Show a) => a -> Term a
  V :: HasSpec a => Var a -> Term a

instance Eq a => Eq (Term a) where
  V x == V x' = x == x'
  Lit a == Lit b = a == b
  App (w1 :: x1) (ts :: List Term dom1) == App (w2 :: x2) (ts' :: List Term dom2) =
    case (eqT @dom1 @dom2, eqT @x1 @x2) of
      (Just Refl, Just Refl) ->
        w1 == w2
          && same ts ts'
      _ -> False
  _ == _ = False

same :: All HasSpec as => List Term as -> List Term as -> Bool
same Nil Nil = True
same (x :> xs) (y :> ys) = x == y && same xs ys
-}

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

data Term a where
  App ::
    forall c sym t dom rng.
    (FunctionSymbol c sym t dom rng, c, All HasSpec dom, HasSpec rng) =>
    t c sym dom rng -> List Term dom -> Term rng
  Lit :: (Typeable a, Eq a, Show a) => a -> Term a
  V :: HasSpec a => Var a -> Term a
  To :: GenericC a => Term a -> Term (SimpleRep a)
  From :: GenericC a => Term (SimpleRep a) -> Term a
  Equal :: HasSpec a => Term a -> Term a -> Term Bool

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

sameTerms :: All HasSpec as => List Term as -> List Term as -> Bool
sameTerms Nil Nil = True
sameTerms (x :> xs) (y :> ys) = x == y && sameTerms xs ys

appSym ::
  forall c sym t as b.
  FunctionSymbol c sym t as b =>
  t c sym as b -> List Term as -> Term b
appSym w xs = App w xs

-- | This extracts the semantics of a witness (i.e. a function over Terms)
--   Recall FunctionSymbols are functions that you can use when writing Terms
--   Usually the Haskel name ends in '_', for example consider: not_, subset_ ,lookup_
--   Infix FunctionSymbols names end in '.', for example: ==. , <=.
--   E.g  app NotW :: Term Bool -> Term Bool
--        app NotW (lit False)  ==reducesto==> not_ False
--   this functionality is embedded in the Haskel function not_
--   Note the witness (NotW) must have a FunctionSymbol instance like:
--   instance FunctionSymbol "not_" BaseW '[Bool] Bool where ...
--             Name in Haskell^      ^  its arguments^   ^ its result
--                  The type of NotW |
appTerm ::
  forall c sym t ds b.
  FunctionSymbol c sym t ds b =>
  t c sym ds b -> FunTy (MapList Term ds) (Term b)
appTerm sym = curryList @ds (App @c @sym @t @ds @b sym)

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

data WithPrec a = WithPrec Int a

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

prettyPrec :: Pretty (WithPrec a) => Int -> a -> Doc ann
prettyPrec p = pretty . WithPrec p

ppList ::
  forall f as ann.
  All HasSpec as =>
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

-- | A context identifies a Term with exactly 1 variable.
data
  Context
    (c :: Constraint)
    (s :: Symbol)
    (t :: Constraint -> Symbol -> [Type] -> Type -> Type)
    (dom :: [Type])
    rng
    hole
  where
  Context :: t c s dom rng -> CtxtList Pre dom hole -> Context c s t dom rng hole

deriving instance (Show (t c s dom rng), All Show dom) => Show (Context c s t dom rng hole)

data Ctx hole rng where
  Ctx :: (HasSpec hole, FunctionSymbol c s t dom rng) => Context c s t dom rng hole -> Ctx hole rng

data Mode = Pre | Post

-- Note arrows point towards the HOLE
infixr 5 :<|
infixr 1 :>|

data CtxtList (x :: Mode) (as :: [Type]) (hole :: Type) where
  End :: CtxtList Post '[] h
  (:<|) :: Typeable a => a -> CtxtList Post as h -> CtxtList Post (a : as) h
  HOLE :: CtxtList Post as i -> CtxtList Pre (b : as) b
  (:>|) :: Typeable a => a -> CtxtList Pre as h -> CtxtList Pre (a : as) h

-- Here is an Example
c6 :: CtxtList Pre [Bool, String, b, Bool, ()] b
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

-- ===========================================================================

-- | Construct a Context from a Var and a Term (which we hope has exactly
--   one occurrence of that Var). Runs monadically, and fails with a error message
--   if that property does not hold.
toCtx ::
  forall m v a.
  ( MonadGenError m
  , HasCallStack
  , HasSpec v
  , HasSpec a
  ) =>
  Var v ->
  Term a ->
  m (Ctx v a)
toCtx v term = case term of
  (To (V v')) -- Special Syntax for (toSimpleRep_ HOLE)
    | Just Refl <- eqVar v v' -> pure $ Ctx (Context ToGenericW (HOLE End))
    | otherwise -> fatalError1 "oops"
  (From (V v')) -- Special Syntax for (fromSimpleRep_ HOLE)
    | Just Refl <- eqVar v v' -> pure $ Ctx (Context FromGenericW (HOLE End))
    | otherwise -> fatalError1 "oops"
  {- FIX ME The Function Symbol instance is not in scope, and can't be
   (Equal (V v') x)  -- Special Syntax for ( HOLE ==. 6)
     | Just Refl <- eqVar v v' -> do
         y <- checkForVar x
         pure $ Ctx (Context EqualW (HOLE (y :<| End)))
   (Equal x (V v')) -- Special Syntax for ( 13 ==. HOLE)
     | Just Refl <- eqVar v v' -> do
         y <- checkForVar x
         pure $ Ctx (Context EqualW (y :>| (HOLE End)))
   -}
  (App w (V v' :> Nil))
    | Just Refl <- eqVar v v' -> pure $ Ctx (Context w (HOLE End))
    | otherwise -> fatalError1 "oops"
  (App w (V v' :> x :> Nil))
    | Just Refl <- eqVar v v' -> do
        y <- checkForVar x
        pure $ Ctx (Context w (HOLE (y :<| End)))
  (App w (x :> V v' :> Nil))
    | Just Refl <- eqVar v v' -> do
        y <- checkForVar x
        pure $ Ctx (Context w (y :>| (HOLE End)))
  (App w (V v' :> a :> b :> Nil))
    | Just Refl <- eqVar v v' -> do
        av <- checkForVar a
        bv <- checkForVar b
        pure $ Ctx (Context w (HOLE $ av :<| bv :<| End))
  (App w (a :> V v' :> b :> Nil))
    | Just Refl <- eqVar v v' -> do
        av <- checkForVar a
        bv <- checkForVar b
        pure $ Ctx (Context w (av :>| (HOLE $ bv :<| End)))
  (App w (a :> b :> V v' :> Nil))
    | Just Refl <- eqVar v v' -> do
        av <- checkForVar a
        bv <- checkForVar b
        pure $ Ctx (Context w (av :>| bv :>| (HOLE End)))
  _ -> fatalError1 "Can't make a context"
  where
    checkForVar :: forall t. Term t -> m t
    checkForVar = \case
      Lit a -> pure a
      V v'
        | Just Refl <- eqVar v v' ->
            fatalError $
              NE.fromList
                [ "Trying to compute a context for variable " ++ show v
                , "From the term " ++ show term
                , show v ++ " appears more than once in the term"
                ]
        | otherwise ->
            fatalError $
              NE.fromList
                [ "Trying to compute a context for variable " ++ show v
                , "From the term " ++ show term
                , "An unknown variable " ++ show v' ++ " occurs"
                ]
      App w (ts :: List Term dom) -> do
        vs <- mapMList (fmap Identity . checkForVar) ts
        pure $ uncurryList_ runIdentity (semantics w) vs
      To x -> toSimpleRep <$> checkForVar x
      From x -> fromSimpleRep <$> checkForVar x
      Equal x y -> (==) <$> checkForVar x <*> checkForVar y

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
