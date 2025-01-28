{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

-- | This module contains the most basic parts the implementation. Essentially 
--   everything to define Specification, HasSpec, HasSimpleRep, Term, Pred,
--   and the FunctionSymbol class. It also has a few HasSpec, HasSimpleRep, and FunctionSymbol
--   instances for basic types needed to define the default types and methods of HasSpec.
--   It also supplies Eq, Pretty, and Show instances on the syntax (Term, Pred, Binder etc.)
--   because many functions require these instances. It exports functions that define the
--   user interface to the domain embedded language (constrained, forall, exists etc.).
--   And, by design, nothing more.
module Constrained.BaseExperiment where

import Debug.Trace
import Control.Monad.Identity
import Control.Monad.Writer (Writer, tell)
import Data.Kind(Type,Constraint)
import Data.Semigroup (Max (..), getMax)
import Data.Typeable
import GHC.Generics
import GHC.Stack
import Prettyprinter hiding (cat)
import Test.QuickCheck hiding (Args, Fun, forAll, Witness, witness)
import Constrained.Core(Var(..),eqVar,Evidence(..))
import Constrained.GenT(MonadGenError(..),GE(..),GenT,fatalError1,catMessageList,catMessages,pureGen)
import Constrained.List
import qualified Data.List.NonEmpty as NE
-- import GHC.TypeLits(SSymbol,KnownSymbol,Symbol,symbolSing,symbolVal,pattern SSymbol,KnownNat)
import  GHC.TypeLits hiding(Text)
import Data.Orphans() -- instances on Symbol
import Constrained.GenericExperiment
import Data.Foldable(toList)
import Data.Type.Equality(TestEquality(..))
import Data.String(fromString)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

-- =============================================

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

class (Typeable a, Eq a, Show a, Show (TypeSpec a)) => HasSpec a
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

-- =================================================

typeSpec :: HasSpec a => TypeSpec a -> Specification a
typeSpec ts = TypeSpec ts mempty

-- Used to show binary operators like SumSpec and PairSpec
data BinaryShow where
  BinaryShow :: forall a. String -> [Doc a] -> BinaryShow
  NonBinary :: BinaryShow

------------------------------------------------------------------------
-- Generics
------------------------------------------------------------------------

{-
`HasSimpleRep` and `GenericsFn` are meant to allow you to express that a
type is isomorphic to some other type 't' that has a (HasSpec t) instance.

The trick is that the default instance of `HasSpec a` assumes
`HasSimpleRep a` and defines `TypeSpec a = TypeSpec (SimpleRep a)`.

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

-- ===================================================================
-- toGeneric and fromGeneric as Terms    

instance  ( HasSimpleRep a, HasSpec a, HasSpec rng
          , rng ~ SimpleRep a,TypeSpec a ~ TypeSpec (SimpleRep a)) => 
          FunctionSymbol  "toGenericFn" BaseWitness '[a] rng where
  semantics = baseSem 
  witness = ToGenericW
  type Wit "toGenericFn" = BaseWitness
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate (Context ToGenericW  (HOLE End)) TrueSpec = TrueSpec
  propagate (Context ToGenericW  (HOLE End)) (ErrorSpec xs) = ErrorSpec xs
  propagate (Context ToGenericW  (HOLE End)) (SuspendedSpec v ps) = 
     constrained $ \ v' -> Let (App ToGenericW (v' :> Nil)) (v :-> ps)
  propagate (Context ToGenericW  (HOLE End)) (TypeSpec s cant) = TypeSpec s (fromSimpleRep <$> cant)
  propagate (Context ToGenericW  (HOLE End)) (MemberSpec es) = MemberSpec (fmap fromSimpleRep es)  
  propagate ctx _ = error ("ToGenricW\nUnreachable context, too many args\n"++show ctx)

toGeneric_ ::
  forall a .
  ( TypeSpec a ~ TypeSpec (SimpleRep a) 
  , HasSimpleRep a
  , HasSpec a
  , HasSpec (SimpleRep a)
  ) =>
  Term a ->
  Term (SimpleRep a)
toGeneric_ = appTerm ToGenericW 


instance  ( HasSimpleRep a, HasSpec a, HasSpec (SimpleRep a), HasSpec dom
          , dom ~ SimpleRep a, TypeSpec a ~ TypeSpec (SimpleRep a) ) => 
          FunctionSymbol "fromGenericFn" BaseWitness '[dom] a where
  semantics = baseSem  
  witness = FromGenericW      
  type Wit "fromGenericFn" = BaseWitness   
  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate (Context FromGenericW  (HOLE End)) TrueSpec = TrueSpec
  propagate (Context FromGenericW  (HOLE End)) (ErrorSpec xs) = ErrorSpec xs
  propagate (Context FromGenericW  (HOLE End)) (SuspendedSpec v ps) = 
    constrained $ \ v' -> Let (App FromGenericW (v' :> Nil)) (v :-> ps)
  propagate (Context FromGenericW  (HOLE End)) (TypeSpec s cant) = TypeSpec s (toSimpleRep <$> cant)
  propagate (Context FromGenericW  (HOLE End)) (MemberSpec es) = MemberSpec (fmap toSimpleRep es)  
  propagate ctx _ = error ("FromGenericW\nUnreachable context, too many args\n"++show ctx)

fromGeneric_ ::
  forall a .
  ( TypeSpec a ~ TypeSpec (SimpleRep a)
  , HasSimpleRep a
  , HasSpec a
  , HasSpec (SimpleRep a) ) =>
  Term (SimpleRep a) ->
  Term a
fromGeneric_ = appTerm FromGenericW 


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
  ( HasSimpleRep a, 
    TypeSpec a ~ TypeSpec (SimpleRep a)
  , FunctionSymbol  "fromGenericFn" BaseWitness '[SimpleRep a] a ) =>
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

-- ==============================================================================

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

-- ============================================================
-- One Witness to FunctionSymbol . This is the Witness for many
-- of the functions symbols available in the the Base modules
-- I dont think all the Typeable instances are necessary

data BaseWitness (sym :: Symbol) (dom :: [Type]) (rng :: Type) where 
  EqualW :: forall a . Eq a => BaseWitness "==." '[a,a] Bool  
  -- List
  ElemW :: forall a. (Eq a,Typeable a) => BaseWitness "elem_" '[a,[a]] Bool
  -- Bool
  NotW :: BaseWitness "not_" '[Bool] Bool
  OrW :: BaseWitness "or_" '[Bool, Bool] Bool
  -- Pair
  PairW :: forall a b. BaseWitness "pair_" '[a, b] (Prod a b)
  FstW :: forall a b. BaseWitness "fst_" '[Prod a b] a
  SndW :: forall a b. BaseWitness "snd_" '[Prod a b] b  
  -- Sum
  InjLeftW :: forall a b. BaseWitness "injectLeft_" '[a] (Sum a b)
  InjRightW ::forall a b. BaseWitness "injectRight_" '[b] (Sum a b)
  -- Set
  SubsetW :: (Ord a, Show a, Typeable a) => BaseWitness "subset_" '[Set a, Set a] Bool
  DisjointW :: (Ord a, Show a, Typeable a) => BaseWitness "disjoint_" '[Set a, Set a] Bool
  MemberW :: (Ord a, Show a, Typeable a) => BaseWitness "member_" '[a, Set a] Bool
  SingletonW :: Ord a => BaseWitness "ssingleton_" '[a] (Set a)
  UnionW :: (Ord a, Show a, Typeable a) => BaseWitness "union_" '[Set a, Set a] (Set a)
  FromListW :: Ord a => BaseWitness "fromList_" '[[a]] (Set a)
  -- Map
  DomW :: forall k v. (Typeable k,Typeable v,Ord k) => BaseWitness "dom_" '[Map k v] (Set k)
  RngW :: forall k v. (Typeable k,Typeable v) => BaseWitness "rng_" '[Map k v] [v]
  LookupW :: forall k v. (Typeable k,Typeable v,Ord k) => BaseWitness "lookup_" '[k, Map k v] (Maybe v)
  -- Generic (SimpleRep)
  FromGenericW :: forall a. HasSimpleRep a => BaseWitness "fromGenericFn" '[SimpleRep a] a 
  ToGenericW :: forall a. HasSimpleRep a => BaseWitness "toGenericFn" '[a] (SimpleRep a)
  
deriving instance Eq (BaseWitness s dom rng)

instance KnownSymbol s => Show (BaseWitness s dom rng) where
  show (_ :: BaseWitness s dom rng) = show (ppSymbol (SSymbol @s))

baseSem :: BaseWitness sym dom rng -> FunTy dom rng
baseSem EqualW = (==)
baseSem ElemW = elem
baseSem NotW = not
baseSem OrW = (||)
baseSem PairW = Prod
baseSem FstW = prodFst
baseSem SndW = prodSnd
baseSem InjLeftW = SumLeft
baseSem InjRightW = SumRight
baseSem SubsetW = Set.isSubsetOf
baseSem DisjointW = Set.disjoint
baseSem MemberW = Set.member
baseSem SingletonW = Set.singleton
baseSem UnionW = Set.union
baseSem FromListW = Set.fromList
baseSem DomW = Map.keysSet
baseSem RngW = Map.elems
baseSem LookupW = Map.lookup
baseSem FromGenericW = fromSimpleRep
baseSem ToGenericW = toSimpleRep

-- -----------------------------------
-- Patterns over Function Symbols

-- | When writing patterns over (App witness arglist), we have to obtain proof that 'witness' 
--   is some particular constructor of BaseWitness (or some other witness type). For example
--   suppose we were writing a pattern for (PairPat x y) <- (App (x::(f s d r)) (x :> y :> Nil)) 
--   we want proof that (x:: f s d r) is (PairW :: BaseWitness "pair_" '[a, b] (Prod a b))
--   So we test 4 things 
--   (1) f :~: BaseWitness
--   (2) s :~: "pair_" 
--   (3) d :~: '[a,b]
--   (4) r :~: (Prod a b)
isBaseWit :: forall (a :: Symbol) (b :: [Type]) c f (x:: Symbol) (y:: [ Type ]) z. 
    (Typeable a, Typeable b,Typeable c,Typeable f,Typeable x,Typeable y, Typeable z) => 
    BaseWitness a b c -> 
    f x y z -> 
    (Maybe (BaseWitness x y z), Maybe (f :~: BaseWitness),Maybe(a :~: x),Maybe(b :~: y),Maybe (c :~: z))
isBaseWit t x = trace "HERE" $
   case (eqT @f @BaseWitness, eqT @a @x, eqT @b @y, eqT @c @z) of
     (p1@(Just Refl), p2@(Just Refl), p3@(Just Refl), p4@(Just Refl)) 
       -> if t==x then (Just t,p1,p2,p3,p4) else (Nothing,p1,p2,p3,p4)
     _ -> (Nothing,Nothing,Nothing,Nothing,Nothing)


pattern EqualPat :: 
  forall a rng . (Typeable a, Eq a) => 
  forall        . (rng ~ Bool,FunctionSymbol "==." BaseWitness '[a,a] Bool) 
                 => Term a -> Term a -> Term rng
pattern EqualPat x y <- App (isBaseWit (EqualW @a) -> (Just EqualW, Just Refl,Just Refl,Just Refl,Just Refl)) (x :> y :> Nil)
   where EqualPat x y = App EqualW (x :> y :> Nil)


pattern PairPat :: 
  forall a b rng . (Typeable a, Typeable b) => 
  forall         . (rng ~ Prod a b,FunctionSymbol "pair_" BaseWitness '[a,b] (Prod a b)) 
                 => Term a -> Term b -> Term rng
pattern PairPat x y <- App (isBaseWit (PairW @a @b) -> (Just PairW, Just Refl, Just Refl,Just Refl,Just Refl)) (x :> y :> Nil)
   where PairPat x y = App PairW (x :> y :> Nil)
   -- You can't actually apply PairPat, because when this pattern was written,
   -- there was no instance (FunctionSymbol "pair_" BaseWitness '[a,b] (Prod a b)) yet!

pattern FromGenericPat :: 
  forall a rng . (Typeable a,Typeable (SimpleRep a),HasSimpleRep a) => 
  forall       . (rng ~ a,FunctionSymbol "fromGenericFn" BaseWitness '[SimpleRep a] a) 
              => Term (SimpleRep a) -> Term rng
pattern FromGenericPat x <- App (isBaseWit (FromGenericW @a) -> (Just FromGenericW, Just Refl, Just Refl,Just Refl,Just Refl)) (x :> Nil)
   where FromGenericPat x = App FromGenericW (x :> Nil)   

pattern ToGenericPat :: 
  forall a rng . (Typeable a,Typeable (SimpleRep a),HasSimpleRep a) => 
  forall       . (rng ~ SimpleRep a,FunctionSymbol "toGenericFn" BaseWitness '[a] (SimpleRep a))
              => Term a -> Term rng
pattern ToGenericPat x <- App (isBaseWit (ToGenericW @a) -> (Just ToGenericW, Just Refl, Just Refl,Just Refl,Just Refl)) (x :> Nil)
   -- where ToGenericPat x = App ToGenericW (x :> Nil)      

-- ====================================================================
-- The FunctionSymbol class for implementing new function symbols in
-- the first order logic. Note that a function symbol is first order
-- data, that uniquely identifies a higher order function (the 'semantics' method)
-- Sort of a combination of the FunctionLike and Functions classes
-- An instance "assigns" several functions to each Symbol 's' 
class ( KnownSymbol s

      , Show (t s dom rng)
      , Eq (t s dom rng)
      , Typeable t
      , TypeList dom
      , Typeable dom
      , All HasSpec dom
      , Typeable rng
      , HasSpec rng ) =>
      FunctionSymbol  (s::Symbol) (t:: Symbol -> [Type] -> Type -> Type) (dom::[Type]) rng | s -> t 
      where
      witness :: t s dom rng
      type Wit s :: Symbol -> [Type] -> Type -> Type
      semantics :: t s dom rng -> FunTy dom rng
      propagate :: Context s t dom rng hole -> Specification rng -> Specification hole
      prop :: Ctx rng hole -> Specification rng -> Specification hole
      prop (Ctx text@(Context _ _)) = propagate text 
                  -- ^ The pattern match brings the FunctionSymbol constraint into scope

-- ================ Probably Move this, but it was good practice since it is simple
instance FunctionSymbol "not_" BaseWitness '[Bool] Bool where
    witness = NotW
    type Wit "not_" = BaseWitness
    semantics = baseSem
    propagate ctxt (ExplainSpec [] s) = propagate ctxt s
    propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
    propagate (Context NotW (HOLE End)) TrueSpec = TrueSpec
    propagate (Context NotW (HOLE End))  (ErrorSpec msgs) = ErrorSpec msgs
    propagate (Context NotW (HOLE End)) (SuspendedSpec v ps) = 
      constrained $ \ v' -> Let (App NotW (v' :> Nil)) (v :-> ps)
    propagate (Context NotW (HOLE End)) spec = caseBoolSpec spec (equalSpec . not)
    propagate ctx _ = error ("NotW\nUnreachable context, too many args\n"++show ctx)

not_ :: Term Bool -> Term Bool
not_ = appTerm NotW

caseBoolSpec :: forall a. HasSpec a => Specification Bool -> (Bool -> Specification a) -> Specification a
caseBoolSpec = undefined
   where _ = emptySpec @a

instance HasSpec a => FunctionSymbol "==."  BaseWitness '[a, a] Bool where
    witness = EqualW
    type Wit "==." = BaseWitness 
    semantics = baseSem

    propagate ctxt (ExplainSpec [] s) = propagate ctxt s
    propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
    propagate _ (ErrorSpec err) = ErrorSpec err

    propagate (Context EqualW (HOLE (x :<| End))) (SuspendedSpec v ps) =
      constrained $ \v' -> Let (App EqualW (v' :> Lit x :> Nil)) (v :-> ps)
    propagate (Context EqualW ( x :>| (HOLE End))) (SuspendedSpec v ps) = 
      constrained $ \v' ->  Let (App EqualW (Lit x :> v' :> Nil)) (v :-> ps)
    
    propagate (Context EqualW (HOLE (a :<| End)))  spec =  
      caseBoolSpec spec $ \case {True -> equalSpec a; False -> notEqualSpec a}
    propagate (Context EqualW (a :>| (HOLE End))) spec = 
      caseBoolSpec spec $ \case { True -> equalSpec a; False -> notEqualSpec a}
    propagate ctx _ = error ("EqualW\nUnreachable context, too many args\n"++show ctx)      


(==.) :: forall a. (HasSpec a) => Term a -> Term a -> Term Bool
(==.) = appTerm EqualW

-- =================================================
-- Term

data Term a where
  App :: forall sym t dom rng. 
        (FunctionSymbol sym t dom rng, Typeable t, Typeable sym, Typeable dom, All HasSpec dom, Typeable rng, HasSpec rng) => 
        t sym dom rng -> List Term dom -> Term rng
  Lit :: Show a => a -> Term a
  V :: HasSpec a => Var a -> Term a

instance Eq a => Eq (Term a) where
  V x == V x' = x == x'
  Lit a == Lit b = a == b
  App (w1 :: x1) (ts :: List Term dom1) == App (w2 :: x2) (ts' :: List Term dom2) =
    case (eqT @dom1 @dom2, eqT @x1 @x2) of
      (Just Refl,Just Refl) -> 
            w1 == w2
            && same ts ts' 
      _ -> False
  _ == _ = False  

same :: All HasSpec as => List Term as -> List Term as -> Bool
same Nil Nil = True
same (x :> xs) (y :> ys) = x==y && same xs ys  

appSym :: forall sym t as b. FunctionSymbol sym t as b => t sym as b -> List Term as -> Term b
appSym w xs = App w xs


-- | This extracts the semantics of a witness (i.e. a function over Terms) 
--   Recall FunctionSymbols are functions that you can use when writing Terms
--   Usually the Haskel name ends in '_', i.e. not_, subset_ ,lookup_
--   And infix FunctionSymbols names end in '.', ie. ==. , <=. etc.
--   E.g  app NotW :: Term Bool -> Term Bool
--        app NotW (lit False)  ==reducesto==> not_ False
--   this functionality is embedded in the Haskel function not_
--   Note the witness (NotW) must have a FunctionSymbol instance like
--   instance FunctionSymbol "not_" BaseWitness '[Bool] Bool where ...
--             Name in Haskell^      ^  its arguments^   ^ its result
--                  The type of NotW |
appTerm :: forall sym t ds b. FunctionSymbol sym t ds b => t sym ds b -> FunTy (MapList Term ds) (Term b)
appTerm sym = curryList @ds (App @sym @t @ds @b sym)  

-- ===========================================
-- Binder

data Binder a where
  (:->) ::
    HasSpec a =>
    Var a ->
    Pred ->
    Binder a

deriving instance Show (Binder a)

bind :: (HasSpec a,IsPred p) => (Term a -> p) -> Binder a
bind bodyf = newv :-> bodyPred
  where
    bodyPred = toPred body
    newv = Var (nextVar bodyPred) "v"
    body = bodyf (V newv)

    nextVar q = 1 + bound q

    boundBinder :: Binder a -> Int
    boundBinder (x :-> p) = max (nameOf x) (bound p)

    bound (Explain _ p) = bound p
    bound (Subst x _ p) = max (nameOf x) (bound p)
    bound (And  ps) = maximum $ (-1) : map bound ps -- (-1) as the default to get 0 as `nextVar p`
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

data Pred  where
  Monitor :: ((forall a. Term a -> a) -> Property -> Property) -> Pred
  And  :: [Pred] -> Pred 
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

instance (IsPred p) => IsPred [p] where
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

showType :: forall t. Typeable t => String
showType = show (typeRep (Proxy @t))

ppSymbol :: KnownSymbol a => (SSymbol a) -> Doc ann
ppSymbol (_ :: SSymbol z) = fromString (symbolVal (Proxy @z))

-- ==========================================================================
-- Pretty and Show instances

extractSym :: 
  forall (a :: Symbol) (b :: Symbol) . 
  (KnownSymbol a) => 
  SSymbol b -> Maybe (b :~: a)
extractSym b = testEquality b (SSymbol @a) 

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

instance Show a => Pretty (Term a) where
  pretty = prettyPrec 0

instance Show a => Show (Term a) where
  showsPrec p t = shows $ pretty (WithPrec p t)

-- ------------ Pred -----------------

instance Pretty Pred where
  pretty = \case
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


-- ==============================================
-- Language constructs 

constrained ::
  forall a p.
  (IsPred p, HasSpec a) =>
  (Term a -> p) ->
  Specification a
constrained body =
  let x :-> p = bind body
   in SuspendedSpec x p

assertExplain ::
  IsPred p =>
  NE.NonEmpty String ->
  p ->
  Pred
assertExplain nes p = Explain nes (toPred p)

assert ::
  IsPred p =>
  p ->
  Pred 
assert p = toPred p

forAll ::
  ( Forallable t a
  , HasSpec t
  , HasSpec a
  , IsPred p
  ) =>
  Term t ->
  (Term a -> p) ->
  Pred 
forAll tm = mkForAll tm . bind

mkForAll ::
  ( Forallable t a
  , HasSpec t
  , HasSpec a
  ) =>
  Term t ->
  Binder a ->
  Pred 
mkForAll (Lit (forAllToList -> [])) _ = TruePred
mkForAll _ (_ :-> TruePred) = TruePred
mkForAll tm binder = ForAll tm binder

exists ::
  forall a p.
  (HasSpec a, IsPred p) =>
  ((forall b. Term b -> b) -> GE a) ->
  (Term a -> p) ->
  Pred 
exists sem k =
  Exists sem $ bind k

unsafeExists ::
  forall a p.
  (HasSpec a, IsPred p) =>
  (Term a -> p) ->
  Pred
unsafeExists = exists (\_ -> fatalError1 "unsafeExists")

letBind ::
  ( HasSpec a
  , IsPred p
  ) =>
  Term a ->
  (Term a -> p) ->
  Pred 
letBind tm@V {} body = toPred (body tm)
letBind tm body = Let tm (bind body)

reify ::
  ( HasSpec a
  , HasSpec b
  , IsPred p 
  ) =>
  Term a ->
  (a -> b) ->
  (Term b -> p) ->
  Pred 
reify t f body =
  exists (\eval -> pure $ f (eval t)) $ \x ->
    [ reifies x t f
    , Explain (pure ("reifies " ++ show x)) $ toPred (body x)
    ]

-- | Wrap an 'Explain' around a Pred, unless there is a simpler form.
explanation :: NE.NonEmpty String -> Pred -> Pred
explanation _ p@DependsOn {} = p
explanation _ TruePred = TruePred
explanation es (FalsePred es') = FalsePred (es <> es')
explanation es (Assert t) = Explain es $ Assert t
explanation es p = Explain es p

-- | Add QuickCheck monitoring (e.g. 'Test.QuickCheck.collect' or 'Test.QuickCheck.counterexample')
--   to a predicate. To use the monitoring in a property call 'monitorSpec' on the 'Specification'
--   containing the monitoring and a value generated from the specification.
monitor :: ((forall a. Term a -> a) -> Property -> Property) -> Pred
monitor = Monitor

reifies :: (HasSpec a, HasSpec b) => Term b -> Term a -> (a -> b) -> Pred 
reifies = Reifies

dependsOn :: (HasSpec a, HasSpec b) => Term a -> Term b -> Pred
dependsOn = DependsOn

lit :: Show a => a -> Term a
lit = Lit

genHint :: forall t. HasGenHint t => Hint t -> Term t -> Pred
genHint = GenHint

-- =========================================================
-- HasSpec Bool (Temporary solution)


instance HasSpec Bool where
  type TypeSpec Bool = Set Bool

  emptySpec = Set.fromList[True,False]
  combineSpec x y = TypeSpec (Set.intersection x y) []
  genFromTypeSpec set = pureGen $ elements (Set.toList set)

  -- | Check conformance to the spec.
  conformsTo x set = Set.member x set

  -- | Shrink an `a` with the aide of a `TypeSpec`
  shrinkWithTypeSpec _ _ = []

  toPreds term set = case Set.toList set of 
        [] -> FalsePred (pure "Empty Set in toPreds")
        [True] -> Assert $ term ==. lit True
        [False] -> Assert $ term ==. lit False
        [False,True] -> TruePred
        xs -> FalsePred $ NE.fromList["Spec in toPreds has too may entries ",show xs]
  cardinalTypeSpec set = equalSpec(toInteger $ Set.size set)
  cardinalTrueSpec = MemberSpec (pure 2)

-- ==========================================================
-- Contexts 

-- | A context identifies a Term with exactly 1 variable. It is a key part
--   of implementing the 'propagate' method instances. We have patterns that
--   Specialize Contexts for Function symbols with 1 or 2 arguments
--   function symbols with 3 or more arguments can us the Raw Constructors.
--   Ctx1of1 :: witness sym '[hole] rng -> Hole hole -> Contxt hole rng
--   Ctx1of2 :: witness sym '[hole,b] rng -> Hole hole -> b -> Contxt hole rng
--   Ctx2of2 :: witness sym '[b,hole] rng -> b -> Hole hole -> Contxt hole rng

data Context s t dom rng hole where 
  Context :: FunctionSymbol s t dom rng => 
             t s dom rng -> CtxtList Pre dom hole -> Context s t dom rng hole

deriving instance All Show dom => Show (Context s t dom rng hole)

data Ctx rng hole where 
  Ctx :: (HasSpec hole) => Context s t dom rng hole -> Ctx rng hole  
 
data Mode = Pre | Post

-- Note arrows point towards the HOLE
infixr 5 :<|
infixr 1 :>|

data CtxtList (x::Mode) (as :: [Type]) (hole :: Type) where
  End :: CtxtList Post '[] h
  (:<|) :: Typeable a => a -> CtxtList Post as h -> CtxtList Post (a : as) h
  HOLE :: CtxtList Post as i -> CtxtList Pre (b : as) b
  (:>|) :: Typeable a => a -> CtxtList Pre as h -> CtxtList Pre (a : as) h

-- Here is an Example
c6 :: CtxtList Pre [Bool, String, b, Bool, ()] b
c6 =  True :>| ("abc" :: String)  :>| (HOLE $ True :<| () :<| End)
--                      rightmost ^   ^--parens here ------------^
-- We need to put parenthesis around the segment from (Hole $ .... End), 
-- before the rightmost (:>|), to make the fixity work. 

t6 :: List [] [Bool, String, Int, Bool, ()]
t6 = [] :> ["abc"] :> [67,12] :> [True,False] :> [()] :> Nil

-- Show instance puts parens in the output correctly to 
-- remind us where the parens go.
instance (All Show as) => Show (CtxtList m as h) where
  show End = "End)"
  show (x :<| xs) = show x++" :<| "++show xs
  show (HOLE xs) = "(HOLE $ "++show xs
  show (x :>| xs) = show x ++ " :>| "++show xs

pattern Ctx1of1 :: 
   forall rng hole .  () => 
   forall (f :: Symbol -> [Type] -> Type -> Type) (sym :: Symbol) . 
             (FunctionSymbol sym f '[hole] rng) => 
             f sym '[hole] rng -> (CtxtList Pre '[hole] hole) -> Ctx rng hole
pattern Ctx1of1 wit lst <- Ctx (Context wit lst@(HOLE End))
  where Ctx1of1 wit lst = Ctx (Context wit lst)

{- Some thing is wrong here
pattern Ctx1of2 :: 
   forall rng hole .  () => 
   forall (f :: Symbol -> [Type] -> Type -> Type) (sym :: Symbol) x . 
             (FunctionSymbol sym f '[hole,x] rng) => 
             f sym '[hole,x] rng -> (CtxtList Pre '[hole,x] hole) -> Ctx rng hole
pattern Ctx1of2 wit lst x <- Ctx (Context wit lst@(HOLE ( x :<| End)))
  where Ctx1of2 wit lst x = Ctx (Context wit lst)  

pattern Ctx2of2 :: 
   forall rng hole .  () => 
   forall (f :: Symbol -> [Type] -> Type -> Type) (sym :: Symbol) x . 
             (FunctionSymbol sym f '[x,hole] rng) => 
             f sym '[x,hole] rng -> (CtxtList Pre '[x,hole] hole) -> Ctx rng hole
pattern Ctx2of2 wit lst <- Ctx (Context wit lst@(x :>| (HOLE End)))
  where Ctx2of2 wit lst = Ctx (Context wit lst)    
-}
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
  m (Ctx a v)
toCtx v term = case term of  
    (App w ( V v' :> Nil))
      | Just Refl <- eqVar v v' -> pure $ Ctx (Context w (HOLE End))
      | otherwise -> fatalError1 "oops"     
    (App w ( V v' :> x :> Nil))
      | Just Refl <- eqVar v v' -> do
          y <- checkForVar x 
          pure $ Ctx (Context w (HOLE ( y :<| End)))
    (App w (x :> V v' :> Nil))
      | Just Refl <- eqVar v v' -> do
          y <- checkForVar x  
          pure $ Ctx (Context w ( y :>| (HOLE End)))
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
 where checkForVar:: forall t. Term t -> m t
       checkForVar = \case
         Lit a -> pure a
         V v' | Just Refl <- eqVar v v' -> fatalError $
                      NE.fromList [ "Trying to compute a context for variable "++show v
                                  , "From the term " ++ show term
                                  , show v++" appears more than once in the term"]
              | otherwise -> fatalError $
                      NE.fromList [ "Trying to compute a context for variable "++show v
                                  , "From the term " ++ show term
                                  , "An unknown variable " ++ show v' ++" occurs"]
         App w (ts :: List Term dom)-> do
              vs <- mapMList (fmap Identity . checkForVar) ts
              pure $ uncurryList_ runIdentity (semantics w) vs

-- ====================================================================

{- Might be usefull, might not
type family PP (a :: Nat) (n :: Type) (b :: [Type]) ::  Type where
  PP 0 t (t ': _)  = t :~: t
  PP 1 t ( _ ': t ': _) = t :~: t
  PP 2 t ( _ ': _ ': t ': _) = t :~: t
  PP 3 t ( _ ': _ ': _ ': t ': _) = t :~: t
  PP 4 t ( _ ': _ ': _ ': _ ': t ': _) = t :~: t
  

holeType :: forall xs f t . (CtxtList Pre xs t) -> List f xs -> (t :~: t)
holeType (HOLE End) ( _ :> Nil) = Refl
holeType (HOLE ( _ :<| xs)) ( _ :> ys) = Refl
holeType (x :>| xs) (y :> ys) = holeType xs ys

holeV :: CtxtList Pre xs t -> List f xs -> f t
holeV (HOLE End) ( x :> Nil) = x
holeV (HOLE ( _ :<| xs)) ( y :> ys) = y
holeV (x :>| xs) (y :> ys) = holeV xs ys
  
castUnaryCtxt :: forall hole x . CtxtList Pre '[x] hole -> x -> hole 
castUnaryCtxt (HOLE End) x = x
-}