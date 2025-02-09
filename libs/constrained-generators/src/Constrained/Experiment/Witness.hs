{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Show Evidence
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A First-order typed logic has 4 components
--     1. Terms        (Variables (x), Constants (5), and Applications (F x 5) i.e. FunctionSymbol term1 .. termn)
--     2. Predicates   (Ordered, Odd, ...)
--     3. Connectives  (And, Or, Not, =>, ...)
--     4. Quantifiers  (Forall, Exists)
--  In this module we talk about 3 things.
--     1. How do we use types to make the logic terms well formed.
--     2. What are some of the properties that FunctionSymbols have
module Constrained.Experiment.Witness where

import Constrained.Experiment.Generic -- (Sum(..),Prod(..),SOPOF,SOP)

import Constrained.Core (Evidence (..))
import Constrained.List
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Typeable
import GHC.TypeLits hiding (Text)
import Prettyprinter hiding (cat)

-- ==========================================================================
-- Printing a few things

showType :: forall t. Typeable t => String
showType = show (typeRep (Proxy @t))

ppSymbol :: KnownSymbol a => (SSymbol a) -> Doc ann
ppSymbol (_ :: SSymbol z) = fromString (symbolVal (Proxy @z))

instance forall (c :: Constraint). Typeable c => Show (Evidence c) where
  show _ = "Evidence (" ++ showType @c ++ ")"

-- ============================================================================
data Possible c where Possible :: forall (c :: Constraint). c => Possible c
invoke :: forall (c :: Constraint). c => Possible c -> Evidence c
invoke Possible = Evidence


data ConName a where 
    ConName :: forall (c :: Constraint) . String -> ConName c

instance Show (ConName c) where
  show (ConName s) = "(ConName "++s++")"

-- | A FunctionSymbol has 4 components, all reflected in its type,
--   And a bunch of operations, reflected in the classes for which the type has instances.
--   The first class 'Witness' witnesses the semantic properties of a FunctionSymbol
--   What Haskell function gives it a meaning: 'semantics'
--   What Haskell Constraints must be met to do so: 'getevidence'
--   The type 't' witnesses one or more FunctionSymbols. There can be several witness types
--   each having a Witness instance. This is one step in extending the system.
class Witness (t :: Constraint -> Symbol -> [Type] -> Type -> Type) where
  semantics :: forall c s d r. c => t c s d r -> FunTy d r -- e.g. FunTy '[a,Int] Bool == a -> Int -> Bool
  constraint :: forall c s f d r . Show (f c s d r) => f c s d r -> ConName c
  constraint (t :: tx con sym dom rng) = ConName @con (show t)
 
-- | Here is one witness type, witnessing FunctionSymbols over Bool, List,
-- Set, Map, Products, Sums, and the types used to interface with GHC.Generics.
-- There will be others, and  if you want.you can add your own types, and witnesses
-- to your own FunctionSymbols over those types. These are some of the Base
-- FunctionSymbols that come with the system "out of the box"
data BaseW (c :: Constraint) (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  -- List
  ElemW :: forall a. BaseW (Eq a) "elem_" '[a, [a]] Bool
  -- Bool
  NotW :: BaseW () "not_" '[Bool] Bool
  OrW :: BaseW () "or_" '[Bool, Bool] Bool
  -- Pair
  PairW ::forall a b. BaseW () "pair_" '[a, b] (Prod a b)
  FstW :: forall a b. BaseW () "fst_" '[Prod a b] a
  SndW :: forall a b. BaseW () "snd_" '[Prod a b] b
  -- Sum
  InjLeftW :: forall a b. BaseW () "sumleft_" '[a] (Sum a b)
  InjRightW :: forall a b. BaseW () "sumright_" '[b] (Sum a b)
  -- Set
  SubsetW :: BaseW (Ord a) "subset_" '[Set a, Set a] Bool
  DisjointW :: BaseW (Ord a) "disjoint_" '[Set a, Set a] Bool
  MemberW :: BaseW (Ord a) "member_" '[a, Set a] Bool
  SingletonW :: BaseW (Ord a) "ssingleton_" '[a] (Set a)
  UnionW :: BaseW (Ord a) "union_" '[Set a, Set a] (Set a)
  FromListW :: BaseW (Ord a) "fromList_" '[[a]] (Set a)
  -- Map
  DomW :: forall k v. BaseW (Ord k) "dom_" '[Map k v] (Set k)
  RngW :: forall k v. BaseW () "rng_" '[Map k v] [v]
  LookupW :: forall k v. BaseW (Ord k) "lookup_" '[k, Map k v] (Maybe v)

deriving instance Eq (BaseW c s dom rng)

instance Show (BaseW c s dom rng) where
  show ElemW = "elem_"
  show NotW = "not_"
  show OrW = "or_"
  show PairW = "pair_"
  show FstW = "fst_"
  show SndW = "snd_"
  show InjLeftW = "sumleft_"
  show InjRightW = "sumright_"
  show SubsetW = "subset_"
  show DisjointW = "disjoint_"
  show MemberW = "member_"
  show SingletonW = "singleton_"
  show UnionW = "union_"
  show FromListW = "fromList_"
  show DomW = "dom_"
  show RngW = "rng_"
  show LookupW = "lookup_"

-- =================================================================
-- Witness class instance
-- =================================================================

-- | Haskell functions give meaning to the BaseW constructors
baseSem :: c => BaseW c sym dom rng -> FunTy dom rng
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

instance Witness BaseW where
  semantics = baseSem

{-
  getevidence (EqualW @a) = Evidence @(Eq a)
  getevidence (ElemW @a) = Evidence @(Eq a)
  getevidence NotW = Evidence @()
  getevidence OrW = Evidence @()
  getevidence (PairW @a @b) = Evidence @()
  getevidence (FstW @a @b) = Evidence @()
  getevidence (SndW @a @b) = Evidence @()
  getevidence (InjLeftW @a @b) = Evidence @()
  getevidence (InjRightW @a @b) = Evidence @()
  getevidence (SubsetW @a) = Evidence @(Ord a)
  getevidence (DisjointW @a) = Evidence @(Ord a)
  getevidence (MemberW @a) = Evidence @(Ord a)
  getevidence (SingletonW @a) = Evidence @(Ord a)
  getevidence (UnionW @a) = Evidence @(Ord a)
  getevidence (FromListW @a) = Evidence @(Ord a)
  getevidence (DomW @k @v) = Evidence @(Ord k)
  getevidence (RngW @k @v) = Evidence @()
  getevidence (LookupW @k @v) = Evidence @(Ord k)
-}
