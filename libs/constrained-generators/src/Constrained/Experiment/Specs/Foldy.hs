{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module FoldyExperiment where


import Constrained.Experiment.Witness
import Constrained.Experiment.Base
import Constrained.Experiment.Conformance(mapSpec,conformsToSpec,satisfies)
import Constrained.Experiment.Generic
import Constrained.Experiment.Syntax()
import Constrained.Experiment.NumSpec
import Constrained.Experiment.TheKnot
import Debug.Trace

import Constrained.Core
import Constrained.Env
import Constrained.GenT
import Constrained.Graph hiding (dependency, irreflexiveDependencyOn, noDependencies)
import qualified Constrained.Graph as Graph
import Constrained.List
import Constrained.SumList (Cost (..), Solution (..), pickAll)
import Constrained.Univ
import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable
import Data.Kind
import Data.List (intersect, isPrefixOf, isSuffixOf, nub, partition, (\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Semigroup (Any (..), Max (..), getAll, getMax, sconcat)
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Typeable
import Data.Word
import GHC.Generics
import GHC.Int
import GHC.Natural
import GHC.Real
import GHC.Stack
import GHC.TypeLits
import Prettyprinter hiding (cat)
import System.Random
import System.Random.Stateful
import Test.QuickCheck(Arbitrary(..))
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
-- import Constrained.SumList
import Data.Int
import Data.Word

-- =============================================================
-- All Foldy class instances are over Numbers (so far).
-- So that's why the Foldy class is in the NumberSpec module.
-- Foldy class requires higher order functions, so here they are.
-- Note this is a new witness type, different from BaseWitness
-- but serving the same purpose. Note it can take Witnesses from
-- other classes as inputs. See FlipW amd ComposeW
-- ==============================================================

data FunW (c :: Constraint) (sym :: Symbol) (dom :: [Type]) (rng :: Type) where
  IdW :: forall a c h. FunW () "id_" '[a] a
  ComposeW ::
    forall b c1 c2 s1 s2 t1 t2 a r.
    ( FunSym c1 s1 t1 '[b] r
    , FunSym c2 s2 t2 '[a] b
    , HasSpec b
    ) => t1 c1 s1 '[b] r -> t2 c2 s2 '[a] b -> FunW (c1,c2) "composeFn" '[a] r
  FlipW ::
    forall c sym t a b r h.
    (FunSym c sym t '[a, b] r) => 
    t c sym '[a, b] r -> FunW c "flip_" '[b, a] r

funSem :: c => FunW c sym dom rng -> FunTy dom rng
funSem IdW = id
funSem (ComposeW f g) = (\a -> semantics f (semantics g a))
funSem (FlipW (f :: g c s d r)) = flip (semantics f)

instance Witness FunW where
  semantics = funSem

instance KnownSymbol s => Show (FunW c s dom rng) where
  show IdW = "IdW[id_]"
  show (FlipW f) = "(FlipW " ++ show f ++ ")[flip_]"
  show (ComposeW x y) = "(ComposeW " ++ show x ++ " " ++ show y ++ ")[composeFn]"

instance Eq (FunW c s dom rng) where
  IdW == IdW = True
  FlipW t1 == FlipW t2 = compareWit t1 t2
  ComposeW f f' == ComposeW g g' = compareWit f g && compareWit f' g'

compareWit ::
  forall s1 t1 c1 c2 bs1 r1 s2 t2 bs2 r2.
  (FunSym c1 s1 t1 bs1 r1, FunSym c2 s2 t2 bs2 r2) =>
  t1 c1 s1 bs1 r1 -> t2 c2 s2 bs2 r2 -> Bool
compareWit x y = case (eqT @t1 @t2, eqT @c1 @c2, eqT @s1 @s2, eqT @bs1 @bs2, eqT @r1 @r2) of
  (Just Refl, Just Refl, Just Refl, Just Refl, Just Refl) -> x == y
  x -> trace ("compareWit " ++ show x) False


-- ===================================
-- FunSym instances for IdW, FlipW and ComposeW
-- Also their Haskell implementations id_ flip_ composeFn

instance HasSpec a => FunSym () "id_" FunW '[a] a where

  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate ctxt (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context _ IdW (HOLE End)) spec = spec
  propagate ctxt _ = ErrorSpec (NE.fromList ["IdW (id_)", "Unreachable context, too many args", show ctxt])

  mapTypeSpec IdW ts = typeSpec ts
  rewriteRules IdW (x :> Nil) Evidence = Just x
    

id_ :: forall a. HasSpec a => Term a -> Term a
id_ = appTerm IdW


instance
  (forall sym t. FunSym c sym t '[a, b] r, All Typeable [a, b, r]) =>
  FunSym c "flip_" FunW '[b, a] r
  where

  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate ctxt (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context ev (FlipW f) (HOLE (v :<| End))) spec = propagate (Context ev f (v :>| HOLE End)) spec
  propagate (Context ev (FlipW f) (v :>| (HOLE End))) spec = propagate (Context ev f (HOLE $ v :<| End)) spec
  propagate ctxt@(Context Evidence _ _) _ = ErrorSpec (NE.fromList ["FlipW (flip_)", "Unreachable context, too many args", show ctxt])

  -- Note we need Evidence to apply App to f 
  rewriteRules (FlipW f) (a@Lit {} :> b :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW f) (a :> b@Lit {} :> Nil) Evidence = Just $ App f (b :> a :> Nil)
  rewriteRules (FlipW {}) _  Evidence = Nothing

flip_ ::
  forall (c :: Constraint) (t :: FSType) (sym :: Symbol) a b r.
  (HasSpec b, HasSpec a, HasSpec r, c, forall c sym t. FunSym c sym t '[a, b] r) =>
  t c sym '[a, b] r -> Term b -> Term a -> Term r
flip_ x = appTerm (FlipW x)


instance
  ( All Typeable [a, r]
  , Typeable c1
  , Typeable c2
  ) =>
  FunSym (c1,c2) "composeFn" FunW '[a] r
  where

  propagate ctxt (ExplainSpec [] s) = propagate ctxt s
  propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
  propagate ctxt (ErrorSpec msgs) = ErrorSpec msgs
  propagate (Context Evidence (ComposeW (f :: t1' c1' s1' '[b'] r') (g :: t2' c2' s2' '[a'] b'')) (HOLE End)) spec = 
        propagate @c2' @s2' @t2' @'[a'] @b' (Context (Evidence @c2') g (HOLE End)) $ 
        propagate @c1' @s1' @t1' @'[b'] @r' (Context (Evidence @c1') f (HOLE End)) spec 
  propagate ctxt@(Context Evidence _ _) _ = ErrorSpec (NE.fromList ["ComposeW (composeFn)", "Unreachable context, too many args", show ctxt])
  
  mapTypeSpec (ComposeW g h) ts = mapSpec g . mapSpec h $ typeSpec ts

  -- Note we need the Evidence to apply App to f, and to apply App to g
  rewriteRules (ComposeW f g) (x :> Nil) Evidence = Just $ App f (App g (x :> Nil) :> Nil)

compose_ :: forall b c1 c2 s1 s2 t1 t2 a r.
  ( AppRequires c1 s1 t1 '[b] r
  , AppRequires c2 s2 t2 '[a] b )
  => 
  t1 c1 s1 '[b] r -> t2 c2 s2 '[a] b -> Term a -> Term r
compose_ f g = appTerm $ ComposeW f g -- @b @c1 @c2 @s1 @s2 @t1 @t2 @a @r f g


data Fun dom rng where 
  Fun :: forall c s t dom rng. 
         FunSym c s t dom rng => 
         Evidence c -> t c s dom rng -> Fun dom rng

instance Show (Fun d r) where
  show (Fun Evidence (f :: t c s dom rng)) = "(Fun "++ show f++")"

extractf :: forall c s t d r . FSPre c s t d r => Fun d r -> Maybe (t c s d r)
extractf (Fun _ (x :: t1 c1 s1 d1 r1)) =
      case (eqT @t @t1,eqT @c @c1,eqT @s @s1,eqT @d @d1,eqT @r @r1) of
        (Just Refl,Just Refl,Just Refl,Just Refl,Just Refl) -> Just x
        _ -> Nothing

eqFn :: forall a. (Typeable a,Eq a) => Fun '[a,a] Bool
eqFn = Fun (Evidence @(Eq a)) EqualW 

composeFn :: HasSpec b => Fun '[b] c -> Fun '[a] b -> Fun '[a] c
composeFn (Fun (Evidence :: Evidence x) f) (Fun (Evidence :: Evidence y) g) = (Fun (Evidence @(x,y)) (ComposeW f g))

-- flipFn :: forall a b r. (All HasSpec '[b, a], HasSpec r) => Fun '[a,b] r -> Fun '[b,a] r
-- flipFn (Fun (f :: t c s '[a',b'] r')) = Fun (FlipW (f :: t c s '[a',b'] r'))
-- flipFn (Fun f) = Fun (FlipW f)

idFn :: HasSpec a => Fun '[a] a 
idFn = Fun (Evidence @()) IdW

 
-- =======================================================
-- All he Foldy class instances are intimately tied to
-- Numbers. But that is not required, but this is a
-- convenient place to put the code.
-- =======================================================

class (HasSpec a,FunSym (NumLike a) "addFn" IntW '[a, a] a,NumLike a) => Foldy a where
  genList ::
    MonadGenError m => Specification a -> Specification a -> GenT m [a]
  theAddFn :: IntW (NumLike a) "addFn" '[a,a] a
  theAddFn = AddW
  theZero :: a
  theZero = 0
  genSizedList ::
    MonadGenError m =>
    Specification Integer -> Specification a -> Specification a -> GenT m [a]
  noNegativeValues :: Bool

instance Foldy Integer where
   noNegativeValues = False
   genList = undefined
   genSizedList = undefined

instance Foldy Int where
   noNegativeValues = False
   genList = undefined
   genSizedList = undefined   

instance Foldy Int8 where
   noNegativeValues = False
   genList = undefined
   genSizedList = undefined      

instance Foldy Int16 where
   noNegativeValues = False
   genList = undefined
   genSizedList = undefined      

instance Foldy Int32 where
   noNegativeValues = False
   genList = undefined
   genSizedList = undefined      

instance Foldy Int64 where
   noNegativeValues = False
   genList = undefined
   genSizedList = undefined               

instance Foldy Natural where
   noNegativeValues = True
   genList = undefined
   genSizedList = undefined 

instance Foldy Word8 where
   noNegativeValues = True
   genList = undefined
   genSizedList = undefined   

instance Foldy Word16 where
   noNegativeValues = True
   genList = undefined
   genSizedList = undefined   

instance Foldy Word32 where
   noNegativeValues = True
   genList = undefined
   genSizedList = undefined   

instance Foldy Word64 where
   noNegativeValues = True
   genList = undefined
   genSizedList = undefined   

adds :: NumLike a => Foldy a => [a] -> a
adds = foldr (semantics theAddFn) theZero


data FoldSpec a where
  NoFold :: FoldSpec a
  FoldSpec ::
    forall b a c t sym.
    ( HasSpec a
    , HasSpec b
    , Foldy b
  --  , FunSym c sym t '[a] b
    ) =>
    -- t c sym '[a] b -> Specification b -> FoldSpec a
    Fun '[a] b -> Specification b -> FoldSpec a

{-
-- We need Arbitrary Specification to do this 
instance {-# OVERLAPPABLE #-} (Arbitrary (Specification a), Arbitrary (TypeSpec a), Foldy a) => Arbitrary (FoldSpec a) where
  arbitrary = oneof [FoldSpec IdW <$> arbitrary, pure NoFold]
  shrink NoFold = []
  shrink (FoldSpec (sameFunSym (IdW @a) -> Just(idW,Refl,Refl,Refl,Refl,Refl)) spec) = FoldSpec idW <$> shrink spec
  shrink FoldSpec {} = [NoFold]
-}

preMapFoldSpec :: (FunSym c sym t '[a] b,HasSpec a) => Fun '[a] b -> FoldSpec b -> FoldSpec a
preMapFoldSpec _ NoFold = NoFold
preMapFoldSpec f (FoldSpec g s) = FoldSpec (composeFn g f) s


combineFoldSpec :: FoldSpec a -> FoldSpec a -> Either [String] (FoldSpec a)
combineFoldSpec NoFold s = pure s
combineFoldSpec s NoFold = pure s
combineFoldSpec (FoldSpec (Fun ev f) s) (FoldSpec (Fun _ g) s') =
  case sameFunSym f g of
    Just(h,Refl,Refl,Refl,Refl,Refl) -> pure $ FoldSpec (Fun ev f) (s <> s')
    Nothing-> Left ["Can't combine fold specs on different functions", "  " ++ show f, "  " ++ show g]


conformsToFoldSpec :: forall a. [a] -> FoldSpec a -> Bool
conformsToFoldSpec _ NoFold = True
conformsToFoldSpec xs (FoldSpec (Fun Evidence f) s) = adds (map (semantics f) xs) `conformsToSpec` s


-- This is how Fold interacts with List. Here is thw Witness type for List

data ListW (c :: Constraint) (s::Symbol) (args :: [Type]) (res :: Type) where
  FoldMap ::
    ( HasSpec a
    , Foldy b ) 
    =>
    Fun '[a] b -> ListW () "foldMap_" '[[a]] b
  SingletonList :: ListW () "singelton_" '[a] [a]
  AppendFn :: (Typeable a, Show a) => ListW () "append_" '[[a], [a]] [a]


foldMapFn ::
  forall a b.
  ( FunSym () "foldMap_" ListW '[[a]] b
  , HasSpec a
  , Foldy b
  ) =>
  Fun '[a] b  ->
  Fun '[[a]] b
foldMapFn f = Fun Evidence (FoldMap f)


{-
toPredsFoldSpec :: forall a b. (FunSym () "foldMap_" ListW '[[a]] b) => Term [a] -> FoldSpec a -> Pred
toPredsFoldSpec _ NoFold = TruePred
toPredsFoldSpec x (FoldSpec funAB sspec) =
  satisfies (appFun (foldMapFn funAB) x) sspec
  

appFun :: Fun '[[a]] b -> Term [a] -> Term b
appFun = undefined -- FIX ME

-}