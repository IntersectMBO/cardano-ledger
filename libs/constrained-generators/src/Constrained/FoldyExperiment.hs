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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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

import Debug.Trace
import Constrained.BaseExperiment
import Constrained.BaseExperiment(FunctionSymbol(witness))
import Constrained.GenericExperiment
import Constrained.SyntaxExperiment


import Constrained.Core
import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable
import Data.Kind
import Data.List (intersect, isPrefixOf, isSuffixOf, nub, partition, (\\))
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
import Test.QuickCheck hiding (Args, Fun, forAll,Witness,witness)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Constrained.Core
import Constrained.Env
import Constrained.GenT
import Constrained.Graph hiding (dependency, irreflexiveDependencyOn, noDependencies)
import qualified Constrained.Graph as Graph
import Constrained.List
import Constrained.SumList (Cost (..), Solution (..), pickAll)
import Constrained.Univ
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

-- =============================================================
-- All Foldy class instances are over Numbers (so far).
-- So that's why the Foldy class is in the NumberSpec module.
-- Foldy class requires higher order functions, so here they are.
-- Note this is a new witness type, different from BaseWitness
-- but serving the same purpose. Note it can take Witnesses from
-- other classes as inputs. See FlipW amd ComposeW
-- ==============================================================

data FunWitness (sym :: Symbol) (dom :: [Type]) (rng :: Type) where 
  IdW :: forall a. FunWitness "id_" '[a] a
  ComposeW :: forall s1 t1 s2 t2 a b c . 
              ( FunctionSymbol s1 t1 '[b] c
              , FunctionSymbol s2 t2 '[a] b ) =>
              t1 s1 '[b] c -> 
              t2 s2 '[a] b ->
              FunWitness "composeFn" '[a] c
  FlipW :: forall sym t a b c . 
           FunctionSymbol sym t '[a,b] c => 
           t sym '[a,b] c ->  FunWitness "flip_" '[b,a] c

funSem :: FunWitness sym dom rng -> FunTy dom rng
funSem IdW = id
funSem (ComposeW f g) = (\ a -> semantics f (semantics g a))
funSem (FlipW f) = flip (semantics f)           

instance KnownSymbol s => Show (FunWitness s dom rng) where
  show IdW = "IdW[id_]"
  show (FlipW f) = "(FlipW "++show f++")[flip_]"
  show (ComposeW x y) = "(ComposeW "++show x++" "++show y++")[composeFn]"

instance Eq (FunWitness s dom rng) where
  IdW == IdW = True
  FlipW t1 == FlipW t2 = compareWit t1 t2
  ComposeW f f' == ComposeW g g' = compareWit f g && compareWit f' g'

compareWit :: 
  forall s1 t1 bs1 c1 s2 t2 bs2 c2. 
         (FunctionSymbol s1 t1 bs1 c1, FunctionSymbol s2 t2 bs2 c2) => 
         t1 s1 bs1 c1 -> t2 s2 bs2 c2 -> Bool
compareWit x y = case (eqT @t1 @t2, eqT @s1 @s2, eqT @bs1 @bs2, eqT @c1 @c2) of
    (Just Refl, Just Refl, Just Refl, Just Refl) -> x==y
    x -> trace ("compareWit "++show x) False

-- ===================================
-- FunctionSymbol instances for IdW, FlipW and ComposeW 
-- Also their Haskell implementations id_ flip_ composeFn   

instance (HasSpec a) => FunctionSymbol "id_" FunWitness '[a] a where
    witness = "IdW[id_]"
   
    semantics = funSem
    propagate ctxt (ExplainSpec [] s) = propagate ctxt s
    propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
    propagate ctxt (ErrorSpec msgs) = ErrorSpec msgs
    propagate (Context IdW (HOLE End)) spec = spec
    propagate ctxt _ = ErrorSpec (NE.fromList["IdW (id_)","Unreachable context, too many args",show ctxt])

id_ :: forall a. (HasSpec a) => Term a -> Term a 
id_ = appTerm IdW

instance (HasSpec b, HasSpec a,forall sym t. FunctionSymbol sym t '[a,b] c,All Typeable [a,b,c]) =>
          FunctionSymbol "flip_" FunWitness '[b,a] c  where
    witness = "Flip w1 w2[flip_]"

    semantics = funSem
    propagate ctxt (ExplainSpec [] s) = propagate ctxt s
    propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
    propagate ctxt (ErrorSpec msgs) = ErrorSpec msgs
    propagate (Context (FlipW f) (HOLE (v :<| End))) spec = propagate (Context f (v :>| HOLE End)) spec
    propagate (Context (FlipW f) (v :>| (HOLE End))) spec = propagate (Context f (HOLE $ v :<| End)) spec
    propagate ctxt _ = ErrorSpec (NE.fromList["FlipW (flip_)","Unreachable context, too many args",show ctxt])

type W = Symbol -> [Type] -> Type -> Type
flip_ ::  forall (t :: W) (sym :: Symbol) a b c. 
                 (HasSpec b, HasSpec a, HasSpec c, forall sym t. FunctionSymbol sym t '[a,b] c) =>
                 t sym '[a,b] c -> Term b -> Term a -> Term c
flip_ x = appTerm (FlipW x)          

instance ( HasSpec a,HasSpec c,All Typeable [a,c]
         , forall s1 t1 b. FunctionSymbol s1 t1 '[b] c
         , forall s2 t2 b. FunctionSymbol s2 t2 '[a] b ) => 
        FunctionSymbol "composeFn" FunWitness '[a] c where
    witness = "ComposeW w1 w2[composeFn]"
    semantics = funSem
    propagate ctxt (ExplainSpec [] s) = propagate ctxt s
    propagate ctxt (ExplainSpec es s) = ExplainSpec es $ propagate ctxt s
    propagate ctxt (ErrorSpec msgs) = ErrorSpec msgs  
    propagate (Context (ComposeW f g) (HOLE End)) spec = 
        propagate (Context g (HOLE End)) $ propagate (Context f (HOLE End)) spec
    propagate ctxt _ = ErrorSpec (NE.fromList["ComposeW (composeFn)","Unreachable context, too many args",show ctxt])    


composeFn :: forall a b c (t1 :: W) (t2 :: W) s1 s2. 
             ( forall s1 t1 b. FunctionSymbol s1 t1 '[b] c
             , forall s2 t2 b. FunctionSymbol s2 t2 '[a] b 
             ) => t1 s1 '[b] c -> t2 s2 '[a] b -> Term a -> Term c
composeFn f g = appTerm $ ComposeW f g


getwitness :: forall sym t dom rng . FunctionSymbol sym t dom rng => String
getwitness = witness @sym @t @dom @rng

-- =======================================================
-- All he Foldy class instances are intimately tied to 
-- Numbers. But that is not required, but this is a 
-- convenient place to put the code.
-- =======================================================    


class HasSpec a => Foldy a where
  genList ::
    (MonadGenError m) => Specification a -> Specification a -> GenT m [a]
  theAddFn :: FunctionSymbol sym t '[a, a] a => t sym '[a,a] a
  theZero :: a
  genSizedList ::
    (MonadGenError m) =>
    Specification Integer -> Specification a -> Specification a -> GenT m [a]
  noNegativeValues :: Bool

adds :: forall a. (forall t sym. FunctionSymbol sym t [a, a] a) => Foldy a => [a] -> a
adds = foldr (semantics $ theAddFn) theZero

{-
data FoldSpec a where
  NoFold :: FoldSpec a
  FoldSpec ::
    forall b a.
    ( HasSpec a
    , HasSpec b
    , Foldy b
    , Member (ListFn fn) fn
    , BaseUniverse fn
    ) =>
    '[a] b ->
    Specification b ->
    FoldSpec a

instance {-# OVERLAPPABLE #-} (Arbitrary (TypeSpec a), Foldy a, BaseUniverse fn) => Arbitrary (FoldSpec a) where
  arbitrary = oneof [FoldSpec idFn <$> arbitrary, pure NoFold]
  shrink NoFold = []
  shrink (FoldSpec (extractFn @(FunFn fn) @fn -> Just Id) spec) = FoldSpec idFn <$> shrink spec
  shrink FoldSpec {} = [NoFold]

preMapFoldSpec :: HasSpec a => '[a] b -> FoldSpec b -> FoldSpec a
preMapFoldSpec _ NoFold = NoFold
preMapFoldSpec f (FoldSpec g s) = FoldSpec (composeFn g f) s

combineFoldSpec :: FoldSpec a -> FoldSpec a -> Either [String] (FoldSpec a)
combineFoldSpec NoFold s = pure s
combineFoldSpec s NoFold = pure s
combineFoldSpec (FoldSpec (f :: as b) s) (FoldSpec (f' :: fn' as' b') s')
  | Just Refl <- eqT @b @b'
  , Just Refl <- eqT @fn @fn'
  , f == f' =
      pure $ FoldSpec f (s <> s')
  | otherwise =
      Left ["Can't combine fold specs on different functions", "  " ++ show f, "  " ++ show f']

conformsToFoldSpec :: forall a. [a] -> FoldSpec a -> Bool
conformsToFoldSpec _ NoFold = True
conformsToFoldSpec xs (FoldSpec f s) = adds @fn (map (sem f) xs) `conformsToSpec` s

toPredsFoldSpec :: forall a. BaseUniverse => Term [a] -> FoldSpec a -> Pred fn
toPredsFoldSpec _ NoFold = TruePred
toPredsFoldSpec x (FoldSpec sspec) =
  satisfies (app (foldMapFn fn) x) sspec
-}  
