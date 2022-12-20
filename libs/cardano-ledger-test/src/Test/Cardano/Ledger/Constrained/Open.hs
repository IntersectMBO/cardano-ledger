{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Constrained.Open where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import Type.Reflection -- (TypeRep(..), pattern App, pattern Con, pattern Fun)

import Cardano.Ledger.BaseTypes (EpochNo)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraTxOut, TxOut, Value)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.PoolParams (PoolParams (ppId))
import Cardano.Ledger.Pretty (PDoc)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe (Eql, Shape (..), Shaped (..), Singleton (..), cmpIndex, (:~:) (Refl))
import Data.Word (Word64)
import Test.Cardano.Ledger.Constrained.Combinators
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.PrettyCore (
  credSummary,
  keyHashSummary,
  pcTxOut,
  pcVal,
  txInSummary,
 )
import Test.Cardano.Ledger.Generic.Proof (
  C_Crypto,
  Evidence (..),
  Proof (..),
  ShelleyEra,
  unReflect,
 )
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck hiding (Fixed, total)

-- ==========================================

{-
The problem with using TypeRep is several fold
1) We can't be polymorhic over anything, since TypeRep is monomorphic
2) So we must actually store a (Proof era) in every (Rep era t)
3) It is hard to make "polymorphic" Patterns so we can match several patterns with different types.
   The soution to this is the strange "double" nested forall types in the Patterns.
-}

shelley :: Proof (ShelleyEra StandardCrypto)
shelley = Shelley Standard

type Rep :: Type -> Type -> Type
data Rep era t where
  Wrap :: forall (t :: Type) (era :: Type). Proof era -> TypeRep t -> Rep era t

instance Show (Rep e t) where
  show (Wrap (Shelley _) t) = show t
  show (Wrap (Allegra _) t) = show t
  show (Wrap (Mary _) t) = show t
  show (Wrap (Alonzo _) t) = show t
  show (Wrap (Babbage _) t) = show t
  show (Wrap (Conway _) t) = show t

intR = (Wrap shelley (typeRep @Int))
stringR = (Wrap shelley (typeRep @String))
boolR = (Wrap shelley (typeRep @Bool))
setR = Wrap shelley (typeRep @(Set Int))

-- Patterns for nullary types

view :: forall (s :: Type) (t :: Type) era. Typeable s => Rep era t -> Maybe (s :~: t, Proof era)
view (Wrap p t) = case testEquality (typeRep @s) t of
  Just Refl -> Just (Refl, p)
  Nothing -> Nothing

-- | Note the wierd Nested constrained type. We need the (forall t. (t ~ Int)=> Rep era s) so when
--   we pattern match we bring the equality (t ~ Int) into scope. That aows us to use multiple
--   patterns in one function. see 'foo' below
pattern IntR :: forall (s :: Type) (era :: Type). (Typeable s) => Proof era -> (forall t. (Typeable t, t ~ Int) => Rep era s)
pattern IntR p <- (view @s -> Just (Refl, p)) where IntR p = (Wrap p (typeRep @s))

pattern StringR :: forall (s :: Type) (era :: Type). (Typeable s) => Proof era -> (forall t. (t ~ String) => Rep era s)
pattern StringR p <- (view @s -> Just (Refl, p)) where StringR p = (Wrap p (typeRep @s))

-- Patterns for unary type constructors

viewSet :: forall s a era. (Typeable s, Typeable a) => Rep era s -> Maybe (s :~: Set a, Proof era, Rep era a)
viewSet (Wrap p s) = case testEquality s (typeRep @(Set a)) of
  Just Refl -> case s of
    (App t a) -> Just (Refl, p, Wrap p a)
  --  _other -> Nothing
  Nothing -> Nothing

pattern SetR ::
  forall (s :: Type) (a :: Type) (t :: * -> Type) (era :: Type).
  (Typeable a, Typeable s) =>
  forall q.
  (Typeable a, q ~ Set a) =>
  Proof era ->
  Rep era a ->
  Rep era s
pattern SetR p a <- (viewSet @s @a -> Just (Refl, p, a)) where SetR p (Wrap _ a) = (Wrap p (typeRep @s))

-- Patterns in use

foo :: Typeable t => Rep era t -> Int
foo (IntR _) = 1
foo (StringR _) = 2
foo (SetR _ (IntR _)) = 3
foo _ = 5

-- Note the two different use of IntR
-- 1) (IntR _) :: Rep era t         We want it to be polymorphic, so foo is not fixed at a particular type
-- 2) (IntR @Int _) :: Rep era Int  We want it monomorphic, otherwise we get this unfixble missing
--                                  constraint (Typeable a0). We can't fix it since the type variable is not in scope

-- ==============================================================================
-- Conclusion.  A lot of work, so Maybe Closed world approaches are better
