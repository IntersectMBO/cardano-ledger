{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Constrained.TypeRep (
  Rec (..),
  Rep (..),
  (:~:) (Refl),
  type (.:) (..),
  Lookup,
  Shape (..),
  Singleton (..),
  Eql,
  synopsis,
  compareRep,
  genSizedRep,
  genRep,
  TxOut (..),
  unTxOut,
  Value (..),
  unValue,
  PParams (..),
  unPParams,
  PParamsUpdate (..),
  unPParamsUpdate,
  liftUTxO,
) where

import Cardano.Ledger.HKD (HKD)
import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality (TestEquality (..))
import Data.Universe (Eql, Shape (..), Singleton (..), cmpIndex, (:~:) (Refl))
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Constrained.Classes (
  PParams (..),
  PParamsUpdate (..),
  TxOut (..),
  Value (..),
  liftUTxO,
  unPParams,
  unPParamsUpdate,
  unTxOut,
  unValue,
 )
import Test.Cardano.Ledger.Constrained.Combinators
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck hiding (Fixed, Fun, total)
import Type.Reflection (
  TypeRep,
  Typeable,
  typeRep,
  pattern App,
  pattern Fun,
 )
import GHC.TypeLits (Symbol)

-- =======================================================================
infixr 0 :->

type family Lookup n (rec :: [Type]) where
  Lookup n (n .: v ': t) = v
  Lookup n (a .: v ': t) = Lookup n t

data (.:) (s :: Symbol) a where
  Field :: Typeable s => TypeRep s -> a -> s .: a

instance (Arbitrary a, Typeable n) => Arbitrary (n .: a) where
  arbitrary = Field typeRep <$> arbitrary

data Rec f (t :: [Type]) where
  RNil :: Rec f '[]
  (:&) :: s .: (HKD f a) -> !(Rec f as) -> Rec f (s .: a ': as)
  deriving (Typeable)

instance Arbitrary (Rec Identity '[]) where
  arbitrary = pure RNil

instance
  ( Arbitrary a
  , Arbitrary (Rec Identity as)
  , Typeable n
  ) =>
  Arbitrary (Rec Identity (n .: a ': as))
  where
  arbitrary = (:&) <$> arbitrary <*> arbitrary

data Rep (t :: Type) where
  RecR :: Arbitrary (Rec Identity a) => Rec Rep a -> Rep (Rec Identity a)
  RationalR :: Rep Rational
  (:->) :: Rep a -> Rep b -> Rep (a -> b)
  MapR :: Ord a => Rep a -> Rep b -> Rep (Map a b)
  SetR :: Ord a => Rep a -> Rep (Set a)
  ListR :: Rep a -> Rep [a]
  SimpleR :: (Eq a, Show a, Arbitrary a) => TypeRep a -> Rep a
  IntR :: Rep Int
  IntegerR :: Rep Integer
  FloatR :: Rep Float
  NaturalR :: Rep Natural
  Word64R :: Rep Word64

-- ===========================================================
-- Proof of Rep equality

instance Singleton Rep where
  testEql (a :-> b) (x :-> y) = do
    Refl <- testEql a x
    Refl <- testEql b y
    Just Refl
  testEql (MapR a b) (MapR x y) = do
    Refl <- testEql a x
    Refl <- testEql b y
    Just Refl
  testEql (SetR a) (SetR b) = do
    Refl <- testEql a b
    Just Refl
  testEql (ListR a) (ListR b) = do
    Refl <- testEql a b
    Just Refl
  testEql RationalR RationalR = Just Refl
  testEql Word64R Word64R = Just Refl
  testEql IntR IntR = Just Refl
  testEql (SimpleR x) (SimpleR y) = testEquality x y
  testEql _ _ = Nothing
  cmpIndex _ _ = undefined

recToTypeRep :: forall (a :: [Type]). Rec Rep a -> TypeRep a
recToTypeRep RNil = typeRep
recToTypeRep (Field _ a :& as) = typeRep @(:) `App` fieldRep `App` recToTypeRep as
  where
    fieldRep = typeRep @(.:) `App` typeRep `App` toTypeRep a

toTypeRep :: Rep a -> TypeRep a
toTypeRep (RecR a) = App typeRep $ recToTypeRep a
toTypeRep (a :-> b) = Fun (toTypeRep a) (toTypeRep b)
toTypeRep (MapR a b) = App (App typeRep (toTypeRep a)) (toTypeRep b)
toTypeRep (SetR a) = App typeRep (toTypeRep a)
toTypeRep (ListR a) = App typeRep (toTypeRep a)
toTypeRep RationalR = typeRep
toTypeRep Word64R = typeRep
toTypeRep IntR = typeRep
toTypeRep IntegerR = typeRep
toTypeRep FloatR = typeRep
toTypeRep NaturalR = typeRep
toTypeRep (SimpleR t) = t

-- ============================================================
-- Show instances

instance Show (Rep t) where
  show = show . toTypeRep

synopsis :: forall t. Rep t -> t -> String
synopsis (RecR t) r = "{" ++ help t r
  where
    help :: Rec Rep a -> Rec Identity a -> String
    help RNil _ = "}"
    help (Field na a :& as) (Field _ v :& vs) = show na ++ " = " ++ synopsis a v ++ ", " ++ help as vs
synopsis RationalR r = show r
synopsis (a :-> b) _ = "(Arrow " ++ show a ++ " " ++ show b ++ ")"
synopsis Word64R w = show w
synopsis rep@(MapR a b) mp = case Map.toList mp of
  [] -> "(empty::Map " ++ show a ++ " " ++ show b ++ ")"
  ((d, r) : _) -> "Map{" ++ synopsis a d ++ " -> " ++ synopsis b r ++ " | size = " ++ show (Map.size mp) ++ synSum rep mp ++ "}"
synopsis rep@(SetR a) t
  | Set.null t = "(empty::Set " ++ show a ++ ")"
  | otherwise = "Set{" ++ synopsis a (head (Set.elems t)) ++ " | size = " ++ show (Set.size t) ++ synSum rep t ++ "}"
synopsis rep@(ListR a) ll = case ll of
  [] -> "(empty::" ++ show (ListR a) ++ "]"
  (d : _) -> "[" ++ synopsis a d ++ " | size = " ++ show (length ll) ++ synSum rep ll ++ "]"
synopsis IntR n = show n
synopsis IntegerR n = show n
synopsis NaturalR n = show n
synopsis FloatR n = show n
synopsis (SimpleR _) n = show n

synSum :: Rep a -> a -> String
synSum (MapR _ RationalR) m = ", sum = " ++ show (Map.foldl' (+) 0 m)
synSum (SetR RationalR) m = ", sum = " ++ show (Set.foldl' (+) 0 m)
synSum (ListR RationalR) m = ", sum = " ++ show (List.foldl' (+) 0 m)
synSum _ _ = ""

compareRep :: forall t s. Rep t -> Rep s -> Ordering
compareRep x y = cmpIndex @(Rep) x y

-- ================================================

genSizedRep ::
  Int ->
  Rep t ->
  Gen t
genSizedRep n (_a :-> b) = const <$> genSizedRep n b
genSizedRep n (MapR a b) = do
  mapSized n (genSizedRep n a) (genSizedRep n b)
genSizedRep n (SetR a) = do
  setSized n (genSizedRep n a)
genSizedRep n (ListR a) = vectorOf n (genSizedRep n a)
genSizedRep _ RationalR = arbitrary
genSizedRep _ (SimpleR _) = arbitrary
genSizedRep _ Word64R = arbitrary
genSizedRep _ IntR = arbitrary
genSizedRep _ NaturalR = arbitrary
genSizedRep _ FloatR = arbitrary
genSizedRep _ IntegerR = arbitrary
genSizedRep _ (RecR _) = arbitrary

genRep ::
  Rep b ->
  Gen b
genRep x = do (NonZero n) <- arbitrary; genSizedRep n x
