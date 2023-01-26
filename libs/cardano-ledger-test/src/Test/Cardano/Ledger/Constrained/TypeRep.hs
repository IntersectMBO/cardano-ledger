{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.TypeRep (
  Rep (..),
  (:~:) (Refl),
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

import Cardano.Ledger.Core (Era)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality (TestEquality (..))
import Data.Universe (Eql, Shape (..), Shaped (..), Singleton (..), cmpIndex, (:~:) (Refl))
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
  typeRep,
  pattern App,
  pattern Fun,
 )

-- =======================================================================
infixr 0 :->

data Rep era t where
  RationalR :: Rep era Rational
  (:->) :: Rep era a -> Rep era b -> Rep era (a -> b)
  MapR :: Ord a => Rep era a -> Rep era b -> Rep era (Map a b)
  SetR :: Ord a => Rep era a -> Rep era (Set a)
  ListR :: Rep era a -> Rep era [a]
  SimpleR :: (Eq a, Show a, Arbitrary a) => TypeRep a -> Rep era a
  IntR :: Rep era Int
  IntegerR :: Rep era Integer
  FloatR :: Rep era Float
  NaturalR :: Rep era Natural
  Word64R :: Rep era Word64

-- ===========================================================
-- Proof of Rep equality

instance Singleton (Rep e) where
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
  cmpIndex x y = compare (shape x) (shape y)

toTypeRep :: Rep era a -> TypeRep a
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

instance Show (Rep era t) where
  show = show . toTypeRep

synopsis :: forall e t. Rep e t -> t -> String
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

synSum :: Rep era a -> a -> String
synSum (MapR _ RationalR) m = ", sum = " ++ show (Map.foldl' (+) 0 m)
synSum (SetR RationalR) m = ", sum = " ++ show (Set.foldl' (+) 0 m)
synSum (ListR RationalR) m = ", sum = " ++ show (List.foldl' (+) 0 m)
synSum _ _ = ""

-- ==================================================

instance Shaped (Rep era) any where
  shape (a :-> b) = Nary 0 [shape a, shape b]
  shape (MapR a b) = Nary 1 [shape a, shape b]
  shape (SetR a) = Nary 2 [shape a]
  shape (ListR a) = Nary 3 [shape a]
  shape RationalR = Nullary 4
  shape Word64R = Nullary 5
  shape IntR = Nullary 6
  shape IntegerR = Nullary 7
  shape NaturalR = Nullary 8
  shape FloatR = Nullary 9
  shape (SimpleR _) = Nullary 10

compareRep :: forall era t s. Rep era t -> Rep era s -> Ordering
compareRep x y = cmpIndex @(Rep era) x y

-- ================================================

genSizedRep ::
  ( Era era
  -- , Mock (EraCrypto era)
  ) =>
  Int ->
  Rep era t ->
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

genRep ::
  Era era =>
  Rep era b ->
  Gen b
genRep x = do (NonZero n) <- arbitrary; genSizedRep n x
