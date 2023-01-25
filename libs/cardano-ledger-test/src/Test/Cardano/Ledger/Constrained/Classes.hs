{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Cardano.Ledger.Constrained.Classes
where

import qualified Cardano.Ledger.Core as Core

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Pretty (PDoc)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (coin, modifyCoin, (<+>)))
import Data.Default.Class (Default (def))

-- import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.PrettyCore (pcTxOut, pcVal)
import Test.Cardano.Ledger.Generic.Proof (
  AllegraEra,
  GoodCrypto,
  Proof (..),
  Reflect (..),
  ShelleyEra,
  unReflect,
 )
import Test.Cardano.Ledger.Shelley.Generator.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Update (genShelleyPParamsUpdate)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary (..), Gen, chooseInt, shuffle)

-- =====================================================================
-- Partioning a value into a bunch of pieces, that sum to that value

-- | Generate a list of length 'size' that sums to 'total', where the minimum element is (>= 'smallest')
intPartition :: Int -> Int -> Int -> Gen [Int]
intPartition smallest size total
  | size > total = error ("Can't partition " ++ show total ++ " into " ++ show size ++ " positive pieces.")
  | size < 1 = error ("Can only make a partion of positive number of pieces: " ++ show size ++ " total: " ++ show total ++ " smallest: " ++ show smallest)
  | smallest < 0 = error ("The minimum choice must be positive : " ++ show smallest)
  | smallest * size > total =
      error
        ("Can't partition " ++ show total ++ " into " ++ show size ++ " pieces, each (>= " ++ show smallest ++ ")")
  | total < 1 = error ("Total must be positive: " ++ show total)
  | otherwise =
      let mean = total `div` size + 1
          go 1 total1
            | total1 < 1 = error ("Ran out of choices(2), total went negative: " ++ show total1)
            | otherwise = pure [total1]
          go 2 total1 = do
            z <- chooseInt (smallest, total1 - 1)
            pure [z, total1 - z]
          go size1 total1 = do
            let hi =
                  min
                    (max 1 mean)
                    (total1 - (size1 - 1))
            x <- chooseInt (smallest, hi)
            xs <- go (size1 - 1) (total1 - x)
            pure (x : xs)
       in do
            ws <- go size total
            shuffle ws

gauss :: Floating a => a -> a -> a -> a
gauss mean stdev x = (1 / (stdev * (sqrt (2 * pi)))) * exp (negate ((1 / 2) * ((x - mean) / stdev) ** 2))

rationalPartition :: Int -> Rational -> Gen [Rational]
rationalPartition n total = do
  let iScale = n * 1000
      rScale :: Rational
      rScale = fromIntegral iScale
  is <- intPartition (iScale `div` n) n (round (total * rScale))
  pure (map ((/ rScale) . fromIntegral) is)

coinPartition :: Coin -> Int -> Coin -> Gen [Coin]
coinPartition (Coin smallest) size (Coin total) =
  map (Coin . fromIntegral) <$> intPartition (fromIntegral smallest) size (fromIntegral total)

-- ==========================================
-- The Adds class

class (Eq x, Show x) => Adds x where
  add :: x -> x -> x
  subtract :: x -> x -> x
  zero :: x
  partition :: Int -> x -> Gen [x]

instance Adds Int where
  add = (+)
  subtract = (-)
  zero = 0
  partition = intPartition 1

instance Adds Rational where
  add = (+)
  subtract = (-)
  zero = 0
  partition = rationalPartition

instance Adds Coin where
  add = (<+>)
  subtract (Coin n) (Coin m) = Coin (n - m)
  zero = Coin 0
  partition = coinPartition (Coin 1)

-- ===========================================================================
-- The Sums class, for summing a projected c (where Adds c) from some richer type

class (Show x, Adds x) => Sums t x | t -> x where
  getsum :: t -> x
  genT :: x -> Gen t

instance GoodCrypto c => Sums (IndividualPoolStake c) Rational where
  getsum (IndividualPoolStake r _) = r
  genT r = IndividualPoolStake r <$> arbitrary

instance (Reflect era) => Sums (TxOut era) Coin where
  getsum (TxOut (Shelley _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Allegra _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Mary _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Alonzo _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Babbage _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Conway _) t) = coin (t ^. Core.valueTxOutL)
  genT cn = genTxOutX reify cn

txOutCoinL :: (Core.EraTxOut era) => Lens' (Core.TxOut era) Coin
txOutCoinL = lens (\x -> coin (x ^. Core.valueTxOutL)) (\x c -> x & Core.valueTxOutL .~ (modifyCoin (const c) (x ^. Core.valueTxOutL)))

genTxOutX :: Proof era -> Coin -> Gen (TxOut era)
genTxOutX proof coins = do
  (TxOut p txout) <- genTxOut proof
  case p of
    Shelley _ -> pure $ TxOut p (txout & txOutCoinL .~ coins)
    Allegra _ -> pure $ TxOut p (txout & txOutCoinL .~ coins)
    Mary _ -> pure $ TxOut p (txout & txOutCoinL .~ coins)
    Alonzo _ -> pure $ TxOut p (txout & txOutCoinL .~ coins)
    Babbage _ -> pure $ TxOut p (txout & txOutCoinL .~ coins)
    Conway _ -> pure $ TxOut p (txout & txOutCoinL .~ coins)

instance Reflect era => Sums (Value era) Coin where
  getsum (Value (Shelley _) v) = v
  getsum (Value (Allegra _) v) = v
  getsum (Value (Mary _) v) = coin v
  getsum (Value (Alonzo _) v) = coin v
  getsum (Value (Babbage _) v) = coin v
  getsum (Value (Conway _) v) = coin v
  genT cn = genValueX reify cn

genValueX :: Proof era -> Coin -> Gen (Value era)
genValueX proof cn = do
  (Value p v) <- genValue proof
  case p of
    (Shelley _) -> pure (Value p cn)
    (Allegra _) -> pure (Value p cn)
    (Mary _) -> pure (Value p (modifyCoin (const cn) v))
    (Alonzo _) -> pure (Value p (modifyCoin (const cn) v))
    (Babbage _) -> pure (Value p (modifyCoin (const cn) v))
    (Conway _) -> pure (Value p (modifyCoin (const cn) v))

-- ===========================================================
-- Sizeable Class

class Show t => Sizeable t where
  getsize :: t -> Word64

instance (Show dom, Show rng) => Sizeable (Map dom rng) where
  getsize m = fromIntegral (Map.size m)

instance Show t => Sizeable (Set t) where
  getsize m = fromIntegral (Set.size m)

instance Sizeable Coin where
  getsize (Coin n) = fromIntegral n

-- ============================================================================
-- Special accomodation for Type Families
-- ============================================================================

data TxOut era where
  TxOut :: Proof era -> Core.TxOut era -> TxOut era

unTxOut :: TxOut era -> Core.TxOut era
unTxOut (TxOut _ x) = x

data Value era where
  Value :: Proof era -> Core.Value era -> Value era

instance Ord (Value (ShelleyEra c)) where
  compare (Value _ coin1) (Value _ coin2) = compare coin1 coin2
instance Eq (Value (ShelleyEra c)) where
  x == y = compare x y == EQ

instance Ord (Value (AllegraEra c)) where
  compare (Value _ coin1) (Value _ coin2) = compare coin1 coin2
instance Eq (Value (AllegraEra c)) where
  x == y = compare x y == EQ

unValue :: Value era -> Core.Value era
unValue (Value _ v) = v

data PParams era where
  PParams :: Proof era -> Core.PParams era -> PParams era

unPParams :: PParams era -> Core.PParams era
unPParams (PParams _ p) = p

data PParamsUpdate era where
  PParamsUpdate :: Proof era -> Core.PParamsUpdate era -> PParamsUpdate era

unPParamsUpdate :: PParamsUpdate era -> Core.PParamsUpdate era
unPParamsUpdate (PParamsUpdate _ p) = p

liftUTxO :: Map (TxIn (EraCrypto era)) (TxOut era) -> UTxO era
liftUTxO m = UTxO (Map.map unTxOut m)

instance Show (TxOut era) where
  show (TxOut p t) = show (unReflect pcTxOut p t :: PDoc)

instance Show (Value era) where
  show (Value p t) = show (pcVal p t)

instance Show (PParams era) where
  show (PParams _ _) = "PParams ..."

instance Show (PParamsUpdate era) where
  show (PParamsUpdate _ _) = "PParamsUpdate ..."

genValue :: Proof era -> Gen (Value era)
genValue p = case p of
  (Shelley _) -> Value p <$> arbitrary
  (Allegra _) -> Value p <$> arbitrary
  (Mary _) -> Value p <$> arbitrary
  (Alonzo _) -> Value p <$> arbitrary
  (Babbage _) -> Value p <$> arbitrary
  (Conway _) -> Value p <$> arbitrary

genTxOut :: Proof era -> Gen (TxOut era)
genTxOut p = case p of
  (Shelley _) -> TxOut p <$> arbitrary
  (Allegra _) -> TxOut p <$> arbitrary
  (Mary _) -> TxOut p <$> arbitrary
  (Alonzo _) -> TxOut p <$> arbitrary
  (Babbage _) -> TxOut p <$> arbitrary
  (Conway _) -> TxOut p <$> arbitrary

genPParams :: Proof era -> Gen (PParams era)
genPParams p = case p of
  (Shelley _) -> PParams p <$> arbitrary
  (Allegra _) -> PParams p <$> arbitrary
  (Mary _) -> PParams p <$> arbitrary
  (Alonzo _) -> PParams p <$> arbitrary
  (Babbage _) -> PParams p <$> arbitrary
  (Conway _) -> PParams p <$> arbitrary

genPParamsUpdate :: Proof era -> Gen (PParamsUpdate era)
genPParamsUpdate p = case p of
  (Shelley _) -> PParamsUpdate p <$> genShelleyPParamsUpdate defaultConstants def
  (Allegra _) -> PParamsUpdate p <$> genShelleyPParamsUpdate defaultConstants def
  (Mary _) -> PParamsUpdate p <$> genShelleyPParamsUpdate defaultConstants def
  (Alonzo _) -> PParamsUpdate p <$> arbitrary
  (Babbage _) -> PParamsUpdate p <$> arbitrary
  (Conway _) -> PParamsUpdate p <$> arbitrary

genUTxO :: Proof era -> Gen (UTxO era)
genUTxO p = case p of
  (Shelley _) -> arbitrary
  (Allegra _) -> arbitrary
  (Mary _) -> arbitrary
  (Alonzo _) -> arbitrary
  (Babbage _) -> arbitrary
  (Conway _) -> arbitrary
