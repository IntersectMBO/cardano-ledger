{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Cardano.Ledger.Constrained.Classes
where

-- import Debug.Trace(trace)

import Cardano.Ledger.BaseTypes (EpochNo (..), ProtVer (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Pretty (PDoc)
import Cardano.Ledger.Shelley.PParams (pvCanFollow)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (coin, modifyCoin, (<+>)))
import Data.Default.Class (Default (def))
import Data.Maybe.Strict (StrictMaybe (SJust))
import Test.Cardano.Ledger.Constrained.Combinators (errorMess)
import Prelude hiding (subtract)

-- import qualified Data.List as List

import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro
import Numeric.Natural (Natural)
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
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Positive (..),
  choose,
  chooseInt,
  elements,
  frequency,
  shuffle,
  suchThatMaybe,
  vectorOf,
 )

-- =====================================================================
-- Partioning a value into a bunch of pieces, that sum to that value

-- | Generate a list of length 'size' that sums to 'total', where the minimum element is (>= 'smallest')
intPartition :: [String] -> String -> Int -> Int -> Int -> Gen [Int]
intPartition msgs typname smallest size total
  | size == 0 = errorMess (zeroCount "intPartition" total) msgs
  | size > total = errorMess ("Can't partition " ++ show total ++ " into " ++ show size ++ " positive pieces at type " ++ typname) msgs
  | size < 1 = errorMess ("Can only make a partion of positive number of pieces: " ++ show size ++ " total: " ++ show total ++ " smallest: " ++ show smallest) msgs
  | smallest < 0 = errorMess ("The minimum choice must be positive : " ++ show smallest) msgs
  | smallest * size > total =
      errorMess
        ("Can't partition " ++ show total ++ " into " ++ show size ++ " pieces, each (>= " ++ show smallest ++ ")")
        msgs
  | total < 1 = errorMess ("Total must be positive: " ++ show total) msgs
  | otherwise =
      let mean = total `div` size + 1
          go 1 total1
            | total1 < 1 = errorMess ("Ran out of choices(2), total went negative: " ++ show total1) msgs
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

zeroCount :: Show a => [Char] -> a -> [Char]
zeroCount fname total =
  fname
    ++ " called with count=(0) and total=("
    ++ show total
    ++ ") \n"
    ++ "Probably due to (SumsTo comparison "
    ++ show total
    ++ " [SumMap x]) where 'x' is the emptyset.\n"
    ++ "Try adding (Sized (Range 1 m) (Dom x)) constraint to force 'x' to have at least 1 element"

rationalPartition :: [String] -> Int -> Rational -> Gen [Rational]
rationalPartition msgs 0 total = errorMess (zeroCount "rationalPartition" total) msgs
rationalPartition msgs n total = do
  let iScale = n * 1000
      rScale :: Rational
      rScale = fromIntegral iScale
  is <- intPartition msgs "Rational" (iScale `div` n) n (round (total * rScale))
  pure (map ((/ rScale) . fromIntegral) is)

coinPartition :: [String] -> Coin -> Int -> Coin -> Gen [Coin]
coinPartition msgs (Coin smallest) size (Coin total) =
  map (Coin . fromIntegral) <$> intPartition msgs "Coin" (fromIntegral smallest) size (fromIntegral total)

deltaCoinPartition :: [String] -> DeltaCoin -> Int -> DeltaCoin -> Gen [DeltaCoin]
deltaCoinPartition msgs (DeltaCoin smallest) size (DeltaCoin total) =
  map (DeltaCoin . fromIntegral) <$> intPartition msgs "DeltaCoin" (fromIntegral smallest) size (fromIntegral total)

naturalPartition :: [String] -> Natural -> Int -> Natural -> Gen [Natural]
naturalPartition msgs smallest size total =
  map (fromIntegral) <$> intPartition msgs "Natural" (fromIntegral smallest) size (fromIntegral total)

-- ==========================================
-- The Adds class

suchThatErr :: [String] -> Gen a -> (a -> Bool) -> Gen a
suchThatErr msgs gen p = do
  x <- suchThatMaybe gen p
  case x of
    Just y -> pure y
    Nothing -> errorMess "SuchThat times out" msgs

class (Ord x, Show x) => Adds x where
  add :: x -> x -> x
  subtract :: [String] -> x -> x -> x
  zero :: x
  partition :: [String] -> Int -> x -> Gen [x]
  addCount :: x -> Int
  fromCount :: Int -> x
  greater :: Int -> Gen x -- Generate an 'x' larger than 'Int'

  -- | Adjusts the total 'x' to account for differences in SumCond's EQL LTH LTE GTH and GTE
  adjust :: [String] -> SumCond -> Int -> x -> Gen x

  partitionBy :: [String] -> SumCond -> Int -> x -> Gen [x]
  partitionBy msgs cond count total = do b <- adjust msgs cond count total; partition msgs count b

sumAdds :: (Foldable t, Adds c) => t c -> c
sumAdds xs = List.foldl' add zero xs

projAdds :: (Foldable t, Sums a b) => t a -> b
projAdds xs = List.foldl' accum zero xs
  where
    accum ans x = add ans (getsum x)

greaterDelta :: Int
greaterDelta = 2

instance Adds Word64 where
  add = (+)
  subtract _ = (-)
  zero = 0
  partition msgs count total = map fromIntegral <$> (intPartition msgs "Int" 1 count (fromIntegral total))
  addCount x = fromIntegral x
  fromCount x = fromIntegral x
  greater n = fromIntegral <$> choose (n + 1, n + greaterDelta)
  adjust msgs cond count total = fromInteger <$> adjustTotalForCond msgs cond (fromIntegral count) (fromIntegral total)

instance Adds Int where
  add = (+)
  subtract _ = (-)
  zero = 0
  partition msgs = intPartition msgs "Int" 1
  addCount x = x
  fromCount x = x
  greater n = choose (n + 1, n + greaterDelta)
  adjust msgs cond count total = fromInteger <$> adjustTotalForCond msgs cond (fromIntegral count) (fromIntegral total)

instance Adds Natural where
  add = (+)
  subtract msg x y =
    if x < y
      then errorMess ("(subtract @Natural " ++ show x ++ " " ++ show y ++ ") is not possible") msg
      else x - y
  zero = 0
  partition msgs = naturalPartition msgs 1
  addCount x = fromIntegral x
  fromCount x = fromIntegral x
  greater n = fromIntegral <$> choose (n + 1, n + greaterDelta)
  adjust msgs cond count total = fromInteger <$> adjustTotalForCond msgs cond (fromIntegral count) (fromIntegral total)

instance Adds Rational where
  add = (+)
  subtract _ = (-)
  zero = 0
  partition = rationalPartition
  addCount _ = 2 -- Not as arbitrary as it seems
  fromCount n = fromIntegral n
  greater n = fromIntegral <$> choose (n + 1, n + greaterDelta)
  adjust _ EQL _ x = pure x
  adjust msgs _ _ _ = errorMess ("partition for Rational only works for EQL") msgs

instance Adds Coin where
  add = (<+>)
  subtract msg (Coin n) (Coin m) =
    if n < m
      then errorMess ("(subtract @Coin " ++ show n ++ " " ++ show m ++ ") is not possible") msg
      else Coin (n - m)
  zero = Coin 0
  partition msgs = coinPartition msgs (Coin 1)
  addCount (Coin n) = fromInteger n
  fromCount n = (Coin (fromIntegral n))
  greater n = (Coin . fromIntegral) <$> choose (n + 1, n + greaterDelta)
  adjust msgs cond count (Coin total) = Coin <$> adjustTotalForCond msgs cond (fromIntegral count) total

instance Adds DeltaCoin where
  add = (<+>)
  subtract _ (DeltaCoin n) (DeltaCoin m) = DeltaCoin (n - m)
  zero = DeltaCoin 0
  partition msgs size _ | size < 1 = errorMess ("DeltaCoin partition applied to bad size: " ++ show size) msgs
  partition _ 1 total = pure [total]
  partition msgs n total = do
    x <- arbitrary
    xs <- partition msgs (n - 1) (subtract [] total x)
    pure (x : xs)
  addCount (DeltaCoin n) = if n < 0 then fromIntegral (-n) else fromIntegral n
  fromCount n = DeltaCoin (fromIntegral n)
  greater n = (DeltaCoin . fromIntegral) <$> choose (n + 1, n + greaterDelta)
  adjust msgs cond count (DeltaCoin total) = DeltaCoin <$> adjustTotalForCond msgs cond (fromIntegral count) total

adjustTotalForCond :: [String] -> SumCond -> Integer -> Integer -> Gen Integer
adjustTotalForCond msgs condition count total = case condition of
  EQL -> pure total
  GTE -> do Positive m <- arbitrary; choose (total, total + m)
  GTH -> do Positive m <- arbitrary; choose (total + 1, total + m)
  LTH ->
    if count + 1 > total - 1
      then
        errorMess
          ( "find n : "
              ++ show count
              ++ " < n  &&  n < "
              ++ show total
              ++ "\n"
              ++ "[ SumsTo Term(count="
              ++ show count
              ++ ") LTH Sums(total="
              ++ show total
              ++ ") ]"
              ++ " Cannot be solved."
          )
          msgs
      else choose (count + 1, total - 1) -- (\ n -> count < n && n < total)
  LTE ->
    if count + 1 > total + 1
      then
        errorMess
          ( "find n : "
              ++ show count
              ++ " < n  &&  n <= "
              ++ show total
              ++ "\n"
              ++ "[ SumsTo Term(count="
              ++ show count
              ++ ") LTE Sums(total="
              ++ show total
              ++ ") ]"
              ++ " Cannot be solved."
          )
          msgs
      else choose (count + 1, total) -- (\ n -> count < n && n <= total)
  CondAny -> suchThatErr msgs arbitrary (\x -> x > count)
  CondNever xs -> errorMess "adjustTotalForCond called with (NeverCond _)" (msgs ++ xs)

-- ===========================================================================
-- The Sums class, for summing a projected c (where Adds c) from some richer type

class (Show x, Adds x) => Sums t x | t -> x where
  getsum :: t -> x
  genT :: [String] -> x -> Gen t

instance GoodCrypto c => Sums (IndividualPoolStake c) Rational where
  getsum (IndividualPoolStake r _) = r
  genT _ r = IndividualPoolStake r <$> arbitrary

instance (Reflect era) => Sums (TxOut era) Coin where
  getsum (TxOut (Shelley _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Allegra _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Mary _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Alonzo _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Babbage _) t) = coin (t ^. Core.valueTxOutL)
  getsum (TxOut (Conway _) t) = coin (t ^. Core.valueTxOutL)
  genT _ cn = genTxOutX reify cn

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
  genT _ cn = genValueX reify cn

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

instance Crypto c => Sums [Reward c] Coin where
  getsum ss = List.foldl' accum (Coin 0) ss
    where
      accum ans (Reward _ _ c) = add ans c
  genT _ (Coin 1) = (: []) <$> (updateRew (Coin 1) <$> arbitrary)
  genT msgs (Coin n) | n > 1 = do
    size <- chooseInt (1, fromIntegral n)
    cs <- partition msgs size (Coin n)
    list <- vectorOf size (arbitrary :: Gen (Reward c))
    pure $ zipWith (updateRew @c) cs list
  genT msgs c = errorMess ("Coin in genT must be positive: " ++ show c) msgs

updateRew :: forall c. Coin -> Reward c -> Reward c
updateRew c (Reward a b _) = Reward a b c

-- ===========================================================
-- Sizeable Class

class Show t => Sizeable t where
  getsize :: t -> Int

instance Sizeable Natural where
  getsize n = fromIntegral n

instance Sizeable Int where
  getsize n = n

instance Sizeable Word64 where
  getsize n = fromIntegral n

instance (Show dom, Show rng) => Sizeable (Map dom rng) where
  getsize m = (Map.size m)

instance Show t => Sizeable (Set t) where
  getsize m = (Set.size m)

instance Show t => Sizeable [t] where
  getsize m = length m

instance Sizeable Coin where
  getsize (Coin n) = fromIntegral n

-- | Types which can reasonably be described by a positive Int
class Sizeable t => FromInt t where
  fromInt :: Int -> t

instance FromInt Int where fromInt n = n
instance FromInt Natural where fromInt n = fromIntegral n
instance FromInt Coin where fromInt n = Coin (fromIntegral n)
instance FromInt Word64 where fromInt n = fromIntegral n

-- ===========================================================
-- The Count classs 0,1,2,3,4 ...

class Count t where
  canFollow :: t -> t -> Bool
  genPredFromSucc :: t -> Gen t
  genSuccFromPred :: t -> Gen t

instance Count Int where
  canFollow x y = x + 1 == y
  genPredFromSucc n | n <= 0 = error ("genPredFromSucc @Int is undefined on " ++ show n)
  genPredFromSucc n = pure (n - 1)
  genSuccFromPred n = pure (n + 1)

instance Count ProtVer where
  canFollow succX predX = pvCanFollow predX (SJust succX)
  genPredFromSucc succX@(ProtVer n 0) | n == minBound = error ("genPredFromSucc @ProtVer is undefined on " ++ show succX)
  genPredFromSucc (ProtVer n 0) = ProtVer (pred n) <$> elements [0, 1, 2, 3]
  genPredFromSucc (ProtVer n m) = pure (ProtVer n (m - 1))
  genSuccFromPred (ProtVer n m) = frequency [(1, pure (ProtVer (succ n) 0)), (2, pure (ProtVer n (m + 1)))]

instance Count EpochNo where
  canFollow predX succX = predX + 1 == succX
  genPredFromSucc n | n == 0 = error ("genPredFromSucc @EpochNo is undefined on " ++ show n)
  genPredFromSucc n = pure (n - 1)
  genSuccFromPred n = pure (n + 1)

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

pparamsWrapperL :: Lens' (PParams era) (Core.PParams era)
pparamsWrapperL = lens unPParams (\(PParams p _) pp -> PParams p pp)

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

-- =========================================================================
-- SumsTo x <= [One y, SumMap z]
--          ^ paramerterize over the condition
-- EQL = (==), LTH = (<), LTE = (<=), GTH = (>), GTE = (>=)

data SumCond = EQL | LTH | LTE | GTH | GTE | CondNever [String] | CondAny
  deriving (Eq)

negSumCond :: SumCond -> SumCond
negSumCond EQL = EQL
negSumCond LTH = GTH
negSumCond LTE = GTH
negSumCond GTH = LTH
negSumCond GTE = LTH
negSumCond x = x

instance Show SumCond where
  show EQL = " = ∑ "
  show LTH = " < ∑ "
  show LTE = " <= ∑ "
  show GTH = " > ∑ "
  show GTE = " >= ∑ "
  show (CondNever xs) = unlines xs
  show CondAny = " ∑? "

instance Monoid SumCond where
  mempty = CondAny

instance Semigroup SumCond where
  CondAny <> x = x
  x <> CondAny = x
  CondNever xs <> CondNever ys = CondNever (xs ++ ys)
  CondNever xs <> _ = CondNever xs
  _ <> CondNever xs = CondNever xs
  EQL <> EQL = EQL
  EQL <> LTH = CondNever ["EQL and LTH are not compatible."]
  EQL <> LTE = EQL
  EQL <> GTH = CondNever ["EQL and GTH are not compatible."]
  EQL <> GTE = EQL
  LTH <> EQL = CondNever ["LTH and EQL are not compatible."]
  LTH <> LTH = LTH
  -- while technically (LTH <> LTE = LTH) holds, moving to LTH,
  -- changes the adjust value, this could cause failures.
  -- Same reasoning holds for (LTE <> LTH = LTH)
  LTH <> LTE = CondNever ["LTE and LTH  are not compatible."]
  LTH <> GTH = CondNever ["LTH and GTH are not compatible."]
  LTH <> GTE = CondNever ["LTH and GTE are not compatible."]
  LTE <> EQL = EQL
  LTE <> LTH = CondNever ["LTE and LTH  are not compatible."]
  LTE <> LTE = LTE
  LTE <> GTH = CondNever ["LTE and GTH are not compatible."]
  LTE <> GTE = CondNever ["LTE and GTE are not compatible."]
  GTH <> EQL = CondNever ["GTH and EQL are not compatible."]
  GTH <> LTH = CondNever ["GTH and LTH are not compatible."]
  GTH <> LTE = CondNever ["GTH and LTE are not compatible."]
  GTH <> GTH = GTH
  GTH <> GTE = GTH
  GTE <> EQL = EQL
  GTE <> LTH = CondNever ["GTE and LTH are not compatible."]
  GTE <> LTE = CondNever ["GTE and LTE are not compatible."]
  GTE <> GTH = GTH
  GTE <> GTE = GTE

runCond :: Adds c => SumCond -> c -> c -> Bool
runCond EQL x y = x == y
runCond LTH x y = x < y
runCond LTE x y = x <= y
runCond GTH x y = x > y
runCond GTE x y = x >= y
runCond CondAny _ _ = True
runCond (CondNever xs) _ _ = error (unlines xs)
