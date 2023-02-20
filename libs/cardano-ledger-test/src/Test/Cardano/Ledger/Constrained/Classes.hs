{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Cardano.Ledger.Constrained.Classes where

import Cardano.Ledger.BaseTypes (EpochNo (..), ProtVer (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Pretty (PDoc)
import Cardano.Ledger.Shelley.PParams (pvCanFollow)
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (coin, modifyCoin, (<+>)))
import Data.Default.Class (Default (def))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Constrained.Combinators (errorMess)
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
  choose,
  chooseInt,
  elements,
  frequency,
  shuffle,
  vectorOf,
 )
import Prelude hiding (subtract)

import Cardano.Ledger.Shelley.Governance (ShelleyPPUPState (..))
import qualified Cardano.Ledger.Shelley.Governance as Core (GovernanceState (..))
import qualified Cardano.Ledger.Shelley.PParams as Core (ProposedPPUpdates (..))
import Test.Cardano.Ledger.Constrained.Size (SumV (..), genFromIntRange, genFromSize)

-- import  Cardano.Ledger.Conway.Governance(ConwayTallyState(..))
-- import Lens.Micro
import GHC.Real (denominator, numerator, (%))

-- =====================================================================
-- Partitioning a value into a bunch of pieces, that sum to that value
{-
-- | Generate a list of length 'size' that sums to 'total', where the minimum element is (>= 'smallest')
intPartition :: [String] -> String -> Int -> Int -> Int -> Gen [Int]
intPartition msgs typname smallest size total
  | size == 0 = errorMess (zeroCount "intPartition" total) msgs
  | size > total =
      errorMess
        ( "Can't partition "
            ++ show total
            ++ " into "
            ++ show size
            ++ " positive pieces at type "
            ++ typname
        )
        msgs
  | size < 1 =
      errorMess
        ( "Can only make a partion of positive number of pieces: "
            ++ show size
            ++ " total: "
            ++ show total
            ++ " smallest: "
            ++ show smallest
        )
        msgs
  | smallest < 0 = errorMess ("The minimum choice must be positive : " ++ show smallest) msgs
  | smallest * size > total =
      errorMess
        ( "Can't partition "
            ++ show total
            ++ " into "
            ++ show size
            ++ " pieces, each (>= "
            ++ show smallest
            ++ ")"
        )
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
-}
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

{-
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
-}
-- ==========================================
-- The Adds class

class (Ord x, Show x) => Adds x where
  add :: x -> x -> x
  subtract :: [String] -> x -> x -> x
  zero :: x
  partition :: [String] -> Int -> x -> Gen [x]
  genAddsSumV :: [String] -> SumV -> Gen x
  fromI :: [String] -> Int -> x
  toI :: x -> Int
  smallI :: x

sumAdds :: (Foldable t, Adds c) => t c -> c
sumAdds xs = List.foldl' add zero xs

projAdds :: (Foldable t, Sums a b) => t a -> b
projAdds xs = List.foldl' accum zero xs
  where
    accum ans x = add ans (getsum x)

genFromSumV :: forall c. Adds c => [String] -> SumV -> Gen c
genFromSumV _ SumVAny = pure zero
genFromSumV msgs s@(SumVSize _ size) = fromI (("genFromSumV " ++ show s) : msgs) <$> genFromIntRange size
genFromSumV msgs (SumVNever _) = errorMess ("genFromSumV applied to SumVNever") msgs

genFromNonNegSumV :: forall c. Adds c => [String] -> SumV -> Gen c
genFromNonNegSumV _ SumVAny = pure zero
genFromNonNegSumV msgs s@(SumVSize _ size) = fromI (("genFromSumV " ++ show s) : msgs) <$> genFromSize size
genFromNonNegSumV msgs (SumVNever _) = errorMess ("genFromSumV applied to SumVNever") msgs

instance Adds Word64 where
  add = (+)
  subtract _ = (-)
  zero = 0
  partition = partitionWord64 0
  genAddsSumV = genFromNonNegSumV
  fromI _ n | n >= 0 = fromIntegral n
  fromI msgs m = errorMess ("can't convert " ++ show m ++ " into a Word64.") msgs
  toI = fromIntegral
  smallI = 0

instance Adds Int where
  add = (+)
  subtract _ = (-)
  zero = 0
  partition = partitionInt 0
  genAddsSumV = genFromSumV
  fromI _ n = n
  toI n = n
  smallI = 0

instance Adds Natural where
  add = (+)
  subtract msg x y =
    if x < y
      then errorMess ("(subtract @Natural " ++ show x ++ " " ++ show y ++ ") is not possible") msg
      else x - y
  zero = 0
  partition = partitionNatural 1
  genAddsSumV = genFromNonNegSumV
  fromI _ n | n >= 0 = fromIntegral n
  fromI msgs m = errorMess ("can't convert " ++ show m ++ " into a Natural.") msgs
  toI = fromIntegral
  smallI = 1

instance Adds Rational where
  add = (+)
  subtract _ = (-)
  zero = 0
  partition = partitionRational (1 % 10000)
  genAddsSumV = genFromSumV
  fromI _ n = (fromIntegral n `div` 1000) % 1
  toI r = round (r * 1000)
  smallI = (1 % 10000)

instance Adds Coin where
  add = (<+>)
  subtract msg (Coin n) (Coin m) =
    if n < m
      then errorMess ("(subtract @Coin " ++ show n ++ " " ++ show m ++ ") is not possible") msg
      else Coin (n - m)
  zero = Coin 0
  partition = partitionCoin (Coin 1)
  genAddsSumV = genFromNonNegSumV
  fromI _ n | n >= 0 = Coin (fromIntegral n)
  fromI msgs m = errorMess ("can't convert " ++ show m ++ " into a Coin.") msgs
  toI (Coin n) = fromIntegral n
  smallI = (Coin 1)

instance Adds DeltaCoin where
  add = (<+>)
  subtract _ (DeltaCoin n) (DeltaCoin m) = DeltaCoin (n - m)
  zero = DeltaCoin 0
  partition msgs size total = partitionDeltaCoin (DeltaCoin (-4)) msgs size total
  genAddsSumV = genFromSumV
  fromI _ n = DeltaCoin (fromIntegral n)
  toI (DeltaCoin n) = (fromIntegral n)
  smallI = DeltaCoin (-4)

smallInc :: forall c. Adds c => Int
smallInc = toI (smallI @c)

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

genTxOutX :: forall era. Proof era -> Coin -> Gen (TxOut era)
genTxOutX p coins =
  case p of
    Shelley _ -> do txout <- arbitrary; pure $ TxOut p (txout & txOutCoinL .~ coins)
    Allegra _ -> do txout <- arbitrary; pure $ TxOut p (txout & txOutCoinL .~ coins)
    Mary _ -> do txout <- arbitrary; pure $ TxOut p (txout & txOutCoinL .~ coins)
    Alonzo _ -> do txout <- arbitrary; pure $ TxOut p (txout & txOutCoinL .~ coins)
    Babbage _ -> do txout <- arbitrary; pure $ TxOut p (txout & txOutCoinL .~ coins)
    Conway _ -> do txout <- arbitrary; pure $ TxOut p (txout & txOutCoinL .~ coins)

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

-- ======
data Value era where
  Value :: Proof era -> Core.Value era -> Value era

unValue :: Value era -> Core.Value era
unValue (Value _ v) = v

instance Ord (Value (ShelleyEra c)) where
  compare (Value _ coin1) (Value _ coin2) = compare coin1 coin2
instance Eq (Value (ShelleyEra c)) where
  x == y = compare x y == EQ

instance Ord (Value (AllegraEra c)) where
  compare (Value _ coin1) (Value _ coin2) = compare coin1 coin2
instance Eq (Value (AllegraEra c)) where
  x == y = compare x y == EQ

-- ======
data PParams era where
  PParams :: Proof era -> Core.PParams era -> PParams era

unPParams :: PParams era -> Core.PParams era
unPParams (PParams _ p) = p

pparamsWrapperL :: Lens' (PParams era) (Core.PParams era)
pparamsWrapperL = lens unPParams (\(PParams p _) pp -> PParams p pp)

-- =======

data PParamsUpdate era where
  PParamsUpdate :: Proof era -> Core.PParamsUpdate era -> PParamsUpdate era

unPParamsUpdate :: PParamsUpdate era -> Core.PParamsUpdate era
unPParamsUpdate (PParamsUpdate _ p) = p

pparamsUpdateWrapperL :: Lens' (PParamsUpdate era) (Core.PParamsUpdate era)
pparamsUpdateWrapperL = lens unPParamsUpdate (\(PParamsUpdate p _) pp -> PParamsUpdate p pp)

-- =====================

data ProposedPPUpdates era where
  ProposedPPUpdates :: Proof era -> Core.ProposedPPUpdates era -> ProposedPPUpdates era

-- newtype Core.ProposedPPUpdates era = Core.ProposedPPUpdates (Map (KeyHash 'Genesis (EraCrypto era)) (Core.PParamsUpdate era))

unProposedPPUpdates :: ProposedPPUpdates era -> Core.ProposedPPUpdates era
unProposedPPUpdates (ProposedPPUpdates _ x) = x

proposedCoreL :: Lens' (Core.ProposedPPUpdates era) (Map (KeyHash 'Genesis (EraCrypto era)) (Core.PParamsUpdate era))
proposedCoreL = lens (\(Core.ProposedPPUpdates m) -> m) (\(Core.ProposedPPUpdates _) m -> Core.ProposedPPUpdates m)

proposedWrapperL :: Lens' (ProposedPPUpdates era) (Core.ProposedPPUpdates era)
proposedWrapperL = lens unProposedPPUpdates (\(ProposedPPUpdates p _) pp -> ProposedPPUpdates p pp)

coreMapL ::
  Proof era ->
  Lens'
    (Map (KeyHash 'Genesis (EraCrypto era)) (Core.PParamsUpdate era))
    (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era))
coreMapL p = lens (fmap (PParamsUpdate p)) (\_ b -> fmap unPParamsUpdate b)

proposedMapL :: Lens' (ProposedPPUpdates era) (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era))
proposedMapL =
  lens
    (\(ProposedPPUpdates p x) -> x ^. (proposedCoreL . (coreMapL p)))
    (\(ProposedPPUpdates p x) y -> ProposedPPUpdates p (x & (proposedCoreL . (coreMapL p)) .~ y))

-- ====================

data GovernanceState era = GovernanceState (Proof era) (Core.GovernanceState era)

unGovernanceState :: GovernanceState era -> Core.GovernanceState era
unGovernanceState (GovernanceState _ x) = x

governanceProposedL :: Lens' (GovernanceState era) (ShelleyPPUPState era)
governanceProposedL =
  lens
    (\(GovernanceState p x) -> getPPUP p x)
    (\(GovernanceState p _) y -> GovernanceState p (putPPUP p y))

getPPUP :: forall era. Proof era -> Core.GovernanceState era -> ShelleyPPUPState era
getPPUP (Shelley _) x = x
getPPUP (Allegra _) x = x
getPPUP (Mary _) x = x
getPPUP (Alonzo _) x = x
getPPUP (Babbage _) x = x
getPPUP (Conway _) _ = def @(ShelleyPPUPState era)

putPPUP :: forall era. Proof era -> ShelleyPPUPState era -> Core.GovernanceState era
putPPUP (Shelley _) x = x
putPPUP (Allegra _) x = x
putPPUP (Mary _) x = x
putPPUP (Alonzo _) x = x
putPPUP (Babbage _) x = x
putPPUP (Conway _) _ = Core.emptyGovernanceState @era

-- ================
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

instance Show (ProposedPPUpdates era) where
  show (ProposedPPUpdates _ _) = "ProposedPPUdates ..."

genValue :: Proof era -> Gen (Value era)
genValue p = case p of
  (Shelley _) -> Value p <$> arbitrary
  (Allegra _) -> Value p <$> arbitrary
  (Mary _) -> Value p <$> arbitrary
  (Alonzo _) -> Value p <$> arbitrary
  (Babbage _) -> Value p <$> arbitrary
  (Conway _) -> Value p <$> arbitrary

genTxOut :: Proof era -> Gen (TxOut era)
genTxOut p = do
  n <- choose (1, 100)
  genTxOutX p (Coin n)

{-
genTxOut p = case p of
  (Shelley _) -> TxOut p <$> arbitrary
  (Allegra _) -> TxOut p <$> arbitrary
  (Mary _) -> TxOut p <$> arbitrary
  (Alonzo _) -> TxOut p <$> arbitrary
  (Babbage _) -> TxOut p <$> arbitrary
  (Conway _) -> TxOut p <$> arbitrary
-}

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

genProposedPPUpdates :: Proof era -> Gen (ProposedPPUpdates era)
genProposedPPUpdates p = case p of
  (Shelley _) -> ProposedPPUpdates p . Core.ProposedPPUpdates <$> arbitrary
  (Allegra _) -> ProposedPPUpdates p . Core.ProposedPPUpdates <$> arbitrary
  (Mary _) -> ProposedPPUpdates p . Core.ProposedPPUpdates <$> arbitrary
  (Alonzo _) -> ProposedPPUpdates p . Core.ProposedPPUpdates <$> arbitrary
  (Babbage _) -> ProposedPPUpdates p . Core.ProposedPPUpdates <$> arbitrary
  (Conway _) -> ProposedPPUpdates p . Core.ProposedPPUpdates <$> arbitrary

genGovernanceState :: Proof era -> Gen (GovernanceState era)
genGovernanceState p = case p of
  (Shelley _) -> GovernanceState p <$> arbitrary
  (Allegra _) -> GovernanceState p <$> arbitrary
  (Mary _) -> GovernanceState p <$> arbitrary
  (Alonzo _) -> GovernanceState p <$> arbitrary
  (Babbage _) -> GovernanceState p <$> arbitrary
  (Conway _) -> pure $ GovernanceState p Core.emptyGovernanceState

genUTxO :: Proof era -> Gen (UTxO era)
genUTxO p = case p of
  (Shelley _) -> arbitrary
  (Allegra _) -> arbitrary
  (Mary _) -> arbitrary
  (Alonzo _) -> arbitrary
  (Babbage _) -> arbitrary
  (Conway _) -> arbitrary

-- ==========================================================================
-- ==========================================================================

-- | Generate a list of length 'size' that sums to 'total', where the minimum element is (>= 'smallest')
integerPartition :: [String] -> String -> Integer -> Int -> Integer -> Gen [Integer]
integerPartition msgs typname smallest size total
  | size == 0 = errorMess (zeroCount "integerPartition" total) msgs
  | fromIntegral size > total && smallest /= 0 =
      errorMess
        ( "Can't partition "
            ++ show total
            ++ " into "
            ++ show size
            ++ " positive pieces at type "
            ++ typname
            ++ " (smallest = "
            ++ show smallest
            ++ ")"
        )
        msgs
  | size < 1 =
      errorMess
        ( "Can only make a partion of positive number of pieces: "
            ++ show size
            ++ " total: "
            ++ show total
            ++ " smallest: "
            ++ show smallest
        )
        msgs
  --   | smallest < 0 = errorMess ("The minimum choice must be positive : " ++ show smallest) msgs
  | smallest * (fromIntegral size) > total =
      errorMess
        ( "Can't partition "
            ++ show total
            ++ " into "
            ++ show size
            ++ " pieces, each (>= "
            ++ show smallest
            ++ ")"
        )
        msgs
  | total < 1 = errorMess ("Total must be positive: " ++ show total) msgs
  | otherwise =
      let mean = total `div` (fromIntegral (size + 1))
          go 1 total1
            | total1 < 1 = errorMess ("Ran out of choices(2), total went negative: " ++ show total1) msgs
            | otherwise = pure [total1]
          go 2 total1 = do
            z <- choose (smallest, total1 - 1)
            pure [z, total1 - z]
          go size1 total1 = do
            let hi =
                  min
                    (max 1 mean)
                    (total1 - (size1 - 1))
            x <- choose (smallest, hi)
            xs <- go (size1 - 1) (total1 - x)
            pure (x : xs)
       in do
            ws <- go (fromIntegral size) total
            shuffle ws

partitionRational :: Rational -> [String] -> Int -> Rational -> Gen [Rational]
partitionRational smallest msgs size total = do
  let scale = lcm (denominator smallest) (denominator total)
      iSmallest = numerator (smallest * (scale % 1))
      iTotal = numerator (total * (scale % 1))
  is <- integerPartition msgs ("Rational*" ++ show scale) iSmallest size iTotal
  pure (map (\i -> i % scale) is)

partitionCoin :: Coin -> [String] -> Int -> Coin -> Gen [Coin]
partitionCoin (Coin small) msgs n (Coin total) = map Coin <$> integerPartition msgs "Coin" small n total

partitionDeltaCoin :: DeltaCoin -> [String] -> Int -> DeltaCoin -> Gen [DeltaCoin]
partitionDeltaCoin (DeltaCoin small) msgs n (DeltaCoin total) = map DeltaCoin <$> integerPartition msgs "DeltaCoin" small n total

partitionInt :: Int -> [String] -> Int -> Int -> Gen [Int]
partitionInt small msgs n total = map fromIntegral <$> integerPartition msgs "Int" (fromIntegral small) n (fromIntegral total)

partitionWord64 :: Word64 -> [String] -> Int -> Word64 -> Gen [Word64]
partitionWord64 small msgs n total = map fromIntegral <$> integerPartition msgs "Word64" (fromIntegral small) n (fromIntegral total)

partitionNatural :: Natural -> [String] -> Int -> Natural -> Gen [Natural]
partitionNatural small msgs n total = map fromIntegral <$> integerPartition msgs "Word64" (fromIntegral small) n (fromIntegral total)
