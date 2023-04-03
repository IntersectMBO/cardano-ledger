{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Constrained.Classes where

import Cardano.Ledger.BaseTypes (EpochNo (..), ProtVer (..), SlotNo (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Pretty (PDoc)
import Cardano.Ledger.Shelley.Governance (ShelleyPPUPState (..))
import qualified Cardano.Ledger.Shelley.Governance as Gov (GovernanceState (..))
import Cardano.Ledger.Shelley.PParams (pvCanFollow)
import qualified Cardano.Ledger.Shelley.PParams as PP (ProposedPPUpdates (..))
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
import Data.Universe (Singleton (..), (:~:) (Refl))
import Data.Word (Word64)
import GHC.Real (denominator, numerator, (%))
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Constrained.Combinators (errorMess)
import Test.Cardano.Ledger.Constrained.Size (AddsSpec (..), Size (..), genFromIntRange, genFromNonNegIntRange)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Generic.PrettyCore (pcTxOut, pcVal)
import Test.Cardano.Ledger.Generic.Proof (
  AllegraEra,
  GoodCrypto,
  Proof (..),
  Reflect (..),
  ShelleyEra,
  unReflect,
 )
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Update (genShelleyPParamsUpdate)
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

-- =====================================================================
-- Helper functions

gauss :: Floating a => a -> a -> a -> a
gauss mean stdev x = (1 / (stdev * (sqrt (2 * pi)))) * exp (negate ((1 / 2) * ((x - mean) / stdev) ** 2))

-- ==========================================
-- The Adds class

-- Some methods of 'Adds' like 'minus', 'genAdds', 'partition' and 'fromI' are partial.
-- That is they might not be defined on all inputs. The [String] is a representation
-- of a stack trace, that describes what the sytem was doing, so if the function is partial
-- it can raise an appropriate error. The function
-- Test.Cardano.Ledger.Constrained.Combinators(errorMess) is used to raise an error
-- and properly report the stack trace.

class (Ord x, Show x) => Adds x where
  add :: x -> x -> x
  minus :: [String] -> x -> x -> x
  zero :: x

  -- | 'partition 7 trace 4 235', generate a list of length 4 that
  --   adds up t0 235, where the smallest number is >= 7
  partition :: x -> [String] -> Int -> x -> Gen [x]

  -- | 'genAdds trace spec' generate an 'x' in the range specified by 'spec'
  genAdds :: [String] -> AddsSpec x -> Gen x

  -- | Analogous to fromIntegral, translate an Int to an appropriate 'x'
  fromI :: [String] -> Int -> x

  -- | translate an 'x' Int to an appropriate Int
  toI :: x -> Int

  -- | Used in testing to get appropriate 'smallest' values to test
  --   'partition smallest trace count total'. The generator should choose from
  --   several values appropriate for the type 'x'. choose [0,1,2] would be
  --   appropriate for Natural, since there are no negative Natural numbers.
  genSmall :: Gen Int

-- ================
-- helper functions

sumAdds :: (Foldable t, Adds c) => t c -> c
sumAdds xs = List.foldl' add zero xs

projAdds :: (Foldable t, Sums a b) => t a -> b
projAdds xs = List.foldl' accum zero xs
  where
    accum ans x = add ans (getSum x)

genFromAddsSpec :: [String] -> AddsSpec c -> Gen Int
genFromAddsSpec _ AddsSpecAny = genFromIntRange SzAny
genFromAddsSpec _ (AddsSpecSize _ size) = genFromIntRange size
genFromAddsSpec msgs (AddsSpecNever _) = errorMess ("genFromAddsSpec applied to AddsSpecNever") msgs

genFromNonNegAddsSpec :: [String] -> AddsSpec c -> Gen Int
genFromNonNegAddsSpec _ AddsSpecAny = genFromNonNegIntRange SzAny
genFromNonNegAddsSpec _ (AddsSpecSize _ size) = genFromNonNegIntRange size
genFromNonNegAddsSpec msgs (AddsSpecNever _) = errorMess ("genFromAddsSpec applied to AddsSpecNever") msgs

-- ================
-- Adds instances

instance Adds Word64 where
  add = (+)
  minus _ = (-)
  zero = 0
  partition = partitionWord64
  genAdds msgs spec = fromI ms <$> genFromNonNegAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Word64"]
  fromI _ m | m >= 0 = fromIntegral m
  fromI msgs m = errorMess ("can't convert " ++ show m ++ " into a Word64.") msgs
  toI = fromIntegral
  genSmall = elements [0, 1, 2]

instance Adds Int where
  add = (+)
  minus _ = (-)
  zero = 0
  partition = partitionInt
  genAdds msgs spec = fromI ms <$> genFromAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Int"]
  fromI _ n = n
  toI n = n
  genSmall = elements [-2, -1, 0, 1, 2]

instance Adds Natural where
  add = (+)
  minus msg x y =
    if x < y
      then errorMess ("(minus @Natural " ++ show x ++ " " ++ show y ++ ") is not possible") msg
      else x - y
  zero = 0
  partition = partitionNatural
  genAdds msgs spec = fromI ms <$> genFromNonNegAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Natural"]
  fromI _ n | n >= 0 = fromIntegral n
  fromI msgs m = errorMess ("can't convert " ++ show m ++ " into a Natural.") msgs
  toI = fromIntegral
  genSmall = elements [0, 1, 2]

instance Adds Rational where
  add = (+)
  minus _ = (-)
  zero = 0
  partition = partitionRational
  genAdds msgs spec = fromI ms <$> genFromAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Rational"]
  fromI _ n = (fromIntegral n `div` 1000) % 1
  toI r = round (r * 1000)
  genSmall = elements [0, 1]

instance Adds Coin where
  add = (<+>)
  minus msg (Coin n) (Coin m) =
    if n < m
      then errorMess ("(minus @Coin " ++ show n ++ " " ++ show m ++ ") is not possible") msg
      else Coin (n - m)
  zero = Coin 0
  partition = partitionCoin
  genAdds msgs spec = fromI ms <$> genFromNonNegAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Coin"]
  fromI _ n | n >= 0 = Coin (fromIntegral n)
  fromI msgs m = errorMess ("can't convert " ++ show m ++ " into a Coin.") msgs
  toI (Coin n) = fromIntegral n
  genSmall = elements [0, 1, 2]

instance Adds DeltaCoin where
  add = (<+>)
  minus _ (DeltaCoin n) (DeltaCoin m) = DeltaCoin (n - m)
  zero = DeltaCoin 0
  partition = partitionDeltaCoin
  genAdds msgs spec = fromI ms <$> genFromAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds DeltaCoin"]
  fromI _ n = DeltaCoin (fromIntegral n)
  toI (DeltaCoin n) = fromIntegral n
  genSmall = elements [-2, 0, 1, 2]

-- ===========================================================================
-- The Sums class, for summing a projected c (where Adds c) from some richer type

class (Show x, Adds x) => Sums t x | t -> x where
  getSum :: t -> x
  genT :: [String] -> x -> Gen t

instance GoodCrypto c => Sums (IndividualPoolStake c) Rational where
  getSum (IndividualPoolStake r _) = r
  genT _ r = IndividualPoolStake r <$> arbitrary

instance (Reflect era) => Sums (TxOutF era) Coin where
  getSum (TxOutF _ txout) = coin (txout ^. valueTxOutL)
  genT _ cn = genTxOutX reify cn

genTxOutX :: Reflect era => Proof era -> Coin -> Gen (TxOutF era)
genTxOutX p coins = do
  txout <- case p of
    Shelley _ -> arbitrary
    Allegra _ -> arbitrary
    Mary _ -> arbitrary
    Alonzo _ -> arbitrary
    Babbage _ -> arbitrary
    Conway _ -> arbitrary
  pure $ TxOutF p (txout & coinTxOutL .~ coins)

instance Reflect era => Sums (ValueF era) Coin where
  getSum (ValueF _ v) = coin v
  genT _ cn = genValueX reify cn

genValueX :: Reflect era => Proof era -> Coin -> Gen (ValueF era)
genValueX proof cn = do
  ValueF p v <- genValue proof
  pure (ValueF p (modifyCoin (const cn) v))

instance Crypto c => Sums [Reward c] Coin where
  getSum ss = List.foldl' accum (Coin 0) ss
    where
      accum ans (Reward _ _ c) = add ans c
  genT _ (Coin 1) = (: []) <$> (updateRew (Coin 1) <$> arbitrary)
  genT msgs (Coin n) | n > 1 = do
    size <- chooseInt (1, fromIntegral n)
    cs <- partition (Coin 1) msgs size (Coin n)
    list <- vectorOf size (arbitrary :: Gen (Reward c))
    pure $ zipWith (updateRew @c) cs list
  genT msgs c = errorMess ("Coin in genT must be positive: " ++ show c) msgs

updateRew :: forall c. Coin -> Reward c -> Reward c
updateRew c (Reward a b _) = Reward a b c

-- ===========================================================
-- Sizeable Class

class Show t => Sizeable t where
  -- | extract the 'size' of 't'
  getSize :: t -> Int

instance Sizeable Natural where
  getSize n = fromIntegral n

instance Sizeable Int where
  getSize n = n

instance Sizeable Word64 where
  getSize n = fromIntegral n

instance Sizeable EpochNo where
  getSize (EpochNo n) = fromIntegral n

instance Sizeable SlotNo where
  getSize (SlotNo n) = fromIntegral n

instance (Show dom, Show rng) => Sizeable (Map dom rng) where
  getSize m = Map.size m

instance Show t => Sizeable (Set t) where
  getSize m = Set.size m

instance Show t => Sizeable [t] where
  getSize m = length m

instance Sizeable Coin where
  getSize (Coin n) = fromIntegral n

-- ===========================================================
-- The Count classs 0,1,2,3,4 ...

class Count t where
  -- | 'canFollow x y', is 'x' an appropriate successor to 'y'
  canFollow :: t -> t -> Bool

  -- | Generate the predecessor, given the successor
  genPred :: t -> Gen t

  -- | Generate the successor, given the predecessor
  genSucc :: t -> Gen t

instance Count Int where
  canFollow x y = x + 1 == y
  genPred n | n <= 0 = error ("genPredFromSucc @Int is undefined on " ++ show n)
  genPred n = pure (n - 1)
  genSucc n = pure (n + 1)

instance Count ProtVer where
  canFollow succX predX = pvCanFollow predX (SJust succX)
  genPred succX@(ProtVer n 0)
    | n == minBound = error ("genPredFromSucc @ProtVer is undefined on " ++ show succX)
  genPred (ProtVer n 0) = ProtVer (pred n) <$> elements [0, 1, 2, 3]
  genPred (ProtVer n m) = pure (ProtVer n (m - 1))
  genSucc (ProtVer n m) = frequency [(1, pure (ProtVer (succ n) 0)), (2, pure (ProtVer n (m + 1)))]

instance Count EpochNo where
  canFollow predX succX = predX + 1 == succX
  genPred n | n == 0 = error ("genPredFromSucc @EpochNo is undefined on " ++ show n)
  genPred n = pure (n - 1)
  genSucc n = pure (n + 1)

instance Count SlotNo where
  canFollow predX succX = predX + 1 == succX
  genPred n | n == 0 = error ("genPredFromSucc @SlotNo is undefined on " ++ show n)
  genPred n = pure (n - 1)
  genSucc n = pure (n + 1)

-- =======================================================================
-- The FromList class

class (Eq a, Eq (t a)) => FromList t a where
  makeFromList :: [a] -> t a
  getList :: t a -> [a]

instance Eq a => FromList [] a where
  makeFromList xs = xs
  getList xs = xs

instance (Ord a) => FromList Set a where
  makeFromList xs = Set.fromList xs
  getList = Set.toList

instance Eq a => FromList Maybe a where
  makeFromList [] = Nothing
  makeFromList (x : _) = Just x
  getList Nothing = []
  getList (Just x) = [x]

-- ============================================================================
-- Special accomodation for Type Families
-- ============================================================================

data TxOutF era where
  TxOutF :: Proof era -> TxOut era -> TxOutF era

unTxOut :: TxOutF era -> TxOut era
unTxOut (TxOutF _ x) = x

instance Eq (TxOutF era) where
  TxOutF p1 x1 == TxOutF p2 x2 = case testEql p1 p2 of
    Just Refl -> case p1 of
      Shelley _ -> x1 == x2
      Allegra _ -> x1 == x2
      Mary _ -> x1 == x2
      Alonzo _ -> x1 == x2
      Babbage _ -> x1 == x2
      Conway _ -> x1 == x2
    Nothing -> False

-- ======
data ValueF era where
  ValueF :: Proof era -> Value era -> ValueF era

unValue :: ValueF era -> Value era
unValue (ValueF _ v) = v

instance Ord (ValueF (ShelleyEra c)) where
  compare (ValueF _ coin1) (ValueF _ coin2) = compare coin1 coin2
instance Eq (ValueF (ShelleyEra c)) where
  x == y = compare x y == EQ

instance Ord (ValueF (AllegraEra c)) where
  compare (ValueF _ coin1) (ValueF _ coin2) = compare coin1 coin2
instance Eq (ValueF (AllegraEra c)) where
  x == y = compare x y == EQ

-- ======
data PParamsF era where
  PParamsF :: Proof era -> PParams era -> PParamsF era

unPParams :: PParamsF era -> PParams era
unPParams (PParamsF _ p) = p

pparamsWrapperL :: Lens' (PParamsF era) (PParams era)
pparamsWrapperL = lens unPParams (\(PParamsF p _) pp -> PParamsF p pp)

-- =======

data PParamsUpdateF era where
  PParamsUpdateF :: Proof era -> PParamsUpdate era -> PParamsUpdateF era

unPParamsUpdate :: PParamsUpdateF era -> PParamsUpdate era
unPParamsUpdate (PParamsUpdateF _ p) = p

pparamsUpdateWrapperL :: Lens' (PParamsUpdateF era) (PParamsUpdate era)
pparamsUpdateWrapperL = lens unPParamsUpdate (\(PParamsUpdateF p _) pp -> PParamsUpdateF p pp)

-- =====================

data ProposedPPUpdatesF era where
  ProposedPPUpdatesF :: Proof era -> PP.ProposedPPUpdates era -> ProposedPPUpdatesF era

unProposedPPUpdates :: ProposedPPUpdatesF era -> PP.ProposedPPUpdates era
unProposedPPUpdates (ProposedPPUpdatesF _ x) = x

proposedCoreL :: Lens' (PP.ProposedPPUpdates era) (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era))
proposedCoreL = lens (\(PP.ProposedPPUpdates m) -> m) (\(PP.ProposedPPUpdates _) m -> PP.ProposedPPUpdates m)

proposedWrapperL :: Lens' (ProposedPPUpdatesF era) (PP.ProposedPPUpdates era)
proposedWrapperL = lens unProposedPPUpdates (\(ProposedPPUpdatesF p _) pp -> ProposedPPUpdatesF p pp)

coreMapL ::
  Proof era ->
  Lens'
    (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era))
    (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
coreMapL p = lens (fmap (PParamsUpdateF p)) (\_ b -> fmap unPParamsUpdate b)

proposedMapL :: Lens' (ProposedPPUpdatesF era) (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
proposedMapL =
  lens
    (\(ProposedPPUpdatesF p x) -> x ^. (proposedCoreL . coreMapL p))
    (\(ProposedPPUpdatesF p x) y -> ProposedPPUpdatesF p (x & (proposedCoreL . coreMapL p) .~ y))

-- ====================

data GovernanceState era = GovernanceState (Proof era) (Gov.GovernanceState era)

unGovernanceState :: GovernanceState era -> Gov.GovernanceState era
unGovernanceState (GovernanceState _ x) = x

governanceProposedL :: Lens' (GovernanceState era) (ShelleyPPUPState era)
governanceProposedL =
  lens
    (\(GovernanceState p x) -> getPPUP p x)
    (\(GovernanceState p _) y -> GovernanceState p (putPPUP p y))

getPPUP :: forall era. Proof era -> Gov.GovernanceState era -> ShelleyPPUPState era
getPPUP (Shelley _) x = x
getPPUP (Allegra _) x = x
getPPUP (Mary _) x = x
getPPUP (Alonzo _) x = x
getPPUP (Babbage _) x = x
getPPUP (Conway _) _ = def @(ShelleyPPUPState era)

putPPUP :: forall era. Proof era -> ShelleyPPUPState era -> Gov.GovernanceState era
putPPUP (Shelley _) x = x
putPPUP (Allegra _) x = x
putPPUP (Mary _) x = x
putPPUP (Alonzo _) x = x
putPPUP (Babbage _) x = x
putPPUP (Conway _) _ = Gov.emptyGovernanceState @era

-- ================
liftUTxO :: Map (TxIn (EraCrypto era)) (TxOutF era) -> UTxO era
liftUTxO m = UTxO (Map.map unTxOut m)

instance Show (TxOutF era) where
  show (TxOutF p t) = show (unReflect pcTxOut p t :: PDoc)

instance Show (ValueF era) where
  show (ValueF p t) = show (pcVal p t)

instance Show (PParamsF era) where
  show (PParamsF _ _) = "PParamsF ..."

instance Show (PParamsUpdateF era) where
  show (PParamsUpdateF _ _) = "PParamsUpdateF ..."

instance Show (ProposedPPUpdatesF era) where
  show (ProposedPPUpdatesF _ _) = "ProposedPPUdatesF ..."

genValue :: Proof era -> Gen (ValueF era)
genValue p = case p of
  (Shelley _) -> ValueF p <$> arbitrary
  (Allegra _) -> ValueF p <$> arbitrary
  (Mary _) -> ValueF p <$> arbitrary
  (Alonzo _) -> ValueF p <$> arbitrary
  (Babbage _) -> ValueF p <$> arbitrary
  (Conway _) -> ValueF p <$> arbitrary

genTxOut :: Proof era -> Gen (TxOutF era)
genTxOut p = do
  n <- frequency [(2, choose (1, 100)), (1, choose (101, 1000))]
  unReflect genTxOutX p (Coin n)

genPParams :: Proof era -> Gen (PParamsF era)
genPParams p = case p of
  (Shelley _) -> PParamsF p <$> arbitrary
  (Allegra _) -> PParamsF p <$> arbitrary
  (Mary _) -> PParamsF p <$> arbitrary
  (Alonzo _) -> PParamsF p <$> arbitrary
  (Babbage _) -> PParamsF p <$> arbitrary
  (Conway _) -> PParamsF p <$> arbitrary

genPParamsUpdate :: Proof era -> Gen (PParamsUpdateF era)
genPParamsUpdate p = case p of
  (Shelley _) -> PParamsUpdateF p <$> genShelleyPParamsUpdate defaultConstants def
  (Allegra _) -> PParamsUpdateF p <$> genShelleyPParamsUpdate defaultConstants def
  (Mary _) -> PParamsUpdateF p <$> genShelleyPParamsUpdate defaultConstants def
  (Alonzo _) -> PParamsUpdateF p <$> arbitrary
  (Babbage _) -> PParamsUpdateF p <$> arbitrary
  (Conway _) -> PParamsUpdateF p <$> arbitrary

genProposedPPUpdates :: Proof era -> Gen (ProposedPPUpdatesF era)
genProposedPPUpdates p = case p of
  (Shelley _) -> ProposedPPUpdatesF p . PP.ProposedPPUpdates <$> arbitrary
  (Allegra _) -> ProposedPPUpdatesF p . PP.ProposedPPUpdates <$> arbitrary
  (Mary _) -> ProposedPPUpdatesF p . PP.ProposedPPUpdates <$> arbitrary
  (Alonzo _) -> ProposedPPUpdatesF p . PP.ProposedPPUpdates <$> arbitrary
  (Babbage _) -> ProposedPPUpdatesF p . PP.ProposedPPUpdates <$> arbitrary
  (Conway _) -> ProposedPPUpdatesF p . PP.ProposedPPUpdates <$> arbitrary

genGovernanceState :: Proof era -> Gen (GovernanceState era)
genGovernanceState p = case p of
  (Shelley _) -> GovernanceState p <$> arbitrary
  (Allegra _) -> GovernanceState p <$> arbitrary
  (Mary _) -> GovernanceState p <$> arbitrary
  (Alonzo _) -> GovernanceState p <$> arbitrary
  (Babbage _) -> GovernanceState p <$> arbitrary
  (Conway _) -> pure $ GovernanceState p Gov.emptyGovernanceState

genUTxO :: Proof era -> Gen (UTxO era)
genUTxO p = case p of
  (Shelley _) -> arbitrary
  (Allegra _) -> arbitrary
  (Mary _) -> arbitrary
  (Alonzo _) -> arbitrary
  (Babbage _) -> arbitrary
  (Conway _) -> arbitrary

-- ==========================================================================
-- A Single Partition function on Integer, we use to do all partitions by
-- using wrapper functions.
-- ==========================================================================

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

legalCallPartition :: [String] -> String -> Integer -> Int -> Integer -> Maybe [String]
legalCallPartition msgs typname smallest size total
  | size == 0 && smallest > 0 =
      Just
        ( [ "partition at type " ++ typname
          , "smallest="
              ++ show smallest
              ++ ", size="
              ++ show size
              ++ ", total="
              ++ show total
          ]
            ++ msgs
        )
  | fromIntegral size > total && smallest > 0 =
      Just $
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
          : msgs
  | size <= 0 =
      Just $
        ( "Can only make a partition of a positive number of pieces: "
            ++ show size
            ++ ", total: "
            ++ show total
            ++ ", smallest: "
            ++ show smallest
        )
          : msgs
  | smallest > 0 && smallest * (fromIntegral size) > total =
      Just $
        ( "Can't partition "
            ++ show total
            ++ " into "
            ++ show size
            ++ " pieces, each (>= "
            ++ show smallest
            ++ ")"
        )
          : msgs
  | total < 1 && smallest > 0 =
      Just $
        ( "Total ("
            ++ show total
            ++ ") must be positive when smallest("
            ++ show smallest
            ++ ") is positive."
        )
          : msgs
  | True = Nothing

-- | Generate a list of length 'size' that sums to 'total', where the minimum element is (>= 'smallest')
integerPartition :: [String] -> String -> Integer -> Int -> Integer -> Gen [Integer]
integerPartition msgs typname smallest size total
  | total == 0 && smallest <= 0 = pure (replicate size 0)
  | True = case legalCallPartition msgs typname smallest size total of
      Just (x : xs) -> errorMess x xs
      Just [] -> errorMess "legalCallPartition returns []" []
      Nothing ->
        let mean = total `div` fromIntegral (size + 1)
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
partitionCoin (Coin small) msgs n (Coin total) =
  map Coin <$> integerPartition msgs "Coin" small n total

partitionDeltaCoin :: DeltaCoin -> [String] -> Int -> DeltaCoin -> Gen [DeltaCoin]
partitionDeltaCoin (DeltaCoin small) msgs n (DeltaCoin total) =
  map DeltaCoin <$> integerPartition msgs "DeltaCoin" small n total

partitionInt :: Int -> [String] -> Int -> Int -> Gen [Int]
partitionInt small msgs n total =
  map fromIntegral <$> integerPartition msgs "Int" (fromIntegral small) n (fromIntegral total)

partitionWord64 :: Word64 -> [String] -> Int -> Word64 -> Gen [Word64]
partitionWord64 small msgs n total =
  map fromIntegral <$> integerPartition msgs "Word64" (fromIntegral small) n (fromIntegral total)

partitionNatural :: Natural -> [String] -> Int -> Natural -> Gen [Natural]
partitionNatural small msgs n total =
  map fromIntegral <$> integerPartition msgs "Natural" (fromIntegral small) n (fromIntegral total)
