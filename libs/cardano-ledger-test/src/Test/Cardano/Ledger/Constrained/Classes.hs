{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Constrained.Classes where

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits ()
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), ProtVer (..), SlotNo (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Pretty (PDoc, ppString)
import Cardano.Ledger.Shelley.Governance (ShelleyPPUPState (..))
import qualified Cardano.Ledger.Shelley.Governance as Gov (GovernanceState (..))
import Cardano.Ledger.Shelley.PParams (pvCanFollow)
import qualified Cardano.Ledger.Shelley.PParams as PP (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (ScriptsNeeded, UTxO (..))
import Cardano.Ledger.Val (Val (coin, modifyCoin, (<+>)))
import Data.Default.Class (Default (def))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Real (denominator, numerator, (%))
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Constrained.Combinators (errorMess)
import Test.Cardano.Ledger.Constrained.Monad (
  LiftT (..),
  Typed (..),
  failT,
 )
import Test.Cardano.Ledger.Constrained.Pairing (pair, unpair)
import Test.Cardano.Ledger.Constrained.Scripts (genCoreScript)
import Test.Cardano.Ledger.Constrained.Size (
  Size (..),
  genFromIntRange,
  genFromNonNegIntRange,
  genFromSize,
  negateSize,
  sepsP,
 )
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.Functions (protocolVersion)
import Test.Cardano.Ledger.Generic.PrettyCore (
  pcScript,
  pcScriptsNeeded,
  pcTx,
  pcTxBody,
  pcTxCert,
  pcTxOut,
  pcVal,
  pcWitnesses,
 )
import Test.Cardano.Ledger.Generic.Proof (
  GoodCrypto,
  Proof (..),
  Reflect (..),
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
  oneof,
  shuffle,
  suchThat,
  vectorOf,
 )

-- =====================================================================
-- Helper functions

gauss :: Floating a => a -> a -> a -> a
gauss mean stdev x = (1 / (stdev * sqrt (2 * pi))) * exp (negate ((1 / 2) * ((x - mean) / stdev) ** 2))

-- | The Adds class
--
-- Some methods of 'Adds' like 'minus', 'genAdds', 'partition' and 'fromI' are partial.
-- That is they might not be defined on all inputs. The `[string]` is a representation
-- of a stack trace, that describes what the sytem was doing, so if the function is partial
-- it can raise an appropriate error. The function
-- Test.Cardano.Ledger.Constrained.Combinators(errorMess) is used to raise an error
-- and properly report the stack trace.
class (Eq x, Show x) => Adds x where
  -- | Additive identity
  zero :: x

  -- | Just the unit of increment.
  one :: x

  -- | Add two of these
  add :: x -> x -> x

  -- | Subtract one from another
  minus :: [String] -> x -> x -> x

  -- | Increase by unit of increment
  increaseBy1 :: Int -> Int
  increaseBy1 n = add n one

  -- | Decrease by unit of increment
  decreaseBy1 :: Int -> Int
  decreaseBy1 n = minus ["Minus"] n one

  -- | Generate a list of values
  -- @ partition 7 trace 4 235 @ generate a list of length 4 that
  -- adds up t0 235, where the smallest number is >= 7
  partition :: x -> [String] -> Int -> x -> Gen [x]

  -- | Generate a single value
  -- @ genAdds trace spec @ generates an 'x' in the range specified by 'spec'
  genAdds :: [String] -> AddsSpec x -> Gen x

  -- | Analogous to fromIntegral, translate an Int to an appropriate 'x'
  fromI :: [String] -> Int -> x

  -- | translate an 'x' Int to an appropriate Int
  toI :: x -> Int

  -- | Used in testing to get appropriate 'smallest' values to test
  -- 'partition smallest trace count total'. The generator should choose from
  -- several values appropriate for the type 'x'. choose [0,1,2] would be
  -- appropriate for Natural, since there are no negative Natural numbers.
  genSmall :: Gen Int

  runOrdCondition :: OrdCond -> x -> x -> Bool

  smallerOf :: x -> x -> x

sumAdds :: (Foldable t, Adds c) => t c -> c
sumAdds = List.foldl' add zero

lensAdds :: (Foldable t, Adds b) => Lens' a b -> t a -> b
lensAdds l = List.foldl' accum zero
  where
    accum ans x = add ans (x ^. l)

genFromAddsSpec :: [String] -> AddsSpec c -> Gen Int
genFromAddsSpec _ AddsSpecAny = genFromIntRange SzAny
genFromAddsSpec _ (AddsSpecSize _ size) = genFromIntRange size
genFromAddsSpec msgs (AddsSpecNever _) = errorMess "genFromAddsSpec applied to AddsSpecNever" msgs

genFromNonNegAddsSpec :: [String] -> AddsSpec c -> Gen Int
genFromNonNegAddsSpec _ AddsSpecAny = genFromNonNegIntRange SzAny
genFromNonNegAddsSpec _ (AddsSpecSize _ size) = genFromNonNegIntRange size
genFromNonNegAddsSpec msgs (AddsSpecNever _) = errorMess "genFromAddsSpec applied to AddsSpecNever" msgs

-- ================
-- Adds instances

instance Adds ExUnits where
  zero = ExUnits 0 0
  one = ExUnits 1 1
  add (ExUnits a b) (ExUnits c d) = ExUnits (a + c) (b + d)
  minus msgs (ExUnits a b) (ExUnits c d) =
    ExUnits
      (minus ("Ex memory" : msgs) a c)
      (minus ("Ex steps" : msgs) b d)
  increaseBy1 n = let (i, j) = unpair n in pair (increaseBy1 @Natural i) (increaseBy1 @Natural j)
  decreaseBy1 n = let (i, j) = unpair n in pair (decreaseBy1 @Natural i) (decreaseBy1 @Natural j)
  partition (ExUnits smallestmemory smalleststeps) msgs count (ExUnits memory steps) = do
    memG <- partition smallestmemory ("Ex memory" : msgs) count memory
    stepsG <- partition smalleststeps ("Ex steps" : msgs) count steps
    pure (zipWith ExUnits memG stepsG)
  genAdds msgs = \case
    AddsSpecAny -> errorMess "AddsSpecAny" ms
    AddsSpecNever msgs' -> errorMess "AddsSpecNever" $ ms <> msgs'
    AddsSpecSize msg sz -> case sz of
      SzLeast n ->
        let (i, j) = unpair n
         in do
              ig <- fromIntegral <$> genFromSize (SzLeast i)
              jg <- fromIntegral <$> genFromSize (SzLeast j)
              pure $ ExUnits ig jg
      SzMost n ->
        let (i, j) = unpair n
         in do
              ig <- fromIntegral <$> genFromSize (SzMost i)
              jg <- fromIntegral <$> genFromSize (SzMost j)
              pure $ ExUnits ig jg
      SzExact n ->
        let (i, j) = unpair n
         in do
              ig <- fromIntegral <$> genFromSize (SzExact i)
              jg <- fromIntegral <$> genFromSize (SzExact j)
              pure $ ExUnits ig jg
      SzNever m -> errorMess "AddsSpecSize SzNever" $ ms <> [msg] <> m
      _ -> errorMess "AddsSpecSize SzAny or SzRng" $ ms <> [msg]
    where
      ms = msgs ++ ["genAdds ExUnits"]
  fromI msgs n = ExUnits mem step
    where
      (memInt, stepInt) = unpair n
      mem = fromI ("Ex memory" : msgs) memInt
      step = fromI ("Ex steps" : msgs) stepInt
  toI (ExUnits mem step) = pair (toI mem) (toI step)
  genSmall = oneof [pure $ toI (ExUnits 1 1), pure $ toI (ExUnits 2 2), pure $ toI (ExUnits 3 1)]

  -- Some ExUnits are incomparable: i.e. x=(ExUnits 5 7) and y=(ExUnits 8 3)
  -- neither x<y  or y<x is true.
  runOrdCondition EQL (ExUnits a b) (ExUnits c d) = a == c && b == d
  runOrdCondition LTH (ExUnits a b) (ExUnits c d) = a < c && b < d
  runOrdCondition LTE x y = runOrdCondition LTH x y || runOrdCondition EQL x y
  runOrdCondition GTH (ExUnits a b) (ExUnits c d) = a > c && b > d
  runOrdCondition GTE x y = runOrdCondition GTH x y || runOrdCondition EQL x y
  smallerOf x y
    | runOrdCondition LTE x y = x
    | runOrdCondition GTE x y = y
    | otherwise = errorMess "ExUnits are incomparable, can't choose the 'smallerOf'" [show x, show y]

-- ================
instance Adds Word64 where
  zero = 0
  one = 1
  add = (+)
  minus msg x y =
    if x < y
      then errorMess ("(minus @Word64 " ++ show x ++ " " ++ show y ++ ") is not possible") msg
      else x - y
  partition = partitionWord64
  genAdds msgs spec = fromI ms <$> genFromNonNegAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Word64"]
  fromI _ m | m >= 0 = fromIntegral m
  fromI msgs m = errorMess ("can't convert " ++ show m ++ " into a Word64.") msgs
  toI = fromIntegral
  genSmall = elements [0, 1, 2]
  runOrdCondition = runOrdCond
  smallerOf = min

instance Adds Int where
  zero = 0
  one = 1
  add = (+)
  minus _ = (-)
  partition = partitionInt
  genAdds msgs spec = fromI ms <$> genFromAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Int"]
  fromI _ n = n
  toI n = n
  genSmall = elements [-2, -1, 0, 1, 2]
  runOrdCondition = runOrdCond
  smallerOf = min

instance Adds Natural where
  zero = 0
  one = 1
  add = (+)
  minus msg x y =
    if x < y
      then errorMess ("(minus @Natural " ++ show x ++ " " ++ show y ++ ") is not possible") msg
      else x - y
  partition = partitionNatural
  genAdds msgs spec = fromI ms <$> genFromNonNegAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Natural"]
  fromI _ n | n >= 0 = fromIntegral n
  fromI msgs m = errorMess ("can't convert " ++ show m ++ " into a Natural.") msgs
  toI = fromIntegral
  genSmall = elements [0, 1, 2]
  runOrdCondition = runOrdCond
  smallerOf = min

instance Adds Rational where
  zero = 0
  one = 1
  add = (+)
  minus _ = (-)
  partition = partitionRational
  genAdds msgs spec = fromI ms <$> genFromAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Rational"]
  fromI _ n = (fromIntegral n `div` 1000) % 1
  toI r = round (r * 1000)
  genSmall = elements [0, 1]
  runOrdCondition = runOrdCond
  smallerOf = min

instance Adds Coin where
  zero = Coin 0
  one = Coin 1
  add = (<+>)
  minus msg (Coin n) (Coin m) =
    if n < m
      then errorMess ("(minus @Coin " ++ show n ++ " " ++ show m ++ ") is not possible") msg
      else Coin (n - m)
  partition = partitionCoin
  genAdds msgs spec = fromI ms <$> genFromNonNegAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds Coin"]
  fromI _ n | n >= 0 = Coin (fromIntegral n)
  fromI msgs m = errorMess ("can't convert " ++ show m ++ " into a Coin.") msgs
  toI (Coin n) = fromIntegral n
  genSmall = elements [0, 1, 2]
  runOrdCondition = runOrdCond
  smallerOf = min

instance Adds DeltaCoin where
  zero = DeltaCoin 0
  one = DeltaCoin 1
  add = (<+>)
  minus _ (DeltaCoin n) (DeltaCoin m) = DeltaCoin (n - m)
  partition = partitionDeltaCoin
  genAdds msgs spec = fromI ms <$> genFromAddsSpec ms spec
    where
      ms = msgs ++ ["genAdds DeltaCoin"]
  fromI _ n = DeltaCoin (fromIntegral n)
  toI (DeltaCoin n) = fromIntegral n
  genSmall = elements [-2, 0, 1, 2]
  runOrdCondition = runOrdCond
  smallerOf = min

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

instance Sizeable (MultiAsset c) where
  getSize (MultiAsset m) = Map.size m

-- ===========================================================
-- The Count class 0,1,2,3,4 ...

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

-- ============================================================================
-- Special accomodation for Type Families
-- ============================================================================

data TxAuxDataF era where
  TxAuxDataF :: Proof era -> TxAuxData era -> TxAuxDataF era

hashTxAuxDataF :: Reflect era => TxAuxDataF era -> AuxiliaryDataHash (EraCrypto era)
hashTxAuxDataF (TxAuxDataF _ x) = hashTxAuxData x

unTxAuxData :: TxAuxDataF era -> TxAuxData era
unTxAuxData (TxAuxDataF _ x) = x

instance Show (TxAuxDataF era) where
  show (TxAuxDataF p x) = show ((unReflect pcAuxData p x) :: PDoc)

instance Eq (TxAuxDataF era) where
  (TxAuxDataF (Shelley _) x) == (TxAuxDataF (Shelley _) y) = x == y
  (TxAuxDataF (Allegra _) x) == (TxAuxDataF (Allegra _) y) = x == y
  (TxAuxDataF (Mary _) x) == (TxAuxDataF (Mary _) y) = x == y
  (TxAuxDataF (Alonzo _) x) == (TxAuxDataF (Alonzo _) y) = x == y
  (TxAuxDataF (Babbage _) x) == (TxAuxDataF (Babbage _) y) = x == y
  (TxAuxDataF (Conway _) x) == (TxAuxDataF (Conway _) y) = x == y

pcAuxData :: Proof era -> TxAuxData era -> PDoc
pcAuxData p _x = ppString ("TxAuxData " ++ show p) -- TODO make this more accurate

genTxAuxDataF :: Proof era -> Gen (TxAuxDataF era)
genTxAuxDataF p@(Shelley _) = TxAuxDataF p <$> suchThat arbitrary (validateTxAuxData (protocolVersion p))
genTxAuxDataF p@(Allegra _) = TxAuxDataF p <$> suchThat arbitrary (validateTxAuxData (protocolVersion p))
genTxAuxDataF p@(Mary _) = TxAuxDataF p <$> suchThat arbitrary (validateTxAuxData (protocolVersion p))
genTxAuxDataF p@(Alonzo _) = TxAuxDataF p <$> suchThat arbitrary (validateTxAuxData (protocolVersion p))
genTxAuxDataF p@(Babbage _) = TxAuxDataF p <$> suchThat arbitrary (validateTxAuxData (protocolVersion p))
genTxAuxDataF p@(Conway _) = TxAuxDataF p <$> suchThat arbitrary (validateTxAuxData (protocolVersion p))

-- ==============

data TxF era where
  TxF :: Proof era -> Tx era -> TxF era

unTxF :: TxF era -> Tx era
unTxF (TxF _ x) = x

instance Show (TxF era) where
  show (TxF p x) = show ((unReflect pcTx p x) :: PDoc)

instance Eq (TxF era) where
  (TxF (Shelley _) x) == (TxF (Shelley _) y) = x == y
  (TxF (Allegra _) x) == (TxF (Allegra _) y) = x == y
  (TxF (Mary _) x) == (TxF (Mary _) y) = x == y
  (TxF (Alonzo _) x) == (TxF (Alonzo _) y) = x == y
  (TxF (Babbage _) x) == (TxF (Babbage _) y) = x == y
  (TxF (Conway _) x) == (TxF (Conway _) y) = x == y

-- ==============

data TxWitsF era where
  TxWitsF :: Proof era -> TxWits era -> TxWitsF era

unTxWitsF :: TxWitsF era -> TxWits era
unTxWitsF (TxWitsF _ x) = x

instance Show (TxWitsF era) where
  show (TxWitsF p x) = show ((unReflect pcWitnesses p x) :: PDoc)

instance Eq (TxWitsF era) where
  (TxWitsF (Shelley _) x) == (TxWitsF (Shelley _) y) = x == y
  (TxWitsF (Allegra _) x) == (TxWitsF (Allegra _) y) = x == y
  (TxWitsF (Mary _) x) == (TxWitsF (Mary _) y) = x == y
  (TxWitsF (Alonzo _) x) == (TxWitsF (Alonzo _) y) = x == y
  (TxWitsF (Babbage _) x) == (TxWitsF (Babbage _) y) = x == y
  (TxWitsF (Conway _) x) == (TxWitsF (Conway _) y) = x == y

-- ==============================

data TxBodyF era where
  TxBodyF :: Proof era -> TxBody era -> TxBodyF era

unTxBodyF :: TxBodyF era -> TxBody era
unTxBodyF (TxBodyF _ x) = x

instance Show (TxBodyF era) where
  show (TxBodyF p x) = show ((unReflect pcTxBody p x) :: PDoc)

instance Eq (TxBodyF era) where
  (TxBodyF (Shelley _) x) == (TxBodyF (Shelley _) y) = x == y
  (TxBodyF (Allegra _) x) == (TxBodyF (Allegra _) y) = x == y
  (TxBodyF (Mary _) x) == (TxBodyF (Mary _) y) = x == y
  (TxBodyF (Alonzo _) x) == (TxBodyF (Alonzo _) y) = x == y
  (TxBodyF (Babbage _) x) == (TxBodyF (Babbage _) y) = x == y
  (TxBodyF (Conway _) x) == (TxBodyF (Conway _) y) = x == y

-- ==================
data TxCertF era where
  TxCertF :: Proof era -> TxCert era -> TxCertF era

unTxCertF :: TxCertF era -> TxCert era
unTxCertF (TxCertF _ x) = x

instance Show (TxCertF era) where
  show (TxCertF p x) = show (pcTxCert p x)

instance Eq (TxCertF era) where
  (TxCertF (Shelley _) x) == (TxCertF (Shelley _) y) = x == y
  (TxCertF (Allegra _) x) == (TxCertF (Allegra _) y) = x == y
  (TxCertF (Mary _) x) == (TxCertF (Mary _) y) = x == y
  (TxCertF (Alonzo _) x) == (TxCertF (Alonzo _) y) = x == y
  (TxCertF (Babbage _) x) == (TxCertF (Babbage _) y) = x == y
  (TxCertF (Conway _) x) == (TxCertF (Conway _) y) = x == y

-- =========
data TxOutF era where
  TxOutF :: Proof era -> TxOut era -> TxOutF era

unTxOut :: TxOutF era -> TxOut era
unTxOut (TxOutF _ x) = x

instance Eq (TxOutF era) where
  x1 == x2 = compare x1 x2 == EQ

instance Ord (TxOutF era) where
  compare (TxOutF (Shelley _) (ShelleyTxOut a1 v1)) (TxOutF (Shelley _) (ShelleyTxOut a2 v2)) =
    compare a1 a2 <> compare v1 v2
  compare (TxOutF (Allegra _) (ShelleyTxOut a1 v1)) (TxOutF (Allegra _) (ShelleyTxOut a2 v2)) =
    compare (a1, v1) (a2, v2)
  compare (TxOutF (Mary _) (ShelleyTxOut a1 v1)) (TxOutF (Mary _) (ShelleyTxOut a2 v2)) =
    compare (a1, v1) (a2, v2)
  compare (TxOutF (Alonzo _) (AlonzoTxOut a1 v1 d1)) (TxOutF (Alonzo _) (AlonzoTxOut a2 v2 d2)) =
    compare (a1, v1, d1) (a2, v2, d2)
  compare (TxOutF (Babbage _) (BabbageTxOut a1 v1 d1 x1)) (TxOutF (Babbage _) (BabbageTxOut a2 v2 d2 x2)) =
    compare (a1, v1, d1, fmap hashScript x1) (a2, v2, d2, fmap hashScript x2)
  compare (TxOutF (Conway _) (BabbageTxOut a1 v1 d1 x1)) (TxOutF (Conway _) (BabbageTxOut a2 v2 d2 x2)) =
    compare (a1, v1, d1, fmap hashScript x1) (a2, v2, d2, fmap hashScript x2)

-- ======
data ValueF era where
  ValueF :: Proof era -> Value era -> ValueF era

unValue :: ValueF era -> Value era
unValue (ValueF _ v) = v

instance Crypto c => Ord (MaryValue c) where
  compare (MaryValue c1 m1) (MaryValue c2 m2) = compare (c1, m1) (c2, m2)

instance Crypto c => Ord (MultiAsset c) where
  compare (MultiAsset m1) (MultiAsset m2) = compare m1 m2

instance Eq (ValueF era) where
  x == y = compare x y == EQ

instance Ord (ValueF era) where
  (ValueF (Shelley _) x) `compare` (ValueF (Shelley _) y) = compare x y
  (ValueF (Allegra _) x) `compare` (ValueF (Allegra _) y) = compare x y
  (ValueF (Mary _) (MaryValue c1 m1)) `compare` (ValueF (Mary _) (MaryValue c2 m2)) = compare c1 c2 <> compare m1 m2
  (ValueF (Alonzo _) (MaryValue c1 m1)) `compare` (ValueF (Alonzo _) (MaryValue c2 m2)) = compare c1 c2 <> compare m1 m2
  (ValueF (Babbage _) (MaryValue c1 m1)) `compare` (ValueF (Babbage _) (MaryValue c2 m2)) = compare c1 c2 <> compare m1 m2
  (ValueF (Conway _) (MaryValue c1 m1)) `compare` (ValueF (Conway _) (MaryValue c2 m2)) = compare c1 c2 <> compare m1 m2

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

-- ========================

data ScriptsNeededF era where
  ScriptsNeededF :: Proof era -> ScriptsNeeded era -> ScriptsNeededF era

unScriptsNeededF :: ScriptsNeededF era -> ScriptsNeeded era
unScriptsNeededF (ScriptsNeededF _ v) = v

instance Show (ScriptsNeededF era) where
  show (ScriptsNeededF p t) = show (pcScriptsNeeded p t)

-- ========================

data ScriptF era where
  ScriptF :: Proof era -> Script era -> ScriptF era

unScriptF :: ScriptF era -> Script era
unScriptF (ScriptF _ v) = v

instance Show (ScriptF era) where
  show (ScriptF p t) = show ((unReflect pcScript p t) :: PDoc)

instance Eq (ScriptF era) where
  (ScriptF (Shelley _) x) == (ScriptF (Shelley _) y) = x == y
  (ScriptF (Allegra _) x) == (ScriptF (Allegra _) y) = x == y
  (ScriptF (Mary _) x) == (ScriptF (Mary _) y) = x == y
  (ScriptF (Alonzo _) x) == (ScriptF (Alonzo _) y) = x == y
  (ScriptF (Babbage _) x) == (ScriptF (Babbage _) y) = x == y
  (ScriptF (Conway _) x) == (ScriptF (Conway _) y) = x == y

genScriptF :: Era era => Proof era -> Gen (ScriptF era)
genScriptF proof = do
  tag <- arbitrary
  vi <- arbitrary
  m <- Map.fromList <$> (vectorOf 5 arbitrary)
  corescript <- genCoreScript proof tag m vi
  pure (ScriptF proof corescript)

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
              | total1 < 1 && smallest > 0 = errorMess ("Ran out of choices(2), total went negative: " ++ show total1) msgs
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

-- =======================================================

-- | Translate (s,cond,n), into a Size which
--   specifies the Int range on which the OrdCond is True.
--   The triple (s, EQL, 2) denotes s = 2
--              (s, LTH, 7) denotes s < 7
--              (s, GTH, 5) denotes s > 5 ...
ordCondToSize :: forall a. Adds a => (String, OrdCond, a) -> Size
ordCondToSize (_label, cond, n) = case cond of
  EQL -> SzExact $ toI n
  LTH -> SzMost $ decreaseBy1 @a $ toI n
  LTE -> SzMost $ toI n
  GTH -> SzLeast $ increaseBy1 @a $ toI n
  GTE -> SzLeast $ toI n

-- Translate some thing like [SumsTo _ x <= 4 + 6 + 9] where the variable 'x' is on the left
varOnLeft :: Adds a => String -> OrdCond -> a -> AddsSpec c
varOnLeft x cond n = AddsSpecSize x (varOnLeftSize x cond n)

varOnLeftSize :: Adds a => String -> OrdCond -> a -> Size
varOnLeftSize x cond n = ordCondToSize (x, cond, n)

-- Translate some thing like [SumsTo c 8 < 2 + x + 3] where the variable 'x' is on the right
varOnRight :: Adds a => a -> OrdCond -> a -> String -> AddsSpec c
varOnRight n cond m s = AddsSpecSize s (varOnRightSize n cond m s)

varOnRightSize :: Adds a => a -> OrdCond -> a -> String -> Size
varOnRightSize n cond m s = ordCondToSize (s, reverseOrdCond cond, minus [s] n m)

-- Translate some thing like [SumsTo (Negate x) <= 4 + 6 + 9] where the variable 'x'
-- is on the left, and we want to produce its negation.
varOnLeftNeg :: Adds a => String -> OrdCond -> a -> AddsSpec c
varOnLeftNeg s cond n = AddsSpecSize s (negateSize (ordCondToSize (s, cond, n)))

-- Translate some thing like [SumsTo 8 < 2 + (Negate x) + 3] where the
-- variable 'x' is on the right, and we want to produce its negation.
varOnRightNeg :: Adds a => a -> OrdCond -> a -> String -> AddsSpec c
varOnRightNeg n cond m s = AddsSpecSize s (negateSize (ordCondToSize (s, reverseOrdCond cond, minus [s] n m)))

-- | This function `reverseOrdCond` has been defined to handle the Pred SumsTo when the
--   variable is on the right-hand-side (rhs) of the OrdCond operator. In order to do that
--   we must multiply both sides of the inequality by (-1). For example consider
--   [SumsTo (DeltaCoin 1) ▵₳ -2 > ∑ ▵₳ -1 + x]
--                 Note variable x on the rhs ^
--    To solve we subtract 'x' from both sides, and add '▵₳ -2' from bothsides
--    getting      (-x) > ∑  (▵₳ -1) + (▵₳ -2)
--    reduced to   (-x) > ∑  (▵₳ -3)
--    to solve we must multiply both sides by (-1)
--                 x ?? ∑  (▵₳ 3)
-- What operator do we replace ?? by to make the original (▵₳ -2 > ∑ ▵₳ -1 + x) True?
-- The change in the operator is called "reversing" the operator. See
-- https://www.mathsisfun.com/algebra/inequality-solving.html for one explantion.
reverseOrdCond :: OrdCond -> OrdCond
reverseOrdCond EQL = EQL
reverseOrdCond LTH = GTH
reverseOrdCond LTE = GTE
reverseOrdCond GTH = LTH
reverseOrdCond GTE = LTE

-- =========================================================================
-- OrdCond
-- x <= y
--   ^     paramerterize over the condition
--
-- EQL = (==), LTH = (<), LTE = (<=), GTH = (>), GTE = (>=)
-- =========================================================================

-- | First order representation of the Ord comparisons
data OrdCond = EQL | LTH | LTE | GTH | GTE
  deriving (Eq)

instance Show OrdCond where
  show EQL = " = ∑ "
  show LTH = " < ∑ "
  show LTE = " <= ∑ "
  show GTH = " > ∑ "
  show GTE = " >= ∑ "

runOrdCond :: Ord c => OrdCond -> c -> c -> Bool
runOrdCond EQL x y = x == y
runOrdCond LTH x y = x < y
runOrdCond LTE x y = x <= y
runOrdCond GTH x y = x > y
runOrdCond GTE x y = x >= y

-- =========================================================================

-- | A specification of summation. like: lhs = ∑ rhs
--   The idea is that the 'rhs' can contain multiple terms: rhs = ∑ r1 + r2 + r3
--   Other example conditions:  (lhs < ∑ rhs), and (lhs >= ∑ rhs)
--   The invariant is that only a single variable appears in the summation.
--   It can appear on either side. If it appears in the 'rhs' then there
--   may be other, constant terms, in the rhs:  7 = ∑ 3 + v + 9
--   We always do the sums and solving at type Int, and cast back and forth to
--   accommodate other types with (Adds c) instances, using the methods 'fromI" and 'toI'
--   This allows the instance to deal with special conditions.
--   There are two (non-failure) possibilities 1) Var on the left, 2) Var on the right
--   We supply functions
--      varOnLeft  :: String -> OrdCond -> Integer -> AddsSpec c
--                SumsTo _ x <= 4 + 6 + 9 ===> (varOnLeft x LTE 19) == (AddsSpecSize x (AtMost 19))
--      varOnRight :: Integer -> OrdCond -> Integer -> String -> AddsSpec c
--                SumsTo _ 8 < 2 + x + 3 ===> (varOnRight 8 LTH 5 x) == (AddsSpecSize x (AtLeast 4))
--   But internally we store the information as a String and a Size (I.e. a range of Int)
data AddsSpec c where
  AddsSpecSize ::
    -- | name
    String ->
    -- | total (range like (4 .. 12))
    Size ->
    AddsSpec c
  AddsSpecAny :: AddsSpec c
  AddsSpecNever :: [String] -> AddsSpec c

instance LiftT (AddsSpec c) where
  liftT (AddsSpecNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = AddsSpecNever s
  dropT (Typed (Right x)) = x

instance Show (AddsSpec c) where show = showAddsSpec

instance Semigroup (AddsSpec c) where (<>) = mergeAddsSpec
instance Monoid (AddsSpec c) where mempty = AddsSpecAny

showAddsSpec :: AddsSpec c -> String
showAddsSpec AddsSpecAny = "AddsSpecAny"
showAddsSpec (AddsSpecSize s size) = sepsP ["AddsSpecSize", s, show size]
showAddsSpec (AddsSpecNever _) = "AddsSpecNever"

mergeAddsSpec :: AddsSpec c -> AddsSpec c -> AddsSpec c
mergeAddsSpec (AddsSpecNever xs) (AddsSpecNever ys) = AddsSpecNever (xs ++ ys)
mergeAddsSpec x@(AddsSpecNever _) _ = x
mergeAddsSpec _ x@(AddsSpecNever _) = x
mergeAddsSpec AddsSpecAny x = x
mergeAddsSpec x AddsSpecAny = x
mergeAddsSpec a@(AddsSpecSize nam1 size1) b@(AddsSpecSize nam2 size2) =
  if nam1 /= nam2
    then
      AddsSpecNever
        [ "vars " ++ nam1 ++ " and " ++ nam2 ++ " are not the same."
        , show a ++ " " ++ show b ++ " are inconsistent."
        ]
    else case size1 <> size2 of
      (SzNever xs) -> AddsSpecNever (xs ++ [show a ++ " " ++ show a ++ " are inconsistent."])
      size3 -> AddsSpecSize nam1 size3

-- =======================================
-- Helper function to create AddsSpecSize

-- Translate some thing like [SumsTo _ x <= 4 + 6 + 9] where the variable 'x' is on the left
vLeft :: String -> OrdCond -> Int -> (AddsSpec c)
vLeft x cond n = AddsSpecSize x (vLeftSize x cond n)

vLeftSize :: String -> OrdCond -> Int -> Size
vLeftSize x cond n = ordCondToSize (x, cond, n)

-- Translate some thing like [SumsTo c 8 < 2 + x + 3] where the variable 'x' is on the right
vRight :: Int -> OrdCond -> Int -> String -> AddsSpec c
vRight n cond m s = AddsSpecSize s (vRightSize n cond m s)

-- vRightSize :: Adds c => c -> OrdCond -> Int ->String - Size
vRightSize :: Int -> OrdCond -> Int -> String -> Size
vRightSize n cond m s = ordCondToSize (s, reverseOrdCond cond, n - m)

-- Translate some thing like [SumsTo (Negate x) <= 4 + 6 + 9] where the variable 'x'
-- is on the left, and we want to produce its negation.
vLeftNeg :: String -> OrdCond -> Int -> (AddsSpec c)
vLeftNeg s cond n = AddsSpecSize s (negateSize (ordCondToSize (s, cond, n)))

-- Translate some thing like [SumsTo 8 < 2 + (Negate x) + 3] where the
-- variable 'x' is on the right, and we want to produce its negation.
vRightNeg :: Int -> OrdCond -> Int -> String -> AddsSpec c
vRightNeg n cond m s = AddsSpecSize s (negateSize (ordCondToSize (s, reverseOrdCond cond, n - m)))
