{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

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
  Proof (..),
  Evidence (..),
  Size (SzExact, ..),
)
where

import Cardano.Ledger.BaseTypes (EpochNo, ProtVer (..), SlotNo (..))
import Cardano.Ledger.Binary.Version (Version)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.EpochBoundary (SnapShots (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (GenDelegPair (..), KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.PoolParams (PoolParams (ppId))
import Cardano.Ledger.Pretty (ppInteger, ppString)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val ((<+>)))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe (Eql, Shape (..), Shaped (..), Singleton (..), cmpIndex, (:~:) (Refl))
import Data.Word (Word64)
import Lens.Micro
import Numeric.Natural (Natural)
import Prettyprinter (hsep)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Constrained.Classes (
  PParams (..),
  PParamsUpdate (..),
  TxOut (..),
  Value (..),
  genPParams,
  genPParamsUpdate,
  genTxOut,
  genUTxO,
  genValue,
  liftUTxO,
  txOutCoinL,
  unPParams,
  unPParamsUpdate,
  unTxOut,
  unValue,
 )
import Test.Cardano.Ledger.Constrained.Combinators (mapSized, setSized)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.PrettyCore (
  credSummary,
  keyHashSummary,
  pcCoin,
  pcFutureGenDeleg,
  pcGenDelegPair,
  pcIndividualPoolStake,
  pcReward,
  pcTxIn,
  withEraPParams,
 )
import Test.Cardano.Ledger.Generic.Proof (Evidence (..), Proof (..))
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck hiding (Fixed, total)

-- =======================================================================
infixr 0 :->

data Rep era t where
  RationalR :: Rep era Rational
  CoinR :: Rep era Coin
  EpochR :: Rep era EpochNo
  (:->) :: Rep era a -> Rep era b -> Rep era (a -> b)
  MapR :: Ord a => Rep era a -> Rep era b -> Rep era (Map a b)
  SetR :: Ord a => Rep era a -> Rep era (Set a)
  ListR :: Rep era a -> Rep era [a]
  CredR :: Rep era (Credential 'Staking (EraCrypto era))
  PoolHashR :: Rep era (KeyHash 'StakePool (EraCrypto era))
  WitHashR :: Rep era (KeyHash 'Witness (EraCrypto era))
  GenHashR :: Rep era (KeyHash 'Genesis (EraCrypto era))
  GenDelegHashR :: Rep era (KeyHash 'GenesisDelegate (EraCrypto era))
  PoolParamsR :: Rep era (PoolParams (EraCrypto era))
  NewEpochStateR :: Rep era (NewEpochState era)
  IntR :: Rep era Int
  FloatR :: Rep era Float
  NaturalR :: Rep era Natural
  Word64R :: Rep era Word64
  TxInR :: Rep era (TxIn (EraCrypto era))
  StringR :: Rep era String
  UnitR :: Rep era ()
  ProtVerR :: Proof era -> Rep era ProtVer -- We need the Proof to get arbitrary instances correct
  -- \^ Rep's for type families (or those that embed type families)
  ValueR :: Proof era -> Rep era (Value era)
  UTxOR :: Proof era -> Rep era (UTxO era)
  TxOutR :: Proof era -> Rep era (TxOut era)
  PParamsR :: Proof era -> Rep era (PParams era)
  PParamsUpdateR :: Proof era -> Rep era (PParamsUpdate era)
  --
  DeltaCoinR :: Rep era DeltaCoin
  GenDelegPairR :: Rep era (GenDelegPair (EraCrypto era))
  FutureGenDelegR :: Rep era (FutureGenDeleg (EraCrypto era))
  PPUPStateR :: Proof era -> Rep era (ShelleyPPUPState era)
  PtrR :: Rep era Ptr
  IPoolStakeR :: Rep era (IndividualPoolStake (EraCrypto era))
  SnapShotsR :: Rep era (SnapShots (EraCrypto era))
  RewardR :: Rep era (Reward (EraCrypto era))
  MaybeR :: Rep era t -> Rep era (Maybe t)
  SlotNoR :: Rep era SlotNo
  SizeR :: Rep era Size

-- ===========================================================
-- Proof of Rep equality

instance Singleton (Rep e) where
  testEql RationalR RationalR = Just Refl
  testEql CoinR CoinR = Just Refl
  testEql EpochR EpochR = Just Refl
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
  testEql CredR CredR = Just Refl
  testEql PoolHashR PoolHashR = Just Refl
  testEql WitHashR WitHashR = Just Refl
  testEql GenHashR GenHashR = Just Refl
  testEql GenDelegHashR GenDelegHashR = Just Refl
  testEql PoolParamsR PoolParamsR = Just Refl
  testEql NewEpochStateR NewEpochStateR = Just Refl
  testEql IntR IntR = Just Refl
  testEql FloatR FloatR = Just Refl
  testEql NaturalR NaturalR = Just Refl
  testEql Word64R Word64R = Just Refl
  testEql TxInR TxInR = Just Refl
  testEql StringR StringR = Just Refl
  testEql UnitR UnitR = Just Refl
  testEql (ProtVerR c) (ProtVerR d) =
    do Refl <- testEql c d; pure Refl
  testEql (ValueR c) (ValueR d) =
    do Refl <- testEql c d; pure Refl
  testEql (UTxOR p1) (UTxOR p2) = do
    Refl <- testEql p1 p2
    pure Refl
  testEql (TxOutR c) (TxOutR d) =
    do Refl <- testEql c d; pure Refl
  testEql (PParamsR c) (PParamsR d) =
    do Refl <- testEql c d; pure Refl
  testEql (PParamsUpdateR c) (PParamsUpdateR d) =
    do Refl <- testEql c d; pure Refl
  testEql DeltaCoinR DeltaCoinR = Just Refl
  testEql GenDelegPairR GenDelegPairR = Just Refl
  testEql FutureGenDelegR FutureGenDelegR = Just Refl
  testEql (PPUPStateR c) (PPUPStateR d) = do Refl <- testEql c d; pure Refl
  testEql PtrR PtrR = Just Refl
  testEql IPoolStakeR IPoolStakeR = Just Refl
  testEql SnapShotsR SnapShotsR = Just Refl
  testEql RewardR RewardR = Just Refl
  testEql (MaybeR c) (MaybeR d) =
    do Refl <- testEql c d; pure Refl
  testEql SlotNoR SlotNoR = Just Refl
  testEql SizeR SizeR = Just Refl
  testEql _ _ = Nothing
  cmpIndex x y = compare (shape x) (shape y)

-- ============================================================
-- Show instances

instance Show (Rep era t) where
  show CoinR = "Coin"
  show (a :-> b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (MapR a b) = "(Map " ++ show a ++ " " ++ show b ++ ")"
  show (SetR a) = "(Set " ++ show a ++ " " ++ ")"
  show (ListR a) = "[" ++ show a ++ "]"
  show CredR = "Cred" -- "(Credential 'Staking c)"
  show PoolHashR = "(KeyHash 'StakePool c)"
  show WitHashR = "(KeyHash 'Witness c)"
  show GenHashR = "(KeyHash 'Genesis c)"
  show GenDelegHashR = "(KeyHash 'GenesisDelegate c)"
  show PoolParamsR = "(PoolParams c)"
  show EpochR = "EpochNo"
  show RationalR = "Rational"
  show Word64R = "Word64"
  show IntR = "Int"
  show NaturalR = "Natural"
  show FloatR = "Float"
  show TxInR = "TxIn"
  show (ValueR x) = "(Value " ++ show x ++ ")"
  show (TxOutR x) = "(TxOut " ++ show x ++ ")"
  show (UTxOR x) = "(UTxO " ++ show x ++ ")"
  show (PParamsR x) = "(PParams " ++ show x ++ ")"
  show (PParamsUpdateR x) = "(PParamsUpdate " ++ show x ++ ")"
  show StringR = "String"
  show DeltaCoinR = "DeltaCoin"
  show GenDelegPairR = "(GenDelegPair c)"
  show FutureGenDelegR = "(FutureGenDeleg c)"
  show (PPUPStateR p) = "(ShelleyPPUPState " ++ show p ++ ")"
  show PtrR = "Ptr"
  show IPoolStakeR = "(IndividualPoolStake c)"
  show SnapShotsR = "(SnapShots c)"
  show UnitR = "()"
  show RewardR = "(Reward c)"
  show (MaybeR x) = "(Maybe " ++ show x ++ ")"
  show NewEpochStateR = "NewEpochState"
  show (ProtVerR x) = "(ProtVer " ++ show x ++ ")"
  show SlotNoR = "(SlotNo c)"
  show SizeR = "Size"

synopsis :: forall e t. Rep e t -> t -> String
synopsis RationalR r = show r
synopsis CoinR c = show (pcCoin c)
synopsis EpochR e = show e
synopsis (a :-> b) _ = "(Arrow " ++ show a ++ " " ++ show b ++ ")"
synopsis Word64R w = show w
synopsis rep@(MapR a b) mp = case Map.toList mp of
  [] -> "(empty::Map " ++ show a ++ " " ++ show b ++ ")"
  ((d, r) : _) -> "Map{" ++ synopsis a d ++ " -> " ++ synopsis b r ++ " | size = " ++ show (Map.size mp) ++ synSum rep mp ++ "}"
synopsis (SetR IntR) x = "Set" ++ show (Set.toList x)
synopsis rep@(SetR a) t
  | Set.null t = "(empty::Set " ++ show a ++ ")"
  | otherwise = "Set{" ++ synopsis a (head (Set.elems t)) ++ " | size = " ++ show (Set.size t) ++ synSum rep t ++ "}"
synopsis (ListR IntR) x = show x
synopsis rep@(ListR a) ll = case ll of
  [] -> "(empty::" ++ show (ListR a) ++ "]"
  (d : _) -> "[" ++ synopsis a d ++ " | size = " ++ show (length ll) ++ synSum rep ll ++ "]"
synopsis CredR c = show (credSummary c)
synopsis PoolHashR k = "(KeyHash 'PoolStake " ++ show (keyHashSummary k) ++ ")"
synopsis GenHashR k = "(KeyHash 'Genesis " ++ show (keyHashSummary k) ++ ")"
synopsis WitHashR k = "(KeyHash 'Witness " ++ show (keyHashSummary k) ++ ")"
synopsis GenDelegHashR k = "(KeyHash 'GenesisDelegate " ++ show (keyHashSummary k) ++ ")"
synopsis PoolParamsR pp = "(PoolParams " ++ synopsis @e PoolHashR (ppId pp) ++ ")"
synopsis IntR n = show n
synopsis NaturalR n = show n
synopsis FloatR n = show n
synopsis TxInR txin = show (pcTxIn txin)
synopsis StringR s = show s
synopsis (ValueR _) x = show x
synopsis (TxOutR _) x = show x
synopsis (UTxOR p) (UTxO mp) = "UTxO( " ++ synopsis (MapR TxInR (TxOutR p)) (Map.map (TxOut p) mp) ++ " )"
synopsis (PParamsR _) (PParams p x) = synopsisPParam p x
synopsis (PParamsUpdateR _) _ = "PParamsUpdate ..."
synopsis DeltaCoinR (DeltaCoin n) = show (hsep [ppString "▵₳", ppInteger n])
synopsis GenDelegPairR x = show (pcGenDelegPair x)
synopsis FutureGenDelegR x = show (pcFutureGenDeleg x)
synopsis (PPUPStateR _) _ = "PPUPStateR ..."
synopsis PtrR p = show p
synopsis IPoolStakeR p = show (pcIndividualPoolStake p)
synopsis SnapShotsR _ = "SnapShots ..."
synopsis UnitR () = "()"
synopsis RewardR x = show (pcReward x)
synopsis (MaybeR _) Nothing = "Nothing"
synopsis (MaybeR x) (Just y) = "(Just " ++ synopsis x y ++ ")"
synopsis NewEpochStateR _ = "NewEpochStateR ..."
synopsis (ProtVerR _) (ProtVer x y) = "(" ++ show x ++ " " ++ show y ++ ")"
synopsis SlotNoR x = show x
synopsis SizeR x = show x

synSum :: Rep era a -> a -> String
synSum (MapR _ CoinR) m = ", sum = " ++ show (pcCoin (Map.foldl' (<>) mempty m))
synSum (MapR _ RationalR) m = ", sum = " ++ show (Map.foldl' (+) 0 m)
synSum (MapR _ IntR) m = ", sum = " ++ show (Map.foldl' (+) 0 m)
synSum (MapR _ Word64R) m = ", sum = " ++ show (Map.foldl' (+) 0 m)
synSum (MapR _ IPoolStakeR) m = ", sum = " ++ show (Map.foldl' accum 0 m)
  where
    accum z (IndividualPoolStake rat _) = z + rat
synSum (MapR _ (TxOutR proof)) m = ", sum = " ++ show (Map.foldl' (accum proof) (Coin 0) m)
  where
    accum :: Proof era -> Coin -> TxOut era -> Coin
    accum (Shelley _) z (TxOut _ out) = z <+> (out ^. txOutCoinL)
    accum (Allegra _) z (TxOut _ out) = z <+> (out ^. txOutCoinL)
    accum (Mary _) z (TxOut _ out) = z <+> (out ^. txOutCoinL)
    accum (Alonzo _) z (TxOut _ out) = z <+> (out ^. txOutCoinL)
    accum (Babbage _) z (TxOut _ out) = z <+> (out ^. txOutCoinL)
    accum (Conway _) z (TxOut _ out) = z <+> (out ^. txOutCoinL)
synSum (SetR CoinR) m = ", sum = " ++ show (pcCoin (Set.foldl' (<>) mempty m))
synSum (SetR RationalR) m = ", sum = " ++ show (Set.foldl' (+) 0 m)
synSum (ListR CoinR) m = ", sum = " ++ show (List.foldl' (<>) mempty m)
synSum (ListR RationalR) m = ", sum = " ++ show (List.foldl' (+) 0 m)
synSum (ListR IntR) m = ", sum = " ++ show (List.foldl' (+) 0 m)
synSum (ListR Word64R) m = ", sum = " ++ show (List.foldl' (+) 0 m)
synSum _ _ = ""

-- ==================================================

instance Shaped (Rep era) any where
  shape CoinR = Nullary 0
  shape (a :-> b) = Nary 1 [shape a, shape b]
  shape (MapR a b) = Nary 2 [shape a, shape b]
  shape (SetR a) = Nary 3 [shape a]
  shape (ListR a) = Nary 4 [shape a]
  shape CredR = Nullary 5
  shape PoolHashR = Nullary 6
  shape WitHashR = Nullary 7
  shape GenHashR = Nullary 8
  shape GenDelegHashR = Nullary 9
  shape PoolParamsR = Nullary 10
  shape EpochR = Nullary 11
  shape RationalR = Nullary 12
  shape Word64R = Nullary 13
  shape IntR = Nullary 14
  shape TxInR = Nullary 15
  shape StringR = Nullary 16
  shape (ValueR p) = Nary 17 [shape p]
  shape (TxOutR p) = Nary 18 [shape p]
  shape (UTxOR p) = Nary 19 [shape p]
  shape (PParamsR p) = Nary 20 [shape p]
  shape (PParamsUpdateR p) = Nary 21 [shape p]
  shape DeltaCoinR = Nullary 22
  shape GenDelegPairR = Nullary 23
  shape FutureGenDelegR = Nullary 24
  shape (PPUPStateR p) = Nary 25 [shape p]
  shape PtrR = Nullary 26
  shape IPoolStakeR = Nullary 27
  shape SnapShotsR = Nullary 28
  shape NaturalR = Nullary 29
  shape FloatR = Nullary 30
  shape UnitR = Nullary 31
  shape RewardR = Nullary 32
  shape (MaybeR x) = Nary 33 [shape x]
  shape NewEpochStateR = Nullary 34
  shape (ProtVerR x) = Nary 35 [shape x]
  shape SlotNoR = Nullary 36
  shape SizeR = Nullary 37

compareRep :: forall era t s. Rep era t -> Rep era s -> Ordering
compareRep x y = cmpIndex @(Rep era) x y

-- ================================================

genSizedRep ::
  (Era era) =>
  Int ->
  Rep era t ->
  Gen t
genSizedRep _ CoinR = do Positive n <- arbitrary; pure (Coin n)
genSizedRep n (_a :-> b) = const <$> genSizedRep n b
genSizedRep n r@(MapR a b) = do
  mapSized ["From genSizedRep " ++ show r] n (genSizedRep n a) (genSizedRep n b)
genSizedRep n r@(SetR a) = do
  setSized ["From genSizedRep " ++ show r] n (genSizedRep n a)
genSizedRep n (ListR a) = vectorOf n (genSizedRep n a)
genSizedRep _ CredR = arbitrary
genSizedRep _ PoolHashR = arbitrary
genSizedRep _ WitHashR = arbitrary
genSizedRep _ GenHashR = arbitrary
genSizedRep _ GenDelegHashR = arbitrary
genSizedRep _ PoolParamsR = arbitrary
genSizedRep _ EpochR = arbitrary
genSizedRep _ RationalR = arbitrary
genSizedRep _ Word64R = choose (1, 1000)
genSizedRep _ IntR = resize 10 arbitrary
genSizedRep _ NaturalR = arbitrary
genSizedRep _ FloatR = arbitrary
genSizedRep _ TxInR = arbitrary
genSizedRep n StringR = vectorOf n arbitrary
genSizedRep _ (ValueR p) = genValue p
genSizedRep _ (TxOutR p) = genTxOut p
genSizedRep _n (UTxOR p) = genUTxO p
genSizedRep _ (PParamsR p) = genPParams p
genSizedRep _ (PParamsUpdateR p) = genPParamsUpdate p
genSizedRep _ DeltaCoinR = arbitrary
genSizedRep _ GenDelegPairR = arbitrary
genSizedRep _ FutureGenDelegR = arbitrary
genSizedRep _ r@(PPUPStateR _) = genpup r
genSizedRep _ PtrR = arbitrary
genSizedRep _ IPoolStakeR = arbitrary
genSizedRep _ SnapShotsR = arbitrary
genSizedRep _ UnitR = arbitrary
genSizedRep _ RewardR = arbitrary
genSizedRep n (MaybeR x) = frequency [(1, pure Nothing), (5, Just <$> genSizedRep n x)]
genSizedRep _ NewEpochStateR = undefined
genSizedRep _ (ProtVerR proof) = genProtVer proof
genSizedRep _ SlotNoR = arbitrary
genSizedRep _ SizeR = do lo <- choose (1, 6); hi <- choose (6, 10); pure (SzRng lo hi)

genRep ::
  Era era =>
  Rep era b ->
  Gen b
genRep x = do (NonNegative n) <- arbitrary; genSizedRep n x

genProtVer :: Era era => Proof era -> Gen ProtVer
genProtVer proof = frequency (zipWith pair [count, count - 1 .. 1] versions)
  where
    versions = protVerRange proof
    count = length versions
    pair n version = (n, ProtVer version <$> elements [0 .. 4])

protVerRange :: forall era. Era era => Proof era -> [Version]
protVerRange _ = [Core.eraProtVerLow @era .. Core.eraProtVerHigh @era]

genpup :: Rep era (ShelleyPPUPState era) -> Gen (ShelleyPPUPState era)
genpup (PPUPStateR (Shelley _)) = arbitrary
genpup (PPUPStateR (Allegra _)) = arbitrary
genpup (PPUPStateR (Mary _)) = arbitrary
genpup (PPUPStateR (Alonzo _)) = arbitrary
genpup (PPUPStateR (Babbage _)) = arbitrary
genpup (PPUPStateR (Conway _)) = arbitrary

-- ===========================

synopsisPParam :: forall era. Proof era -> Core.PParams era -> String
synopsisPParam p x = withEraPParams p help
  where
    help :: Core.EraPParams era => String
    help =
      "PParams{maxBBSize="
        ++ show (x ^. Core.ppMaxBBSizeL)
        ++ ", maxBHSize="
        ++ show (x ^. Core.ppMaxBBSizeL)
        ++ ", maxTxSize="
        ++ show (x ^. Core.ppMaxTxSizeL)
        ++ ", protoVersion="
        ++ (synopsis (ProtVerR p) (x ^. Core.ppProtocolVersionL))
        ++ "}"

-- ==========================================================================
-- The type Size is defined in TypeRep.hs, because its type must be known in
-- Ast.hs (which it needs an (Rep era Size) SizeR defined here). It also acts
-- like a Spec, so here are its Spec like Monoid and Semigroup instances.

data Size
  = SzNever [String]
  | SzAny
  | SzLeast Int
  | SzMost Int
  | SzRng Int Int -- (SzRng i j) = [i .. j] . Invariant i <= j
  deriving (Ord, Eq)

sameR :: Size -> Maybe Int
sameR (SzRng x y) = if x == y then Just x else Nothing
sameR _ = Nothing

pattern SzExact :: Int -> Size
pattern SzExact x <- (sameR -> Just x)
  where
    SzExact x = (SzRng x x)

instance Show Size where
  show (SzNever _) = "NeverSize"
  show SzAny = "AnySize"
  show (SzLeast n) = "(AtLeast " ++ show n ++ ")"
  show (SzMost n) = "(AtMost " ++ show n ++ ")"
  show (SzRng i j) = "(Range " ++ show i ++ " " ++ show j ++ ")"

mergeSize :: Size -> Size -> Size
mergeSize SzAny x = x
mergeSize x SzAny = x
mergeSize (SzNever xs) (SzNever ys) = SzNever (xs ++ ys)
mergeSize _ (SzNever xs) = SzNever xs
mergeSize (SzNever xs) _ = SzNever xs
mergeSize (SzLeast x) (SzLeast y) = SzLeast (max x y)
mergeSize (SzLeast x) (SzMost y) | x <= y = SzRng x y
mergeSize (SzLeast x) (SzRng i j) | x <= i = SzRng i j
mergeSize (SzLeast x) (SzRng i j) | x >= i && x <= j = SzRng x j
mergeSize (SzMost x) (SzMost y) = SzMost (min x y)
mergeSize (SzMost y) (SzLeast x) | x <= y = SzRng x y
mergeSize (SzMost x) (SzRng i j) | x >= j = SzRng i j
mergeSize (SzMost x) (SzRng i j) | x >= i && x <= j = SzRng i x
mergeSize (SzRng i j) (SzLeast x) | x <= i = SzRng i j
mergeSize (SzRng i j) (SzLeast x) | x >= i && x <= j = SzRng x j
mergeSize (SzRng i j) (SzMost x) | x >= j = SzRng i j
mergeSize (SzRng i j) (SzMost x) | x >= i && x <= j = SzRng i x
mergeSize (SzRng i j) (SzRng m n) | x <= y = SzRng x y
  where
    x = max i m
    y = min j n
mergeSize a b = SzNever ["Size specifications " ++ show a ++ " and " ++ show b ++ " are inconsistent."]

instance Monoid Size where mempty = SzAny

instance Semigroup Size where
  (<>) = mergeSize
