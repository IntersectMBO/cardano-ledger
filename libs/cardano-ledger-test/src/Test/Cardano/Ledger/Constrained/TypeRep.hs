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
  shrinkRep,
  TxOutF (..),
  unTxOut,
  ValueF (..),
  unValue,
  PParamsF (..),
  unPParams,
  PParamsUpdateF (..),
  unPParamsUpdate,
  liftUTxO,
  Proof (..),
  Evidence (..),
  stringR,
)
where

import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits, Tag)
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), Network (..), ProtVer (..), SlotNo (..))
import Cardano.Ledger.Binary.Version (Version)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.EpochBoundary (SnapShots (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Hashes (DataHash, ScriptHash (..))
import Cardano.Ledger.Keys (GenDelegPair (..), KeyHash, KeyRole (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.PoolParams (PoolParams (ppId))
import Cardano.Ledger.Pretty (ppInteger, ppRecord', ppString)
import Cardano.Ledger.Pretty.Alonzo (ppRdmrPtr)
import Cardano.Ledger.Pretty.Mary (ppValidityInterval)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val ((<+>)))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
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
  PParamsF (..),
  PParamsUpdateF (..),
  ScriptF (..),
  TxOutF (..),
  ValueF (..),
  genPParams,
  genPParamsUpdate,
  genScriptF,
  genTxOut,
  genUTxO,
  genValue,
  liftUTxO,
  unPParams,
  unPParamsUpdate,
  unTxOut,
  unValue,
 )
import Test.Cardano.Ledger.Constrained.Combinators (mapSized, setSized)
import Test.Cardano.Ledger.Constrained.Size (Size (..))
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields (WitnessesField (..))
import Test.Cardano.Ledger.Generic.PrettyCore (
  credSummary,
  keyHashSummary,
  pcAddr,
  pcCoin,
  pcDCert,
  pcData,
  pcDataHash,
  pcExUnits,
  pcFutureGenDeleg,
  pcGenDelegPair,
  pcIndividualPoolStake,
  pcPParamsSynopsis,
  pcReward,
  pcRewardAcnt,
  pcScriptHash,
  pcTxIn,
  pcWitnessesField,
 )
import Test.Cardano.Ledger.Generic.Proof (Evidence (..), Proof (..), unReflect)
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
  VCredR :: Rep era (Credential 'DRepRole (EraCrypto era))
  PoolHashR :: Rep era (KeyHash 'StakePool (EraCrypto era))
  WitHashR :: Rep era (KeyHash 'Witness (EraCrypto era))
  GenHashR :: Rep era (KeyHash 'Genesis (EraCrypto era))
  GenDelegHashR :: Rep era (KeyHash 'GenesisDelegate (EraCrypto era))
  VHashR :: Rep era (KeyHash 'DRepRole (EraCrypto era))
  CommColdHashR :: Rep era (Credential 'ColdCommitteeRole (EraCrypto era))
  CommHotHashR :: Rep era (Credential 'HotCommitteeRole (EraCrypto era))
  PoolParamsR :: Rep era (PoolParams (EraCrypto era))
  NewEpochStateR :: Rep era (NewEpochState era)
  IntR :: Rep era Int
  FloatR :: Rep era Float
  NaturalR :: Rep era Natural
  Word64R :: Rep era Word64
  TxInR :: Rep era (TxIn (EraCrypto era))
  CharR :: Rep era Char
  UnitR :: Rep era ()
  PairR :: Rep era a -> Rep era b -> Rep era (a, b)
  ProtVerR :: Proof era -> Rep era ProtVer -- We need the Proof to get arbitrary instances correct
  -- \^ Rep's for type families (or those that embed type families)
  ValueR :: Proof era -> Rep era (ValueF era)
  UTxOR :: Proof era -> Rep era (UTxO era)
  TxOutR :: Proof era -> Rep era (TxOutF era)
  PParamsR :: Proof era -> Rep era (PParamsF era)
  PParamsUpdateR :: Proof era -> Rep era (PParamsUpdateF era)
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
  MultiAssetR :: Rep era (MultiAsset (EraCrypto era))
  PolicyIDR :: Rep era (PolicyID (EraCrypto era))
  WitnessesFieldR :: Proof era -> Rep era (WitnessesField era)
  AssetNameR :: Rep era AssetName
  DCertR :: Rep era (DCert (EraCrypto era))
  RewardAcntR :: Rep era (RewardAcnt (EraCrypto era))
  ValidityIntervalR :: Rep era ValidityInterval
  KeyPairR :: Rep era (KeyPair 'Witness (EraCrypto era))
  GenR :: Rep era x -> Rep era (Gen x)
  ScriptR :: Proof era -> Rep era (ScriptF era)
  ScriptHashR :: Rep era (ScriptHash (EraCrypto era))
  NetworkR :: Rep era Network
  RdmrPtrR :: Rep era RdmrPtr
  DataR :: Proof era -> Rep era (Data era)
  ExUnitsR :: Rep era ExUnits
  TagR :: Rep era Tag
  DataHashR :: Rep era (DataHash (EraCrypto era))
  AddrR :: Rep era (Addr (EraCrypto era))
  PCredR :: Rep era (Credential 'Payment (EraCrypto era))

stringR :: Rep era String
stringR = ListR CharR

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
  testEql VCredR VCredR = Just Refl
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
  testEql CharR CharR = Just Refl
  testEql UnitR UnitR = Just Refl
  testEql (PairR a b) (PairR x y) = do
    Refl <- testEql a x
    Refl <- testEql b y
    Just Refl
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
  testEql (WitnessesFieldR c) (WitnessesFieldR d) =
    do Refl <- testEql c d; pure Refl
  testEql MultiAssetR MultiAssetR = pure Refl
  testEql PolicyIDR PolicyIDR = pure Refl
  testEql AssetNameR AssetNameR = pure Refl
  testEql DCertR DCertR = Just Refl
  testEql ValidityIntervalR ValidityIntervalR = Just Refl
  testEql RewardAcntR RewardAcntR = Just Refl
  testEql KeyPairR KeyPairR = Just Refl
  testEql (GenR x) (GenR y) = do Refl <- testEql x y; pure Refl
  testEql (ScriptR c) (ScriptR d) =
    do Refl <- testEql c d; pure Refl
  testEql ScriptHashR ScriptHashR = Just Refl
  testEql NetworkR NetworkR = Just Refl
  testEql RdmrPtrR RdmrPtrR = Just Refl
  testEql (DataR c) (DataR d) = do Refl <- testEql c d; pure Refl
  testEql ExUnitsR ExUnitsR = Just Refl
  testEql TagR TagR = Just Refl
  testEql DataHashR DataHashR = Just Refl
  testEql AddrR AddrR = Just Refl
  testEql PCredR PCredR = Just Refl
  testEql _ _ = Nothing
  cmpIndex x y = compare (shape x) (shape y)

-- ============================================================
-- Show instances

instance Show (Rep era t) where
  show CoinR = "Coin"
  show (a :-> b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (MapR a b) = "(Map " ++ show a ++ " " ++ show b ++ ")"
  show (SetR a) = "(Set " ++ show a ++ ")"
  show (ListR a) = "[" ++ show a ++ "]"
  show CredR = "(Credential 'Staking c)"
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
  show CharR = "Char"
  show DeltaCoinR = "DeltaCoin"
  show GenDelegPairR = "(GenDelegPair c)"
  show FutureGenDelegR = "(FutureGenDeleg c)"
  show (PPUPStateR p) = "(ShelleyPPUPState " ++ show p ++ ")"
  show PtrR = "Ptr"
  show IPoolStakeR = "(IndividualPoolStake c)"
  show SnapShotsR = "(SnapShots c)"
  show UnitR = "()"
  show (PairR a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show RewardR = "(Reward c)"
  show (MaybeR x) = "(Maybe " ++ show x ++ ")"
  show NewEpochStateR = "NewEpochState"
  show (ProtVerR x) = "(ProtVer " ++ show x ++ ")"
  show SlotNoR = "(SlotNo c)"
  show SizeR = "Size"
  show VCredR = "VCredR"
  show VHashR = "VHashR"
  show CommColdHashR = "CommColdHash"
  show CommHotHashR = "CommHotHash"
  show MultiAssetR = "(MutiAsset c)"
  show PolicyIDR = "(PolicyID c)"
  show (WitnessesFieldR p) = "(WitnessesField " ++ show p ++ ")"
  show AssetNameR = "AssetName"
  show DCertR = "(DCert c)"
  show RewardAcntR = "(RewardAcnt c)"
  show ValidityIntervalR = "ValidityInterval"
  show KeyPairR = "(KeyPair 'Witness era)"
  show (GenR x) = "(Gen " ++ show x ++ ")"
  show (ScriptR x) = "(Script " ++ show x ++ ")"
  show ScriptHashR = "(ScriptHash c)"
  show NetworkR = "Network"
  show RdmrPtrR = "RdmrPtr"
  show (DataR p) = "(Data " ++ show p ++ ")"
  show ExUnitsR = "ExUnits"
  show TagR = "Tag"
  show DataHashR = "(DataHash c)"
  show AddrR = "(Addr c)"
  show PCredR = "(Credential 'Payment c)"

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
synopsis (SetR Word64R) x = "Set" ++ show (Set.toList x)
synopsis rep@(SetR a) t
  | Set.null t = "(empty::Set " ++ show a ++ ")"
  | otherwise = "Set{" ++ synopsis a (head (Set.elems t)) ++ " | size = " ++ show (Set.size t) ++ synSum rep t ++ "}"
synopsis (ListR IntR) x = show x
synopsis (ListR Word64R) x = show x
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
synopsis CharR s = show s
synopsis (ValueR _) x = show x
synopsis (TxOutR _) x = show x
synopsis (UTxOR p) (UTxO mp) = "UTxO( " ++ synopsis (MapR TxInR (TxOutR p)) (Map.map (TxOutF p) mp) ++ " )"
synopsis (PParamsR _) (PParamsF p x) = show $ pcPParamsSynopsis p x
synopsis (PParamsUpdateR _) _ = "PParamsUpdate ..."
synopsis DeltaCoinR (DeltaCoin n) = show (hsep [ppString "▵₳", ppInteger n])
synopsis GenDelegPairR x = show (pcGenDelegPair x)
synopsis FutureGenDelegR x = show (pcFutureGenDeleg x)
synopsis (PPUPStateR _) _ = "PPUPStateR ..."
synopsis PtrR p = show p
synopsis IPoolStakeR p = show (pcIndividualPoolStake p)
synopsis SnapShotsR _ = "SnapShots ..."
synopsis UnitR () = "()"
synopsis (PairR a b) (x, y) = "(" ++ synopsis a x ++ ", " ++ synopsis b y ++ ")"
synopsis RewardR x = show (pcReward x)
synopsis (MaybeR _) Nothing = "Nothing"
synopsis (MaybeR x) (Just y) = "(Just " ++ synopsis x y ++ ")"
synopsis NewEpochStateR _ = "NewEpochStateR ..."
synopsis (ProtVerR _) (ProtVer x y) = "(" ++ show x ++ " " ++ show y ++ ")"
synopsis SlotNoR x = show x
synopsis SizeR x = show x
synopsis VCredR x = show x
synopsis VHashR x = show x
synopsis CommColdHashR x = show x
synopsis CommHotHashR x = show x
synopsis VCredR x = show (credSummary x)
synopsis VHashR x = "(KeyHash 'Voting " ++ show (keyHashSummary x) ++ ")"
synopsis MultiAssetR (MultiAsset x) = "(MultiAsset num tokens = " ++ show (Map.size x) ++ ")"
synopsis PolicyIDR (PolicyID x) = show (pcScriptHash x)
synopsis (WitnessesFieldR p) x = show $ ppRecord' mempty $ unReflect pcWitnessesField p x
synopsis AssetNameR (AssetName x) = take 10 (show x)
synopsis DCertR x = show (pcDCert x)
synopsis RewardAcntR x = show (pcRewardAcnt x)
synopsis ValidityIntervalR x = show (ppValidityInterval x)
synopsis KeyPairR _ = "(KeyPairR ...)"
synopsis (GenR x) _ = "(Gen " ++ show x ++ " ...)"
synopsis (ScriptR _) x = show x -- The Show instance uses pcScript
synopsis ScriptHashR x = show (pcScriptHash x)
synopsis NetworkR x = show x
synopsis RdmrPtrR x = show (ppRdmrPtr x)
synopsis (DataR p) x = case p of
  Shelley _ -> show (pcData x)
  Allegra _ -> show (pcData x)
  Mary _ -> show (pcData x)
  Alonzo _ -> show (pcData x)
  Babbage _ -> show (pcData x)
  Conway _ -> show (pcData x)
synopsis ExUnitsR x = show (pcExUnits x)
synopsis TagR x = show x
synopsis DataHashR x = show (pcDataHash x)
synopsis AddrR x = show (pcAddr x)
synopsis PCredR c = show (credSummary c)


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
    accum :: Proof era -> Coin -> TxOutF era -> Coin
    accum (Shelley _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
    accum (Allegra _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
    accum (Mary _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
    accum (Alonzo _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
    accum (Babbage _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
    accum (Conway _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
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
  shape CharR = Nullary 16
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
  shape (PairR a b) = Nary 38 [shape a, shape b]
  shape VCredR = Nullary 39
  shape VHashR = Nullary 40
  shape CommColdHashR = Nullary 41
  shape CommHotHashR = Nullary 42
  shape MultiAssetR = Nullary 43
  shape PolicyIDR = Nullary 44
  shape (WitnessesFieldR p) = Nary 45 [shape p]
  shape AssetNameR = Nullary 46
  shape DCertR = Nullary 47
  shape RewardAcntR = Nullary 48
  shape ValidityIntervalR = Nullary 49
  shape MultiAssetR = Nullary 50
  shape PolicyIDR = Nullary 51
  shape (WitnessesFieldR p) = Nary 52 [shape p]
  shape AssetNameR = Nullary 53
  shape DCertR = Nullary 54
  shape RewardAcntR = Nullary 55
  shape ValidityIntervalR = Nullary 56
  shape KeyPairR = Nullary 57
  shape (GenR x) = Nary 58 [shape x]
  shape (ScriptR p) = Nary 59 [shape p]
  shape ScriptHashR = Nullary 60
  shape NetworkR = Nullary 61
  shape RdmrPtrR = Nullary 62
  shape (DataR p) = Nary 63 [shape p]
  shape ExUnitsR = Nullary 64
  shape TagR = Nullary 65
  shape DataHashR = Nullary 67
  shape AddrR = Nullary 68
  shape PCredR = Nullary 69

compareRep :: forall era t s. Rep era t -> Rep era s -> Ordering
compareRep x y = cmpIndex @(Rep era) x y

-- ================================================

genSizedRep ::
  forall era t.
  (Era era) =>
  Int ->
  Rep era t ->
  Gen t
genSizedRep _ CoinR =
  frequency [(1, pure (Coin 0)), (5, do Positive n <- arbitrary; pure (Coin n))] -- We never store (Coin 0) so we don't generate it
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
genSizedRep n EpochR = pure $ EpochNo $ fromIntegral n
genSizedRep _ RationalR = arbitrary
genSizedRep _ Word64R = choose (0, 1000)
genSizedRep _ IntR = arbitrary
genSizedRep _ NaturalR = arbitrary
genSizedRep _ FloatR = arbitrary
genSizedRep _ TxInR = arbitrary
genSizedRep _ CharR = arbitrary
genSizedRep _ (ValueR p) = genValue p
genSizedRep _ (TxOutR p) = genTxOut p
genSizedRep _n (UTxOR p) = genUTxO p
genSizedRep _ (PParamsR p) = genPParams p
genSizedRep _ (PParamsUpdateR p) = genPParamsUpdate p
genSizedRep _ DeltaCoinR = DeltaCoin <$> choose (-1000, 1000)
genSizedRep _ GenDelegPairR = arbitrary
genSizedRep _ FutureGenDelegR = arbitrary
genSizedRep _ r@(PPUPStateR _) = genpup r
genSizedRep _ PtrR = arbitrary
genSizedRep _ IPoolStakeR = arbitrary
genSizedRep _ SnapShotsR = arbitrary
genSizedRep _ UnitR = arbitrary
genSizedRep n (PairR a b) = (,) <$> genSizedRep n a <*> genSizedRep n b
genSizedRep _ RewardR = arbitrary
genSizedRep n (MaybeR x) = frequency [(1, pure Nothing), (5, Just <$> genSizedRep n x)]
genSizedRep _ NewEpochStateR = undefined
genSizedRep _ (ProtVerR proof) = genProtVer proof
genSizedRep _ SlotNoR = arbitrary
genSizedRep _ SizeR = do lo <- choose (1, 6); hi <- choose (6, 10); pure (SzRng lo hi)
genSizedRep _ VCredR = arbitrary
genSizedRep _ VHashR = arbitrary
genSizedRep _ CommColdHashR = arbitrary
genSizedRep _ CommHotHashR = arbitrary
genSizedRep _ MultiAssetR = arbitrary
genSizedRep _ PolicyIDR = arbitrary
genSizedRep _ (WitnessesFieldR _) = pure $ AddrWits Set.empty
genSizedRep _ AssetNameR = arbitrary
genSizedRep _ RewardAcntR = arbitrary
genSizedRep _ DCertR = arbitrary
genSizedRep _ ValidityIntervalR = arbitrary
genSizedRep _ KeyPairR = arbitrary
genSizedRep n (GenR x) = pure (genSizedRep n x)
genSizedRep _ (ScriptR p) = genScriptF p
genSizedRep _ ScriptHashR = arbitrary
genSizedRep _ NetworkR = arbitrary
genSizedRep n RdmrPtrR = RdmrPtr <$> arbitrary <*> choose (0, fromIntegral n)
genSizedRep _ (DataR _) = arbitrary
genSizedRep _ ExUnitsR = arbitrary
genSizedRep _ TagR = arbitrary
genSizedRep _ DataHashR = arbitrary
genSizedRep _ AddrR = arbitrary
genSizedRep _ PCredR = arbitrary

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
genpup (PPUPStateR (Conway _)) = arbitrary -- FIXME when Conway is fully defined.

-- ===========================
-- QuickCheck shrinking

-- Not all types in the universe have Arbitrary instances and thus don't shrink (the `[]` cases).
-- TODO: add instances for these types.
shrinkRep :: Era era => Rep era t -> t -> [t]
shrinkRep CoinR t = shrink t
shrinkRep (_ :-> _) _ = []
shrinkRep (MapR a b) t = shrinkMapBy Map.fromList Map.toList (shrinkRep $ ListR (PairR a b)) t
shrinkRep (SetR a) t = shrinkMapBy Set.fromList Set.toList (shrinkRep $ ListR a) t
shrinkRep (ListR a) t = shrinkList (shrinkRep a) t
shrinkRep CredR t = shrink t
shrinkRep PoolHashR t = shrink t
shrinkRep WitHashR t = shrink t
shrinkRep GenHashR t = shrink t
shrinkRep GenDelegHashR t = shrink t
shrinkRep PoolParamsR t = shrink t
shrinkRep EpochR t = shrink t
shrinkRep RationalR t = shrink t
shrinkRep Word64R t = shrink t
shrinkRep IntR t = shrink t
shrinkRep NaturalR t = shrink t
shrinkRep FloatR t = shrink t
shrinkRep TxInR t = shrink t
shrinkRep (ValueR _) _ = []
shrinkRep (TxOutR _) _ = []
shrinkRep (UTxOR _) _ = []
shrinkRep (PParamsR _) _ = []
shrinkRep (PParamsUpdateR _) _ = []
shrinkRep CharR t = shrink t
shrinkRep DeltaCoinR t = shrink t
shrinkRep GenDelegPairR t = shrink t
shrinkRep FutureGenDelegR t = shrink t
shrinkRep (PPUPStateR _) _ = []
shrinkRep PtrR t = shrink t
shrinkRep IPoolStakeR t = shrink t
shrinkRep SnapShotsR t = shrink t
shrinkRep UnitR t = shrink t
shrinkRep (PairR a b) (x, y) = [(x', y) | x' <- shrinkRep a x] ++ [(x, y') | y' <- shrinkRep b y]
shrinkRep RewardR t = shrink t
shrinkRep (MaybeR a) t = shrinkMapBy listToMaybe maybeToList (shrinkRep $ ListR a) t
shrinkRep NewEpochStateR _ = []
shrinkRep (ProtVerR _) t = shrink t
shrinkRep SlotNoR t = shrink t
shrinkRep SizeR _ = []
shrinkRep MultiAssetR t = shrink t
shrinkRep PolicyIDR t = shrink t
shrinkRep (WitnessesFieldR _) _ = []
shrinkRep AssetNameR t = shrink t
shrinkRep DCertR t = shrink t
shrinkRep RewardAcntR t = shrink t
shrinkRep ValidityIntervalR _ = []
shrinkRep KeyPairR t = shrink t
shrinkRep (GenR _) _ = []
shrinkRep (ScriptR _) _ = []
shrinkRep ScriptHashR t = shrink t
shrinkRep VCredR t = shrink t
shrinkRep VHashR t = shrink t
shrinkRep NetworkR t = shrink t
shrinkRep RdmrPtrR t = shrink t
shrinkRep (DataR _) _ = []
shrinkRep ExUnitsR t = shrink t
shrinkRep TagR t = shrink t
shrinkRep DataHashR t = shrink t
shrinkRep AddrR t = shrink t
shrinkRep PCredR t = shrink t

-- ===========================

{-
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
-}
