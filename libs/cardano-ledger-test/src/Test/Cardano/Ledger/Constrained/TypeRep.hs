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
  hasOrd,
  hasEq,
  format,
  genSigningKey,
)
where

import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Language (Language (..))

-- import Cardano.Ledger.Alonzo.Core (ScriptIntegrityHash)

import Cardano.Crypto.Signing (SigningKey (..), shortVerificationKeyHexF, toVerification)
import qualified Cardano.Crypto.Wallet as Byron
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag)
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..), Datum (..), dataToBinaryData)
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptPurpose (..))
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), Network (..), ProtVer (..), SlotNo (..), mkTxIxPartial)
import Cardano.Ledger.Binary.Version (Version)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Governance (GovernanceAction (..))
import Cardano.Ledger.Conway.TxCert (ConwayTxCert (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.EpochBoundary (SnapShots (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Hashes (DataHash, EraIndependentScriptIntegrity, ScriptHash (..))
import Cardano.Ledger.Keys (GenDelegPair (..), KeyHash, KeyRole (..))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.PoolParams (PoolParams (ppId))
import Cardano.Ledger.Pretty (
  PDoc,
  ppHash,
  ppInteger,
  ppList,
  ppMap,
  ppMaybe,
  ppRecord',
  ppSet,
  ppString,
  ppVKey,
 )
import Cardano.Ledger.Pretty.Alonzo (ppRdmrPtr)
import Cardano.Ledger.Pretty.Mary (ppValidityInterval)
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Cardano.Ledger.Shelley.TxBody (WitVKey (..))
import Cardano.Ledger.Shelley.TxCert (MIRPot (..), ShelleyTxCert (..))
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val ((<+>)))
import Data.ByteString (ByteString)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe (Eql, Shape (..), Shaped (..), Singleton (..), cmpIndex, (:~:) (Refl))
import Data.Word (Word16, Word64)
import Formatting (formatToString)
import Lens.Micro
import Numeric.Natural (Natural)
import Prettyprinter (hsep)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Constrained.Classes (
  PParamsF (..),
  PParamsUpdateF (..),
  ScriptF (..),
  ScriptsNeededF (..),
  TxAuxDataF (..),
  TxBodyF (..),
  TxCertF (..),
  TxF (..),
  TxOutF (..),
  TxWitsF (..),
  ValueF (..),
  genPParams,
  genPParamsUpdate,
  genScriptF,
  genTxAuxDataF,
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
import Test.Cardano.Ledger.Constrained.Monad (HasConstraint (With), Typed, explain, failT)
import Test.Cardano.Ledger.Constrained.Size (Size (..))
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields (WitnessesField (..))
import Test.Cardano.Ledger.Generic.PrettyCore (
  credSummary,
  keyHashSummary,
  pcAddr,
  pcCoin,
  pcConwayTxCert,
  pcDRep,
  pcData,
  pcDataHash,
  pcDatum,
  pcFutureGenDeleg,
  pcGenDelegPair,
  pcIndividualPoolStake,
  pcLedgerState,
  pcMultiAsset,
  pcPParamsSynopsis,
  pcReward,
  pcRewardAcnt,
  pcScriptHash,
  pcScriptPurpose,
  pcShelleyTxCert,
  pcTxCert,
  pcTxIn,
  pcTxOut,
  pcVal,
  pcWitVKey,
  pcWitnesses,
  pcWitnessesField,
  trim,
 )
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters (newTxBody)
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
  VCredR :: Rep era (Credential 'Voting (EraCrypto era))
  PoolHashR :: Rep era (KeyHash 'StakePool (EraCrypto era))
  WitHashR :: Rep era (KeyHash 'Witness (EraCrypto era))
  GenHashR :: Rep era (KeyHash 'Genesis (EraCrypto era))
  GenDelegHashR :: Rep era (KeyHash 'GenesisDelegate (EraCrypto era))
  VHashR :: Rep era (KeyHash 'Voting (EraCrypto era))
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
  MultiAssetR :: Crypto (EraCrypto era) => Rep era (MultiAsset (EraCrypto era))
  PolicyIDR :: Rep era (PolicyID (EraCrypto era))
  WitnessesFieldR :: Proof era -> Rep era (WitnessesField era)
  AssetNameR :: Rep era AssetName
  TxCertR :: Proof era -> Rep era (TxCertF era)
  RewardAcntR :: Rep era (RewardAcnt (EraCrypto era))
  ValidityIntervalR :: Rep era ValidityInterval
  KeyPairR :: Rep era (KeyPair 'Witness (EraCrypto era))
  GenR :: Rep era x -> Rep era (Gen x)
  ScriptR :: Proof era -> Rep era (ScriptF era)
  ScriptHashR :: Rep era (ScriptHash (EraCrypto era))
  NetworkR :: Rep era Network
  RdmrPtrR :: Rep era RdmrPtr
  DataR :: Era era => Rep era (Data era)
  DatumR :: Era era => Rep era (Datum era)
  ExUnitsR :: Rep era ExUnits
  TagR :: Rep era Tag
  DataHashR :: Rep era (DataHash (EraCrypto era))
  AddrR :: Rep era (Addr (EraCrypto era))
  PCredR :: Rep era (Credential 'Payment (EraCrypto era))
  ShelleyTxCertR :: Rep era (ShelleyTxCert era)
  ConwayTxCertR :: Rep era (ConwayTxCert era)
  MIRPotR :: Rep era MIRPot
  IsValidR :: Rep era IsValid
  IntegerR :: Rep era Integer
  ScriptsNeededR :: Proof era -> Rep era (ScriptsNeededF era)
  ScriptPurposeR :: Proof era -> Rep era (ScriptPurpose era)
  TxBodyR :: Proof era -> Rep era (TxBodyF era)
  BootstrapWitnessR :: Crypto (EraCrypto era) => Rep era (BootstrapWitness (EraCrypto era))
  SigningKeyR :: Rep era SigningKey
  TxWitsR :: Proof era -> Rep era (TxWitsF era)
  PayHashR :: Rep era (KeyHash 'Payment (EraCrypto era))
  TxR :: Proof era -> Rep era (TxF era)
  ScriptIntegrityHashR :: Rep era (SafeHash (EraCrypto era) EraIndependentScriptIntegrity)
  AuxiliaryDataHashR :: Rep era (AuxiliaryDataHash (EraCrypto era))
  GovernanceActionR :: Rep era (GovernanceAction era)
  WitVKeyR :: Proof era -> Rep era (WitVKey 'Witness (EraCrypto era))
  TxAuxDataR :: Proof era -> Rep era (TxAuxDataF era)
  CommColdHashR :: Rep era (KeyHash 'CommitteeColdKey (EraCrypto era))
  CommHotHashR :: Rep era (KeyHash 'CommitteeHotKey (EraCrypto era))
  LanguageR :: Rep era Language
  LedgerStateR :: Proof era -> Rep era (LedgerState era)
  StakeHashR :: Rep era (KeyHash 'Staking (EraCrypto era))
  BoolR :: Rep era Bool
  DRepR :: Rep era (Core.DRep (EraCrypto era))

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
  testEql VHashR VHashR = Just Refl
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
  testEql (TxCertR c) (TxCertR d) = do Refl <- testEql c d; pure Refl
  testEql ValidityIntervalR ValidityIntervalR = Just Refl
  testEql RewardAcntR RewardAcntR = Just Refl
  testEql KeyPairR KeyPairR = Just Refl
  testEql (GenR x) (GenR y) = do Refl <- testEql x y; pure Refl
  testEql (ScriptR c) (ScriptR d) =
    do Refl <- testEql c d; pure Refl
  testEql ScriptHashR ScriptHashR = Just Refl
  testEql NetworkR NetworkR = Just Refl
  testEql RdmrPtrR RdmrPtrR = Just Refl
  testEql DataR DataR = pure Refl
  testEql DatumR DatumR = pure Refl
  testEql ExUnitsR ExUnitsR = Just Refl
  testEql TagR TagR = Just Refl
  testEql DataHashR DataHashR = Just Refl
  testEql AddrR AddrR = Just Refl
  testEql PCredR PCredR = Just Refl
  testEql ConwayTxCertR ConwayTxCertR = Just Refl
  testEql ShelleyTxCertR ShelleyTxCertR = Just Refl
  testEql MIRPotR MIRPotR = Just Refl
  testEql IsValidR IsValidR = Just Refl
  testEql IntegerR IntegerR = Just Refl
  testEql (ScriptsNeededR c) (ScriptsNeededR d) =
    do Refl <- testEql c d; pure Refl
  testEql (ScriptPurposeR c) (ScriptPurposeR d) =
    do Refl <- testEql c d; pure Refl
  testEql (TxBodyR c) (TxBodyR d) =
    do Refl <- testEql c d; pure Refl
  testEql BootstrapWitnessR BootstrapWitnessR = Just Refl
  testEql SigningKeyR SigningKeyR = Just Refl
  testEql (TxWitsR c) (TxWitsR d) =
    do Refl <- testEql c d; pure Refl
  testEql PayHashR PayHashR = Just Refl
  testEql (TxR c) (TxR d) =
    do Refl <- testEql c d; pure Refl
  testEql ScriptIntegrityHashR ScriptIntegrityHashR = Just Refl
  testEql AuxiliaryDataHashR AuxiliaryDataHashR = Just Refl
  testEql GovernanceActionR GovernanceActionR = Just Refl
  testEql (WitVKeyR c) (WitVKeyR d) =
    do Refl <- testEql c d; pure Refl
  testEql (TxAuxDataR c) (TxAuxDataR d) =
    do Refl <- testEql c d; pure Refl
  testEql CommColdHashR CommColdHashR = Just Refl
  testEql CommHotHashR CommHotHashR = Just Refl
  testEql LanguageR LanguageR = Just Refl
  testEql (LedgerStateR c) (LedgerStateR d) =
    do Refl <- testEql c d; pure Refl
  testEql StakeHashR StakeHashR = Just Refl
  testEql BoolR BoolR = Just Refl
  testEql DRepR DRepR = Just Refl
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
  show (ValueR x) = "(Value " ++ short x ++ ")"
  show (TxOutR x) = "(TxOut " ++ short x ++ ")"
  show (UTxOR x) = "(UTxO " ++ short x ++ ")"
  show (PParamsR x) = "(PParams " ++ short x ++ ")"
  show (PParamsUpdateR x) = "(PParamsUpdate " ++ short x ++ ")"
  show CharR = "Char"
  show DeltaCoinR = "DeltaCoin"
  show GenDelegPairR = "(GenDelegPair c)"
  show FutureGenDelegR = "(FutureGenDeleg c)"
  show (PPUPStateR p) = "(ShelleyPPUPState " ++ short p ++ ")"
  show PtrR = "Ptr"
  show IPoolStakeR = "(IndividualPoolStake c)"
  show SnapShotsR = "(SnapShots c)"
  show UnitR = "()"
  show (PairR a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show RewardR = "(Reward c)"
  show (MaybeR x) = "(Maybe " ++ show x ++ ")"
  show NewEpochStateR = "NewEpochState"
  show (ProtVerR x) = "(ProtVer " ++ short x ++ ")"
  show SlotNoR = "(SlotNo c)"
  show SizeR = "Size"
  show VCredR = "(Credential 'Voting c)"
  show VHashR = "(KeyHash 'Voting c)"
  show MultiAssetR = "(MultiAsset c)"
  show PolicyIDR = "(PolicyID c)"
  show (WitnessesFieldR p) = "(WitnessesField " ++ short p ++ ")"
  show AssetNameR = "AssetName"
  show (TxCertR p) = "(TxCert " ++ short p ++ ")"
  show RewardAcntR = "(RewardAcnt c)"
  show ValidityIntervalR = "ValidityInterval"
  show KeyPairR = "(KeyPair 'Witness era)"
  show (GenR x) = "(Gen " ++ show x ++ ")"
  show (ScriptR x) = "(Script " ++ short x ++ ")"
  show ScriptHashR = "(ScriptHash c)"
  show NetworkR = "Network"
  show RdmrPtrR = "RdmrPtr"
  show DataR = "(Data era)"
  show DatumR = "(Datum era)"
  show ExUnitsR = "ExUnits"
  show TagR = "Tag"
  show DataHashR = "(DataHash c)"
  show AddrR = "(Addr c)"
  show PCredR = "(Credential 'Payment c)"
  show ConwayTxCertR = "(ConwayTxCert era)"
  show ShelleyTxCertR = "(ShelleyTxCert era)"
  show MIRPotR = "MIRPot"
  show IsValidR = "IsValid"
  show IntegerR = "Integer"
  show (ScriptsNeededR p) = "(ScriptsNeeded " ++ short p ++ ")"
  show (ScriptPurposeR p) = "(ScriptPurpose " ++ short p ++ ")"
  show (TxBodyR p) = "(TxBody " ++ short p ++ ")"
  show BootstrapWitnessR = "(BootstrapWitness c)"
  show SigningKeyR = "Byron.SigningKey"
  show (TxWitsR p) = "(TxWits " ++ short p ++ ")"
  show PayHashR = "(KeyHash 'Payment c)"
  show (TxR p) = "(Tx " ++ short p ++ ")"
  show ScriptIntegrityHashR = "ScriptIntegrityHash"
  show AuxiliaryDataHashR = "AuxiliaryDataHash"
  show GovernanceActionR = "GovernanceAction"
  show (WitVKeyR _) = "(WitVKey 'Witness c)"
  show (TxAuxDataR p) = "(TxAuxData " ++ short p ++ ")"
  show CommColdHashR = "CommColdHash"
  show CommHotHashR = "CommHotHash"
  show LanguageR = "Language"
  show (LedgerStateR p) = "(LedgerState " ++ short p ++ ")"
  show StakeHashR = "(KeyHash 'Staking c)"
  show BoolR = "Bool"
  show DRepR = "(DRep c)"

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
synopsis (ValueR p) (ValueF _ x) = show (pcVal p x)
synopsis (TxOutR p) (TxOutF _ x) = show ((unReflect pcTxOut p x) :: PDoc)
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
synopsis VCredR x = show (credSummary x)
synopsis VHashR x = "(KeyHash 'Voting " ++ show (keyHashSummary x) ++ ")"
synopsis MultiAssetR x = "(MultiAsset " ++ show (pcMultiAsset x) ++ ")"
synopsis PolicyIDR (PolicyID x) = show (pcScriptHash x)
synopsis (WitnessesFieldR p) x = show $ ppRecord' mempty $ unReflect pcWitnessesField p x
synopsis AssetNameR (AssetName x) = take 10 (show x)
synopsis (TxCertR p) (TxCertF _ x) = show (pcTxCert p x)
synopsis RewardAcntR x = show (pcRewardAcnt x)
synopsis ValidityIntervalR x = show (ppValidityInterval x)
synopsis KeyPairR _ = "(KeyPairR ...)"
synopsis (GenR x) _ = "(Gen " ++ show x ++ " ...)"
synopsis (ScriptR _) x = show x -- The Show instance uses pcScript
synopsis ScriptHashR x = show (pcScriptHash x)
synopsis NetworkR x = show x
synopsis RdmrPtrR x = show (ppRdmrPtr x)
synopsis DataR x = show (pcData x)
synopsis DatumR x = show (pcDatum x)
synopsis ExUnitsR (ExUnits m d) = "(ExUnits mem=" ++ show m ++ " data=" ++ show d ++ ")"
synopsis TagR x = show x
synopsis DataHashR x = show (pcDataHash x)
synopsis AddrR x = show (pcAddr x)
synopsis PCredR c = show (credSummary c)
synopsis ConwayTxCertR x = show (pcConwayTxCert x)
synopsis ShelleyTxCertR x = show (pcShelleyTxCert x)
synopsis MIRPotR x = show x
synopsis IsValidR x = show x
synopsis IntegerR x = show x
synopsis (ScriptsNeededR _) x = show x
synopsis (ScriptPurposeR p) x = show (pcScriptPurpose p x)
synopsis (TxBodyR _) x = show x
synopsis BootstrapWitnessR x = "(BootstrapWitness " ++ show (ppVKey (bwKey x)) ++ ")"
synopsis SigningKeyR key = "(publicKeyOfSecretKey " ++ formatToString shortVerificationKeyHexF (toVerification key) ++ ")"
synopsis (TxWitsR p) (TxWitsF _ x) = show ((unReflect pcWitnesses p x) :: PDoc)
synopsis PayHashR k = "(KeyHash 'Payment " ++ show (keyHashSummary k) ++ ")"
synopsis (TxR _) x = show x
synopsis ScriptIntegrityHashR x = show (trim (ppHash (extractHash x)))
synopsis AuxiliaryDataHashR (AuxiliaryDataHash x) = show (trim (ppHash (extractHash x)))
synopsis GovernanceActionR _x = "GovernanceAction ..." -- show(prettyA x)
synopsis (WitVKeyR p) x = show ((unReflect pcWitVKey p x) :: PDoc)
synopsis (TxAuxDataR _) x = show x
synopsis CommColdHashR x = show x
synopsis CommHotHashR x = show x
synopsis LanguageR x = show x
synopsis (LedgerStateR p) x = show ((unReflect pcLedgerState p x) :: PDoc)
synopsis StakeHashR k = "(KeyHash 'Staking " ++ show (keyHashSummary k) ++ ")"
synopsis BoolR x = show x
synopsis DRepR x = show (pcDRep x)

synSum :: Rep era a -> a -> String
synSum (MapR _ CoinR) m = ", sum = " ++ show (pcCoin (Map.foldl' (<>) mempty m))
synSum (MapR _ RationalR) m = ", sum = " ++ show (Map.foldl' (+) 0 m)
synSum (MapR _ IntR) m = ", sum = " ++ show (Map.foldl' (+) 0 m)
synSum (MapR _ Word64R) m = ", sum = " ++ show (Map.foldl' (+) 0 m)
synSum (MapR _ IPoolStakeR) m = ", sum = " ++ show (Map.foldl' accum 0 m)
  where
    accum z (IndividualPoolStake rat _) = z + rat
synSum (MapR _ (TxOutR proof)) m = ", sum = " ++ show (Map.foldl' (accumTxOut proof) (Coin 0) m)
synSum (SetR CoinR) m = ", sum = " ++ show (pcCoin (Set.foldl' (<>) mempty m))
synSum (SetR RationalR) m = ", sum = " ++ show (Set.foldl' (+) 0 m)
synSum (ListR CoinR) m = ", sum = " ++ show (List.foldl' (<>) mempty m)
synSum (ListR RationalR) m = ", sum = " ++ show (List.foldl' (+) 0 m)
synSum (ListR IntR) m = ", sum = " ++ show (List.foldl' (+) 0 m)
synSum (ListR Word64R) m = ", sum = " ++ show (List.foldl' (+) 0 m)
synSum (ListR (TxOutR proof)) m = ", sum = " ++ show (List.foldl' (accumTxOut proof) (Coin 0) m)
synSum _ _ = ""

accumTxOut :: Proof era -> Coin -> TxOutF era -> Coin
accumTxOut (Shelley _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
accumTxOut (Allegra _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
accumTxOut (Mary _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
accumTxOut (Alonzo _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
accumTxOut (Babbage _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)
accumTxOut (Conway _) z (TxOutF _ out) = z <+> (out ^. Core.coinTxOutL)

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
  shape MultiAssetR = Nullary 41
  shape PolicyIDR = Nullary 42
  shape (WitnessesFieldR p) = Nary 43 [shape p]
  shape AssetNameR = Nullary 44
  shape (TxCertR p) = Nary 45 [shape p]
  shape RewardAcntR = Nullary 46
  shape ValidityIntervalR = Nullary 47
  shape KeyPairR = Nullary 48
  shape (GenR x) = Nary 49 [shape x]
  shape (ScriptR p) = Nary 50 [shape p]
  shape ScriptHashR = Nullary 51
  shape NetworkR = Nullary 52
  shape RdmrPtrR = Nullary 53
  shape DataR = Nullary 54
  shape DatumR = Nullary 55
  shape ExUnitsR = Nullary 56
  shape TagR = Nullary 57
  shape DataHashR = Nullary 58
  shape AddrR = Nullary 59
  shape PCredR = Nullary 60
  shape ConwayTxCertR = Nullary 61
  shape ShelleyTxCertR = Nullary 62
  shape MIRPotR = Nullary 63
  shape IsValidR = Nullary 64
  shape IntegerR = Nullary 65
  shape (ScriptsNeededR p) = Nary 66 [shape p]
  shape (ScriptPurposeR p) = Nary 67 [shape p]
  shape (TxBodyR p) = Nary 68 [shape p]
  shape BootstrapWitnessR = Nullary 69
  shape SigningKeyR = Nullary 70
  shape (TxWitsR p) = Nary 71 [shape p]
  shape PayHashR = Nullary 72
  shape (TxR p) = Nary 73 [shape p]
  shape ScriptIntegrityHashR = Nullary 74
  shape AuxiliaryDataHashR = Nullary 75
  shape GovernanceActionR = Nullary 76
  shape (WitVKeyR p) = Nary 77 [shape p]
  shape (TxAuxDataR p) = Nary 78 [shape p]
  shape CommColdHashR = Nullary 79
  shape CommHotHashR = Nullary 80
  shape LanguageR = Nullary 81
  shape (LedgerStateR p) = Nary 82 [shape p]
  shape StakeHashR = Nullary 83
  shape BoolR = Nullary 84
  shape DRepR = Nullary 85

compareRep :: forall era t s. Rep era t -> Rep era s -> Ordering
compareRep x y = cmpIndex @(Rep era) x y

-- ================================================

genSizedRep ::
  forall era t.
  (Era era) =>
  Int ->
  Rep era t ->
  Gen t
genSizedRep n CoinR =
  if n == 0
    then do Positive m <- arbitrary; pure (Coin m)
    else pure (Coin (fromIntegral n))
genSizedRep n (_a :-> b) = const <$> genSizedRep n b
genSizedRep n r@(MapR a b) = do
  mapSized ["From genSizedRep " ++ show r] n (genRep a) (genRep b)
genSizedRep n r@(SetR a) = do
  setSized ["From genSizedRep " ++ show r] n (genRep a)
genSizedRep n (ListR a) = vectorOf n (genRep a)
genSizedRep _ CredR = arbitrary
genSizedRep _ PoolHashR = arbitrary
genSizedRep _ WitHashR = arbitrary
genSizedRep _ GenHashR = arbitrary
genSizedRep _ GenDelegHashR = arbitrary
genSizedRep _ PoolParamsR = arbitrary
genSizedRep n EpochR = pure $ EpochNo $ fromIntegral n
genSizedRep _ RationalR = arbitrary
genSizedRep _ Word64R = choose (0, 1000)
genSizedRep n IntR = pure n
genSizedRep n NaturalR = pure $ fromIntegral n
genSizedRep _ FloatR = arbitrary
genSizedRep n TxInR = TxIn <$> arbitrary <*> (mkTxIxPartial . fromIntegral <$> choose (2, min n (fromIntegral (maxBound :: Word16))))
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
genSizedRep n SlotNoR = pure $ SlotNo (fromIntegral n)
genSizedRep _ SizeR = do lo <- choose (1, 6); hi <- choose (6, 10); pure (SzRng lo hi)
genSizedRep _ VCredR = arbitrary
genSizedRep _ VHashR = arbitrary
genSizedRep n MultiAssetR = MultiAsset <$> genSizedRep n (MapR (PolicyIDR @era) (MapR AssetNameR IntegerR))
genSizedRep _ PolicyIDR = arbitrary
genSizedRep _ (WitnessesFieldR _) = pure $ AddrWits Set.empty
genSizedRep _ AssetNameR = arbitrary
genSizedRep _ RewardAcntR = RewardAcnt <$> pure Testnet <*> arbitrary
genSizedRep _ (TxCertR (Shelley c)) = TxCertF (Shelley c) <$> arbitrary
genSizedRep _ (TxCertR (Allegra c)) = TxCertF (Allegra c) <$> arbitrary
genSizedRep _ (TxCertR (Mary c)) = TxCertF (Mary c) <$> arbitrary
genSizedRep _ (TxCertR (Alonzo c)) = TxCertF (Alonzo c) <$> arbitrary
genSizedRep _ (TxCertR (Babbage c)) = TxCertF (Babbage c) <$> arbitrary
genSizedRep _ (TxCertR (Conway c)) = TxCertF (Conway c) <$> arbitrary
genSizedRep _ ValidityIntervalR = arbitrary
genSizedRep _ KeyPairR = arbitrary
genSizedRep n (GenR x) = pure (genSizedRep n x)
genSizedRep _ (ScriptR p) = genScriptF p
genSizedRep _ ScriptHashR = arbitrary
genSizedRep _ NetworkR = arbitrary
genSizedRep n RdmrPtrR = RdmrPtr <$> arbitrary <*> choose (0, fromIntegral n)
genSizedRep _ DataR = arbitrary
genSizedRep n DatumR =
  oneof
    [ pure NoDatum
    , DatumHash <$> genSizedRep @era n DataHashR
    , Datum . dataToBinaryData <$> genSizedRep @era n DataR
    ]
genSizedRep _ ExUnitsR = arbitrary
genSizedRep _ TagR = arbitrary
genSizedRep _ DataHashR = arbitrary
genSizedRep _ AddrR = arbitrary
genSizedRep _ PCredR = arbitrary
genSizedRep _ ShelleyTxCertR = arbitrary
genSizedRep _ ConwayTxCertR = arbitrary
genSizedRep _ MIRPotR = arbitrary
genSizedRep _ IsValidR = frequency [(1, pure (IsValid False)), (9, pure (IsValid True))]
genSizedRep _ IntegerR = arbitrary
genSizedRep _ (ScriptsNeededR p) = case whichUTxO p of
  UTxOShelleyToMary -> pure $ ScriptsNeededF p (ShelleyScriptsNeeded Set.empty)
  UTxOAlonzoToConway -> pure $ ScriptsNeededF p (AlonzoScriptsNeeded [])
genSizedRep _ (ScriptPurposeR p) = case whichTxCert p of
  TxCertShelleyToBabbage -> arbitrary
  TxCertConwayToConway -> arbitrary
genSizedRep _ (TxBodyR p) =
  case p of
    Shelley _ -> pure (TxBodyF p (newTxBody p []))
    Allegra _ -> pure (TxBodyF p (newTxBody p []))
    Mary _ -> pure (TxBodyF p (newTxBody p []))
    Alonzo _ -> pure (TxBodyF p (newTxBody p []))
    Babbage _ -> pure (TxBodyF p (newTxBody p []))
    Conway _ -> pure (TxBodyF p (newTxBody p []))
genSizedRep _ BootstrapWitnessR = arbitrary
genSizedRep _ SigningKeyR = genSigningKey
genSizedRep _ (TxWitsR p) =
  case p of
    Shelley _ -> TxWitsF p <$> arbitrary
    Allegra _ -> TxWitsF p <$> arbitrary
    Mary _ -> TxWitsF p <$> arbitrary
    Alonzo _ -> TxWitsF p <$> arbitrary
    Babbage _ -> TxWitsF p <$> arbitrary
    Conway _ -> TxWitsF p <$> arbitrary
genSizedRep _ PayHashR = arbitrary
genSizedRep _ (TxR p) =
  case p of
    Shelley _ -> TxF p <$> arbitrary
    Allegra _ -> TxF p <$> arbitrary
    Mary _ -> TxF p <$> arbitrary
    Alonzo _ -> TxF p <$> arbitrary
    Babbage _ -> TxF p <$> arbitrary
    Conway _ -> TxF p <$> arbitrary
genSizedRep _ ScriptIntegrityHashR = arbitrary
genSizedRep _ AuxiliaryDataHashR = arbitrary
genSizedRep _ GovernanceActionR = pure NoConfidence
genSizedRep _ (WitVKeyR _) = arbitrary
genSizedRep _ (TxAuxDataR p) = genTxAuxDataF p
genSizedRep _ CommColdHashR = arbitrary
genSizedRep _ CommHotHashR = arbitrary
genSizedRep _ LanguageR = arbitrary
genSizedRep _ (LedgerStateR p) = case p of
  Shelley _ -> arbitrary
  Allegra _ -> arbitrary
  Mary _ -> arbitrary
  Alonzo _ -> arbitrary
  Babbage _ -> arbitrary
  Conway _ -> arbitrary
genSizedRep _ StakeHashR = arbitrary
genSizedRep _ BoolR = arbitrary
genSizedRep _ DRepR = arbitrary

genRep ::
  Era era =>
  Rep era b ->
  Gen b
genRep x = do (NonNegative n) <- arbitrary; genSizedRep n x

-- | A Byron address is made of a 32 byte ByteString, so here we
--   generate some random ones with size 32 Bytes
gen32ByteString :: Gen ByteString
gen32ByteString = do
  list <- vectorOf 32 (elements ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"])
  pure (List.foldl' (<>) mempty list)

-- | Turn a random bytestring into a SigningKey
genSigningKey :: Gen SigningKey
genSigningKey = do
  seed <- gen32ByteString
  pure (SigningKey $ Byron.generate seed (mempty :: ByteString))

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
shrinkRep (TxCertR (Shelley _)) (TxCertF p x) = map (TxCertF p) (shrink x)
shrinkRep (TxCertR (Allegra _)) (TxCertF p x) = map (TxCertF p) (shrink x)
shrinkRep (TxCertR (Mary _)) (TxCertF p x) = map (TxCertF p) (shrink x)
shrinkRep (TxCertR (Alonzo _)) (TxCertF p x) = map (TxCertF p) (shrink x)
shrinkRep (TxCertR (Babbage _)) (TxCertF p x) = map (TxCertF p) (shrink x)
shrinkRep (TxCertR (Conway _)) (TxCertF p x) = map (TxCertF p) (shrink x)
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
shrinkRep DataR t = shrink t
shrinkRep DatumR _ = []
shrinkRep ExUnitsR t = shrink t
shrinkRep TagR t = shrink t
shrinkRep DataHashR t = shrink t
shrinkRep AddrR t = shrink t
shrinkRep PCredR t = shrink t
shrinkRep ShelleyTxCertR t = shrink t
shrinkRep ConwayTxCertR t = shrink t
shrinkRep MIRPotR t = shrink t
shrinkRep IsValidR _ = []
shrinkRep IntegerR t = shrink t
shrinkRep (ScriptsNeededR _) _ = []
shrinkRep (ScriptPurposeR _) _ = []
shrinkRep (TxBodyR _) _ = []
shrinkRep BootstrapWitnessR t = shrink t
shrinkRep SigningKeyR _ = []
shrinkRep (TxWitsR _p) _ = []
shrinkRep PayHashR t = shrink t
shrinkRep (TxR _) _ = []
shrinkRep ScriptIntegrityHashR x = shrink x
shrinkRep AuxiliaryDataHashR x = shrink x
shrinkRep GovernanceActionR _ = []
shrinkRep (WitVKeyR _) x = shrink x
shrinkRep (TxAuxDataR _) _ = []
shrinkRep CommColdHashR x = shrink x
shrinkRep CommHotHashR x = shrink x
shrinkRep LanguageR x = shrink x
shrinkRep (LedgerStateR _) _ = []
shrinkRep StakeHashR x = shrink x
shrinkRep BoolR x = shrink x
shrinkRep DRepR x = shrink x

-- ===========================

short :: Proof era -> String
short (Shelley _) = "Shelley"
short (Allegra _) = "Allegra"
short (Mary _) = "Mary"
short (Alonzo _) = "Alonzo"
short (Babbage _) = "Babbage"
short (Conway _) = "Conway"

hasOrd :: Rep era t -> s t -> Typed (HasConstraint Ord (s t))
hasOrd rep xx = explain ("'hasOrd " ++ show rep ++ "' fails") (help rep xx)
  where
    err t c = error ("hasOrd function 'help' evaluates its second arg at type " ++ show t ++ ", in " ++ c ++ " case.")
    help :: Rep era t -> s t -> Typed (HasConstraint Ord (s t))
    help CoinR t = pure $ With t
    help r@(_ :-> _) _ = failT [show r ++ " does not have an Ord instance."]
    help r@(MapR _ b) m = do
      With _ <- help b (err b (show r))
      pure (With m)
    help (SetR _) s = pure $ With s
    help r@(ListR a) l = do
      With _ <- help a (err a (show r))
      pure $ With l
    help CredR c = pure $ With c
    help PoolHashR p = pure $ With p
    help GenHashR p = pure $ With p
    help GenDelegHashR p = pure $ With p
    help WitHashR p = pure $ With p
    help PoolParamsR pp = pure $ With pp
    help EpochR e = pure $ With e
    help RationalR r = pure $ With r
    help Word64R w = pure $ With w
    help IntR i = pure $ With i
    help NaturalR i = pure $ With i
    help FloatR i = pure $ With i
    help TxInR t = pure $ With t
    help CharR s = pure $ With s
    help UnitR v = pure $ With v
    help (PairR a b) p = do
      With _ <- help a undefined
      With _ <- help b undefined
      pure $ With p
    help (ValueR _) v = pure $ With v
    help (TxOutR _) v = pure $ With v
    help (UTxOR _) _ = failT ["UTxO does not have Ord instance"]
    help DeltaCoinR v = pure $ With v
    help GenDelegPairR v = pure $ With v
    help FutureGenDelegR v = pure $ With v
    help (PPUPStateR _) _ = failT ["PPUPState does not have Ord instance"]
    help PtrR v = pure $ With v
    help SnapShotsR _ = failT ["SnapShot does not have Ord instance"]
    help IPoolStakeR _ = failT ["IndividualPoolStake does not have Ord instance"]
    help (PParamsR _) _ = failT ["PParams does not have Ord instance"]
    help (PParamsUpdateR _) _ = failT ["PParamsUpdate does not have Ord instance"]
    help RewardR v = pure $ With v
    help r@(MaybeR a) l = do
      With _ <- help a (err a (show r))
      pure $ With l
    help NewEpochStateR _ = failT ["NewEpochStateR does not have Ord instance"]
    help (ProtVerR _) v = pure $ With v
    help SlotNoR v = pure $ With v
    help SizeR v = pure $ With v
    help VCredR v = pure $ With v
    help VHashR v = pure $ With v
    help MultiAssetR v = pure $ With v
    help PolicyIDR v = pure $ With v
    help (WitnessesFieldR _) _ = failT ["WitnessesField does not have Ord instance"]
    help AssetNameR v = pure $ With v
    help (TxCertR _) _ = failT ["TxCert does not have Ord instance"]
    help RewardAcntR v = pure $ With v
    help ValidityIntervalR v = pure $ With v
    help KeyPairR _ = failT ["KeyPair does not have Ord instance"]
    help (GenR _) _ = failT ["Gen does not have Ord instance"]
    help (ScriptR _) _ = failT ["Script does not have Ord instance"]
    help ScriptHashR v = pure $ With v
    help NetworkR v = pure $ With v
    help TagR v = pure $ With v
    help ExUnitsR _ = failT ["ExUnits does not have Ord instance"]
    help RdmrPtrR v = pure $ With v
    help DataR _ = failT ["Data does not have Ord instance"]
    help DatumR v = pure $ With v
    help DataHashR v = pure $ With v
    help AddrR v = pure $ With v
    help PCredR v = pure $ With v
    help ConwayTxCertR _ = failT ["ConwayTxCert does not have Ord instance"]
    help ShelleyTxCertR _ = failT ["ShelleyTxCert does not have Ord instance"]
    help MIRPotR v = pure $ With v
    help IsValidR _ = failT ["IsValid does not have Ord instance"]
    help IntegerR i = pure $ With i
    help (ScriptsNeededR _) _ = failT ["IsValid does not have Ord instance"]
    help (ScriptPurposeR _) _ = failT ["ScriptPurpose does not have Ord instance"]
    help (TxBodyR _) _ = failT ["TxBody does not have Ord instance"]
    help BootstrapWitnessR t = pure $ With t
    help SigningKeyR _ = failT ["SigningKey does not have an Ord instance"]
    help (TxWitsR _) _ = failT ["TxWits does not have an Ord instance"]
    help PayHashR p = pure $ With p
    help (TxR _) _ = failT ["Tx does not have Ord instance"]
    help ScriptIntegrityHashR x = pure $ With x
    help AuxiliaryDataHashR x = pure $ With x
    help GovernanceActionR _ = failT ["GovernanceAction does not have Ord instance"]
    help (WitVKeyR p) x = case p of
      Shelley _ -> pure $ With x
      Allegra _ -> pure $ With x
      Mary _ -> pure $ With x
      Alonzo _ -> pure $ With x
      Babbage _ -> pure $ With x
      Conway _ -> pure $ With x
    help (TxAuxDataR _) _ = failT ["TxAuxData does not have Ord instance"]
    help CommColdHashR x = pure $ With x
    help CommHotHashR x = pure $ With x
    help LanguageR x = pure $ With x
    help (LedgerStateR _) _ = failT ["LedgerState does not have Ord instance"]
    help StakeHashR p = pure $ With p
    help BoolR v = pure $ With v
    help DRepR v = pure $ With v

hasEq :: Rep era t -> s t -> Typed (HasConstraint Eq (s t))
hasEq rep xx = explain ("'hasOrd " ++ show rep ++ "' fails") (help rep xx)
  where
    help :: Rep era t -> s t -> Typed (HasConstraint Eq (s t))
    help (TxOutR _) v = pure $ With v
    help (ScriptR _) v = pure $ With v
    help DataR v = pure $ With v
    help SigningKeyR v = pure $ With v
    help (TxWitsR _) v = pure $ With v
    help (TxR _) v = pure $ With v
    help (TxAuxDataR _) v = pure $ With v
    help IsValidR v = pure $ With v
    help GovernanceActionR _ = failT ["GovernanceAction does have an Eq instance, but it requires (Core.EraPParams era)"]
    help (ScriptPurposeR p) v = case whichTxCert p of
      TxCertShelleyToBabbage -> pure $ With v
      TxCertConwayToConway -> pure $ With v
    help (PairR a b) v = do
      With _ <- hasEq a undefined
      With _ <- hasEq b undefined
      pure (With v)
    help x v = do
      With y <- hasOrd x v
      pure (With y)

format :: Rep era t -> t -> String
format rep@(MapR d r) x = show (ppMap (syn d) (syn r) x) ++ synSum rep x
format rep@(ListR d) x = show (ppList (syn d) x) ++ synSum rep x
format rep@(SetR d) x = show (ppSet (syn d) x) ++ synSum rep x
format (MaybeR d) x = show (ppMaybe (syn d) x)
format r x = synopsis r x

syn :: Rep era t -> t -> PDoc
syn d x = ppString (format d x)
