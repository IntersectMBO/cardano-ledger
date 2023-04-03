{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Generic.PrettyCore where

import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Allegra.Rules as Allegra (AllegraUtxoPredFailure (..))
import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError (..))
import Cardano.Ledger.Alonzo.Rules as Alonzo (
  AlonzoBbodyPredFailure (..),
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
  FailureDescription (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Scripts.Data (
  Data (..),
  Datum (..),
  binaryDataToData,
  hashData,
 )
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptPurpose (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..), unTxDats)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Network (..),
  ProtVer (..),
  SlotNo (..),
  TxIx (..),
  txIxToInt,
 )
import qualified Cardano.Ledger.CertState as DP
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Governance (ConwayTallyState (..))
import Cardano.Ledger.Conway.Rules (
  ConwayEpochPredFailure (..),
  ConwayNewEpochPredFailure,
  EnactPredFailure (..),
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..), ConwayTxCert (..), Delegatee (..))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (
  Credential (
    KeyHashObj,
    ScriptHashObj
  ),
  StakeReference (..),
 )
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.EpochBoundary (
  SnapShot (..),
  SnapShots (..),
  Stake (..),
 )
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  GenDelegs (..),
  HasKeyRole (coerceKeyRole),
  KeyHash (..),
  KeyRole (..),
  VKey (..),
  hashKey,
 )
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..), flattenMultiAsset)
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Pretty
import Cardano.Ledger.Pretty.Alonzo
import qualified Cardano.Ledger.Pretty.Babbage as Babbage
import Cardano.Ledger.Pretty.Conway (ppConwayTxBody)
import Cardano.Ledger.Pretty.Mary
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.AdaPots (
  AdaPots (..),
  totalAdaES,
  totalAdaPotsES,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  CertState (..),
  DState (..),
  EpochState (..),
  FutureGenDeleg (..),
  InstantaneousRewards (..),
  LedgerState (..),
  NewEpochState (..),
  PPUPPredFailure,
  PState (..),
  UTxOState (..),
  VState (..),
 )
import Cardano.Ledger.Shelley.Rules as Shelley (
  ShelleyBbodyPredFailure (..),
  ShelleyBbodyState (..),
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure (..),
  ShelleyDelplPredFailure (..),
  ShelleyEpochPredFailure (..),
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersPredFailure (..),
  ShelleyNewEpochPredFailure (..),
  ShelleyNewppPredFailure (..),
  ShelleyPoolPredFailure (..),
  ShelleyPoolreapPredFailure,
  ShelleyPpupPredFailure (..),
  ShelleySnapPredFailure,
  ShelleyTickPredFailure (..),
  ShelleyUpecPredFailure (..),
  ShelleyUtxoPredFailure (..),
  ShelleyUtxowPredFailure (..),
 )
import qualified Cardano.Ledger.Shelley.Scripts as SS (MultiSig (..))
import Cardano.Ledger.Shelley.TxBody (
  PoolParams (..),
  ShelleyTxOut (..),
  WitVKey (..),
 )
import Cardano.Ledger.Shelley.TxCert (MIRCert (..), MIRTarget (..), ShelleyDelegCert (..), ShelleyTxCert (..))
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap (
  depositMap,
  fromCompact,
  ptrMap,
  rewardMap,
  sPoolMap,
 )
import qualified Cardano.Ledger.UMap as UM (UMap, UView (..), size)
import Cardano.Ledger.UTxO (ScriptsNeeded, UTxO (..))
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition.Extended (STS (..))
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import qualified Data.VMap as VMap
import Data.Void (absurd)
import Lens.Micro ((^.))
import qualified PlutusLedgerApi.V1 as PV1 (Data (..))
import Prettyprinter (hsep, parens, viaShow, vsep)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields (
  TxBodyField (..),
  TxField (..),
  WitnessesField (..),
  abstractTx,
  abstractTxBody,
  abstractWitnesses,
 )
import Test.Cardano.Ledger.Generic.Proof (
  AllegraEra,
  AlonzoEra,
  BabbageEra,
  ConwayEra,
  MaryEra,
  Proof (..),
  Reflect (..),
  ShelleyEra,
 )

-- =====================================================

class Era era => PrettyCore era where
  prettyTx :: Tx era -> PDoc
  prettyScript :: Script era -> PDoc
  prettyTxBody :: TxBody era -> PDoc
  prettyWitnesses :: TxWits era -> PDoc
  prettyValue :: Value era -> PDoc
  prettyTxOut :: TxOut era -> PDoc

instance CC.Crypto c => PrettyCore (ShelleyEra c) where
  prettyTx = Cardano.Ledger.Pretty.ppTx
  prettyScript = ppMultiSig
  prettyTxBody = Cardano.Ledger.Pretty.ppTxBody
  prettyWitnesses = ppWitnessSetHKD
  prettyValue = ppCoin
  prettyTxOut = Cardano.Ledger.Pretty.ppTxOut

instance CC.Crypto c => PrettyCore (AllegraEra c) where
  prettyTx = Cardano.Ledger.Pretty.ppTx
  prettyScript = ppTimelock
  prettyTxBody = prettyA
  prettyWitnesses = ppWitnessSetHKD
  prettyValue = ppCoin
  prettyTxOut = Cardano.Ledger.Pretty.ppTxOut

instance CC.Crypto c => PrettyCore (MaryEra c) where
  prettyTx = Cardano.Ledger.Pretty.ppTx
  prettyScript = ppTimelock
  prettyTxBody = prettyA
  prettyWitnesses = ppWitnessSetHKD
  prettyValue = ppValue
  prettyTxOut = Cardano.Ledger.Pretty.ppTxOut

instance CC.Crypto c => PrettyCore (AlonzoEra c) where
  prettyTx = Cardano.Ledger.Pretty.Alonzo.ppTx
  prettyScript = ppScript
  prettyTxBody = Cardano.Ledger.Pretty.Alonzo.ppTxBody
  prettyWitnesses = ppTxWitness
  prettyValue = ppValue
  prettyTxOut = Cardano.Ledger.Pretty.Alonzo.ppTxOut

instance CC.Crypto c => PrettyCore (BabbageEra c) where
  prettyTx = Cardano.Ledger.Pretty.Alonzo.ppTx
  prettyScript = ppScript
  prettyTxBody = Babbage.ppTxBody
  prettyWitnesses = ppTxWitness
  prettyValue = pcValue
  prettyTxOut = Babbage.ppTxOut

instance CC.Crypto c => PrettyCore (ConwayEra c) where
  prettyTx = Cardano.Ledger.Pretty.Alonzo.ppTx
  prettyScript = ppScript
  prettyTxBody = ppConwayTxBody
  prettyWitnesses = ppTxWitness
  prettyValue = ppValue
  prettyTxOut = Babbage.ppTxOut

prettyUTxO :: Proof era -> UTxO era -> PDoc
prettyUTxO proof = prettyUTxOMap proof . unUTxO

prettyUTxOMap :: Proof era -> Map.Map (TxIn (EraCrypto era)) (TxOut era) -> PDoc
prettyUTxOMap (Conway _) mp = ppMap ppTxIn prettyTxOut mp
prettyUTxOMap (Babbage _) mp = ppMap ppTxIn prettyTxOut mp
prettyUTxOMap (Alonzo _) mp = ppMap ppTxIn prettyTxOut mp
prettyUTxOMap (Mary _) mp = ppMap ppTxIn prettyTxOut mp
prettyUTxOMap (Allegra _) mp = ppMap ppTxIn prettyTxOut mp
prettyUTxOMap (Shelley _) mp = ppMap ppTxIn prettyTxOut mp

-- ===================================================================
-- PrettyA instances for UTXOW, UTXO, UTXOS, PPUP predicate failures
-- There are sometimes two versions:
-- one introduced in Shelley, one introduced in Alonzo
-- ===================================================================

-- Predicate Failure for LEDGER

ppLedgerPredicateFailure ::
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "DELEGS" era))
  ) =>
  ShelleyLedgerPredFailure era ->
  PDoc
ppLedgerPredicateFailure (UtxowFailure x) = prettyA x
ppLedgerPredicateFailure (DelegsFailure x) = ppString (show x)

instance
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "DELEGS" era))
  ) =>
  PrettyA (ShelleyLedgerPredFailure era)
  where
  prettyA = ppLedgerPredicateFailure

instance
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "DELEGS" era))
  ) =>
  PrettyA [ShelleyLedgerPredFailure era]
  where
  prettyA = ppList prettyA

-- =========================================
-- Predicate Failure for Alonzo UTXOW

ppUtxowPredicateFail ::
  ( PrettyA (PredicateFailure (EraRule "UTXO" era))
  , PrettyCore era
  , PrettyA (TxCert era)
  ) =>
  AlonzoUtxowPredFailure era ->
  PDoc
ppUtxowPredicateFail (ShelleyInAlonzoUtxowPredFailure x) = prettyA x
ppUtxowPredicateFail (MissingRedeemers xs) =
  ppSexp "MissingRedeemers" [ppList (ppPair ppScriptPurpose ppScriptHash) xs]
ppUtxowPredicateFail (MissingRequiredDatums s1 s2) =
  ppRecord
    "MissingRequiredDatums"
    [ ("missing data hashes", ppSet ppSafeHash s1)
    , ("received data hashes", ppSet ppSafeHash s2)
    ]
ppUtxowPredicateFail (NonOutputSupplimentaryDatums s1 s2) =
  ppRecord
    "NonOutputSupplimentaryDatums"
    [ ("unallowed data hashes", ppSet ppSafeHash s1)
    , ("acceptable data hashes", ppSet ppSafeHash s2)
    ]
ppUtxowPredicateFail (PPViewHashesDontMatch h1 h2) =
  ppRecord
    "PPViewHashesDontMatch"
    [ ("PPHash in the TxBody", ppStrictMaybe ppSafeHash h1)
    , ("PPHash Computed from the current Protocol Parameters", ppStrictMaybe ppSafeHash h2)
    ]
ppUtxowPredicateFail (MissingRequiredSigners x) =
  ppSexp "MissingRequiredSigners" [ppSet ppKeyHash x]
ppUtxowPredicateFail (UnspendableUTxONoDatumHash x) =
  ppSexp "UnspendableUTxONoDatumHash" [ppSet ppTxIn x]
ppUtxowPredicateFail (ExtraRedeemers x) =
  ppSexp "ExtraRedeemers" [ppList prettyA x]

instance
  ( PrettyA (PredicateFailure (EraRule "UTXO" era))
  , PrettyCore era
  , PrettyA (TxCert era)
  ) =>
  PrettyA (AlonzoUtxowPredFailure era)
  where
  prettyA = ppUtxowPredicateFail

-- ====================================================
-- Predicate Failure for Shelley UTXOW

ppUtxowPredicateFailure ::
  ( PrettyA (PredicateFailure (EraRule "UTXO" era))
  , PrettyCore era
  ) =>
  ShelleyUtxowPredFailure era ->
  PDoc
ppUtxowPredicateFailure (InvalidWitnessesUTXOW vkeyws) =
  ppSexp "InvalidWitnessesUTXOW" [ppList ppVKey vkeyws]
ppUtxowPredicateFailure (MissingVKeyWitnessesUTXOW whs) =
  ppSexp "MissingVKeyWitnessesUTXOW" [ppWitHashes whs]
ppUtxowPredicateFailure (MissingScriptWitnessesUTXOW m) =
  ppSexp "MissingScriptWitnessesUTXOW" [ppSet ppScriptHash m]
ppUtxowPredicateFailure (ScriptWitnessNotValidatingUTXOW m) =
  ppSexp "ScriptWitnessNotValidatingUTXOW" [ppSet ppScriptHash m]
ppUtxowPredicateFailure (UtxoFailure m) = ppSexp "UtxoFailure" [prettyA m]
ppUtxowPredicateFailure (MIRInsufficientGenesisSigsUTXOW m) =
  ppSexp "MIRInsufficientGenesisSigsUTXOW" [ppSet ppKeyHash m]
ppUtxowPredicateFailure (MissingTxBodyMetadataHash m) =
  ppSexp " MissingTxMetadata" [ppAuxiliaryDataHash m]
ppUtxowPredicateFailure (MissingTxMetadata m) =
  ppSexp " MissingTxMetadata" [ppAuxiliaryDataHash m]
ppUtxowPredicateFailure (ConflictingMetadataHash h1 h2) =
  ppRecord
    "ConflictingMetadataHash"
    [("Hash in the body", ppAuxiliaryDataHash h1), ("Hash of full metadata", ppAuxiliaryDataHash h2)]
ppUtxowPredicateFailure InvalidMetadata =
  ppSexp "InvalidMetadata" []
ppUtxowPredicateFailure (ExtraneousScriptWitnessesUTXOW m) =
  ppSexp "ExtraneousScriptWitnessesUTXOW" [ppSet ppScriptHash m]

instance
  ( PrettyA (PredicateFailure (EraRule "UTXO" era))
  , PrettyCore era
  ) =>
  PrettyA (ShelleyUtxowPredFailure era)
  where
  prettyA = ppUtxowPredicateFailure

-- ========================================================
-- Predicate Failure for Alonzo UTXO

ppUtxoPredicateFailure ::
  forall era.
  ( PrettyCore era
  , PrettyA (PredicateFailure (EraRule "UTXOS" era))
  , PrettyA (TxOut era) -- From ppUTxO FIXME
  ) =>
  AlonzoUtxoPredFailure era ->
  PDoc
ppUtxoPredicateFailure (Alonzo.BadInputsUTxO x) =
  ppSexp "BadInputsUTxO" [ppSet ppTxIn x]
ppUtxoPredicateFailure (Alonzo.OutsideValidityIntervalUTxO vi slot) =
  ppRecord "OutsideValidityIntervalUTxO" [("validity interval", ppValidityInterval vi), ("slot", ppSlotNo slot)]
ppUtxoPredicateFailure (Alonzo.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", ppInteger actual)
    , ("max transaction size", ppInteger maxs)
    ]
ppUtxoPredicateFailure Alonzo.InputSetEmptyUTxO =
  ppSexp "InputSetEmptyUTxO" []
ppUtxoPredicateFailure (Alonzo.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for thistransaction", ppCoin computed)
    , ("fee supplied by transaction", ppCoin supplied)
    ]
ppUtxoPredicateFailure (Alonzo.ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", prettyValue @era consumed)
    , ("coin produced", prettyValue @era produced)
    ]
ppUtxoPredicateFailure (Alonzo.WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", ppNetwork n)
    , ("set addresses with wrong network id", ppSet ppAddr add)
    ]
ppUtxoPredicateFailure (Alonzo.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", ppNetwork n)
    , ("set reward address with wrong network id", ppSet ppRewardAcnt' accnt)
    ]
ppUtxoPredicateFailure (Alonzo.OutputTooSmallUTxO xs) =
  ppSexp "OutputTooSmallUTxO" [ppList prettyTxOut xs]
ppUtxoPredicateFailure (UtxosFailure subpred) = prettyA subpred
-- ppSexp "UtxosFailure" [prettyA subpred]
ppUtxoPredicateFailure (Alonzo.OutputBootAddrAttrsTooBig x) =
  ppSexp "OutputBootAddrAttrsTooBig" [ppList prettyTxOut x]
ppUtxoPredicateFailure Alonzo.TriesToForgeADA =
  ppSexp "TriesToForgeADA" []
ppUtxoPredicateFailure (Alonzo.OutputTooBigUTxO xs) =
  ppSexp
    "OutputTooBigUTxO"
    [ ppList
        ( \(a, b, c) ->
            ppRecord
              ""
              [("actual size", ppInteger a), ("PParam max value", ppInteger b), ("TxOut", prettyTxOut c)]
        )
        xs
    ]
ppUtxoPredicateFailure (InsufficientCollateral x y) =
  ppRecord
    "InsufficientCollateral"
    [ ("balance computed", ppCoin x)
    , ("the required collateral for the given fee", ppCoin y)
    ]
ppUtxoPredicateFailure (ScriptsNotPaidUTxO x) =
  ppSexp "ScriptsNotPaidUTxO" [ppUTxO x]
ppUtxoPredicateFailure (ExUnitsTooBigUTxO x y) =
  ppRecord
    "ExUnitsTooBigUTxO"
    [ ("Max EXUnits from the protocol parameters", ppExUnits x)
    , ("EXUnits supplied", ppExUnits y)
    ]
ppUtxoPredicateFailure (CollateralContainsNonADA x) =
  ppSexp "CollateralContainsNonADA" [prettyValue @era x]
ppUtxoPredicateFailure (WrongNetworkInTxBody x y) =
  ppRecord
    "WrongNetworkInTxBody"
    [ ("Actual Network ID", ppNetwork x)
    , ("Network ID in transaction body", ppNetwork y)
    ]
ppUtxoPredicateFailure (OutsideForecast x) =
  ppRecord "OutsideForecast" [("slot number outside consensus forecast range", ppSlotNo x)]
ppUtxoPredicateFailure (TooManyCollateralInputs x y) =
  ppRecord
    "TooManyCollateralInputs"
    [ ("Max allowed collateral inputs", ppNatural x)
    , ("Number of collateral inputs", ppNatural y)
    ]
ppUtxoPredicateFailure (NoCollateralInputs) =
  ppSexp "NoCollateralInputs" []

instance
  ( PrettyCore era
  , PrettyA (PredicateFailure (EraRule "UTXOS" era))
  , PrettyA (TxOut era) -- From ppUTxO FIXME
  ) =>
  PrettyA (AlonzoUtxoPredFailure era)
  where
  prettyA = ppUtxoPredicateFailure

instance
  PrettyA (PredicateFailure (EraRule "DELPL" era)) =>
  PrettyA (ShelleyDelegsPredFailure era)
  where
  prettyA (DelegateeNotRegisteredDELEG x) =
    ppRecord
      "DelegateeNotRegisteredDELEG"
      [("KeyHash", prettyA x)]
  prettyA (WithdrawalsNotInRewardsDELEGS x) =
    ppRecord
      "WithdrawalsNotInRewardsDELEGS"
      [("Missing Withdrawals", prettyA x)]
  prettyA (DelplFailure x) = prettyA x

instance
  ( PrettyA (PredicateFailure (EraRule "POOL" era))
  , PrettyA (PredicateFailure (EraRule "DELEG" era))
  ) =>
  PrettyA (ShelleyDelplPredFailure era)
  where
  prettyA (PoolFailure x) = prettyA x
  prettyA (DelegFailure x) = prettyA x

instance PrettyA (ShelleyPoolPredFailure era) where
  prettyA (StakePoolNotRegisteredOnKeyPOOL kh) =
    ppRecord
      "StakePoolNotRegisteredOnKeyPOOL"
      [ ("KeyHash", prettyA kh)
      ]
  prettyA
    ( StakePoolRetirementWrongEpochPOOL
        curEpoch
        poolRetEpoch
        firstTooFarEpoch
      ) =
      ppRecord
        "StakePoolRetirementWrongEpochPOOL"
        [ ("Current Epoch", prettyA curEpoch)
        , ("Pool Retirement Epoch", prettyA poolRetEpoch)
        , ("First Epoch Too Far", prettyA firstTooFarEpoch)
        ]
  prettyA
    ( StakePoolCostTooLowPOOL
        prcStakePoolCost
        ppStakePoolCost
      ) =
      ppRecord
        "StakePoolCostTooLowPOOL"
        [ ("PRC Stake Pool Cost", prettyA prcStakePoolCost)
        , ("PP Stake Pool Cost", prettyA ppStakePoolCost)
        ]
  prettyA
    ( WrongNetworkPOOL
        nwId
        regCertNwId
        stakePoolId
      ) =
      ppRecord
        "WrongNetworkPOOL"
        [ ("Network ID", prettyA nwId)
        , ("Registration Certificate Network ID", prettyA regCertNwId)
        , ("Stake Pool ID", prettyA stakePoolId)
        ]
  prettyA
    ( PoolMedataHashTooBig
        stakePoolId
        metadataHashSize
      ) =
      ppRecord
        "PoolMedataHashTooBig"
        [ ("Stake Pool ID", prettyA stakePoolId)
        , ("Metadata Hash Size", prettyA metadataHashSize)
        ]

instance PrettyA (ShelleyDelegPredFailure era) where
  prettyA = ppString . show -- TODO

-- =========================================
-- Predicate Failure for Alonzo UTXOS

ppUtxosPredicateFailure ::
  (PrettyA (PPUPPredFailure era), PrettyA (TxCert era)) =>
  AlonzoUtxosPredFailure era ->
  PDoc
ppUtxosPredicateFailure (ValidationTagMismatch isvalid tag) =
  ppRecord
    "ValidationTagMismatch"
    [ ("isValid tag", ppIsValid isvalid)
    , ("mismatch description", ppTagMismatchDescription tag)
    ]
ppUtxosPredicateFailure (CollectErrors es) =
  ppRecord' mempty [("When collecting inputs for twophase scripts, these went wrong.", ppList ppCollectError es)]
ppUtxosPredicateFailure (Alonzo.UpdateFailure p) = prettyA p

instance
  ( PrettyA (PPUPPredFailure era)
  , PrettyA (TxCert era)
  ) =>
  PrettyA (AlonzoUtxosPredFailure era)
  where
  prettyA = ppUtxosPredicateFailure

ppCollectError :: PrettyA (TxCert era) => CollectError era -> PDoc
ppCollectError (NoRedeemer sp) = ppSexp "NoRedeemer" [ppScriptPurpose sp]
ppCollectError (NoWitness sh) = ppSexp "NoWitness" [ppScriptHash sh]
ppCollectError (NoCostModel l) = ppSexp "NoCostModel" [ppLanguage l]
ppCollectError (BadTranslation x) = ppSexp "BadTranslation" [ppString (show x)]

instance PrettyA (TxCert c) => PrettyA (CollectError c) where
  prettyA = ppCollectError

ppTagMismatchDescription :: TagMismatchDescription -> PDoc
ppTagMismatchDescription (PassedUnexpectedly) = ppSexp "PassedUnexpectedly" []
ppTagMismatchDescription (FailedUnexpectedly xs) =
  ppSexp "FailedUnexpectedly" [ppList ppFailureDescription (toList xs)]

instance PrettyA TagMismatchDescription where
  prettyA = ppTagMismatchDescription

ppFailureDescription :: FailureDescription -> PDoc
ppFailureDescription (PlutusFailure txt bytes) =
  ppRecord "PlutusFailure" [("reason", text txt), ("script", ppLong bytes)]

instance PrettyA FailureDescription where
  prettyA = ppFailureDescription

-- =======================================
-- Predicate Failure for Shelley UTxO

ppUtxoPFShelley ::
  forall era.
  ( PrettyCore era
  , PrettyA (PPUPPredFailure era)
  ) =>
  ShelleyUtxoPredFailure era ->
  PDoc
ppUtxoPFShelley (Shelley.BadInputsUTxO x) =
  ppSexp "BadInputsUTxO" [ppSet ppTxIn x]
ppUtxoPFShelley (Shelley.ExpiredUTxO ttl slot) =
  ppRecord "ExpiredUTxO" [("transaction time to live", ppSlotNo ttl), ("current slot", ppSlotNo slot)]
ppUtxoPFShelley (Shelley.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", ppInteger actual)
    , ("max transaction size", ppInteger maxs)
    ]
ppUtxoPFShelley (Shelley.InputSetEmptyUTxO) =
  ppSexp "InputSetEmptyUTxO" []
ppUtxoPFShelley (Shelley.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for this transaction", ppCoin computed)
    , ("fee supplied by this transaction", ppCoin supplied)
    ]
ppUtxoPFShelley (Shelley.ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", prettyValue @era consumed)
    , ("coin produced", prettyValue @era produced)
    ]
ppUtxoPFShelley (Shelley.WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", ppNetwork n)
    , ("set of addresses with wrong network id", ppSet ppAddr add)
    ]
ppUtxoPFShelley (Shelley.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", ppNetwork n)
    , ("set of reward address with wrong network id", ppSet ppRewardAcnt' accnt)
    ]
ppUtxoPFShelley (Shelley.OutputTooSmallUTxO xs) =
  ppRecord
    "OutputTooSmallUTxO"
    [("list of supplied transaction outputs that are too small", ppList prettyTxOut xs)]
ppUtxoPFShelley (Shelley.UpdateFailure x) =
  ppSexp "UpdateFailure" [prettyA x]
ppUtxoPFShelley (Shelley.OutputBootAddrAttrsTooBig xs) =
  ppRecord "OutputBootAddrAttrsTooBig" [("list of supplied bad transaction outputs", ppList prettyTxOut xs)]

instance
  ( PrettyCore era
  , PrettyA (PPUPPredFailure era)
  ) =>
  PrettyA (ShelleyUtxoPredFailure era)
  where
  prettyA = ppUtxoPFShelley

-- =======================================
-- Predicate Failure for Shelley PPUP

ppPpupPredicateFailure :: ShelleyPpupPredFailure era -> PDoc
ppPpupPredicateFailure (NonGenesisUpdatePPUP x y) =
  ppRecord
    "NonGenesisUpdatePPUP"
    [ ("KeyHashes which are voting", ppSet ppKeyHash x)
    , ("KeyHashes which should be voting", ppSet ppKeyHash y)
    ]
ppPpupPredicateFailure (PPUpdateWrongEpoch x y z) =
  ppRecord
    "PPUpdateWrongEpoch"
    [ ("current epoch", ppEpochNo x)
    , ("intended epoch of update", ppEpochNo y)
    , ("voting period within the epoch", ppString (show z))
    ]
ppPpupPredicateFailure (PVCannotFollowPPUP x) =
  ppRecord "PVCannotFollowPPUP" [("the first bad protocol version", ppProtVer x)]

instance PrettyA (ShelleyPpupPredFailure era) where
  prettyA = ppPpupPredicateFailure

-- =====================================================
-- Predicate failure for Mary UTXO

ppUtxoPFMary ::
  forall era.
  ( PrettyCore era
  , PrettyA (PPUPPredFailure era)
  ) =>
  AllegraUtxoPredFailure era ->
  PDoc
ppUtxoPFMary (Allegra.BadInputsUTxO txins) =
  ppSexp "BadInputsUTxO" [ppSet ppTxIn txins]
ppUtxoPFMary (Allegra.OutsideValidityIntervalUTxO vi slot) =
  ppRecord
    "OutsideValidityIntervalUTxO"
    [ ("provided interval", ppValidityInterval vi)
    , ("current slot", ppSlotNo slot)
    ]
ppUtxoPFMary (Allegra.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", ppInteger actual)
    , ("max transaction size", ppInteger maxs)
    ]
ppUtxoPFMary (Allegra.InputSetEmptyUTxO) = ppSexp "InputSetEmptyUTxO" []
ppUtxoPFMary (Allegra.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for this transaction", ppCoin computed)
    , ("fee supplied by this transaction", ppCoin supplied)
    ]
ppUtxoPFMary (Allegra.ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", prettyValue @era consumed)
    , ("coin produced", prettyValue @era produced)
    ]
ppUtxoPFMary (Allegra.WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", ppNetwork n)
    , ("set of addresses with wrong network id", ppSet ppAddr add)
    ]
ppUtxoPFMary (Allegra.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", ppNetwork n)
    , ("set reward address with wrong network id", ppSet ppRewardAcnt' accnt)
    ]
ppUtxoPFMary (Allegra.OutputTooSmallUTxO xs) =
  ppRecord
    "OutputTooSmallUTxO"
    [("list of supplied transaction outputs that are too small", ppList prettyTxOut xs)]
ppUtxoPFMary (Allegra.UpdateFailure x) =
  ppSexp "UpdateFailure" [prettyA x]
ppUtxoPFMary (Allegra.OutputBootAddrAttrsTooBig xs) =
  ppRecord "OutputBootAddrAttrsTooBig" [("list of supplied bad transaction outputs", ppList prettyTxOut xs)]
ppUtxoPFMary (Allegra.TriesToForgeADA) = ppSexp "TriesToForgeADA" []
ppUtxoPFMary (Allegra.OutputTooBigUTxO outs) =
  ppRecord "OutputTooBigUTxO" [("list of TxOuts which are too big", ppList prettyTxOut outs)]

instance
  ( PrettyCore era
  , PrettyA (PPUPPredFailure era)
  ) =>
  PrettyA (AllegraUtxoPredFailure era)
  where
  prettyA = ppUtxoPFMary

-- =====================================================
-- Probably should be moved elsewhere

-- LedgerState.hs
ppWitHashes :: Set (KeyHash 'Witness c) -> PDoc
ppWitHashes hs = ppSexp "WitHashes" [ppSet ppKeyHash hs]

-- Defined in ‘Cardano.Ledger.Alonzo.Tx’
ppScriptPurpose :: PrettyA (TxCert era) => ScriptPurpose era -> PDoc
ppScriptPurpose (Minting policy) = ppSexp "Minting" [prettyA policy] -- FIXME fill in the blanks
ppScriptPurpose (Spending txin) = ppSexp "Spending" [ppTxIn txin]
ppScriptPurpose (Rewarding acct) = ppSexp "Rewarding" [ppRewardAcnt' acct]
ppScriptPurpose (Certifying dcert) = ppSexp "Certifying" [prettyA dcert]

instance PrettyA x => PrettyA [x] where prettyA xs = ppList prettyA xs

-- =====================================================

dots :: PDoc -> PDoc
dots _ = ppString "..."

dotsF :: (a -> PDoc) -> (a -> PDoc)
dotsF _f _x = ppString "..."

ppMyWay :: (Typeable keyrole, CC.Crypto c) => WitVKey keyrole c -> PDoc
ppMyWay (wvk@(WitVKey vkey _)) = ppSexp "MyWay" [ppKeyHash (hashKey vkey), ppWitVKey wvk]

ppCoreWitnesses :: Proof era -> TxWits era -> PDoc
ppCoreWitnesses (Conway _) x = ppTxWitness x
ppCoreWitnesses (Babbage _) x = ppTxWitness x
ppCoreWitnesses (Alonzo _) x = ppTxWitness x
ppCoreWitnesses (Mary _) x = ppWitnessSetHKD x
ppCoreWitnesses (Allegra _) x = ppWitnessSetHKD x
ppCoreWitnesses (Shelley _) x = ppWitnessSetHKD x

ppCoreScript :: Proof era -> Script era -> PDoc
ppCoreScript (Conway _) (PlutusScript _ x) = ppString (show x)
ppCoreScript (Conway _) (TimelockScript x) = ppTimelock x
ppCoreScript (Babbage _) (PlutusScript _ x) = ppString (show x)
ppCoreScript (Babbage _) (TimelockScript x) = ppTimelock x
ppCoreScript (Alonzo _) (PlutusScript _ x) = ppString (show x)
ppCoreScript (Alonzo _) (TimelockScript x) = ppTimelock x
ppCoreScript (Mary _) x = ppTimelock x
ppCoreScript (Allegra _) x = ppTimelock x
ppCoreScript (Shelley _) x = ppMultiSig x

-- =======================================================

ppLedgersPredicateFailure ::
  PrettyA (PredicateFailure (EraRule "LEDGER" era)) => ShelleyLedgersPredFailure era -> PDoc
ppLedgersPredicateFailure (LedgerFailure x) = prettyA x

instance
  PrettyA (PredicateFailure (EraRule "LEDGER" era)) =>
  PrettyA (ShelleyLedgersPredFailure era)
  where
  prettyA = ppLedgersPredicateFailure

-- ================
ppBbodyPredicateFailure ::
  PrettyA (PredicateFailure (EraRule "LEDGERS" era)) =>
  ShelleyBbodyPredFailure era ->
  PDoc
ppBbodyPredicateFailure (WrongBlockBodySizeBBODY x y) =
  ppRecord
    "WrongBlockBodySizeBBODY"
    [ ("actual computed BBody size", ppInt x)
    , ("claimed BBody Size in Header", ppInt y)
    ]
ppBbodyPredicateFailure (InvalidBodyHashBBODY h1 h2) =
  ppRecord
    "(InvalidBodyHashBBODY"
    [ ("actual hash", ppHash h1)
    , ("claimed hash", ppHash h2)
    ]
ppBbodyPredicateFailure (LedgersFailure x) =
  ppSexp "LedgersFailure" [prettyA x]

instance
  PrettyA (PredicateFailure (EraRule "LEDGERS" era)) =>
  PrettyA (ShelleyBbodyPredFailure era)
  where
  prettyA = ppBbodyPredicateFailure

-- ================

ppAlonzoBbodyPredFail ::
  PrettyA (PredicateFailure (EraRule "LEDGERS" era)) =>
  AlonzoBbodyPredFailure era ->
  PDoc
ppAlonzoBbodyPredFail (ShelleyInAlonzoBbodyPredFailure x) =
  ppSexp "ShelleyInAlonzoPredFail" [ppBbodyPredicateFailure x]
ppAlonzoBbodyPredFail (TooManyExUnits e1 e2) =
  ppRecord
    "TooManyExUnits"
    [ ("Computed Sum of ExUnits for all plutus scripts", ppExUnits e1)
    , ("Maximum allowed by protocal parameters", ppExUnits e2)
    ]

instance
  PrettyA (PredicateFailure (EraRule "LEDGERS" era)) =>
  PrettyA (AlonzoBbodyPredFailure era)
  where
  prettyA = ppAlonzoBbodyPredFail

-- ===============
ppTickPredicateFailure ::
  forall era.
  ( Reflect era
  ) =>
  ShelleyTickPredFailure era ->
  PDoc
ppTickPredicateFailure (NewEpochFailure x) = ppNewEpochPredicateFailure @era x
ppTickPredicateFailure (RupdFailure _) =
  ppString "RupdPredicateFailure has no constructors"

instance
  ( ShelleyNewEpochPredFailure era ~ PredicateFailure (EraRule "NEWEPOCH" era)
  , ShelleyEpochPredFailure era ~ PredicateFailure (EraRule "EPOCH" era)
  , Reflect era
  ) =>
  PrettyA (ShelleyTickPredFailure era)
  where
  prettyA = ppTickPredicateFailure

-- ===============
ppNewEpochPredicateFailure ::
  forall era.
  ( Reflect era
  ) =>
  PredicateFailure (EraRule "NEWEPOCH" era) ->
  PDoc
ppNewEpochPredicateFailure x = case reify @era of
  Shelley _ -> ppShelleyNewEpochPredicateFailure x
  Allegra _ -> ppShelleyNewEpochPredicateFailure x
  Mary _ -> ppShelleyNewEpochPredicateFailure x
  Alonzo _ -> ppShelleyNewEpochPredicateFailure x
  Babbage _ -> ppShelleyNewEpochPredicateFailure x
  Conway _ -> ppConwayNewEpochPredicateFailure x

ppShelleyNewEpochPredicateFailure ::
  forall era.
  ( PredicateFailure (EraRule "EPOCH" era) ~ ShelleyEpochPredFailure era
  , PredicateFailure (EraRule "UPEC" era) ~ ShelleyUpecPredFailure era
  , Reflect era
  ) =>
  ShelleyNewEpochPredFailure era ->
  PDoc
ppShelleyNewEpochPredicateFailure (EpochFailure x) = ppEpochPredicateFailure @era x
ppShelleyNewEpochPredicateFailure (CorruptRewardUpdate x) =
  ppSexp "CorruptRewardUpdate" [ppRewardUpdate x]
ppShelleyNewEpochPredicateFailure (MirFailure _) =
  ppString "MirPredicateFailure has no constructors"

ppConwayNewEpochPredicateFailure ::
  forall era.
  ( PredicateFailure (EraRule "EPOCH" era) ~ ConwayEpochPredFailure era
  , PrettyA (PredicateFailure (EraRule "EPOCH" era))
  , PrettyA (PredicateFailure (EraRule "RATIFY" era))
  ) =>
  ConwayNewEpochPredFailure era ->
  PDoc
ppConwayNewEpochPredicateFailure (Conway.EpochFailure x) = ppEpochPredicateFailure @era x
ppConwayNewEpochPredicateFailure (Conway.CorruptRewardUpdate x) =
  ppSexp "CorruptRewardUpdate" [ppRewardUpdate x]
ppConwayNewEpochPredicateFailure (Conway.RatifyFailure x) = ppRatifyPredicateFailure @era x

ppRatifyPredicateFailure :: Conway.EnactPredFailure era -> PDoc
ppRatifyPredicateFailure (Conway.EnactTreasuryInsufficientFunds wdrl tr) =
  ppRecord
    "EnactTreasuryInsufficientFunds"
    [ ("Withdrawals", prettyA wdrl)
    , ("Treasury", prettyA tr)
    ]

instance
  ( Reflect era
  , PredicateFailure (EraRule "EPOCH" era) ~ ShelleyEpochPredFailure era
  , PredicateFailure (EraRule "UPEC" era) ~ ShelleyUpecPredFailure era
  ) =>
  PrettyA (ShelleyNewEpochPredFailure era)
  where
  prettyA = ppShelleyNewEpochPredicateFailure

-- ===============
ppEpochPredicateFailure ::
  forall era.
  Reflect era =>
  ShelleyEpochPredFailure era ->
  PDoc
ppEpochPredicateFailure (PoolReapFailure _) =
  ppString "PoolreapPredicateFailure has no constructors"
ppEpochPredicateFailure (SnapFailure _) =
  ppString "SnapPredicateFailure has no constructors"
ppEpochPredicateFailure (UpecFailure x) = case reify @era of
  Shelley _ -> ppUpecPredicateFailure x
  Mary _ -> ppUpecPredicateFailure x
  Alonzo _ -> ppUpecPredicateFailure x
  Allegra _ -> ppUpecPredicateFailure x
  Babbage _ -> ppUpecPredicateFailure x
  Conway _ -> absurd x

instance
  ( ShelleyEpochPredFailure era ~ PredicateFailure (EraRule "EPOCH" era)
  , ShelleyUpecPredFailure era ~ PredicateFailure (EraRule "UPEC" era)
  , Reflect era
  ) =>
  PrettyA (ShelleyEpochPredFailure era)
  where
  prettyA = ppEpochPredicateFailure

instance
  ( PrettyA (PredicateFailure (EraRule "POOLREAP" era))
  , PrettyA (PredicateFailure (EraRule "SNAP" era))
  , PrettyA (PredicateFailure (EraRule "RATIFY" era))
  ) =>
  PrettyA (ConwayEpochPredFailure era)
  where
  prettyA (ConwayPoolReapFailure x) = prettyA x
  prettyA (ConwaySnapFailure x) = prettyA x
  prettyA (ConwayRatifyFailure x) = prettyA x

instance PrettyA (ShelleyPoolreapPredFailure era) where
  prettyA = \case {}

instance PrettyA (ShelleySnapPredFailure era) where
  prettyA = \case {}

instance PrettyA (Conway.EnactPredFailure era) where
  prettyA (EnactTreasuryInsufficientFunds x y) =
    "Not enough funds in treasury. \nWitdrawals: "
      <> prettyA x
      <> "\nRemaining funds in treasury:"
      <> prettyA y

-- ===============
ppUpecPredicateFailure :: ShelleyUpecPredFailure era -> PDoc
ppUpecPredicateFailure (NewPpFailure x) = ppNewppPredicateFailure x

instance
  (ShelleyUpecPredFailure era ~ PredicateFailure (EraRule "UPEC" era)) =>
  PrettyA (ShelleyUpecPredFailure era)
  where
  prettyA = ppUpecPredicateFailure

-- ===============
ppNewppPredicateFailure :: ShelleyNewppPredFailure era -> PDoc
ppNewppPredicateFailure (UnexpectedDepositPot c1 c2) =
  ppRecord
    "UnexpectedDepositPot"
    [ ("The total outstanding deposits", ppCoin c1)
    , ("The deposit pot", ppCoin c2)
    ]

instance PrettyA (ShelleyNewppPredFailure era) where prettyA = ppNewppPredicateFailure

-- ===================

ppBbodyState ::
  ( PrettyA (TxOut era)
  , PrettyA (PParams era)
  , PrettyA (GovernanceState era)
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  ) =>
  ShelleyBbodyState era ->
  PDoc
ppBbodyState (BbodyState ls (BlocksMade mp)) =
  ppRecord
    "BbodyState"
    [ ("ledger state", ppLedgerState ls)
    , ("blocks made", ppMap ppKeyHash ppNatural mp)
    ]

instance
  ( PrettyA (TxOut era)
  , PrettyA (PParams era)
  , PrettyA (GovernanceState era)
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  ) =>
  PrettyA (ShelleyBbodyState era)
  where
  prettyA = ppBbodyState

-- =======================================================
-- Summaries

txBodyFieldSummary :: EraTxBody era => TxBodyField era -> [(Text, PDoc)]
txBodyFieldSummary txb = case txb of
  (Inputs s) -> [("Inputs", ppInt (Set.size s))]
  (Collateral s) -> [("Collateral", ppInt (Set.size s))]
  (RefInputs s) -> [("RefInputs", ppInt (Set.size s))]
  (Outputs xs) -> [("Outputs", ppInt (length xs))]
  (CollateralReturn (SJust _)) -> [("Collateral Return", ppString "?")]
  (TotalCol (SJust c)) -> [("TotalCollateral", ppCoin c)]
  (Certs xs) -> [("Certs", ppInt (length xs))]
  (Withdrawals' x) -> [("Withdrawals", ppInt (Map.size (unWithdrawals x)))]
  (Vldt x) -> [("Validity interval", ppValidityInterval x)]
  (Txfee c) -> [("Fee", ppCoin c)]
  (Update (SJust _)) -> [("Collateral Return", ppString "?")]
  (ReqSignerHashes x) -> [("Required Signer hashes", ppInt (Set.size x))]
  (Mint ma) -> [("Mint", ppInteger (Val.size (MaryValue 0 ma)) <> ppString " bytes")]
  (WppHash (SJust _)) -> [("WppHash", ppString "?")]
  (AdHash (SJust _)) -> [("AdHash", ppString "?")]
  (Txnetworkid (SJust x)) -> [("Network id", ppNetwork x)]
  _ -> []

bodySummary :: EraTxBody era => Proof era -> TxBody era -> PDoc
bodySummary proof body =
  ppRecord
    "TxBody"
    (concat (map txBodyFieldSummary (abstractTxBody proof body)))

witnessFieldSummary :: Era era => WitnessesField era -> (Text, PDoc)
witnessFieldSummary wit = case wit of
  (AddrWits s) -> ("Address Witnesses", ppInt (Set.size s))
  (BootWits s) -> ("BootStrap Witnesses", ppInt (Set.size s))
  (ScriptWits s) -> ("Script Witnesses", ppInt (Map.size s))
  (DataWits m) -> ("Data Witnesses", ppInt (Map.size (unTxDats m)))
  (RdmrWits (Redeemers m)) -> ("Redeemer Witnesses", ppInt (Map.size m))

witnessSummary :: Era era => Proof era -> TxWits era -> PDoc
witnessSummary proof wits =
  ppRecord
    "Witnesses"
    (map witnessFieldSummary (abstractWitnesses proof wits))

txFieldSummary :: EraTxBody era => Proof era -> TxField era -> [PDoc]
txFieldSummary proof tx = case tx of
  (Body b) -> [bodySummary proof b]
  (BodyI xs) -> [ppRecord "TxBody" (concat (map txBodyFieldSummary xs))]
  (TxWits ws) -> [witnessSummary proof ws]
  (WitnessesI ws) -> [ppRecord "Witnesses" (map witnessFieldSummary ws)]
  (AuxData (SJust _)) -> [ppSexp "AuxData" [ppString "?"]]
  (Valid (IsValid b)) -> [ppSexp "IsValid" [ppBool b]]
  _ -> []

txSummary :: EraTx era => Proof era -> Tx era -> PDoc
txSummary proof tx =
  ppSexp "Tx" (concat (map (txFieldSummary proof) (abstractTx proof tx)))

-- =================================
-- Summary version of UTxO

trim :: PDoc -> PDoc
trim x = ppString (take 10 (show x))

txInSummary :: TxIn era -> PDoc
txInSummary (TxIn (TxId h) n) = ppSexp "TxIn" [trim (ppSafeHash h), ppInt (txIxToInt n)]

txOutSummary :: Proof era -> TxOut era -> PDoc
txOutSummary p@(Conway _) (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [addrSummary addr, pcCoreValue p v, datumSummary d, ppStrictMaybe (scriptSummary p) s]
txOutSummary p@(Babbage _) (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [addrSummary addr, pcCoreValue p v, datumSummary d, ppStrictMaybe (scriptSummary p) s]
txOutSummary p@(Alonzo _) (AlonzoTxOut addr v md) =
  ppSexp "TxOut" [addrSummary addr, pcCoreValue p v, ppStrictMaybe dataHashSummary md]
txOutSummary p@(Mary _) (ShelleyTxOut addr v) = ppSexp "TxOut" [addrSummary addr, pcCoreValue p v]
txOutSummary p@(Allegra _) (ShelleyTxOut addr v) = ppSexp "TxOut" [addrSummary addr, pcCoreValue p v]
txOutSummary p@(Shelley _) (ShelleyTxOut addr v) = ppSexp "TxOut" [addrSummary addr, pcCoreValue p v]

datumSummary :: Era era => Datum era -> PDoc
datumSummary NoDatum = ppString "NoDatum"
datumSummary (DatumHash h) = ppSexp "DHash" [trim (ppSafeHash h)]
datumSummary (Datum b) = dataSummary (binaryDataToData b)

dataSummary :: Era era => Data era -> PDoc
dataSummary (Data x) = plutusDataSummary x

plutusDataSummary :: PV1.Data -> PDoc
plutusDataSummary (PV1.Constr n ds) = (ppString (show n)) <> ppList plutusDataSummary ds
plutusDataSummary (PV1.Map ds) = ppString "Map" <> ppList (ppPair plutusDataSummary plutusDataSummary) ds
plutusDataSummary (PV1.List xs) = ppList plutusDataSummary xs
plutusDataSummary (PV1.I n) = ppInteger n
plutusDataSummary (PV1.B bs) = trim (ppLong bs)

multiAssetSummary :: MultiAsset c -> PDoc
multiAssetSummary (MultiAsset m) = ppString ("num tokens = " ++ show (Map.size m))

vSummary :: MaryValue c -> PDoc
vSummary (MaryValue n ma) =
  ppSexp "Value" [ppInteger n, multiAssetSummary ma]

scriptSummary :: forall era. Proof era -> Script era -> PDoc
scriptSummary p@(Conway _) script = plutusSummary p script
scriptSummary p@(Babbage _) script = plutusSummary p script
scriptSummary p@(Alonzo _) script = plutusSummary p script
scriptSummary (Mary _) script = timelockSummary script
scriptSummary (Allegra _) script = timelockSummary script
scriptSummary (Shelley _) script = multiSigSummary script

networkSummary :: Network -> PDoc
networkSummary Testnet = ppString "Test"
networkSummary Mainnet = ppString "Main"

addrSummary :: Addr c -> PDoc
addrSummary (Addr nw pay stk) =
  ppSexp "Addr" [networkSummary nw, credSummary pay, stakeSummary stk]
addrSummary (AddrBootstrap _) = ppString "Bootstrap"

credSummary :: Credential keyrole c -> PDoc
credSummary (ScriptHashObj (ScriptHash h)) = ppSexp "Script" [trim (ppHash h)]
credSummary (KeyHashObj (KeyHash kh)) = ppSexp "Key" [trim (ppHash kh)]

stakeSummary :: StakeReference c -> PDoc
stakeSummary StakeRefNull = ppString "Null"
stakeSummary (StakeRefPtr _) = ppString "Ptr"
stakeSummary (StakeRefBase x) = ppSexp "Stake" [credSummary (coerceKeyRole x)]

utxoSummary :: Proof era -> UTxO era -> PDoc
utxoSummary proof = ppMap txInSummary (txOutSummary proof) . unUTxO

utxoString :: Proof era -> UTxO era -> String
utxoString proof = show . ppMap txInSummary (txOutSummary proof) . unUTxO

scriptHashSummary :: ScriptHash c -> PDoc
scriptHashSummary (ScriptHash h) = trim (ppHash h)

keyHashSummary :: KeyHash keyrole c -> PDoc
keyHashSummary (KeyHash h) = trim (ppHash h)

dataHashSummary :: DataHash era -> PDoc
dataHashSummary dh = trim (ppSafeHash dh)

keyPairSummary :: CC.Crypto c => KeyPair r c -> PDoc
keyPairSummary (KeyPair x y) =
  ppRecord "KeyPair" [("vKey", vKeySummary x), ("sKey", (viaShow y))]

vKeySummary :: CC.Crypto c => VKey r c -> PDoc
vKeySummary vk@(VKey x) = (viaShow x) <> " (hash " <> keyHashSummary (hashKey vk) <> ")"

timelockSummary :: Era era => Timelock era -> PDoc
timelockSummary (RequireSignature akh) =
  ppSexp "Signature" [keyHashSummary akh]
timelockSummary (RequireAllOf ms) =
  ppSexp "AllOf" (foldr (:) [] (fmap timelockSummary ms))
timelockSummary (RequireAnyOf ms) =
  ppSexp "AnyOf" (foldr (:) [] (fmap timelockSummary ms))
timelockSummary (RequireMOf m ms) =
  ppSexp "MOfN" (ppInteger (fromIntegral m) : foldr (:) [] (fmap timelockSummary ms))
timelockSummary (RequireTimeExpire mslot) =
  ppSexp "Expires" [ppSlotNo mslot]
timelockSummary (RequireTimeStart mslot) =
  ppSexp "Starts" [ppSlotNo mslot]

multiSigSummary :: Era era => SS.MultiSig era -> PDoc
multiSigSummary (SS.RequireSignature hk) = ppSexp "ReqSig" [keyHashSummary hk]
multiSigSummary (SS.RequireAllOf ps) = ppSexp "AllOf" (map multiSigSummary ps)
multiSigSummary (SS.RequireAnyOf ps) = ppSexp "AnyOf" (map multiSigSummary ps)
multiSigSummary (SS.RequireMOf m ps) = ppSexp "MOf" (ppInt m : map multiSigSummary ps)

plutusSummary :: forall era. Proof era -> AlonzoScript era -> PDoc
plutusSummary (Conway _) s@(PlutusScript lang _) =
  ppString (show lang ++ " ") <> scriptHashSummary (hashScript @era s)
plutusSummary (Conway _) (TimelockScript x) = timelockSummary x
plutusSummary (Babbage _) s@(PlutusScript lang _) =
  ppString (show lang ++ " ") <> scriptHashSummary (hashScript @era s)
plutusSummary (Babbage _) (TimelockScript x) = timelockSummary x
plutusSummary (Alonzo _) s@(PlutusScript lang _) =
  ppString (show lang ++ " ") <> scriptHashSummary (hashScript @era s)
plutusSummary (Alonzo _) (TimelockScript x) = timelockSummary x
plutusSummary other _ = ppString ("Plutus script in era " ++ show other ++ "???")

dStateSummary :: DState c -> PDoc
dStateSummary (DState umap future (GenDelegs current) irwd) =
  ppRecord
    "DState"
    [ ("Unified Reward Map", uMapSummary umap)
    , ("Future genesis key delegations", ppInt (Map.size future))
    , ("Genesis key delegations", ppInt (Map.size current))
    , ("Instantaneous Rewards", instantSummary irwd)
    ]

instantSummary :: InstantaneousRewards c -> PDoc
instantSummary (InstantaneousRewards reserves treasury dreserves dtreasury) =
  ppRecord
    "InstantaneousRewards"
    [ ("Rewards from reserves", ppInt (Map.size reserves))
    , ("Rewards from treasury", ppInt (Map.size treasury))
    , ("Treasury to reserves", ppDeltaCoin dreserves)
    , ("Reserves to treasury", ppDeltaCoin dtreasury)
    ]

uMapSummary :: UM.UMap c -> PDoc
uMapSummary umap =
  ppRecord
    "UMap"
    [ ("Reward-Deposit Map", ppInt (UM.size (UM.RewDepUView umap)))
    , ("Ptrs Map", ppInt (UM.size (UM.PtrUView umap)))
    , ("SPoolUView Map", ppInt (UM.size (UM.SPoolUView umap)))
    , ("DRepUView Map", ppInt (UM.size (UM.DRepUView umap)))
    ]

pStateSummary :: PState c -> PDoc
pStateSummary (PState pp fpp retire deposit) =
  ppRecord
    "PState"
    [ ("Pool parameters", ppInt (Map.size pp))
    , ("Future pool parameters", ppInt (Map.size fpp))
    , ("Retiring stake pools", ppInt (Map.size retire))
    , ("Deposits", ppInt (Map.size deposit))
    ]

dpStateSummary :: CertState era -> PDoc
dpStateSummary (CertState v p d) = vsep [prettyA v, pStateSummary p, dStateSummary d]

-- =============================================

class PrettyC t era where
  prettyC :: Proof era -> t -> PDoc

pcTxId :: TxId c -> PDoc
pcTxId (TxId safehash) = trim (ppSafeHash safehash)

instance c ~ EraCrypto era => PrettyC (TxId c) era where prettyC _ = pcTxId

pcTxIn :: TxIn c -> PDoc
pcTxIn (TxIn (TxId h) (TxIx i)) = parens (hsep [ppString "TxIn", trim (ppSafeHash h), ppWord64 i])

instance c ~ EraCrypto era => PrettyC (TxIn c) era where prettyC _ = pcTxIn

pcNetwork :: Network -> PDoc
pcNetwork Testnet = ppString "TestNet"
pcNetwork Mainnet = ppString "Mainnet"

instance PrettyC Network era where prettyC _ = pcNetwork

pcKeyHash :: KeyHash discriminator c -> PDoc
pcKeyHash (KeyHash h) = trim (ppHash h)

instance c ~ EraCrypto era => PrettyC (KeyHash d c) era where prettyC _ = pcKeyHash

pcCredential :: Credential keyrole c -> PDoc
pcCredential (ScriptHashObj (ScriptHash h)) = hsep [ppString "(Script", trim (ppHash h) <> ppString ")"]
pcCredential (KeyHashObj (KeyHash h)) = hsep [ppString "(Key", trim (ppHash h) <> ppString ")"]

instance c ~ EraCrypto era => PrettyC (Credential keyrole c) era where prettyC _ = pcCredential

pcStakeReference :: StakeReference c -> PDoc
pcStakeReference StakeRefNull = ppString "Null"
pcStakeReference (StakeRefBase cred) = pcCredential cred
pcStakeReference (StakeRefPtr _) = ppString "Ptr"

instance c ~ EraCrypto era => PrettyC (StakeReference c) era where prettyC _ = pcStakeReference

pcAddr :: Addr c -> PDoc
pcAddr (Addr nw pay stk) =
  parens $
    hsep
      [ ppString "Addr"
      , pcNetwork nw
      , pcCredential pay
      , pcStakeReference stk
      ]
pcAddr (AddrBootstrap _) = ppString "Bootstrap"

instance c ~ EraCrypto era => PrettyC (Addr c) era where prettyC _ = pcAddr

pcCoreValue :: Proof era -> Value era -> PDoc
pcCoreValue (Conway _) v = vSummary v
pcCoreValue (Babbage _) v = vSummary v
pcCoreValue (Alonzo _) v = vSummary v
pcCoreValue (Mary _) v = vSummary v
pcCoreValue (Allegra _) (Coin n) = hsep [ppString "₳", ppInteger n]
pcCoreValue (Shelley _) (Coin n) = hsep [ppString "₳", ppInteger n]

pcCoin :: Coin -> PDoc
pcCoin (Coin n) = hsep [ppString "₳", ppInteger n]

instance PrettyC Coin era where prettyC _ = pcCoin

pcValue :: MaryValue c -> PDoc
pcValue (MaryValue n (MultiAsset m)) =
  ppSexp
    "Value"
    [ ppInteger n
    , -- , ppString ("num tokens = " ++ show (Map.size m))
      ppSet pcPolicyID (Map.keysSet m)
    ]

instance c ~ EraCrypto era => PrettyC (MaryValue c) era where
  prettyC _ = pcValue

pcVal :: Proof era -> Value era -> PDoc
pcVal (Shelley _) v = pcCoin v
pcVal (Allegra _) v = pcCoin v
pcVal (Mary _) v = pcValue v
pcVal (Alonzo _) v = pcValue v
pcVal (Babbage _) v = pcValue v
pcVal (Conway _) v = pcValue v

pcDatum :: Era era => Datum era -> PDoc
pcDatum NoDatum = ppString "NoDatum"
pcDatum (DatumHash h) = ppSexp "DHash" [trim (ppSafeHash h)]
pcDatum (Datum b) = pcData (binaryDataToData b)

instance Era era => PrettyC (Datum era) era where prettyC _ = pcDatum

pcData :: forall era. Era era => Data era -> PDoc
pcData d@(Data (PV1.Constr n _)) =
  ppSexp (pack ("Constr" ++ show n)) [ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (PV1.Map _)) =
  ppSexp "Map" [ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (PV1.List _)) =
  ppSexp "List" [ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (PV1.I n)) =
  ppSexp "I" [ppInteger n, ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (PV1.B bytes)) =
  ppSexp "B" [trim (viaShow bytes), ppString "Hash", trim $ ppSafeHash (hashData d)]

instance Era era => PrettyC (Data era) era where prettyC _ = pcData

pcTimelock :: forall era. Reflect era => Timelock era -> PDoc
pcTimelock (RequireSignature akh) = ppSexp "Sign" [pcKeyHash akh]
pcTimelock (RequireAllOf ts) = ppSexp "AllOf" [ppList pcTimelock (toList ts)]
pcTimelock (RequireAnyOf ts) = ppSexp "AnyOf" [ppList pcTimelock (toList ts)]
pcTimelock (RequireMOf m ts) = ppSexp "MOfN" (ppInteger (fromIntegral m) : [ppList pcTimelock (toList ts)])
pcTimelock (RequireTimeExpire mslot) = ppSexp "Expires" [ppSlotNo mslot]
pcTimelock (RequireTimeStart mslot) = ppSexp "Starts" [ppSlotNo mslot]

pcMultiSig :: Reflect era => PDoc -> SS.MultiSig era -> PDoc
pcMultiSig h (SS.RequireSignature hk) = ppSexp "ReqSig" [keyHashSummary hk, h]
pcMultiSig h (SS.RequireAllOf _) = ppSexp "AllOf" [h]
pcMultiSig h (SS.RequireAnyOf _) = ppSexp "AnyOf" [h]
pcMultiSig h (SS.RequireMOf m _) = ppSexp "MOf" [ppInt m, h]

pcScriptHash :: ScriptHash era -> PDoc
pcScriptHash (ScriptHash h) = trim (ppHash h)

pcHashScript :: forall era. Reflect era => Proof era -> Script era -> PDoc
pcHashScript (Conway _) s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Babbage _) s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Alonzo _) s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Mary _) s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Allegra _) s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Shelley _) s = ppString "Hash " <> pcScriptHash (hashScript @era s)

pcScript :: forall era. Reflect era => Proof era -> Script era -> PDoc
pcScript (Conway _) (TimelockScript t) = pcTimelock @era t
pcScript p@(Conway _) s@(PlutusScript v _) =
  parens (hsep [ppString ("PlutusScript " <> show v <> " "), pcHashScript p s])
pcScript (Babbage _) (TimelockScript t) = pcTimelock @era t
pcScript p@(Babbage _) s@(PlutusScript v _) =
  parens (hsep [ppString ("PlutusScript " <> show v <> " "), pcHashScript p s])
pcScript (Alonzo _) (TimelockScript t) = pcTimelock @era t
pcScript p@(Alonzo _) s@(PlutusScript v _) =
  parens (hsep [ppString ("PlutusScript " <> show v <> " "), pcHashScript p s])
pcScript (Mary _) s = pcTimelock @era s
pcScript (Allegra _) s = pcTimelock @era s
pcScript p@(Shelley _) s = pcMultiSig @era (pcHashScript @era p s) s

pcDataHash :: DataHash era -> PDoc
pcDataHash dh = trim (ppSafeHash dh)

pcTxOut :: Reflect era => Proof era -> TxOut era -> PDoc
pcTxOut p@(Conway _) (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [pcAddr addr, pcValue v, pcDatum d, ppStrictMaybe (pcScript p) s]
pcTxOut p@(Babbage _) (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [pcAddr addr, pcValue v, pcDatum d, ppStrictMaybe (pcScript p) s]
pcTxOut (Alonzo _) (AlonzoTxOut addr v md) =
  ppSexp "TxOut" [pcAddr addr, pcValue v, ppStrictMaybe pcDataHash md]
pcTxOut p@(Mary _) (ShelleyTxOut addr v) =
  ppSexp "TxOut" [pcAddr addr, pcCoreValue p v]
pcTxOut p@(Allegra _) (ShelleyTxOut addr v) =
  ppSexp "TxOut" [pcAddr addr, pcCoreValue p v]
pcTxOut p@(Shelley _) (ShelleyTxOut addr v) =
  ppSexp "TxOut" [pcAddr addr, pcCoreValue p v]

pcUTxO :: Reflect era => Proof era -> UTxO era -> PDoc
pcUTxO proof = ppMap pcTxIn (pcTxOut proof) . unUTxO

instance Reflect era => PrettyC (UTxO era) era where prettyC = pcUTxO

pcPoolParams :: PoolParams era -> PDoc
pcPoolParams x =
  ppRecord
    "PoolParams"
    [ ("Id", keyHashSummary (ppId x))
    , ("reward accnt", pcCredential (getRwdCred (ppRewardAcnt x)))
    ]

instance PrettyC (PoolParams era) era where prettyC _ = pcPoolParams

pcDelegCert :: ShelleyDelegCert c -> PDoc
pcDelegCert (ShelleyRegCert cred) = ppSexp "ShelleyRegCert" [pcCredential cred]
pcDelegCert (ShelleyUnRegCert cred) = ppSexp "ShelleyUnRegCert" [pcCredential cred]
pcDelegCert (ShelleyDelegCert x y) = ppSexp "ShelleyDelegCert" [pcCredential x, pcKeyHash y]

instance c ~ EraCrypto era => PrettyC (ShelleyDelegCert c) era where prettyC _ = pcDelegCert

pcPoolCert :: PoolCert c -> PDoc
pcPoolCert (RegPool poolp) = ppSexp "RegPool" [pcPoolParams poolp]
pcPoolCert (RetirePool keyhash epoch) = ppSexp "RetirePool" [pcKeyHash keyhash, ppEpochNo epoch]

instance c ~ EraCrypto era => PrettyC (PoolCert c) era where prettyC _ = pcPoolCert

pcShelleyTxCert :: ShelleyTxCert c -> PDoc
pcShelleyTxCert (ShelleyTxCertDelegCert x) = pcDelegCert x
pcShelleyTxCert (ShelleyTxCertPool x) = pcPoolCert x
pcShelleyTxCert (ShelleyTxCertGenesisDeleg _) = ppString "GenesisCert"
pcShelleyTxCert (ShelleyTxCertMir (MIRCert x (StakeAddressesMIR m))) =
  ppRecord
    "MIRStakeAdresses"
    [ ("pot", ppString (show x))
    , ("Addresses", ppMap pcCredential pcDeltaCoin m)
    ]
pcShelleyTxCert (ShelleyTxCertMir (MIRCert x (SendToOppositePotMIR c))) =
  ppRecord
    "MIROppositePot"
    [ ("pot", ppString (show x))
    , ("Amount", pcCoin c)
    ]

pcConwayTxCert :: ConwayTxCert c -> PDoc
pcConwayTxCert (ConwayTxCertDeleg dc) = prettyA dc
pcConwayTxCert (ConwayTxCertPool poolc) = pcPoolCert poolc
pcConwayTxCert (ConwayTxCertCommittee _) = ppString "ConwayTxCertCommittee" -- TODO: @aniketd add pretty instance for the certs

pcConwayDelegCert :: ConwayDelegCert c -> PDoc
pcConwayDelegCert (ConwayRegCert cred mcoin) =
  ppSexp "RegCert" [pcCredential cred, ppStrictMaybe pcCoin mcoin]
pcConwayDelegCert (ConwayUnRegCert cred mcoin) =
  ppSexp "UnRegCert" [pcCredential cred, ppStrictMaybe pcCoin mcoin]
pcConwayDelegCert (ConwayDelegCert cred d) =
  ppSexp "DelegCert" [pcCredential cred, pcDelegatee d]
pcConwayDelegCert (ConwayRegDelegCert cred d c) =
  ppSexp "RegDelegCert" [pcCredential cred, pcDelegatee d, pcCoin c]

pcDelegatee :: Delegatee c -> PDoc
pcDelegatee (DelegStake kh) = ppSexp "DelegStake" [pcKeyHash kh]
pcDelegatee (DelegVote cred) = ppSexp "DelegVote" [pcDRep cred]
pcDelegatee (DelegStakeVote kh cred) = ppSexp "DelegStakeVote" [pcKeyHash kh, pcDRep cred]

pcDRep :: DRep c -> PDoc
pcDRep (DRepCredential cred) = ppSexp "DRepCred" [pcCredential cred]
pcDRep DRepAlwaysAbstain = ppSexp "DRep" [ppString "Abstain"]
pcDRep DRepAlwaysNoConfidence = ppSexp "DRep" [ppString "NoConfidence"]

pcTxCert :: Proof era -> TxCert era -> PDoc
pcTxCert (Shelley _) x = pcShelleyTxCert x
pcTxCert (Allegra _) x = pcShelleyTxCert x
pcTxCert (Mary _) x = pcShelleyTxCert x
pcTxCert (Alonzo _) x = pcShelleyTxCert x
pcTxCert (Babbage _) x = pcShelleyTxCert x
pcTxCert (Conway _) x = pcConwayTxCert x

instance c ~ EraCrypto era => PrettyC (ShelleyTxCert c) era where prettyC _ = pcShelleyTxCert

pcRewardAcnt :: RewardAcnt c -> PDoc
pcRewardAcnt (RewardAcnt net cred) = ppSexp "RewAccnt" [pcNetwork net, pcCredential cred]

instance c ~ EraCrypto era => PrettyC (RewardAcnt c) era where prettyC _ = pcRewardAcnt

pcExUnits :: ExUnits -> PDoc
pcExUnits (ExUnits mem step) =
  ppSexp "ExUnits" [ppNatural mem, ppNatural step]

pcTxBodyField ::
  Reflect era =>
  Proof era ->
  TxBodyField era ->
  [(Text, PDoc)]
pcTxBodyField proof x = case x of
  Inputs s -> [("spend inputs", ppSet pcTxIn s)]
  Collateral s -> [("coll inputs", ppSet pcTxIn s)]
  RefInputs s -> [("ref inputs", ppSet pcTxIn s)]
  Outputs s -> [("outputs", ppList (pcTxOut proof) (toList s))]
  CollateralReturn SNothing -> []
  CollateralReturn (SJust txout) -> [("coll return", pcTxOut proof txout)]
  TotalCol SNothing -> []
  TotalCol (SJust c) -> [("total coll", pcCoin c)]
  Certs xs -> [("certs", ppList (pcTxCert proof) (toList xs))]
  Withdrawals' (Withdrawals m) -> [("withdrawal", ppMap pcRewardAcnt pcCoin m)]
  Txfee c -> [("fee", pcCoin c)]
  Vldt v -> [("validity interval", ppValidityInterval v)]
  TTL slot -> [("time to live", ppSlotNo slot)]
  Update SNothing -> []
  Update (SJust _) -> [("update", ppString "UPDATE")]
  ReqSignerHashes s -> [("required hashes", ppSet pcKeyHash s)]
  -- Mint v -> [("minted", multiAssetSummary v)]
  Mint (MultiAsset m) -> [("minted", ppSet pcPolicyID (Map.keysSet m))]
  WppHash SNothing -> []
  WppHash (SJust h) -> [("integrity hash", trim (ppSafeHash h))]
  AdHash SNothing -> []
  AdHash (SJust (AuxiliaryDataHash h)) -> [("aux data hash", trim (ppSafeHash h))]
  Txnetworkid SNothing -> [("network id", ppString "Nothing")]
  Txnetworkid (SJust nid) -> [("network id", pcNetwork nid)]
  GovernanceProcs ga -> [("governance procedures", prettyA ga)]

pcTxField ::
  forall era.
  Reflect era =>
  Proof era ->
  TxField era ->
  [(Text, PDoc)]
pcTxField proof x = case x of
  Body b -> [("txbody hash", ppSafeHash (hashAnnotated b)), ("body", pcTxBody proof b)]
  BodyI xs -> [("body", ppRecord "TxBody" (concat (map (pcTxBodyField proof) xs)))]
  TxWits w -> [("witnesses", pcWitnesses proof w)]
  WitnessesI ws -> [("witnesses", ppRecord "Witnesses" (concat (map (pcWitnessesField proof) ws)))]
  AuxData SNothing -> []
  AuxData (SJust _auxdata) -> [("aux data", ppString "AUXDATA")] -- ppAuxiliaryData auxdata)]
  Valid (IsValid v) -> [("is valid", ppString (show v))]

pcWitnessesField :: forall era. Reflect era => Proof era -> WitnessesField era -> [(Text, PDoc)]
pcWitnessesField proof x = case x of
  AddrWits set -> [("key wits", ppSet (pcWitVKey proof) set)]
  BootWits bwits -> [("boot wits", ppSet (\z -> ppVKey (bwKey z)) bwits)]
  ScriptWits mp -> [("script wits", ppMap pcScriptHash (pcScript proof) mp)]
  DataWits (TxDats m) -> [("data wits", ppMap pcDataHash pcData m)]
  RdmrWits (Redeemers m) ->
    [("redeemer wits", ppMap ppRdmrPtr (pcPair pcData pcExUnits) m)]

pcPair :: (t1 -> PDoc) -> (t2 -> PDoc) -> (t1, t2) -> PDoc
pcPair pp1 pp2 (x, y) = parens (hsep [pp1 x, ppString ",", pp2 y])

pcWitVKey :: (Reflect era, Typeable keyrole) => Proof era -> WitVKey keyrole (EraCrypto era) -> PDoc
pcWitVKey _p (WitVKey vk@(VKey x) sig) =
  ppSexp
    "WitVKey"
    [ ppString (" VerKey=" ++ (take 10 (drop 19 keystring)))
    , ppString (" SignKey=" ++ (take 10 (drop 29 sigstring)))
    , " VerKeyHash=" <> hash
    ]
  where
    keystring = show x
    hash = pcKeyHash (hashKey vk)
    sigstring = show sig

pcWitnesses :: Reflect era => Proof era -> TxWits era -> PDoc
pcWitnesses proof wits = ppRecord "Witnesses" pairs
  where
    fields = abstractWitnesses proof wits
    pairs = concat (map (pcWitnessesField proof) fields)

pcTx :: Reflect era => Proof era -> Tx era -> PDoc
pcTx proof tx = ppRecord "Tx" pairs
  where
    fields = abstractTx proof tx
    pairs = concatMap (pcTxField proof) fields

pcTxBody :: Reflect era => Proof era -> TxBody era -> PDoc
pcTxBody proof txbody = ppRecord ("TxBody " <> pack (show proof)) pairs
  where
    fields = abstractTxBody proof txbody
    pairs = concatMap (pcTxBodyField proof) fields

instance PrettyC (ConwayTallyState era) era where
  prettyC proof (ConwayTallyState x) = case proof of
    Shelley _ -> ppMap prettyA prettyA x
    Mary _ -> ppMap prettyA prettyA x
    Allegra _ -> ppMap prettyA prettyA x
    Alonzo _ -> ppMap prettyA prettyA x
    Babbage _ -> ppMap prettyA prettyA x
    Conway _ -> ppMap prettyA prettyA x

pc :: PrettyC t era => Proof era -> t -> IO ()
pc proof x = putStrLn (show (prettyC proof x))

-- ===================================================

pcReward :: Reward c -> PDoc
pcReward (Reward ty pl c) =
  ppRecord
    "Reward"
    [ ("type", ppRewardType ty)
    , ("pool", pcKeyHash pl)
    , ("amount", pcCoin c)
    ]

pcFutureGenDeleg :: FutureGenDeleg c -> PDoc
pcFutureGenDeleg (FutureGenDeleg (SlotNo x) y) =
  ppRecord
    "FutGenDeleg"
    [ ("slot", ppWord64 x)
    , ("keyHash", pcKeyHash y)
    ]

instance (PrettyC x era, PrettyC y era) => PrettyC (x, y) era where
  prettyC p (x, y) = vsep [prettyC p x, prettyC p y]

instance
  c ~ EraCrypto era =>
  PrettyC (Map (FutureGenDeleg c) (GenDelegPair c)) era
  where
  prettyC _ x = ppMap pcFutureGenDeleg pcGenDelegPair x

instance
  c ~ EraCrypto era =>
  PrettyC (Map (KeyHash 'Genesis c) (GenDelegPair c)) era
  where
  prettyC _ x = ppMap pcKeyHash pcGenDelegPair x

instance PrettyC (PState era) era where
  prettyC _ x = pcPState x

instance PrettyC (VState era) era where
  prettyC _ st = pcVState st

instance PrettyC (CertState era) era where
  prettyC proof (CertState vst pst dst) =
    ppRecord
      "CertState"
      [ ("pstate", pcPState pst)
      , ("vstate", prettyC proof vst)
      , ("dstate", pcDState dst)
      ]

pcVState :: VState era -> PDoc
pcVState (VState dreps hotkeys) =
  ppRecord
    "VState"
    [ ("dReps", ppSet pcCredential dreps)
    , ("hotKeys", ppMap pcKeyHash (ppMaybe pcKeyHash) hotkeys)
    ]

instance Reflect era => PrettyC (LedgerState era) era where prettyC = pcLedgerState

instance Reflect era => PrettyC (EpochState era) era where prettyC = pcEpochState

pcSnapShotL :: Text -> SnapShot c -> [(Text, PDoc)]
pcSnapShotL prefix ss =
  [ (prefix <> "Stake", ppMap pcCredential (pcCoin . fromCompact) (VMap.toMap (unStake (ssStake ss))))
  , (prefix <> "Delegs", ppMap pcCredential pcKeyHash (VMap.toMap (ssDelegations ss)))
  , (prefix <> "Pools", ppMap pcKeyHash pcPoolParams (VMap.toMap (ssPoolParams ss)))
  ]

pcIndividualPoolStake :: IndividualPoolStake c -> PDoc
pcIndividualPoolStake x =
  ppRecord
    "IPS"
    [ ("ratio", ppRational (individualPoolStake x))
    , ("vrf", trim (ppHash (individualPoolStakeVrf x)))
    ]

instance c ~ EraCrypto era => PrettyC (IndividualPoolStake c) era where prettyC _ = pcIndividualPoolStake

pcSnapShots :: SnapShots c -> PDoc
pcSnapShots sss =
  ppRecord' "" $
    pcSnapShotL "mark" (ssStakeMark sss)
      ++ [("markPoolDistr", pcPoolDistr (ssStakeMarkPoolDistr sss))]
      ++ pcSnapShotL "set" (ssStakeSet sss)
      ++ pcSnapShotL "go" (ssStakeGo sss)
      ++ [("fee", pcCoin (ssFee sss))]

pcPoolDistr :: PoolDistr c -> PDoc
pcPoolDistr (PoolDistr pdistr) =
  ppMap pcKeyHash pcIndividualPoolStake pdistr
    <> ppString " total = "
    <> ppRational (Map.foldl' (+) 0 (fmap individualPoolStake pdistr))

withEraPParams :: forall era a. Proof era -> (Core.EraPParams era => a) -> a
withEraPParams (Shelley _) x = x
withEraPParams (Mary _) x = x
withEraPParams (Allegra _) x = x
withEraPParams (Alonzo _) x = x
withEraPParams (Babbage _) x = x
withEraPParams (Conway _) x = x

-- | Print just a few of the PParams fields
pcPParamsSynopsis :: forall era. Proof era -> Core.PParams era -> PDoc
pcPParamsSynopsis p x = withEraPParams p help
  where
    help :: Core.EraPParams era => PDoc
    help =
      ppRecord
        "PParams"
        [ ("maxBBSize", ppNatural (x ^. Core.ppMaxBBSizeL))
        , ("maxBHSize", ppNatural (x ^. Core.ppMaxBHSizeL))
        , ("maxTxSize", ppNatural (x ^. Core.ppMaxTxSizeL))
        , ("poolDeposit", pcCoin (x ^. Core.ppPoolDepositL))
        , ("keyDeposit", pcCoin (x ^. Core.ppKeyDepositL))
        , ("protVer", ppString (showProtver (x ^. Core.ppProtocolVersionL)))
        ]

showProtver :: ProtVer -> String
showProtver (ProtVer x y) = "(" ++ show x ++ " " ++ show y ++ ")"

pcEpochState :: Reflect era => Proof era -> EpochState era -> PDoc
pcEpochState proof es@(EpochState (AccountState tre res) sss ls ppp pp _) =
  ppRecord
    "EpochState"
    [ ("AccountState", ppRecord' "" [("treasury", pcCoin tre), ("reserves", pcCoin res)])
    , ("SnapShots", pcSnapShots sss)
    , ("LedgerState", pcLedgerState proof ls)
    , ("prevparams", pcPParamsSynopsis proof ppp)
    , ("params", pcPParamsSynopsis proof pp)
    , ("AdaPots", pcAdaPot es)
    ]

pcNewEpochState :: Reflect era => Proof era -> NewEpochState era -> PDoc
pcNewEpochState proof (NewEpochState en (BlocksMade pbm) (BlocksMade cbm) es _ (PoolDistr pd) _) =
  ppRecord
    "NewEpochState"
    [ ("EpochNo", ppEpochNo en)
    , ("EpochState", pcEpochState proof es)
    , ("PoolDistr", ppMap pcKeyHash pcIndividualPoolStake pd)
    , ("Prev Blocks", ppMap pcKeyHash ppNatural pbm)
    , ("Current Blocks", ppMap pcKeyHash ppNatural cbm)
    ]

instance Reflect era => PrettyC (NewEpochState era) era where prettyC = pcNewEpochState

pcUTxOState :: Reflect era => Proof era -> UTxOState era -> PDoc
pcUTxOState proof (UTxOState u dep fs _pups _stakedistro) =
  ppRecord
    "UTxOState"
    [ ("utxo", pcUTxO proof u)
    , ("deposited", pcCoin dep)
    , ("fees", pcCoin fs)
    , ("ppups", ppString "PPUP")
    , ("stake distr", ppString "Stake Distr") -- This is not part of the model
    ]

instance Reflect era => PrettyC (UTxOState era) era where prettyC = pcUTxOState

pcLedgerState :: Reflect era => Proof era -> LedgerState era -> PDoc
pcLedgerState proof ls =
  ppRecord
    "LedgerState"
    [ ("utxoState", pcUTxOState proof (lsUTxOState ls))
    , ("dpState", prettyC proof (lsCertState ls))
    ]

pcPState :: PState era -> PDoc
pcPState (PState regP fregP ret dep) =
  ppRecord
    "PState"
    [ ("regPools", ppMap pcKeyHash pcPoolParams regP)
    , ("futureRegPools", ppMap pcKeyHash pcPoolParams fregP)
    , ("retiring", ppMap pcKeyHash ppEpochNo ret)
    , ("poolDeposits", ppMap pcKeyHash pcCoin dep)
    ]

pcDState :: DState c -> PDoc
pcDState ds =
  ppRecord
    "DState"
    [ ("rewards", ppMap pcCredential pcCoin (rewardMap (dsUnified ds)))
    , ("deposits", ppMap pcCredential pcCoin (depositMap (dsUnified ds)))
    , ("delegate", ppMap pcCredential pcKeyHash (sPoolMap (dsUnified ds)))
    , ("ptrs", ppMap ppPtr ppCredential (ptrMap (dsUnified ds)))
    , ("fGenDel", ppMap pcFutureGenDeleg pcGenDelegPair (dsFutureGenDelegs ds))
    , ("GenDel", ppMap pcKeyHash pcGenDelegPair (unGenDelegs (dsGenDelegs ds)))
    , ("iRewards", pcIRewards (dsIRewards ds))
    ]

pcGenDelegPair :: GenDelegPair c -> PDoc
pcGenDelegPair x =
  ppRecord
    "GDPair"
    [ ("keyhash", pcKeyHash (genDelegKeyHash x))
    , ("vrfhash", trim (ppHash (genDelegVrfHash x)))
    ]

pcIRewards :: InstantaneousRewards c -> PDoc
pcIRewards xs =
  ppRecord
    "IReward"
    [ ("reserves", ppMap pcCredential pcCoin (DP.iRReserves xs))
    , ("treasury", ppMap pcCredential pcCoin (DP.iRTreasury xs))
    , ("deltaR", pcDeltaCoin (DP.deltaReserves xs))
    , ("deltaT", pcDeltaCoin (DP.deltaTreasury xs))
    ]

pcDeltaCoin :: DeltaCoin -> PDoc
pcDeltaCoin (DeltaCoin n) = hsep [ppString "▵₳", ppInteger n]

pcSlotNo :: SlotNo -> PDoc
pcSlotNo (SlotNo n) = ppWord64 n

pcAdaPot :: EraTxOut era => EpochState era -> PDoc
pcAdaPot es =
  let x = totalAdaPotsES es
   in ppRecord
        "AdaPot"
        [ ("treasury", pcCoin (treasuryAdaPot x))
        , ("rewards", pcCoin (rewardsAdaPot x))
        , ("utxo", pcCoin (utxoAdaPot x))
        , ("keydeposit", pcCoin (keyDepositAdaPot x))
        , ("pooldeposit", pcCoin (poolDepositAdaPot x))
        , ("fees", pcCoin (feesAdaPot x))
        , ("totalAda", pcCoin (totalAdaES es))
        ]

-- ========================

pcPolicyID :: PolicyID c -> PDoc
pcPolicyID (PolicyID sh) = pcScriptHash sh

pcAssetName :: AssetName -> PDoc
pcAssetName x = trim (viaShow x)

pcMultiAsset :: MultiAsset c -> PDoc
pcMultiAsset m = ppList pptriple (flattenMultiAsset m)
  where
    pptriple (i, asset, num) = hsep ["(", pcPolicyID i, pcAssetName asset, ppInteger num, ")"]

pcScriptPurpose :: Proof era -> ScriptPurpose era -> PDoc
pcScriptPurpose _ (Minting policy) = ppSexp "Minting" [pcPolicyID policy]
pcScriptPurpose _ (Spending txin) = ppSexp "Spending" [pcTxIn txin]
pcScriptPurpose _ (Rewarding acct) = ppSexp "Rewarding" [pcRewardAcnt acct]
pcScriptPurpose p (Certifying dcert) = ppSexp "Certifying" [pcTxCert p dcert]

instance PrettyC (ScriptPurpose era) era where
  prettyC = pcScriptPurpose

pcScriptsNeeded :: Proof era -> ScriptsNeeded era -> PDoc
pcScriptsNeeded (Shelley _) (ShelleyScriptsNeeded ss) = ppSexp "ScriptsNeeded" [ppSet pcScriptHash ss]
pcScriptsNeeded (Allegra _) (ShelleyScriptsNeeded ss) = ppSexp "ScriptsNeeded" [ppSet pcScriptHash ss]
pcScriptsNeeded (Mary _) (ShelleyScriptsNeeded ss) = ppSexp "ScriptsNeeded" [ppSet pcScriptHash ss]
pcScriptsNeeded p@(Alonzo _) (AlonzoScriptsNeeded pl) =
  ppSexp "ScriptsNeeded" [ppList (ppPair (pcScriptPurpose p) pcScriptHash) pl]
pcScriptsNeeded p@(Babbage _) (AlonzoScriptsNeeded pl) =
  ppSexp "ScriptsNeeded" [ppList (ppPair (pcScriptPurpose p) pcScriptHash) pl]
pcScriptsNeeded p@(Conway _) (AlonzoScriptsNeeded pl) =
  ppSexp "ScriptsNeeded" [ppList (ppPair (pcScriptPurpose p) pcScriptHash) pl]
