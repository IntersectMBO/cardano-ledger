{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..), Datum (..), binaryDataToData, hashData)
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptPurpose (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..), unTxDats)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (BlocksMade (..), Network (..), TxIx (..), txIxToInt)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Delegation.Certificates (ConwayDCert (..))
import Cardano.Ledger.Conway.Governance (ConwayTallyState (..))
import Cardano.Ledger.Conway.Rules (ConwayNewEpochPredFailure)
import qualified Cardano.Ledger.Conway.Rules as Conway

import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (
  GenDelegs (..),
  HasKeyRole (coerceKeyRole),
  KeyHash (..),
  KeyRole (..),
  VKey (..),
  hashKey,
 )
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Pretty
import Cardano.Ledger.Pretty.Alonzo
import qualified Cardano.Ledger.Pretty.Babbage as Babbage
import Cardano.Ledger.Pretty.Mary
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  DPState (..),
  DState (..),
  EpochState (..),
  InstantaneousRewards (..),
  LedgerState (..),
  NewEpochState (..),
  PPUPPredFailure,
  PState (..),
  UTxOState (..),
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
  ShelleyPpupPredFailure (..),
  ShelleyTickPredFailure (..),
  ShelleyUpecPredFailure (..),
  ShelleyUtxoPredFailure (..),
  ShelleyUtxowPredFailure (..),
 )
import qualified Cardano.Ledger.Shelley.Scripts as SS (MultiSig (..))
import Cardano.Ledger.Shelley.TxBody (
  DCert (..),
  DelegCert (..),
  Delegation (..),
  PoolCert (..),
  PoolParams (..),
  ShelleyTxOut (..),
  WitVKey (..),
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Cardano.Ledger.UMapCompact as UM (UMap, View (..), delView, rewView, size)
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition.Extended (STS (..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import Data.Void (absurd)
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
import Test.Cardano.Ledger.Generic.Proof

-- =====================================================

class Era era => PrettyCore era where
  prettyTx :: Tx era -> PDoc
  prettyTx = prettyA
  prettyScript :: Script era -> PDoc
  prettyScript = prettyA
  prettyTxBody :: TxBody era -> PDoc
  prettyTxBody = prettyA
  prettyWitnesses :: TxWits era -> PDoc
  prettyWitnesses = prettyA
  prettyValue :: Value era -> PDoc
  prettyValue = prettyA
  prettyTxOut :: TxOut era -> PDoc
  prettyTxOut = prettyA

instance CC.Crypto c => PrettyCore (ShelleyEra c)

instance CC.Crypto c => PrettyCore (AllegraEra c)

instance CC.Crypto c => PrettyCore (MaryEra c)

instance CC.Crypto c => PrettyCore (AlonzoEra c)

instance CC.Crypto c => PrettyCore (BabbageEra c)

instance CC.Crypto c => PrettyCore (ConwayEra c)

prettyUTxO :: Proof era -> UTxO era -> PDoc
prettyUTxO proof = prettyUTxOMap proof . unUTxO

prettyUTxOMap :: Proof era -> Map.Map (TxIn (EraCrypto era)) (TxOut era) -> PDoc
prettyUTxOMap (Conway _) = prettyA
prettyUTxOMap (Babbage _) = prettyA
prettyUTxOMap (Alonzo _) = prettyA
prettyUTxOMap (Mary _) = prettyA
prettyUTxOMap (Allegra _) = prettyA
prettyUTxOMap (Shelley _) = prettyA

-- ===================================================================
-- PrettyA instances for UTXOW, UTXO, UTXOS, PPUP predicate failures
-- There are sometimes two versions:
-- one introduced in Shelley, one introduced in Alonzo
-- ===================================================================

-- Predicate Failure for LEDGER

instance
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "DELEGS" era))
  ) =>
  PrettyA (ShelleyLedgerPredFailure era)
  where
  prettyA (UtxowFailure x) = prettyA x
  prettyA (DelegsFailure x) = viaShow x

-- =========================================
-- Predicate Failure for Alonzo UTXOW

ppUtxowPredicateFail ::
  ( PrettyA (PredicateFailure (EraRule "UTXO" era))
  , PrettyCore era
  ) =>
  AlonzoUtxowPredFailure era ->
  PDoc
ppUtxowPredicateFail (ShelleyInAlonzoUtxowPredFailure x) = prettyA x
ppUtxowPredicateFail (MissingRedeemers xs) =
  ppSexp "MissingRedeemers" [prettyA xs]
ppUtxowPredicateFail (MissingRequiredDatums s1 s2) =
  ppRecord
    "MissingRequiredDatums"
    [ ("missing data hashes", prettyA s1)
    , ("received data hashes", prettyA s2)
    ]
ppUtxowPredicateFail (NonOutputSupplimentaryDatums s1 s2) =
  ppRecord
    "NonOutputSupplimentaryDatums"
    [ ("unallowed data hashes", prettyA s1)
    , ("acceptable data hashes", prettyA s2)
    ]
ppUtxowPredicateFail (PPViewHashesDontMatch h1 h2) =
  ppRecord
    "PPViewHashesDontMatch"
    [ ("PPHash in the TxBody", prettyA h1)
    , ("PPHash Computed from the current Protocol Parameters", prettyA h2)
    ]
ppUtxowPredicateFail (MissingRequiredSigners x) =
  ppSexp "MissingRequiredSigners" [prettyA x]
ppUtxowPredicateFail (UnspendableUTxONoDatumHash x) =
  ppSexp "UnspendableUTxONoDatumHash" [prettyA x]
ppUtxowPredicateFail (ExtraRedeemers x) =
  ppSexp "ExtraRedeemers" [prettyA x]

instance
  ( PrettyA (PredicateFailure (EraRule "UTXO" era))
  , PrettyCore era
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
  ppSexp "InvalidWitnessesUTXOW" [prettyA vkeyws]
ppUtxowPredicateFailure (MissingVKeyWitnessesUTXOW whs) =
  ppSexp "MissingVKeyWitnessesUTXOW" [prettyA whs]
ppUtxowPredicateFailure (MissingScriptWitnessesUTXOW m) =
  ppSexp "MissingScriptWitnessesUTXOW" [prettyA m]
ppUtxowPredicateFailure (ScriptWitnessNotValidatingUTXOW m) =
  ppSexp "ScriptWitnessNotValidatingUTXOW" [prettyA m]
ppUtxowPredicateFailure (UtxoFailure m) = ppSexp "UtxoFailure" [prettyA m]
ppUtxowPredicateFailure (MIRInsufficientGenesisSigsUTXOW m) =
  ppSexp "MIRInsufficientGenesisSigsUTXOW" [prettyA m]
ppUtxowPredicateFailure (MissingTxBodyMetadataHash m) =
  ppSexp " MissingTxMetadata" [prettyA m]
ppUtxowPredicateFailure (MissingTxMetadata m) =
  ppSexp " MissingTxMetadata" [prettyA m]
ppUtxowPredicateFailure (ConflictingMetadataHash h1 h2) =
  ppRecord
    "ConflictingMetadataHash"
    [("Hash in the body", prettyA h1), ("Hash of full metadata", prettyA h2)]
ppUtxowPredicateFailure InvalidMetadata =
  ppSexp "InvalidMetadata" []
ppUtxowPredicateFailure (ExtraneousScriptWitnessesUTXOW m) =
  ppSexp "ExtraneousScriptWitnessesUTXOW" [prettyA m]

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
  ppSexp "BadInputsUTxO" [prettyA x]
ppUtxoPredicateFailure (Alonzo.OutsideValidityIntervalUTxO vi slot) =
  ppRecord "OutsideValidityIntervalUTxO" [("validity interval", prettyA vi), ("slot", prettyA slot)]
ppUtxoPredicateFailure (Alonzo.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", prettyA actual)
    , ("max transaction size", prettyA maxs)
    ]
ppUtxoPredicateFailure Alonzo.InputSetEmptyUTxO =
  ppSexp "InputSetEmptyUTxO" []
ppUtxoPredicateFailure (Alonzo.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for thistransaction", prettyA computed)
    , ("fee supplied by transaction", prettyA supplied)
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
    [ ("expected network id", prettyA n)
    , ("set addresses with wrong network id", prettyA add)
    ]
ppUtxoPredicateFailure (Alonzo.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", prettyA n)
    , ("set reward address with wrong network id", prettyA accnt)
    ]
ppUtxoPredicateFailure (Alonzo.OutputTooSmallUTxO xs) =
  ppSexp "OutputTooSmallUTxO" [prettyA xs]
ppUtxoPredicateFailure (UtxosFailure subpred) = prettyA subpred
-- ppSexp "UtxosFailure" [prettyA subpred]
ppUtxoPredicateFailure (Alonzo.OutputBootAddrAttrsTooBig x) =
  ppSexp "OutputBootAddrAttrsTooBig" [prettyA x]
ppUtxoPredicateFailure Alonzo.TriesToForgeADA =
  ppSexp "TriesToForgeADA" []
ppUtxoPredicateFailure (Alonzo.OutputTooBigUTxO xs) =
  ppSexp
    "OutputTooBigUTxO"
    [ prettyA $
        ( \(a, b, c) ->
            ppRecord
              ""
              [("actual size", prettyA a), ("PParam max value", prettyA b), ("TxOut", prettyA c)]
        )
          <$> xs
    ]
ppUtxoPredicateFailure (InsufficientCollateral x y) =
  ppRecord
    "InsufficientCollateral"
    [ ("balance computed", prettyA x)
    , ("the required collateral for the given fee", prettyA y)
    ]
ppUtxoPredicateFailure (ScriptsNotPaidUTxO x) =
  ppSexp "ScriptsNotPaidUTxO" [prettyA x]
ppUtxoPredicateFailure (ExUnitsTooBigUTxO x y) =
  ppRecord
    "ExUnitsTooBigUTxO"
    [ ("Max EXUnits from the protocol parameters", prettyA x)
    , ("EXUnits supplied", prettyA y)
    ]
ppUtxoPredicateFailure (CollateralContainsNonADA x) =
  ppSexp "CollateralContainsNonADA" [prettyValue @era x]
ppUtxoPredicateFailure (WrongNetworkInTxBody x y) =
  ppRecord
    "WrongNetworkInTxBody"
    [ ("Actual Network ID", prettyA x)
    , ("Network ID in transaction body", prettyA y)
    ]
ppUtxoPredicateFailure (OutsideForecast x) =
  ppRecord "OutsideForecast" [("slot number outside consensus forecast range", prettyA x)]
ppUtxoPredicateFailure (TooManyCollateralInputs x y) =
  ppRecord
    "TooManyCollateralInputs"
    [ ("Max allowed collateral inputs", prettyA x)
    , ("Number of collateral inputs", prettyA y)
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
  prettyA (WrongCertificateTypePOOL disallowedCertificate) =
    ppRecord
      "WrongCertificateTypePOOL"
      [("Disallowed Certificate", prettyA disallowedCertificate)]
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
  prettyA = viaShow

-- =========================================
-- Predicate Failure for Alonzo UTXOS

ppUtxosPredicateFailure ::
  PrettyA (PPUPPredFailure era) =>
  AlonzoUtxosPredFailure era ->
  PDoc
ppUtxosPredicateFailure (ValidationTagMismatch isvalid tag) =
  ppRecord
    "ValidationTagMismatch"
    [ ("isValid tag", prettyA isvalid)
    , ("mismatch description", prettyA tag)
    ]
ppUtxosPredicateFailure (CollectErrors es) =
  "When collecting inputs for twophase scripts, these went wrong: " <> prettyA es
ppUtxosPredicateFailure (Alonzo.UpdateFailure p) = prettyA p

instance PrettyA (PPUPPredFailure era) => PrettyA (AlonzoUtxosPredFailure era) where
  prettyA = ppUtxosPredicateFailure

instance PrettyA (CollectError c)

instance PrettyA TagMismatchDescription

ppFailureDescription :: FailureDescription -> PDoc
ppFailureDescription (PlutusFailure txt bytes) =
  ppRecord "PlutusFailure" [("reason", text txt), ("script", prettyA bytes)]

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
  ppSexp "BadInputsUTxO" [prettyA x]
ppUtxoPFShelley (Shelley.ExpiredUTxO ttl slot) =
  ppRecord "ExpiredUTxO" [("transaction time to live", prettyA ttl), ("current slot", prettyA slot)]
ppUtxoPFShelley (Shelley.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", prettyA actual)
    , ("max transaction size", prettyA maxs)
    ]
ppUtxoPFShelley (Shelley.InputSetEmptyUTxO) =
  ppSexp "InputSetEmptyUTxO" []
ppUtxoPFShelley (Shelley.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for this transaction", prettyA computed)
    , ("fee supplied by this transaction", prettyA supplied)
    ]
ppUtxoPFShelley (Shelley.ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", prettyA consumed)
    , ("coin produced", prettyA produced)
    ]
ppUtxoPFShelley (Shelley.WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", prettyA n)
    , ("set of addresses with wrong network id", prettyA add)
    ]
ppUtxoPFShelley (Shelley.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", prettyA n)
    , ("set of reward address with wrong network id", prettyA accnt)
    ]
ppUtxoPFShelley (Shelley.OutputTooSmallUTxO xs) =
  ppRecord
    "OutputTooSmallUTxO"
    [("list of supplied transaction outputs that are too small", prettyA xs)]
ppUtxoPFShelley (Shelley.UpdateFailure x) =
  ppSexp "UpdateFailure" [prettyA x]
ppUtxoPFShelley (Shelley.OutputBootAddrAttrsTooBig xs) =
  ppRecord "OutputBootAddrAttrsTooBig" [("list of supplied bad transaction outputs", prettyA xs)]

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
    [ ("KeyHashes which are voting", prettyA x)
    , ("KeyHashes which should be voting", prettyA y)
    ]
ppPpupPredicateFailure (PPUpdateWrongEpoch x y z) =
  ppRecord
    "PPUpdateWrongEpoch"
    [ ("current epoch", prettyA x)
    , ("intended epoch of update", prettyA y)
    , ("voting period within the epoch", prettyA (show z))
    ]
ppPpupPredicateFailure (PVCannotFollowPPUP x) =
  ppRecord "PVCannotFollowPPUP" [("the first bad protocol version", prettyA x)]

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
  ppSexp "BadInputsUTxO" [prettyA txins]
ppUtxoPFMary (Allegra.OutsideValidityIntervalUTxO vi slot) =
  ppRecord
    "OutsideValidityIntervalUTxO"
    [ ("provided interval", prettyA vi)
    , ("current slot", prettyA slot)
    ]
ppUtxoPFMary (Allegra.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", prettyA actual)
    , ("max transaction size", prettyA maxs)
    ]
ppUtxoPFMary (Allegra.InputSetEmptyUTxO) = ppSexp "InputSetEmptyUTxO" []
ppUtxoPFMary (Allegra.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for this transaction", prettyA computed)
    , ("fee supplied by this transaction", prettyA supplied)
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
    [ ("expected network id", prettyA n)
    , ("set of addresses with wrong network id", prettyA add)
    ]
ppUtxoPFMary (Allegra.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", prettyA n)
    , ("set reward address with wrong network id", prettyA accnt)
    ]
ppUtxoPFMary (Allegra.OutputTooSmallUTxO xs) =
  ppRecord
    "OutputTooSmallUTxO"
    [("list of supplied transaction outputs that are too small", prettyA xs)]
ppUtxoPFMary (Allegra.UpdateFailure x) =
  ppSexp "UpdateFailure" [prettyA x]
ppUtxoPFMary (Allegra.OutputBootAddrAttrsTooBig xs) =
  ppRecord "OutputBootAddrAttrsTooBig" [("list of supplied bad transaction outputs", prettyA xs)]
ppUtxoPFMary (Allegra.TriesToForgeADA) = ppSexp "TriesToForgeADA" []
ppUtxoPFMary (Allegra.OutputTooBigUTxO outs) =
  ppRecord "OutputTooBigUTxO" [("list of TxOuts which are too big", prettyA outs)]

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
ppWitHashes hs = ppSexp "WitHashes" [prettyA hs]

-- Defined in ‘Cardano.Ledger.Alonzo.Tx’
ppScriptPurpose :: ScriptPurpose c -> PDoc
ppScriptPurpose (Minting policy) = ppSexp "Minting" [prettyA policy] -- FIXME fill in the blanks
ppScriptPurpose (Spending txin) = ppSexp "Spending" [prettyA txin]
ppScriptPurpose (Rewarding acct) = ppSexp "Rewarding" [prettyA acct]
ppScriptPurpose (Certifying dcert) = ppSexp "Certifying" [prettyA dcert]

instance PrettyA (ScriptPurpose c) where
  prettyA = ppScriptPurpose

-- =====================================================

ppMyWay :: (Typeable keyrole, CC.Crypto c) => WitVKey keyrole c -> PDoc
ppMyWay (wvk@(WitVKey vkey _)) = ppSexp "MyWay" [prettyA (hashKey vkey), prettyA wvk]

ppCoreWitnesses :: Proof era -> TxWits era -> PDoc
ppCoreWitnesses (Conway _) x = prettyA x
ppCoreWitnesses (Babbage _) x = prettyA x
ppCoreWitnesses (Alonzo _) x = prettyA x
ppCoreWitnesses (Mary _) x = prettyA x
ppCoreWitnesses (Allegra _) x = prettyA x
ppCoreWitnesses (Shelley _) x = prettyA x

ppCoreScript :: Proof era -> Script era -> PDoc
ppCoreScript (Conway _) (PlutusScript _ x) = prettyA (show x)
ppCoreScript (Conway _) (TimelockScript x) = prettyA x
ppCoreScript (Babbage _) (PlutusScript _ x) = prettyA (show x)
ppCoreScript (Babbage _) (TimelockScript x) = prettyA x
ppCoreScript (Alonzo _) (PlutusScript _ x) = prettyA (show x)
ppCoreScript (Alonzo _) (TimelockScript x) = prettyA x
ppCoreScript (Mary _) x = prettyA x
ppCoreScript (Allegra _) x = prettyA x
ppCoreScript (Shelley _) x = prettyA x

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
    [ ("actual computed BBody size", prettyA x)
    , ("claimed BBody Size in Header", prettyA y)
    ]
ppBbodyPredicateFailure (InvalidBodyHashBBODY h1 h2) =
  ppRecord
    "(InvalidBodyHashBBODY"
    [ ("actual hash", prettyA h1)
    , ("claimed hash", prettyA h2)
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
    [ ("Computed Sum of ExUnits for all plutus scripts", prettyA e1)
    , ("Maximum allowed by protocal parameters", prettyA e2)
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
ppTickPredicateFailure (NewEpochFailure x) = prettyA x
ppTickPredicateFailure (RupdFailure x) = prettyA x

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
  , Reflect era
  ) =>
  ShelleyNewEpochPredFailure era ->
  PDoc
ppShelleyNewEpochPredicateFailure (EpochFailure x) = ppEpochPredicateFailure @era x
ppShelleyNewEpochPredicateFailure (CorruptRewardUpdate x) =
  ppSexp "CorruptRewardUpdate" [prettyA x]
ppShelleyNewEpochPredicateFailure (MirFailure x) = prettyA x

ppConwayNewEpochPredicateFailure ::
  forall era.
  ( PredicateFailure (EraRule "EPOCH" era) ~ ShelleyEpochPredFailure era
  , Reflect era
  , PrettyA (PredicateFailure (EraRule "ENACTMENT" era))
  ) =>
  ConwayNewEpochPredFailure era ->
  PDoc
ppConwayNewEpochPredicateFailure (Conway.EpochFailure x) = ppEpochPredicateFailure @era x
ppConwayNewEpochPredicateFailure (Conway.CorruptRewardUpdate x) =
  ppSexp "CorruptRewardUpdate" [prettyA x]
ppConwayNewEpochPredicateFailure (Conway.EnactmentFailure x) = prettyA x

instance
  ( Reflect era
  , PredicateFailure (EraRule "EPOCH" era)
      ~ ShelleyEpochPredFailure era
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
ppEpochPredicateFailure (PoolReapFailure x) = prettyA x
ppEpochPredicateFailure (SnapFailure x) = prettyA x
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
    [ ("The total outstanding deposits", prettyA c1)
    , ("The deposit pot", prettyA c2)
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
    [ ("ledger state", prettyA ls)
    , ("blocks made", prettyA mp)
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
  (Inputs s) -> [("Inputs", prettyA (Set.size s))]
  (Collateral s) -> [("Collateral", prettyA (Set.size s))]
  (RefInputs s) -> [("RefInputs", prettyA (Set.size s))]
  (Outputs xs) -> [("Outputs", prettyA (length xs))]
  (CollateralReturn (SJust _)) -> [("Collateral Return", prettyA "?")]
  (TotalCol (SJust c)) -> [("TotalCollateral", prettyA c)]
  (Certs xs) -> [("Certs", prettyA (length xs))]
  (Withdrawals' x) -> [("Withdrawals", prettyA (Map.size (unWithdrawals x)))]
  (Vldt x) -> [("Validity interval", prettyA x)]
  (Txfee c) -> [("Fee", prettyA c)]
  (Update (SJust _)) -> [("Collateral Return", "?")]
  (ReqSignerHashes x) -> [("Required Signer hashes", prettyA (Set.size x))]
  (Mint ma) -> [("Mint", prettyA (Val.size (MaryValue 0 ma)) <> " bytes")]
  (WppHash (SJust _)) -> [("WppHash", "?")]
  (AdHash (SJust _)) -> [("AdHash", "?")]
  (Txnetworkid (SJust x)) -> [("Network id", prettyA x)]
  _ -> []

bodySummary :: EraTxBody era => Proof era -> TxBody era -> PDoc
bodySummary proof body =
  ppRecord
    "TxBody"
    (concat (map txBodyFieldSummary (abstractTxBody proof body)))

witnessFieldSummary :: Era era => WitnessesField era -> (Text, PDoc)
witnessFieldSummary wit = case wit of
  (AddrWits s) -> ("Address Witnesses", prettyA (Set.size s))
  (BootWits s) -> ("BootStrap Witnesses", prettyA (Set.size s))
  (ScriptWits s) -> ("Script Witnesses", prettyA (Map.size s))
  (DataWits m) -> ("Data Witnesses", prettyA (Map.size (unTxDats m)))
  (RdmrWits (Redeemers' m)) -> ("Redeemer Witnesses", prettyA (Map.size m))

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
  (AuxData (SJust _)) -> [ppSexp "AuxData" ["?"]]
  (Valid (IsValid b)) -> [ppSexp "IsValid" [prettyA b]]
  _ -> []

txSummary :: EraTx era => Proof era -> Tx era -> PDoc
txSummary proof tx =
  ppSexp "Tx" (concat (map (txFieldSummary proof) (abstractTx proof tx)))

-- =================================
-- Summary version of UTxO

trim :: PDoc -> PDoc
trim x = prettyA (take 10 (show x))

txInSummary :: TxIn era -> PDoc
txInSummary (TxIn (TxId h) n) = ppSexp "TxIn" [trim (prettyA h), prettyA (txIxToInt n)]

txOutSummary :: Proof era -> TxOut era -> PDoc
txOutSummary p@(Conway _) (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [addrSummary addr, pcCoreValue p v, datumSummary d, prettyA s]
txOutSummary p@(Babbage _) (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [addrSummary addr, pcCoreValue p v, datumSummary d, prettyA s]
txOutSummary p@(Alonzo _) (AlonzoTxOut addr v md) =
  ppSexp "TxOut" [addrSummary addr, pcCoreValue p v, prettyA md]
txOutSummary p@(Mary _) (ShelleyTxOut addr v) = ppSexp "TxOut" [addrSummary addr, pcCoreValue p v]
txOutSummary p@(Allegra _) (ShelleyTxOut addr v) = ppSexp "TxOut" [addrSummary addr, pcCoreValue p v]
txOutSummary p@(Shelley _) (ShelleyTxOut addr v) = ppSexp "TxOut" [addrSummary addr, pcCoreValue p v]

datumSummary :: Era era => Datum era -> PDoc
datumSummary NoDatum = prettyA "NoDatum"
datumSummary (DatumHash h) = ppSexp "DHash" [trim (prettyA h)]
datumSummary (Datum b) = dataSummary (binaryDataToData b)

dataSummary :: Era era => Data era -> PDoc
dataSummary (Data x) = plutusDataSummary x

plutusDataSummary :: PV1.Data -> PDoc
plutusDataSummary (PV1.Constr n ds) = (prettyA (show n)) <> prettyA ds
plutusDataSummary (PV1.Map ds) = "Map " <> prettyA ds
plutusDataSummary (PV1.List xs) = prettyA xs
plutusDataSummary (PV1.I n) = prettyA n
plutusDataSummary (PV1.B bs) = trim (prettyA bs)

multiAssetSummary :: MultiAsset c -> PDoc
multiAssetSummary (MultiAsset m) = prettyA ("num tokens = " ++ show (Map.size m))

vSummary :: MaryValue c -> PDoc
vSummary (MaryValue n ma) =
  ppSexp "Value" [prettyA n, multiAssetSummary ma]

scriptSummary :: forall era. Proof era -> Script era -> PDoc
scriptSummary p@(Conway _) script = plutusSummary p script
scriptSummary p@(Babbage _) script = plutusSummary p script
scriptSummary p@(Alonzo _) script = plutusSummary p script
scriptSummary (Mary _) script = timelockSummary script
scriptSummary (Allegra _) script = timelockSummary script
scriptSummary (Shelley _) script = multiSigSummary script

networkSummary :: Network -> PDoc
networkSummary Testnet = prettyA "Test"
networkSummary Mainnet = prettyA "Main"

addrSummary :: Addr c -> PDoc
addrSummary (Addr nw pay stk) =
  ppSexp "Addr" [networkSummary nw, credSummary pay, stakeSummary stk]
addrSummary (AddrBootstrap _) = prettyA "Bootstrap"

credSummary :: Credential keyrole c -> PDoc
credSummary (ScriptHashObj (ScriptHash h)) = ppSexp "Script" [trim (prettyA h)]
credSummary (KeyHashObj (KeyHash kh)) = ppSexp "Key" [trim (prettyA kh)]

stakeSummary :: StakeReference c -> PDoc
stakeSummary StakeRefNull = "Null"
stakeSummary (StakeRefPtr _) = "Ptr"
stakeSummary (StakeRefBase x) = ppSexp "Stake" [credSummary (coerceKeyRole x)]

utxoSummary :: Proof era -> UTxO era -> PDoc
utxoSummary proof = ppMap txInSummary (txOutSummary proof) . unUTxO

utxoString :: Proof era -> UTxO era -> String
utxoString proof = show . ppMap txInSummary (txOutSummary proof) . unUTxO

scriptHashSummary :: ScriptHash c -> PDoc
scriptHashSummary (ScriptHash h) = trim (prettyA h)

keyHashSummary :: KeyHash keyrole c -> PDoc
keyHashSummary (KeyHash h) = trim (prettyA h)

dataHashSummary :: DataHash era -> PDoc
dataHashSummary dh = trim (prettyA dh)

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
  ppSexp "MOfN" (prettyA (fromIntegral m) : foldr (:) [] (fmap timelockSummary ms))
timelockSummary (RequireTimeExpire mslot) =
  ppSexp "Expires" [prettyA mslot]
timelockSummary (RequireTimeStart mslot) =
  ppSexp "Starts" [prettyA mslot]

multiSigSummary :: Era era => SS.MultiSig era -> PDoc
multiSigSummary (SS.RequireSignature hk) = ppSexp "ReqSig" [keyHashSummary hk]
multiSigSummary (SS.RequireAllOf ps) = ppSexp "AllOf" (map multiSigSummary ps)
multiSigSummary (SS.RequireAnyOf ps) = ppSexp "AnyOf" (map multiSigSummary ps)
multiSigSummary (SS.RequireMOf m ps) = ppSexp "MOf" (prettyA m : map multiSigSummary ps)

plutusSummary :: forall era. Proof era -> AlonzoScript era -> PDoc
plutusSummary (Conway _) s@(PlutusScript lang _) =
  prettyA (show lang ++ " ") <> scriptHashSummary (hashScript @era s)
plutusSummary (Conway _) (TimelockScript x) = timelockSummary x
plutusSummary (Babbage _) s@(PlutusScript lang _) =
  prettyA (show lang ++ " ") <> scriptHashSummary (hashScript @era s)
plutusSummary (Babbage _) (TimelockScript x) = timelockSummary x
plutusSummary (Alonzo _) s@(PlutusScript lang _) =
  prettyA (show lang ++ " ") <> scriptHashSummary (hashScript @era s)
plutusSummary (Alonzo _) (TimelockScript x) = timelockSummary x
plutusSummary other _ = prettyA ("Plutus script in era " ++ show other ++ "???")

dStateSummary :: DState c -> PDoc
dStateSummary (DState umap future (GenDelegs current) irwd) =
  ppRecord
    "DState"
    [ ("Unified Reward Map", uMapSummary umap)
    , ("Future genesis key delegations", prettyA (Map.size future))
    , ("Genesis key delegations", prettyA (Map.size current))
    , ("Instantaneous Rewards", instantSummary irwd)
    ]

instantSummary :: InstantaneousRewards c -> PDoc
instantSummary (InstantaneousRewards reserves treasury dreserves dtreasury) =
  ppRecord
    "InstantaneousRewards"
    [ ("Rewards from reserves", prettyA (Map.size reserves))
    , ("Rewards from treasury", prettyA (Map.size treasury))
    , ("Treasury to reserves", prettyA dreserves)
    , ("Reserves to treasury", prettyA dtreasury)
    ]

uMapSummary :: UM.UMap c -> PDoc
uMapSummary umap =
  ppRecord
    "UMap"
    [ ("Reward Map", prettyA (UM.size (UM.RewardDeposits umap)))
    , ("Delegations Map", prettyA (UM.size (UM.Delegations umap)))
    , ("Ptrs Map", prettyA (UM.size (UM.Ptrs umap)))
    ]

pStateSummary :: PState c -> PDoc
pStateSummary (PState pp fpp retire deposit) =
  ppRecord
    "PState"
    [ ("Pool parameters", prettyA (Map.size pp))
    , ("Future pool parameters", prettyA (Map.size fpp))
    , ("Retiring stake pools", prettyA (Map.size retire))
    , ("Deposits", prettyA (Map.size deposit))
    ]

dpStateSummary :: DPState c -> PDoc
dpStateSummary (DPState d p) = vsep [dStateSummary d, pStateSummary p]

-- =============================================

class PrettyC t era where
  prettyC :: Proof era -> t -> PDoc

pcTxId :: TxId c -> PDoc
pcTxId (TxId safehash) = trim (prettyA safehash)

instance c ~ EraCrypto era => PrettyC (TxId c) era where prettyC _ = pcTxId

pcTxIn :: TxIn c -> PDoc
pcTxIn (TxIn (TxId h) (TxIx i)) = parens (hsep [prettyA "TxIn", trim (prettyA h), prettyA i])

instance c ~ EraCrypto era => PrettyC (TxIn c) era where prettyC _ = pcTxIn

pcNetwork :: Network -> PDoc
pcNetwork Testnet = "TestNet"
pcNetwork Mainnet = "Mainnet"

instance PrettyC Network era where prettyC _ = pcNetwork

pcKeyHash :: KeyHash discriminator c -> PDoc
pcKeyHash (KeyHash h) = trim (prettyA h)

instance c ~ EraCrypto era => PrettyC (KeyHash d c) era where prettyC _ = pcKeyHash

pcCredential :: Credential keyrole c -> PDoc
pcCredential (ScriptHashObj (ScriptHash h)) = hsep ["Script", trim (prettyA h)]
pcCredential (KeyHashObj (KeyHash h)) = hsep ["Key", trim (prettyA h)]

instance c ~ EraCrypto era => PrettyC (Credential keyrole c) era where prettyC _ = pcCredential

pcStakeReference :: StakeReference c -> PDoc
pcStakeReference StakeRefNull = "Null"
pcStakeReference (StakeRefBase cred) = pcCredential cred
pcStakeReference (StakeRefPtr _) = "Ptr"

instance c ~ EraCrypto era => PrettyC (StakeReference c) era where prettyC _ = pcStakeReference

pcAddr :: Addr c -> PDoc
pcAddr (Addr nw pay stk) =
  parens $
    hsep
      [ "Addr"
      , pcNetwork nw
      , pcCredential pay
      , pcStakeReference stk
      ]
pcAddr (AddrBootstrap _) = "Bootstrap"

instance c ~ EraCrypto era => PrettyC (Addr c) era where prettyC _ = pcAddr

pcCoreValue :: Proof era -> Value era -> PDoc
pcCoreValue (Conway _) v = vSummary v
pcCoreValue (Babbage _) v = vSummary v
pcCoreValue (Alonzo _) v = vSummary v
pcCoreValue (Mary _) v = vSummary v
pcCoreValue (Allegra _) (Coin n) = hsep ["₳", prettyA n]
pcCoreValue (Shelley _) (Coin n) = hsep ["₳", prettyA n]

pcCoin :: Coin -> PDoc
pcCoin (Coin n) = hsep ["₳", prettyA n]

instance PrettyC Coin era where prettyC _ = pcCoin

pcValue :: MaryValue c -> PDoc
pcValue (MaryValue n (MultiAsset m)) = ppSexp "Value" [prettyA n, prettyA ("num tokens = " ++ show (Map.size m))]

instance c ~ EraCrypto era => PrettyC (MaryValue c) era where
  prettyC _ = pcValue

pcDatum :: Era era => Datum era -> PDoc
pcDatum NoDatum = "NoDatum"
pcDatum (DatumHash h) = ppSexp "DHash" [trim (prettyA h)]
pcDatum (Datum b) = pcData (binaryDataToData b)

instance Era era => PrettyC (Datum era) era where prettyC _ = pcDatum

pcData :: forall era. Era era => Data era -> PDoc
pcData d@(Data (PV1.Constr n _)) =
  ppSexp (pack ("Constr" ++ show n)) ["Hash", trim $ prettyA (hashData d)]
pcData d@(Data (PV1.Map _)) =
  ppSexp "Map" ["Hash", trim $ prettyA (hashData d)]
pcData d@(Data (PV1.List _)) =
  ppSexp "List" ["Hash", trim $ prettyA (hashData d)]
pcData d@(Data (PV1.I n)) =
  ppSexp "I" [prettyA n, "Hash", trim $ prettyA (hashData d)]
pcData d@(Data (PV1.B bytes)) =
  ppSexp "B" [trim (viaShow bytes), "Hash", trim $ prettyA (hashData d)]

instance Era era => PrettyC (Data era) era where prettyC _ = pcData

pcTimelock :: forall era. Reflect era => PDoc -> Timelock era -> PDoc
pcTimelock hash (RequireSignature akh) = ppSexp "Signature" [keyHashSummary akh, hash]
pcTimelock hash (RequireAllOf _ts) = ppSexp "AllOf" [hash]
pcTimelock hash (RequireAnyOf _) = ppSexp "AnyOf" [hash]
pcTimelock hash (RequireMOf m _) = ppSexp "MOfN" [prettyA (fromIntegral m), hash]
pcTimelock hash (RequireTimeExpire mslot) = ppSexp "Expires" [prettyA mslot, hash]
pcTimelock hash (RequireTimeStart mslot) = ppSexp "Starts" [prettyA mslot, hash]

pcMultiSig :: Reflect era => PDoc -> SS.MultiSig era -> PDoc
pcMultiSig h (SS.RequireSignature hk) = ppSexp "ReqSig" [keyHashSummary hk, h]
pcMultiSig h (SS.RequireAllOf _) = ppSexp "AllOf" [h]
pcMultiSig h (SS.RequireAnyOf _) = ppSexp "AnyOf" [h]
pcMultiSig h (SS.RequireMOf m _) = ppSexp "MOf" [prettyA m, h]

pcScriptHash :: ScriptHash era -> PDoc
pcScriptHash (ScriptHash h) = trim (prettyA h)

pcHashScript :: forall era. Reflect era => Proof era -> Script era -> PDoc
pcHashScript (Conway _) s = "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Babbage _) s = "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Alonzo _) s = "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Mary _) s = "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Allegra _) s = "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript (Shelley _) s = "Hash " <> pcScriptHash (hashScript @era s)

pcScript :: forall era. Reflect era => Proof era -> Script era -> PDoc
pcScript p@(Conway _) s@(TimelockScript t) = pcTimelock @era (pcHashScript @era p s) t
pcScript p@(Conway _) s@(PlutusScript v _) =
  parens (hsep [prettyA ("PlutusScript " <> show v <> " "), pcHashScript p s])
pcScript p@(Babbage _) s@(TimelockScript t) = pcTimelock @era (pcHashScript @era p s) t
pcScript p@(Babbage _) s@(PlutusScript v _) =
  parens (hsep [prettyA ("PlutusScript " <> show v <> " "), pcHashScript p s])
pcScript p@(Alonzo _) s@(TimelockScript t) = pcTimelock @era (pcHashScript @era p s) t
pcScript p@(Alonzo _) s@(PlutusScript v _) =
  parens (hsep [prettyA ("PlutusScript " <> show v <> " "), pcHashScript p s])
pcScript p@(Mary _) s = pcTimelock @era (pcHashScript @era p s) s
pcScript p@(Allegra _) s = pcTimelock @era (pcHashScript @era p s) s
pcScript p@(Shelley _) s = pcMultiSig @era (pcHashScript @era p s) s

pcDataHash :: DataHash era -> PDoc
pcDataHash dh = trim (prettyA dh)

pcTxOut :: Reflect era => Proof era -> TxOut era -> PDoc
pcTxOut p@(Conway _) (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [pcAddr addr, pcValue v, pcDatum d, prettyA (pcScript p) s]
pcTxOut p@(Babbage _) (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [pcAddr addr, pcValue v, pcDatum d, prettyA (pcScript p) s]
pcTxOut (Alonzo _) (AlonzoTxOut addr v md) =
  ppSexp "TxOut" [pcAddr addr, pcValue v, prettyA md]
pcTxOut p@(Mary _) (ShelleyTxOut addr v) =
  ppSexp "TxOut" [pcAddr addr, pcCoreValue p v]
pcTxOut p@(Allegra _) (ShelleyTxOut addr v) =
  ppSexp "TxOut" [pcAddr addr, pcCoreValue p v]
pcTxOut p@(Shelley _) (ShelleyTxOut addr v) =
  ppSexp "TxOut" [pcAddr addr, pcCoreValue p v]

pcUTxO :: Reflect era => Proof era -> UTxO era -> PDoc
pcUTxO proof = ppMap pcTxIn (pcTxOut proof) . unUTxO

instance Reflect era => PrettyC (UTxO era) era where prettyC = pcUTxO

pcIndividualPoolStake :: IndividualPoolStake c -> PDoc
pcIndividualPoolStake (IndividualPoolStake rat vrf) =
  ppRecord
    "pool stake"
    [("ratio", ppRational rat), ("vrf", trim (ppHash vrf))]

instance c ~ EraCrypto era => PrettyC (IndividualPoolStake c) era where prettyC _ = pcIndividualPoolStake

pcPoolParams :: PoolParams era -> PDoc
pcPoolParams x =
  ppRecord
    "PoolParams"
    [ ("Id", keyHashSummary (ppId x))
    , ("reward accnt", pcCredential (getRwdCred (ppRewardAcnt x)))
    ]

instance PrettyC (PoolParams era) era where prettyC _ = pcPoolParams

pcDelegCert :: DelegCert c -> PDoc
pcDelegCert (RegKey cred) = ppSexp "RegKey" [pcCredential cred]
pcDelegCert (DeRegKey cred) = ppSexp "DeRegKey" [pcCredential cred]
pcDelegCert (Delegate (Delegation x y)) = ppSexp "Delegate" [pcCredential x, pcKeyHash y]

instance c ~ EraCrypto era => PrettyC (DelegCert c) era where prettyC _ = pcDelegCert

pcPoolCert :: PoolCert c -> PDoc
pcPoolCert (RegPool poolp) = ppSexp "RegPool" [pcPoolParams poolp]
pcPoolCert (RetirePool keyhash epoch) = ppSexp "RetirePool" [pcKeyHash keyhash, ppEpochNo epoch]

instance c ~ EraCrypto era => PrettyC (PoolCert c) era where prettyC _ = pcPoolCert

pcDCert :: DCert c -> PDoc
pcDCert (DCertDeleg x) = pcDelegCert x
pcDCert (DCertPool x) = pcPoolCert x
pcDCert (DCertGenesis _) = ppString "GenesisCert"
pcDCert (DCertMir _) = ppString "MirCert"

pcConwayDCert :: ConwayDCert c -> PDoc
pcConwayDCert (ConwayDCertDeleg dc) = pcDelegCert dc
pcConwayDCert (ConwayDCertPool poolc) = pcPoolCert poolc
pcConwayDCert (ConwayDCertConstitutional _) = ppString "GenesisCert"

instance c ~ EraCrypto era => PrettyC (DCert c) era where prettyC _ = pcDCert

pcRewardAcnt :: RewardAcnt c -> PDoc
pcRewardAcnt (RewardAcnt net cred) = ppSexp "RewAccnt" [pcNetwork net, pcCredential cred]

instance c ~ EraCrypto era => PrettyC (RewardAcnt c) era where prettyC _ = pcRewardAcnt

pcExUnits :: ExUnits -> PDoc
pcExUnits (ExUnits mem step) =
  ppSexp "ExUnits" [ppNatural mem, ppNatural step]

pcTxBodyField :: (Reflect era, PrettyA (PParamsUpdate era)) => Proof era -> TxBodyField era -> [(Text, PDoc)]
pcTxBodyField proof x = case x of
  Inputs s -> [("spend inputs", ppSet pcTxIn s)]
  Collateral s -> [("coll inputs", ppSet pcTxIn s)]
  RefInputs s -> [("ref inputs", ppSet pcTxIn s)]
  Outputs s -> [("outputs", ppList (pcTxOut proof) (toList s))]
  CollateralReturn SNothing -> []
  CollateralReturn (SJust txout) -> [("coll return", pcTxOut proof txout)]
  TotalCol SNothing -> []
  TotalCol (SJust c) -> [("total coll", pcCoin c)]
  Certs xs -> [("certs", ppList pcDCert (toList xs))]
  Withdrawals' (Withdrawals m) -> [("withdrawal", ppMap pcRewardAcnt pcCoin m)]
  Txfee c -> [("fee", pcCoin c)]
  Vldt v -> [("validity interval", ppValidityInterval v)]
  TTL slot -> [("time to live", ppSlotNo slot)]
  Update SNothing -> []
  Update (SJust _) -> [("update", ppString "UPDATE")]
  ReqSignerHashes s -> [("required hashes", ppSet pcKeyHash s)]
  Mint v -> [("minted", multiAssetSummary v)]
  WppHash SNothing -> []
  WppHash (SJust h) -> [("integrity hash", trim (ppSafeHash h))]
  AdHash SNothing -> []
  AdHash (SJust (AuxiliaryDataHash h)) -> [("aux data hash", trim (ppSafeHash h))]
  Txnetworkid SNothing -> []
  Txnetworkid (SJust nid) -> [("network id", pcNetwork nid)]
  GovernanceActions ga -> [("governance actions", ppStrictSeq prettyA ga)]
  Votes vs -> [("votes", ppStrictSeq prettyA vs)]
  ConwayCerts certs -> [("conway certs", ppStrictSeq prettyA certs)]

pcTxField :: forall era. (Reflect era, PrettyA (PParamsUpdate era)) => Proof era -> TxField era -> [(Text, PDoc)]
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
  AddrWits set -> [("key wits", ppSet (pcWitVKey @era) set)]
  BootWits _ -> [("boot wits", ppString "BOOTWITS")]
  ScriptWits mp -> [("script wits", ppMap pcScriptHash (pcScript proof) mp)]
  DataWits (TxDats m) -> [("data wits", ppMap pcDataHash pcData m)]
  RdmrWits (Redeemers m) ->
    [("redeemer wits", ppMap ppRdmrPtr (pcPair pcData pcExUnits) m)]

pcPair :: (t1 -> PDoc) -> (t2 -> PDoc) -> (t1, t2) -> PDoc
pcPair pp1 pp2 (x, y) = parens (hsep [pp1 x, ppString ",", pp2 y])

pcWitVKey :: (Reflect era, Typeable discriminator) => WitVKey discriminator (EraCrypto era) -> PDoc
pcWitVKey (WitVKey vk@(VKey x) sig) = ppSexp "WitVKey" [ppString keystring, ppString (drop 12 sigstring), hash]
  where
    keystring = show x
    hash = pcKeyHash (hashKey vk)
    sigstring = show sig

pcWitnesses :: Reflect era => Proof era -> TxWits era -> PDoc
pcWitnesses proof wits = ppRecord "Witnesses" pairs
  where
    fields = abstractWitnesses proof wits
    pairs = concat (map (pcWitnessesField proof) fields)

pcTx :: (Reflect era, PrettyA (PParamsUpdate era)) => Proof era -> Tx era -> PDoc
pcTx proof tx = ppRecord "Tx" pairs
  where
    fields = abstractTx proof tx
    pairs = concat (map (pcTxField proof) fields)

pcTxBody :: (Reflect era, PrettyA (PParamsUpdate era)) => Proof era -> TxBody era -> PDoc
pcTxBody proof txbody = ppRecord "TxBody" pairs
  where
    fields = abstractTxBody proof txbody
    pairs = concat (map (pcTxBodyField proof) fields)

pcUTxOState :: Reflect era => Proof era -> UTxOState era -> PDoc
pcUTxOState proof (UTxOState u dep fees _pups _stakedistro) =
  ppRecord
    "UTxOState"
    [ ("utxo", pcUTxO proof u)
    , ("deposited", pcCoin dep)
    , ("fees", pcCoin fees)
    , ("ppups", ppString "PPUP")
    , ("stake distr", ppString "Stake Distr")
    ]

instance Reflect era => PrettyC (UTxOState era) era where prettyC = pcUTxOState

pcDPState :: p -> DPState era -> PDoc
pcDPState _proof (DPState (DState {dsUnified = un}) (PState {psStakePoolParams = pool})) =
  ppRecord
    "DPState summary"
    [ ("rewards", ppMap pcCredential pcCoin (UM.rewView un))
    , ("delegations", ppMap pcCredential keyHashSummary (UM.delView un))
    , ("pool params", ppMap pcKeyHash pcPoolParams pool)
    ]

instance PrettyC (DPState era) era where prettyC = pcDPState

instance PrettyC (ConwayTallyState era) era where
  prettyC proof (ConwayTallyState x) = case proof of
    Shelley _ -> ppMap prettyA prettyA x
    Mary _ -> ppMap prettyA prettyA x
    Allegra _ -> ppMap prettyA prettyA x
    Alonzo _ -> ppMap prettyA prettyA x
    Babbage _ -> ppMap prettyA prettyA x
    Conway _ -> ppMap prettyA prettyA x

pcLedgerState :: Reflect era => Proof era -> LedgerState era -> PDoc
pcLedgerState proof (LedgerState utstate dpstate) =
  ppRecord
    "LedgerState"
    [ ("UTxOState", pcUTxOState proof utstate)
    , ("DPState", pcDPState proof dpstate)
    ]

instance Reflect era => PrettyC (LedgerState era) era where prettyC = pcLedgerState

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

pcEpochState :: Reflect era => Proof era -> EpochState era -> PDoc
pcEpochState proof (EpochState (AccountState tre res) _ ls _ _ _) =
  ppRecord
    "EpochState"
    [ ("AccountState", ppRecord' "" [("treasury", pcCoin tre), ("reserves", pcCoin res)])
    , ("LedgerState", pcLedgerState proof ls)
    ]

instance Reflect era => PrettyC (EpochState era) era where prettyC = pcEpochState

pc :: PrettyC t era => Proof era -> t -> IO ()
pc proof x = putStrLn (show (prettyC proof x))

-- ===================================================
