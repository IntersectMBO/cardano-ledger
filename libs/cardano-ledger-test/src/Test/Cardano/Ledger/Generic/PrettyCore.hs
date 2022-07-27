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
import Cardano.Ledger.Alonzo.Data (Data (..), Datum (..), binaryDataToData, hashData)
import Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError (..))
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoBbodyPredFailure (..),
    AlonzoUtxoPredFailure (..),
    AlonzoUtxosPredFailure (..),
    AlonzoUtxowPredFailure (..),
    FailureDescription (..),
    TagMismatchDescription (..),
  )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptPurpose (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..), unTxDats)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (BlocksMade (..), Network (..), TxIx (..), txIxToInt)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys
  ( GenDelegs (..),
    HasKeyRole (coerceKeyRole),
    KeyHash (..),
    KeyPair (..),
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
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    InstantaneousRewards (..),
    LedgerState (..),
    NewEpochState (..),
    PState (..),
    UTxOState (..),
  )
import Cardano.Ledger.Shelley.Rules.Bbody (ShelleyBbodyPredFailure (..), ShelleyBbodyState (..))
import Cardano.Ledger.Shelley.Rules.Epoch (ShelleyEpochPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Ledger (ShelleyLedgerPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Ledgers (ShelleyLedgersPredFailure (..))
import Cardano.Ledger.Shelley.Rules.NewEpoch (ShelleyNewEpochPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Newpp (ShelleyNewppPredFailure (..))
import qualified Cardano.Ledger.Shelley.Rules.Ppup as Shelley (ShelleyPpupPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Tick (ShelleyTickPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUpecPredFailure (..))
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley (ShelleyUtxoPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxow (ShelleyUtxowPredFailure (..))
import qualified Cardano.Ledger.Shelley.Scripts as SS (MultiSig (..))
import Cardano.Ledger.Shelley.TxBody (DCert (..), DelegCert (..), Delegation (..), PoolCert (..), PoolParams (..), ShelleyTxOut (..), Wdrl (..), WitVKey (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import qualified Cardano.Ledger.ShelleyMA.Rules as Mary (ShelleyMAUtxoPredFailure (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UnifiedMap (UnifiedMap)
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition.Extended (STS (..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import qualified Data.UMap as UMap (View (..), delView, rewView, size)
import qualified PlutusCore.Data as Plutus (Data (..))
import Prettyprinter (hsep, parens, viaShow, vsep)
import Test.Cardano.Ledger.Generic.Fields
  ( TxBodyField (..),
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
  prettyScript :: Script era -> PDoc
  prettyTxBody :: TxBody era -> PDoc
  prettyWitnesses :: Witnesses era -> PDoc
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
  prettyTxBody = Cardano.Ledger.Pretty.Mary.ppTxBody
  prettyWitnesses = ppWitnessSetHKD
  prettyValue = ppCoin
  prettyTxOut = Cardano.Ledger.Pretty.ppTxOut

instance CC.Crypto c => PrettyCore (MaryEra c) where
  prettyTx = Cardano.Ledger.Pretty.ppTx
  prettyScript = ppTimelock
  prettyTxBody = Cardano.Ledger.Pretty.Mary.ppTxBody
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
  prettyValue = ppValue
  prettyTxOut = Babbage.ppTxOut

instance CC.Crypto c => PrettyCore (ConwayEra c) where
  prettyTx = Cardano.Ledger.Pretty.Alonzo.ppTx
  prettyScript = ppScript
  prettyTxBody = Babbage.ppTxBody
  prettyWitnesses = ppTxWitness
  prettyValue = ppValue
  prettyTxOut = Babbage.ppTxOut

prettyUTxO :: Proof era -> UTxO era -> PDoc
prettyUTxO proof = prettyUTxOMap proof . unUTxO

prettyUTxOMap :: Proof era -> Map.Map (TxIn (Crypto era)) (TxOut era) -> PDoc
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
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era)),
    Show (PredicateFailure (EraRule "DELEGS" era))
  ) =>
  ShelleyLedgerPredFailure era ->
  PDoc
ppLedgerPredicateFailure (UtxowFailure x) = prettyA x
ppLedgerPredicateFailure (DelegsFailure x) = ppString (show x)

instance
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era)),
    Show (PredicateFailure (EraRule "DELEGS" era))
  ) =>
  PrettyA (ShelleyLedgerPredFailure era)
  where
  prettyA = ppLedgerPredicateFailure

instance
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era)),
    Show (PredicateFailure (EraRule "DELEGS" era))
  ) =>
  PrettyA [ShelleyLedgerPredFailure era]
  where
  prettyA = ppList prettyA

-- =========================================
-- Predicate Failure for Alonzo UTXOW

ppUtxowPredicateFail ::
  ( PrettyA (PredicateFailure (EraRule "UTXO" era)),
    PrettyCore era
  ) =>
  AlonzoUtxowPredFailure era ->
  PDoc
ppUtxowPredicateFail (ShelleyInAlonzoUtxowPredFailure x) = prettyA x
ppUtxowPredicateFail (MissingRedeemers xs) =
  ppSexp "MissingRedeemers" [ppList (ppPair ppScriptPurpose ppScriptHash) xs]
ppUtxowPredicateFail (MissingRequiredDatums s1 s2) =
  ppRecord
    "MissingRequiredDatums"
    [ ("missing data hashes", ppSet ppSafeHash s1),
      ("received data hashes", ppSet ppSafeHash s2)
    ]
ppUtxowPredicateFail (NonOutputSupplimentaryDatums s1 s2) =
  ppRecord
    "NonOutputSupplimentaryDatums"
    [ ("unallowed data hashes", ppSet ppSafeHash s1),
      ("acceptable data hashes", ppSet ppSafeHash s2)
    ]
ppUtxowPredicateFail (PPViewHashesDontMatch h1 h2) =
  ppRecord
    "PPViewHashesDontMatch"
    [ ("PPHash in the TxBody", ppStrictMaybe ppSafeHash h1),
      ("PPHash Computed from the current Protocol Parameters", ppStrictMaybe ppSafeHash h2)
    ]
ppUtxowPredicateFail (MissingRequiredSigners x) =
  ppSexp "MissingRequiredSigners" [ppSet ppKeyHash x]
ppUtxowPredicateFail (UnspendableUTxONoDatumHash x) =
  ppSexp "UnspendableUTxONoDatumHash" [ppSet ppTxIn x]
ppUtxowPredicateFail (ExtraRedeemers x) =
  ppSexp "ExtraRedeemers" [ppList prettyA x]

instance
  ( PrettyA (PredicateFailure (EraRule "UTXO" era)),
    PrettyCore era
  ) =>
  PrettyA (AlonzoUtxowPredFailure era)
  where
  prettyA = ppUtxowPredicateFail

-- ====================================================
-- Predicate Failure for Shelley UTXOW

ppUtxowPredicateFailure ::
  ( PrettyA (PredicateFailure (EraRule "UTXO" era)),
    PrettyCore era
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
  ( PrettyA (PredicateFailure (EraRule "UTXO" era)),
    PrettyCore era
  ) =>
  PrettyA (ShelleyUtxowPredFailure era)
  where
  prettyA = ppUtxowPredicateFailure

-- ========================================================
-- Predicate Failure for Alonzo UTXO

ppUtxoPredicateFailure ::
  forall era.
  ( PrettyCore era,
    PrettyA (PredicateFailure (EraRule "UTXOS" era)),
    PrettyA (TxOut era) -- From ppUTxO FIXME
  ) =>
  AlonzoUtxoPredFailure era ->
  PDoc
ppUtxoPredicateFailure (BadInputsUTxO x) =
  ppSexp "BadInputsUTxO" [ppSet ppTxIn x]
ppUtxoPredicateFailure (OutsideValidityIntervalUTxO vi slot) =
  ppRecord "OutsideValidityIntervalUTxO" [("validity interval", ppValidityInterval vi), ("slot", ppSlotNo slot)]
ppUtxoPredicateFailure (MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", ppInteger actual),
      ("max transaction size", ppInteger maxs)
    ]
ppUtxoPredicateFailure InputSetEmptyUTxO =
  ppSexp "InputSetEmptyUTxO" []
ppUtxoPredicateFailure (FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for thistransaction", ppCoin computed),
      ("fee supplied by transaction", ppCoin supplied)
    ]
ppUtxoPredicateFailure (ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", prettyValue @era consumed),
      ("coin produced", prettyValue @era produced)
    ]
ppUtxoPredicateFailure (WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", ppNetwork n),
      ("set addresses with wrong network id", ppSet ppAddr add)
    ]
ppUtxoPredicateFailure (WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", ppNetwork n),
      ("set reward address with wrong network id", ppSet ppRewardAcnt accnt)
    ]
ppUtxoPredicateFailure (OutputTooSmallUTxO xs) =
  ppSexp "OutputTooSmallUTxO" [ppList prettyTxOut xs]
ppUtxoPredicateFailure (UtxosFailure subpred) = prettyA subpred
-- ppSexp "UtxosFailure" [prettyA subpred]
ppUtxoPredicateFailure (OutputBootAddrAttrsTooBig x) =
  ppSexp "OutputBootAddrAttrsTooBig" [ppList prettyTxOut x]
ppUtxoPredicateFailure (TriesToForgeADA) =
  ppSexp "TriesToForgeADA" []
ppUtxoPredicateFailure (OutputTooBigUTxO xs) =
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
    [ ("balance computed", ppCoin x),
      ("the required collateral for the given fee", ppCoin y)
    ]
ppUtxoPredicateFailure (ScriptsNotPaidUTxO x) =
  ppSexp "ScriptsNotPaidUTxO" [ppUTxO x]
ppUtxoPredicateFailure (ExUnitsTooBigUTxO x y) =
  ppRecord
    "ExUnitsTooBigUTxO"
    [ ("Max EXUnits from the protocol parameters", ppExUnits x),
      ("EXUnits supplied", ppExUnits y)
    ]
ppUtxoPredicateFailure (CollateralContainsNonADA x) =
  ppSexp "CollateralContainsNonADA" [prettyValue @era x]
ppUtxoPredicateFailure (WrongNetworkInTxBody x y) =
  ppRecord
    "WrongNetworkInTxBody"
    [ ("Actual Network ID", ppNetwork x),
      ("Network ID in transaction body", ppNetwork y)
    ]
ppUtxoPredicateFailure (OutsideForecast x) =
  ppRecord "OutsideForecast" [("slot number outside consensus forecast range", ppSlotNo x)]
ppUtxoPredicateFailure (TooManyCollateralInputs x y) =
  ppRecord
    "TooManyCollateralInputs"
    [ ("Max allowed collateral inputs", ppNatural x),
      ("Number of collateral inputs", ppNatural y)
    ]
ppUtxoPredicateFailure (NoCollateralInputs) =
  ppSexp "NoCollateralInputs" []

instance
  ( PrettyCore era,
    PrettyA (PredicateFailure (EraRule "UTXOS" era)),
    PrettyA (TxOut era) -- From ppUTxO FIXME
  ) =>
  PrettyA (AlonzoUtxoPredFailure era)
  where
  prettyA = ppUtxoPredicateFailure

-- =========================================
-- Predicate Failure for Alonzo UTXOS

ppUtxosPredicateFailure ::
  PrettyA (PredicateFailure (EraRule "PPUP" era)) =>
  AlonzoUtxosPredFailure era ->
  PDoc
ppUtxosPredicateFailure (ValidationTagMismatch isvalid tag) =
  ppRecord
    "ValidationTagMismatch"
    [ ("isValid tag", ppIsValid isvalid),
      ("mismatch description", ppTagMismatchDescription tag)
    ]
ppUtxosPredicateFailure (CollectErrors es) =
  ppRecord' mempty [("When collecting inputs for twophase scripts, these went wrong.", ppList ppCollectError es)]
ppUtxosPredicateFailure (UpdateFailure p) = prettyA p

instance PrettyA (PredicateFailure (EraRule "PPUP" era)) => PrettyA (AlonzoUtxosPredFailure era) where
  prettyA = ppUtxosPredicateFailure

ppCollectError :: CollectError crypto -> PDoc
ppCollectError (NoRedeemer sp) = ppSexp "NoRedeemer" [ppScriptPurpose sp]
ppCollectError (NoWitness sh) = ppSexp "NoWitness" [ppScriptHash sh]
ppCollectError (NoCostModel l) = ppSexp "NoCostModel" [ppLanguage l]
ppCollectError (BadTranslation x) = ppSexp "BadTranslation" [ppString (show x)]

instance PrettyA (CollectError crypto) where
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
  ( PrettyCore era,
    PrettyA (PredicateFailure (EraRule "PPUP" era))
  ) =>
  Shelley.ShelleyUtxoPredFailure era ->
  PDoc
ppUtxoPFShelley (Shelley.BadInputsUTxO x) =
  ppSexp "BadInputsUTxO" [ppSet ppTxIn x]
ppUtxoPFShelley (Shelley.ExpiredUTxO ttl slot) =
  ppRecord "ExpiredUTxO" [("transaction time to live", ppSlotNo ttl), ("current slot", ppSlotNo slot)]
ppUtxoPFShelley (Shelley.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", ppInteger actual),
      ("max transaction size", ppInteger maxs)
    ]
ppUtxoPFShelley (Shelley.InputSetEmptyUTxO) =
  ppSexp "InputSetEmptyUTxO" []
ppUtxoPFShelley (Shelley.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for this transaction", ppCoin computed),
      ("fee supplied by this transaction", ppCoin supplied)
    ]
ppUtxoPFShelley (Shelley.ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", prettyValue @era consumed),
      ("coin produced", prettyValue @era produced)
    ]
ppUtxoPFShelley (Shelley.WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", ppNetwork n),
      ("set of addresses with wrong network id", ppSet ppAddr add)
    ]
ppUtxoPFShelley (Shelley.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", ppNetwork n),
      ("set of reward address with wrong network id", ppSet ppRewardAcnt accnt)
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
  ( PrettyCore era,
    PrettyA (PredicateFailure (EraRule "PPUP" era))
  ) =>
  PrettyA (Shelley.ShelleyUtxoPredFailure era)
  where
  prettyA = ppUtxoPFShelley

-- =======================================
-- Predicate Failure for Shelley PPUP

ppPpupPredicateFailure :: Shelley.ShelleyPpupPredFailure era -> PDoc
ppPpupPredicateFailure (Shelley.NonGenesisUpdatePPUP x y) =
  ppRecord
    "NonGenesisUpdatePPUP"
    [ ("KeyHashes which are voting", ppSet ppKeyHash x),
      ("KeyHashes which should be voting", ppSet ppKeyHash y)
    ]
ppPpupPredicateFailure (Shelley.PPUpdateWrongEpoch x y z) =
  ppRecord
    "PPUpdateWrongEpoch"
    [ ("current epoch", ppEpochNo x),
      ("intended epoch of update", ppEpochNo y),
      ("voting period within the epoch", ppString (show z))
    ]
ppPpupPredicateFailure (Shelley.PVCannotFollowPPUP x) =
  ppRecord "PVCannotFollowPPUP" [("the first bad protocol version", ppProtVer x)]

instance PrettyA (Shelley.ShelleyPpupPredFailure era) where
  prettyA = ppPpupPredicateFailure

-- =====================================================
-- Predicate failure for Mary UTXO

ppUtxoPFMary ::
  forall era.
  ( PrettyCore era,
    PrettyA (PredicateFailure (EraRule "PPUP" era))
  ) =>
  Mary.ShelleyMAUtxoPredFailure era ->
  PDoc
ppUtxoPFMary (Mary.BadInputsUTxO txins) =
  ppSexp "BadInputsUTxO" [ppSet ppTxIn txins]
ppUtxoPFMary (Mary.OutsideValidityIntervalUTxO vi slot) =
  ppRecord
    "OutsideValidityIntervalUTxO"
    [ ("provided interval", ppValidityInterval vi),
      ("current slot", ppSlotNo slot)
    ]
ppUtxoPFMary (Mary.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", ppInteger actual),
      ("max transaction size", ppInteger maxs)
    ]
ppUtxoPFMary (Mary.InputSetEmptyUTxO) = ppSexp "InputSetEmptyUTxO" []
ppUtxoPFMary (Mary.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for this transaction", ppCoin computed),
      ("fee supplied by this transaction", ppCoin supplied)
    ]
ppUtxoPFMary (Mary.ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", prettyValue @era consumed),
      ("coin produced", prettyValue @era produced)
    ]
ppUtxoPFMary (Mary.WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", ppNetwork n),
      ("set of addresses with wrong network id", ppSet ppAddr add)
    ]
ppUtxoPFMary (Mary.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", ppNetwork n),
      ("set reward address with wrong network id", ppSet ppRewardAcnt accnt)
    ]
ppUtxoPFMary (Mary.OutputTooSmallUTxO xs) =
  ppRecord
    "OutputTooSmallUTxO"
    [("list of supplied transaction outputs that are too small", ppList prettyTxOut xs)]
ppUtxoPFMary (Mary.UpdateFailure x) =
  ppSexp "UpdateFailure" [prettyA x]
ppUtxoPFMary (Mary.OutputBootAddrAttrsTooBig xs) =
  ppRecord "OutputBootAddrAttrsTooBig" [("list of supplied bad transaction outputs", ppList prettyTxOut xs)]
ppUtxoPFMary (Mary.TriesToForgeADA) = ppSexp "TriesToForgeADA" []
ppUtxoPFMary (Mary.OutputTooBigUTxO outs) =
  ppRecord "OutputTooBigUTxO" [("list of TxOuts which are too big", ppList prettyTxOut outs)]

instance
  ( PrettyCore era,
    PrettyA (PredicateFailure (EraRule "PPUP" era))
  ) =>
  PrettyA (Mary.ShelleyMAUtxoPredFailure era)
  where
  prettyA = ppUtxoPFMary

-- =====================================================
-- Probably should be moved elsewhere

-- LedgerState.hs
ppWitHashes :: Set (KeyHash 'Witness crypto) -> PDoc
ppWitHashes hs = ppSexp "WitHashes" [ppSet ppKeyHash hs]

-- Defined in ‘Cardano.Ledger.Alonzo.Tx’
ppScriptPurpose :: ScriptPurpose crypto -> PDoc
ppScriptPurpose (Minting policy) = ppSexp "Minting" [prettyA policy] -- FIXME fill in the blanks
ppScriptPurpose (Spending txin) = ppSexp "Spending" [ppTxIn txin]
ppScriptPurpose (Rewarding acct) = ppSexp "Rewarding" [ppRewardAcnt acct]
ppScriptPurpose (Certifying dcert) = ppSexp "Certifying" [ppDCert dcert]

instance PrettyA (ScriptPurpose crypto) where
  prettyA = ppScriptPurpose

instance PrettyA x => PrettyA [x] where prettyA xs = ppList prettyA xs

-- =====================================================

dots :: PDoc -> PDoc
dots _ = ppString "..."

dotsF :: (a -> PDoc) -> (a -> PDoc)
dotsF _f _x = ppString "..."

ppMyWay :: (Typeable keyrole, CC.Crypto c) => WitVKey keyrole c -> PDoc
ppMyWay (wvk@(WitVKey vkey _)) = ppSexp "MyWay" [ppKeyHash (hashKey vkey), ppWitVKey wvk]

ppCoreWitnesses :: Proof era -> Witnesses era -> PDoc
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
    [ ("actual computed BBody size", ppInt x),
      ("claimed BBody Size in Header", ppInt y)
    ]
ppBbodyPredicateFailure (InvalidBodyHashBBODY h1 h2) =
  ppRecord
    "(InvalidBodyHashBBODY"
    [ ("actual hash", ppHash h1),
      ("claimed hash", ppHash h2)
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
    [ ("Computed Sum of ExUnits for all plutus scripts", ppExUnits e1),
      ("Maximum allowed by protocal parameters", ppExUnits e2)
    ]

instance
  PrettyA (PredicateFailure (EraRule "LEDGERS" era)) =>
  PrettyA (AlonzoBbodyPredFailure era)
  where
  prettyA = ppAlonzoBbodyPredFail

-- ===============
ppTickPredicateFailure ::
  ( ShelleyNewEpochPredFailure era ~ PredicateFailure (EraRule "NEWEPOCH" era),
    ShelleyEpochPredFailure era ~ PredicateFailure (EraRule "EPOCH" era),
    ShelleyUpecPredFailure era ~ PredicateFailure (EraRule "UPEC" era)
  ) =>
  ShelleyTickPredFailure era ->
  PDoc
ppTickPredicateFailure (NewEpochFailure x) = ppNewEpochPredicateFailure x
ppTickPredicateFailure (RupdFailure _) =
  ppString "RupdPredicateFailure has no constructors"

instance
  ( ShelleyNewEpochPredFailure era ~ PredicateFailure (EraRule "NEWEPOCH" era),
    ShelleyEpochPredFailure era ~ PredicateFailure (EraRule "EPOCH" era),
    ShelleyUpecPredFailure era ~ PredicateFailure (EraRule "UPEC" era)
  ) =>
  PrettyA (ShelleyTickPredFailure era)
  where
  prettyA = ppTickPredicateFailure

-- ===============
ppNewEpochPredicateFailure ::
  ( ShelleyEpochPredFailure era ~ PredicateFailure (EraRule "EPOCH" era),
    ShelleyUpecPredFailure era ~ PredicateFailure (EraRule "UPEC" era)
  ) =>
  ShelleyNewEpochPredFailure era ->
  PDoc
ppNewEpochPredicateFailure (EpochFailure x) = ppEpochPredicateFailure x
ppNewEpochPredicateFailure (CorruptRewardUpdate x) =
  ppSexp "CorruptRewardUpdate" [ppRewardUpdate x]
ppNewEpochPredicateFailure (MirFailure _) =
  ppString "MirPredicateFailure has no constructors"

instance
  ( ShelleyNewEpochPredFailure era ~ PredicateFailure (EraRule "NEWEPOCH" era),
    ShelleyEpochPredFailure era ~ PredicateFailure (EraRule "EPOCH" era),
    ShelleyUpecPredFailure era ~ PredicateFailure (EraRule "UPEC" era)
  ) =>
  PrettyA (ShelleyNewEpochPredFailure era)
  where
  prettyA = ppNewEpochPredicateFailure

-- ===============
ppEpochPredicateFailure ::
  ( ShelleyUpecPredFailure era ~ PredicateFailure (EraRule "UPEC" era)
  ) =>
  ShelleyEpochPredFailure era ->
  PDoc
ppEpochPredicateFailure (PoolReapFailure _) =
  ppString "PoolreapPredicateFailure has no constructors"
ppEpochPredicateFailure (SnapFailure _) =
  ppString "SnapPredicateFailure has no constructors"
ppEpochPredicateFailure (UpecFailure x) = ppUpecPredicateFailure x

instance
  ( ShelleyEpochPredFailure era ~ PredicateFailure (EraRule "EPOCH" era),
    ShelleyUpecPredFailure era ~ PredicateFailure (EraRule "UPEC" era)
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
    [ ("The total outstanding deposits", ppCoin c1),
      ("The deposit pot", ppCoin c2)
    ]

instance PrettyA (ShelleyNewppPredFailure era) where prettyA = ppNewppPredicateFailure

-- ===================

ppBbodyState ::
  ( PrettyA (TxOut era),
    PrettyA (PParams era),
    PrettyA (State (EraRule "PPUP" era))
  ) =>
  ShelleyBbodyState era ->
  PDoc
ppBbodyState (BbodyState ls (BlocksMade mp)) =
  ppRecord
    "BbodyState"
    [ ("ledger state", ppLedgerState ls),
      ("blocks made", ppMap ppKeyHash ppNatural mp)
    ]

instance
  ( PrettyA (TxOut era),
    PrettyA (PParams era),
    PrettyA (State (EraRule "PPUP" era))
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
  (Wdrls x) -> [("Withdrawals", ppInt (Map.size (unWdrl x)))]
  (Vldt x) -> [("Validity interval", ppValidityInterval x)]
  (Txfee c) -> [("Fee", ppCoin c)]
  (Update (SJust _)) -> [("Collateral Return", ppString "?")]
  (ReqSignerHashes x) -> [("Required Signer hashes", ppInt (Set.size x))]
  (Mint v) -> [("Mint", ppInteger (Val.size v) <> ppString " bytes")]
  (WppHash (SJust _)) -> [("WppHash", ppString "?")]
  (AdHash (SJust _)) -> [("AdHash", ppString "?")]
  (Txnetworkid (SJust x)) -> [("Network id", ppNetwork x)]
  _ -> []

bodySummary :: EraTxBody era => Proof era -> TxBody era -> PDoc
bodySummary proof body =
  ppRecord
    "TxBody"
    (concat (map txBodyFieldSummary (abstractTxBody proof body)))

witnessFieldSummary :: WitnessesField era -> (Text, PDoc)
witnessFieldSummary wit = case wit of
  (AddrWits s) -> ("Address Witnesses", ppInt (Set.size s))
  (BootWits s) -> ("BootStrap Witnesses", ppInt (Set.size s))
  (ScriptWits s) -> ("Script Witnesses", ppInt (Map.size s))
  (DataWits m) -> ("Data Witnesses", ppInt (Map.size (unTxDats m)))
  (RdmrWits (Redeemers' m)) -> ("Redeemer Witnesses", ppInt (Map.size m))

witnessSummary :: Proof era -> Witnesses era -> PDoc
witnessSummary proof wits =
  ppRecord
    "Witnesses"
    (map witnessFieldSummary (abstractWitnesses proof wits))

txFieldSummary :: EraTxBody era => Proof era -> TxField era -> [PDoc]
txFieldSummary proof tx = case tx of
  (Body b) -> [bodySummary proof b]
  (BodyI xs) -> [ppRecord "TxBody" (concat (map txBodyFieldSummary xs))]
  (Witnesses ws) -> [witnessSummary proof ws]
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

datumSummary :: Datum era -> PDoc
datumSummary NoDatum = ppString "NoDatum"
datumSummary (DatumHash h) = ppSexp "DHash" [trim (ppSafeHash h)]
datumSummary (Datum b) = dataSummary (binaryDataToData b)

dataSummary :: Cardano.Ledger.Alonzo.Data.Data era -> PDoc
dataSummary (Data x) = plutusDataSummary x

plutusDataSummary :: Plutus.Data -> PDoc
plutusDataSummary (Plutus.Constr n ds) = (ppString (show n)) <> ppList plutusDataSummary ds
plutusDataSummary (Plutus.Map ds) = ppString "Map" <> ppList (ppPair plutusDataSummary plutusDataSummary) ds
plutusDataSummary (Plutus.List xs) = ppList plutusDataSummary xs
plutusDataSummary (Plutus.I n) = ppInteger n
plutusDataSummary (Plutus.B bs) = trim (ppLong bs)

vSummary :: MaryValue c -> PDoc
vSummary (MaryValue n (MultiAsset m)) =
  ppSexp "Value" [ppInteger n, ppString ("num tokens = " ++ show (Map.size m))]

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

addrSummary :: Addr crypto -> PDoc
addrSummary (Addr nw pay stk) =
  ppSexp "Addr" [networkSummary nw, credSummary pay, stakeSummary stk]
addrSummary (AddrBootstrap _) = ppString "Bootstrap"

credSummary :: Credential keyrole crypto -> PDoc
credSummary (ScriptHashObj (ScriptHash h)) = ppSexp "Script" [trim (ppHash h)]
credSummary (KeyHashObj (KeyHash kh)) = ppSexp "Key" [trim (ppHash kh)]

stakeSummary :: StakeReference crypto -> PDoc
stakeSummary StakeRefNull = ppString "Null"
stakeSummary (StakeRefPtr _) = ppString "Ptr"
stakeSummary (StakeRefBase x) = ppSexp "Stake" [credSummary (coerceKeyRole x)]

utxoSummary :: Proof era -> UTxO era -> PDoc
utxoSummary proof = ppMap txInSummary (txOutSummary proof) . unUTxO

utxoString :: Proof era -> UTxO era -> String
utxoString proof = show . ppMap txInSummary (txOutSummary proof) . unUTxO

scriptHashSummary :: ScriptHash crypto -> PDoc
scriptHashSummary (ScriptHash h) = trim (ppHash h)

keyHashSummary :: KeyHash keyrole crypto -> PDoc
keyHashSummary (KeyHash h) = trim (ppHash h)

dataHashSummary :: DataHash era -> PDoc
dataHashSummary dh = trim (ppSafeHash dh)

keyPairSummary :: CC.Crypto c => KeyPair r c -> PDoc
keyPairSummary (KeyPair x y) =
  ppRecord "KeyPair" [("vKey", vKeySummary x), ("sKey", (viaShow y))]

vKeySummary :: CC.Crypto c => VKey r c -> PDoc
vKeySummary vk@(VKey x) = (viaShow x) <> " (hash " <> keyHashSummary (hashKey vk) <> ")"

timelockSummary :: CC.Crypto crypto => Timelock crypto -> PDoc
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

multiSigSummary :: CC.Crypto crypto => SS.MultiSig crypto -> PDoc
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

dStateSummary :: DState crypto -> PDoc
dStateSummary (DState umap future (GenDelegs current) irwd) =
  ppRecord
    "DState"
    [ ("Unified Reward Map", uMapSummary umap),
      ("Future genesis key delegations", ppInt (Map.size future)),
      ("Genesis key delegations", ppInt (Map.size current)),
      ("Instantaneous Rewards", instantSummary irwd)
    ]

instantSummary :: InstantaneousRewards crypto -> PDoc
instantSummary (InstantaneousRewards reserves treasury dreserves dtreasury) =
  ppRecord
    "InstantaneousRewards"
    [ ("Rewards from reserves", ppInt (Map.size reserves)),
      ("Rewards from treasury", ppInt (Map.size treasury)),
      ("Treasury to reserves", ppDeltaCoin dreserves),
      ("Reserves to treasury", ppDeltaCoin dtreasury)
    ]

uMapSummary :: UnifiedMap crypto -> PDoc
uMapSummary umap =
  ppRecord
    "UMap"
    [ ("Reward Map", ppInt (UMap.size (UMap.Rewards umap))),
      ("Delegations Map", ppInt (UMap.size (UMap.Delegations umap))),
      ("Ptrs Map", ppInt (UMap.size (UMap.Ptrs umap)))
    ]

pStateSummary :: PState crypto -> PDoc
pStateSummary (PState pp fpp retire) =
  ppRecord
    "PState"
    [ ("Pool parameters", ppInt (Map.size pp)),
      ("Future pool parameters", ppInt (Map.size fpp)),
      ("Retiring stake pools", ppInt (Map.size retire))
    ]

dpStateSummary :: DPState crypto -> PDoc
dpStateSummary (DPState d p) = vsep [dStateSummary d, pStateSummary p]

-- =============================================

class PrettyC t era where
  prettyC :: Proof era -> t -> PDoc

pcTxId :: TxId crypto -> PDoc
pcTxId (TxId safehash) = trim (ppSafeHash safehash)

instance c ~ Crypto era => PrettyC (TxId c) era where prettyC _ = pcTxId

pcTxIn :: TxIn crypto -> PDoc
pcTxIn (TxIn (TxId h) (TxIx i)) = parens (hsep [ppString "TxIn", trim (ppSafeHash h), ppWord64 i])

instance c ~ Crypto era => PrettyC (TxIn c) era where prettyC _ = pcTxIn

pcNetwork :: Network -> PDoc
pcNetwork Testnet = ppString "TestNet"
pcNetwork Mainnet = ppString "Mainnet"

instance PrettyC Network era where prettyC _ = pcNetwork

pcKeyHash :: KeyHash discriminator crypto -> PDoc
pcKeyHash (KeyHash h) = trim (ppHash h)

instance c ~ Crypto era => PrettyC (KeyHash d c) era where prettyC _ = pcKeyHash

pcCredential :: Credential keyrole c -> PDoc
pcCredential (ScriptHashObj (ScriptHash h)) = hsep [ppString "Script", trim (ppHash h)]
pcCredential (KeyHashObj (KeyHash h)) = hsep [ppString "Key", trim (ppHash h)]

instance c ~ Crypto era => PrettyC (Credential keyrole c) era where prettyC _ = pcCredential

pcStakeReference :: StakeReference c -> PDoc
pcStakeReference StakeRefNull = ppString "Null"
pcStakeReference (StakeRefBase cred) = pcCredential cred
pcStakeReference (StakeRefPtr _) = ppString "Ptr"

instance c ~ Crypto era => PrettyC (StakeReference c) era where prettyC _ = pcStakeReference

pcAddr :: Addr c -> PDoc
pcAddr (Addr nw pay stk) =
  parens $
    hsep
      [ ppString "Addr",
        pcNetwork nw,
        pcCredential pay,
        pcStakeReference stk
      ]
pcAddr (AddrBootstrap _) = ppString "Bootstrap"

instance c ~ Crypto era => PrettyC (Addr c) era where prettyC _ = pcAddr

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
pcValue (MaryValue n (MultiAsset m)) = ppSexp "Value" [ppInteger n, ppString ("num tokens = " ++ show (Map.size m))]

instance c ~ Crypto era => PrettyC (MaryValue c) era where
  prettyC _ = pcValue

pcDatum :: Era era => Datum era -> PDoc
pcDatum NoDatum = ppString "NoDatum"
pcDatum (DatumHash h) = ppSexp "DHash" [trim (ppSafeHash h)]
pcDatum (Datum b) = pcData (binaryDataToData b)

instance Era era => PrettyC (Datum era) era where prettyC _ = pcDatum

pcData :: forall era. Era era => Data era -> PDoc
pcData d@(Data (Plutus.Constr n _)) =
  ppSexp (pack ("Constr" ++ show n)) [ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (Plutus.Map _)) =
  ppSexp "Map" [ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (Plutus.List _)) =
  ppSexp "List" [ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (Plutus.I n)) =
  ppSexp "I" [ppInteger n, ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (Plutus.B bytes)) =
  ppSexp "B" [trim (viaShow bytes), ppString "Hash", trim $ ppSafeHash (hashData d)]

instance Era era => PrettyC (Data era) era where prettyC _ = pcData

pcTimelock :: forall era. Reflect era => PDoc -> Timelock (Crypto era) -> PDoc
pcTimelock hash (RequireSignature akh) = ppSexp "Signature" [keyHashSummary akh, hash]
pcTimelock hash (RequireAllOf _ts) = ppSexp "AllOf" [hash]
pcTimelock hash (RequireAnyOf _) = ppSexp "AnyOf" [hash]
pcTimelock hash (RequireMOf m _) = ppSexp "MOfN" [ppInteger (fromIntegral m), hash]
pcTimelock hash (RequireTimeExpire mslot) = ppSexp "Expires" [ppSlotNo mslot, hash]
pcTimelock hash (RequireTimeStart mslot) = ppSexp "Starts" [ppSlotNo mslot, hash]

pcMultiSig :: Reflect era => PDoc -> SS.MultiSig (Crypto era) -> PDoc
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
pcScript p@(Conway _) s@(TimelockScript t) = pcTimelock @era (pcHashScript @era p s) t
pcScript p@(Conway _) s@(PlutusScript v _) =
  parens (hsep [ppString ("PlutusScript " <> show v <> " "), pcHashScript p s])
pcScript p@(Babbage _) s@(TimelockScript t) = pcTimelock @era (pcHashScript @era p s) t
pcScript p@(Babbage _) s@(PlutusScript v _) =
  parens (hsep [ppString ("PlutusScript " <> show v <> " "), pcHashScript p s])
pcScript p@(Alonzo _) s@(TimelockScript t) = pcTimelock @era (pcHashScript @era p s) t
pcScript p@(Alonzo _) s@(PlutusScript v _) =
  parens (hsep [ppString ("PlutusScript " <> show v <> " "), pcHashScript p s])
pcScript p@(Mary _) s = pcTimelock @era (pcHashScript @era p s) s
pcScript p@(Allegra _) s = pcTimelock @era (pcHashScript @era p s) s
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

pcIndividualPoolStake :: IndividualPoolStake crypto -> PDoc
pcIndividualPoolStake (IndividualPoolStake rat vrf) =
  ppRecord
    "pool stake"
    [("ratio", ppRational rat), ("vrf", trim (ppHash vrf))]

instance c ~ Crypto era => PrettyC (IndividualPoolStake c) era where prettyC _ = pcIndividualPoolStake

pcPoolParams :: PoolParams era -> PDoc
pcPoolParams x =
  ppRecord
    "PoolParams"
    [ ("Id", keyHashSummary (_poolId x)),
      ("reward accnt", pcCredential (getRwdCred (_poolRAcnt x)))
    ]

instance PrettyC (PoolParams era) era where prettyC _ = pcPoolParams

pcDelegCert :: DelegCert crypto -> PDoc
pcDelegCert (RegKey cred) = ppSexp "RegKey" [pcCredential cred]
pcDelegCert (DeRegKey cred) = ppSexp "DeRegKey" [pcCredential cred]
pcDelegCert (Delegate (Delegation x y)) = ppSexp "Delegate" [pcCredential x, pcKeyHash y]

instance c ~ Crypto era => PrettyC (DelegCert c) era where prettyC _ = pcDelegCert

pcPoolCert :: PoolCert crypto -> PDoc
pcPoolCert (RegPool poolp) = ppSexp "RegPool" [pcPoolParams poolp]
pcPoolCert (RetirePool keyhash epoch) = ppSexp "RetirePool" [pcKeyHash keyhash, ppEpochNo epoch]

instance c ~ Crypto era => PrettyC (PoolCert c) era where prettyC _ = pcPoolCert

pcDCert :: DCert crypto -> PDoc
pcDCert (DCertDeleg x) = pcDelegCert x
pcDCert (DCertPool x) = pcPoolCert x
pcDCert (DCertGenesis _) = ppString "GenesisCert"
pcDCert (DCertMir _) = ppString "MirCert"

instance c ~ Crypto era => PrettyC (DCert c) era where prettyC _ = pcDCert

pcRewardAcnt :: RewardAcnt crypto -> PDoc
pcRewardAcnt (RewardAcnt net cred) = ppSexp "RewAccnt" [pcNetwork net, pcCredential cred]

instance c ~ Crypto era => PrettyC (RewardAcnt c) era where prettyC _ = pcRewardAcnt

pcExUnits :: ExUnits -> PDoc
pcExUnits (ExUnits mem step) =
  ppSexp "ExUnits" [ppNatural mem, ppNatural step]

pcTxBodyField :: Reflect era => Proof era -> TxBodyField era -> [(Text, PDoc)]
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
  Wdrls (Wdrl m) -> [("withdrawal", ppMap pcRewardAcnt pcCoin m)]
  Txfee c -> [("fee", pcCoin c)]
  Vldt v -> [("validity interval", ppValidityInterval v)]
  TTL slot -> [("time to live", ppSlotNo slot)]
  Update SNothing -> []
  Update (SJust _) -> [("update", ppString "UPDATE")]
  ReqSignerHashes s -> [("required hashes", ppSet pcKeyHash s)]
  Mint v -> [("minted", pcCoreValue proof v)]
  WppHash SNothing -> []
  WppHash (SJust h) -> [("integrity hash", trim (ppSafeHash h))]
  AdHash SNothing -> []
  AdHash (SJust (AuxiliaryDataHash h)) -> [("aux data hash", trim (ppSafeHash h))]
  Txnetworkid SNothing -> []
  Txnetworkid (SJust nid) -> [("network id", pcNetwork nid)]

pcTxField :: forall era. Reflect era => Proof era -> TxField era -> [(Text, PDoc)]
pcTxField proof x = case x of
  Body b -> [("txbody hash", ppSafeHash (hashAnnotated @(Crypto era) @EraIndependentTxBody b)), ("body", pcTxBody proof b)]
  BodyI xs -> [("body", ppRecord "TxBody" (concat (map (pcTxBodyField proof) xs)))]
  Witnesses w -> [("witnesses", pcWitnesses proof w)]
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

pcWitVKey :: (Reflect era, Typeable discriminator) => WitVKey discriminator (Crypto era) -> PDoc
pcWitVKey (WitVKey vk@(VKey x) sig) = ppSexp "WitVKey" [ppString keystring, ppString (drop 12 sigstring), hash]
  where
    keystring = show x
    hash = pcKeyHash (hashKey vk)
    sigstring = show sig

pcWitnesses :: Reflect era => Proof era -> Witnesses era -> PDoc
pcWitnesses proof wits = ppRecord "Witnesses" pairs
  where
    fields = abstractWitnesses proof wits
    pairs = concat (map (pcWitnessesField proof) fields)

pcTx :: Reflect era => Proof era -> Tx era -> PDoc
pcTx proof tx = ppRecord "Tx" pairs
  where
    fields = abstractTx proof tx
    pairs = concat (map (pcTxField proof) fields)

pcTxBody :: Reflect era => Proof era -> TxBody era -> PDoc
pcTxBody proof txbody = ppRecord "TxBody" pairs
  where
    fields = abstractTxBody proof txbody
    pairs = concat (map (pcTxBodyField proof) fields)

pcUTxOState :: Reflect era => Proof era -> UTxOState era -> PDoc
pcUTxOState proof (UTxOState u dep fees _pups _stakedistro) =
  ppRecord
    "UTxOState"
    [ ("utxo", pcUTxO proof u),
      ("deposited", pcCoin dep),
      ("fees", pcCoin fees),
      ("ppups", ppString "PPUP"),
      ("stake distr", ppString "Stake Distr")
    ]

instance Reflect era => PrettyC (UTxOState era) era where prettyC = pcUTxOState

pcDPState :: p -> DPState era -> PDoc
pcDPState _proof (DPState (DState {_unified = un}) (PState {_pParams = pool})) =
  ppRecord
    "DPState summary"
    [ ("rewards", ppMap pcCredential pcCoin (UMap.rewView un)),
      ("delegations", ppMap pcCredential keyHashSummary (UMap.delView un)),
      ("pool params", ppMap pcKeyHash pcPoolParams pool)
    ]

instance PrettyC (DPState era) era where prettyC = pcDPState

pcLedgerState :: Reflect era => Proof era -> LedgerState era -> PDoc
pcLedgerState proof (LedgerState utstate dpstate) =
  ppRecord
    "LedgerState"
    [ ("UTxOState", pcUTxOState proof utstate),
      ("DPState", pcDPState proof dpstate)
    ]

instance Reflect era => PrettyC (LedgerState era) era where prettyC = pcLedgerState

pcNewEpochState :: Reflect era => Proof era -> NewEpochState era -> PDoc
pcNewEpochState proof (NewEpochState en (BlocksMade pbm) (BlocksMade cbm) es _ (PoolDistr pd) _) =
  ppRecord
    "NewEpochState"
    [ ("EpochNo", ppEpochNo en),
      ("EpochState", pcEpochState proof es),
      ("PoolDistr", ppMap pcKeyHash pcIndividualPoolStake pd),
      ("Prev Blocks", ppMap pcKeyHash ppNatural pbm),
      ("Current Blocks", ppMap pcKeyHash ppNatural cbm)
    ]

instance Reflect era => PrettyC (NewEpochState era) era where prettyC = pcNewEpochState

pcEpochState :: Reflect era => Proof era -> EpochState era -> PDoc
pcEpochState proof (EpochState (AccountState tre res) _ ls _ _ _) =
  ppRecord
    "EpochState"
    [ ("AccountState", ppRecord' "" [("treasury", pcCoin tre), ("reserves", pcCoin res)]),
      ("LedgerState", pcLedgerState proof ls)
    ]

instance Reflect era => PrettyC (EpochState era) era where prettyC = pcEpochState

pc :: PrettyC t era => Proof era -> t -> IO ()
pc proof x = putStrLn (show (prettyC proof x))

-- ===================================================
