{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- PrettyA (PredicateFailure (Core.EraRule "UTXO" era))
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Generic.PrettyCore where

-- ------------------------------
-- Predicatefailures

-- import qualified  Cardano.Ledger.Shelley.Rules.Utxo as Shelley(UtxoPredicateFailure(..))

-- -------------
-- Specific types

-- import Cardano.Ledger.Alonzo.TxWitness(TxWitness (..))

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (..), binaryDataToData)
import Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError (..))
import Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBbodyPredFail (..))
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo (UtxoPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (TagMismatchDescription (..), UtxosPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (..))
import Cardano.Ledger.Alonzo.Scripts (Script (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptPurpose (..))
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (FailureDescription (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), unTxDats)
import Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.BaseTypes (BlocksMade (..), Network (..), txIxToInt)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), PaymentCredential, StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (..), hashScript)
import Cardano.Ledger.Hashes (DataHash, ScriptHash (..))
import Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole), KeyHash (..), hashKey)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (Value (..))
import Cardano.Ledger.Pretty
import Cardano.Ledger.Pretty.Alonzo
import qualified Cardano.Ledger.Pretty.Babbage as Babbage
import Cardano.Ledger.Pretty.Mary
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (WitHashes (..))
import Cardano.Ledger.Shelley.Rules.Bbody (BbodyPredicateFailure (..), BbodyState (..))
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Ledgers (LedgersPredicateFailure (..))
import qualified Cardano.Ledger.Shelley.Rules.Ppup as Shelley (PpupPredicateFailure (..))
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley (UtxoPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (..))
import Cardano.Ledger.Shelley.TxBody (WitVKey (..), unWdrl)
import qualified Cardano.Ledger.Shelley.TxBody as Shelley (TxOut (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as Mary (UtxoPredicateFailure (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition.Extended (STS (..))
import qualified Data.Compact.SplitMap as Split
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable (Typeable)
import PlutusCore.Data (Data (..))
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
  prettyTx :: Core.Tx era -> PDoc
  prettyScript :: Core.Script era -> PDoc
  prettyTxBody :: Core.TxBody era -> PDoc
  prettyWitnesses :: Core.Witnesses era -> PDoc
  prettyValue :: Core.Value era -> PDoc
  prettyTxOut :: Core.TxOut era -> PDoc

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

prettyUTxO :: Proof era -> UTxO era -> PDoc
prettyUTxO (Babbage _) (UTxO mp) = ppMap ppTxIn prettyTxOut (Split.toMap mp)
prettyUTxO (Alonzo _) (UTxO mp) = ppMap ppTxIn prettyTxOut (Split.toMap mp)
prettyUTxO (Mary _) (UTxO mp) = ppMap ppTxIn prettyTxOut (Split.toMap mp)
prettyUTxO (Allegra _) (UTxO mp) = ppMap ppTxIn prettyTxOut (Split.toMap mp)
prettyUTxO (Shelley _) (UTxO mp) = ppMap ppTxIn prettyTxOut (Split.toMap mp)

-- ===================================================================
-- PrettyA instances for UTXOW, UTXO, UTXOS, PPUP predicate failures
-- There are sometimes two versions:
-- one introduced in Shelley, one introduced in Alonzo
-- ===================================================================

-- Predicate Failure for LEDGER

ppLedgerPredicateFailure ::
  ( PrettyA (PredicateFailure (Core.EraRule "UTXOW" era)),
    Show (PredicateFailure (Core.EraRule "DELEGS" era))
  ) =>
  LedgerPredicateFailure era ->
  PDoc
ppLedgerPredicateFailure (UtxowFailure x) = prettyA x
ppLedgerPredicateFailure (DelegsFailure x) = ppString (show x)

instance
  ( PrettyA (PredicateFailure (Core.EraRule "UTXOW" era)),
    Show (PredicateFailure (Core.EraRule "DELEGS" era))
  ) =>
  PrettyA (LedgerPredicateFailure era)
  where
  prettyA = ppLedgerPredicateFailure

instance
  ( PrettyA (PredicateFailure (Core.EraRule "UTXOW" era)),
    Show (PredicateFailure (Core.EraRule "DELEGS" era))
  ) =>
  PrettyA [LedgerPredicateFailure era]
  where
  prettyA = ppList prettyA

-- =========================================
-- Predicate Failure for Alonzo UTXOW

ppUtxowPredicateFail ::
  ( PrettyA (PredicateFailure (Core.EraRule "UTXO" era)),
    PrettyCore era
  ) =>
  UtxowPredicateFail era ->
  PDoc
ppUtxowPredicateFail (WrappedShelleyEraFailure x) = prettyA x
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
  ( PrettyA (PredicateFailure (Core.EraRule "UTXO" era)),
    PrettyCore era
  ) =>
  PrettyA (UtxowPredicateFail era)
  where
  prettyA = ppUtxowPredicateFail

-- ====================================================
-- Predicate Failure for Shelley UTXOW

ppUtxowPredicateFailure ::
  ( PrettyA (PredicateFailure (Core.EraRule "UTXO" era)),
    PrettyCore era
  ) =>
  UtxowPredicateFailure era ->
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
  ppRecord "ConflictingMetadataHash" [("Hash in the body", ppAuxiliaryDataHash h1), ("Hash of full metadata", ppAuxiliaryDataHash h2)]
ppUtxowPredicateFailure (InvalidMetadata) =
  ppSexp "InvalidMetadata" []
ppUtxowPredicateFailure (ExtraneousScriptWitnessesUTXOW m) =
  ppSexp "ExtraneousScriptWitnessesUTXOW" [ppSet ppScriptHash m]

instance
  ( PrettyA (PredicateFailure (Core.EraRule "UTXO" era)),
    PrettyCore era
  ) =>
  PrettyA (UtxowPredicateFailure era)
  where
  prettyA = ppUtxowPredicateFailure

-- ========================================================
-- Predicate Failure for Alonzo UTXO

ppUtxoPredicateFailure ::
  forall era.
  ( PrettyCore era,
    PrettyA (PredicateFailure (Core.EraRule "UTXOS" era)),
    PrettyA (Core.TxOut era) -- From ppUTxO FIXME
  ) =>
  Alonzo.UtxoPredicateFailure era ->
  PDoc
ppUtxoPredicateFailure (Alonzo.BadInputsUTxO x) =
  ppSexp "BadInputsUTxO" [ppSet ppTxIn x]
ppUtxoPredicateFailure (Alonzo.OutsideValidityIntervalUTxO vi slot) =
  ppRecord "OutsideValidityIntervalUTxO" [("validity interval", ppValidityInterval vi), ("slot", ppSlotNo slot)]
ppUtxoPredicateFailure (Alonzo.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", ppInteger actual),
      ("max transaction size", ppInteger maxs)
    ]
ppUtxoPredicateFailure (Alonzo.InputSetEmptyUTxO) =
  ppSexp "InputSetEmptyUTxO" []
ppUtxoPredicateFailure (Alonzo.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for thistransaction", ppCoin computed),
      ("fee supplied by transaction", ppCoin supplied)
    ]
ppUtxoPredicateFailure (Alonzo.ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", prettyValue @era consumed),
      ("coin produced", prettyValue @era produced)
    ]
ppUtxoPredicateFailure (Alonzo.WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", ppNetwork n),
      ("set addresses with wrong network id", ppSet ppAddr add)
    ]
ppUtxoPredicateFailure (Alonzo.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", ppNetwork n),
      ("set reward address with wrong network id", ppSet ppRewardAcnt accnt)
    ]
ppUtxoPredicateFailure (Alonzo.OutputTooSmallUTxO xs) =
  ppSexp "OutputTooSmallUTxO" [ppList prettyTxOut xs]
ppUtxoPredicateFailure (Alonzo.UtxosFailure subpred) = prettyA subpred
-- ppSexp "UtxosFailure" [prettyA subpred]
ppUtxoPredicateFailure (Alonzo.OutputBootAddrAttrsTooBig x) =
  ppSexp "OutputBootAddrAttrsTooBig" [ppList prettyTxOut x]
ppUtxoPredicateFailure (Alonzo.TriesToForgeADA) =
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
ppUtxoPredicateFailure (Alonzo.InsufficientCollateral x y) =
  ppRecord
    "InsufficientCollateral"
    [ ("balance computed", ppCoin x),
      ("the required collateral for the given fee", ppCoin y)
    ]
ppUtxoPredicateFailure (Alonzo.ScriptsNotPaidUTxO x) =
  ppSexp "ScriptsNotPaidUTxO" [ppUTxO x]
ppUtxoPredicateFailure (Alonzo.ExUnitsTooBigUTxO x y) =
  ppRecord
    "ExUnitsTooBigUTxO"
    [ ("Max EXUnits from the protocol parameters", ppExUnits x),
      ("EXUnits supplied", ppExUnits y)
    ]
ppUtxoPredicateFailure (Alonzo.CollateralContainsNonADA x) =
  ppSexp "CollateralContainsNonADA" [prettyValue @era x]
ppUtxoPredicateFailure (Alonzo.WrongNetworkInTxBody x y) =
  ppRecord
    "WrongNetworkInTxBody"
    [ ("Actual Network ID", ppNetwork x),
      ("Network ID in transaction body", ppNetwork y)
    ]
ppUtxoPredicateFailure (Alonzo.OutsideForecast x) =
  ppRecord "OutsideForecast" [("slot number outside consensus forecast range", ppSlotNo x)]
ppUtxoPredicateFailure (Alonzo.TooManyCollateralInputs x y) =
  ppRecord
    "TooManyCollateralInputs"
    [ ("Max allowed collateral inputs", ppNatural x),
      ("Number of collateral inputs", ppNatural y)
    ]
ppUtxoPredicateFailure (Alonzo.NoCollateralInputs) =
  ppSexp "NoCollateralInputs" []
ppUtxoPredicateFailure (Alonzo.TotalCollateralInequality x y) =
  ppRecord
    "TotalCollateralInequality"
    [ ("balance computed", ppCoin x),
      ("the asserted total collateral", ppCoin y)
    ]

instance
  ( PrettyCore era,
    PrettyA (PredicateFailure (Core.EraRule "UTXOS" era)),
    PrettyA (Core.TxOut era) -- From ppUTxO FIXME
  ) =>
  PrettyA (Alonzo.UtxoPredicateFailure era)
  where
  prettyA = ppUtxoPredicateFailure

-- =========================================
-- Predicate Failure for Alonzo UTXOS

ppUtxosPredicateFailure ::
  PrettyA (PredicateFailure (Core.EraRule "PPUP" era)) =>
  UtxosPredicateFailure era ->
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

instance PrettyA (PredicateFailure (Core.EraRule "PPUP" era)) => PrettyA (UtxosPredicateFailure era) where
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
  ppSexp "FailedUnexpectedly" [ppList ppFailureDescription xs]

instance PrettyA TagMismatchDescription where
  prettyA = ppTagMismatchDescription

ppFailureDescription :: FailureDescription -> PDoc
ppFailureDescription (OnePhaseFailure txt) =
  ppSexp "OnePhaseFailure" [text txt]
ppFailureDescription (PlutusFailure txt bytes) =
  ppRecord "PlutusFailure" [("reason", text txt), ("script", ppLong bytes)]

instance PrettyA FailureDescription where
  prettyA = ppFailureDescription

-- =======================================
-- Predicate Failure for Shelley UTxO

ppUtxoPFShelley ::
  forall era.
  ( PrettyCore era,
    PrettyA (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Shelley.UtxoPredicateFailure era ->
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
    PrettyA (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  PrettyA (Shelley.UtxoPredicateFailure era)
  where
  prettyA = ppUtxoPFShelley

-- =======================================
-- Predicate Failure for Shelley PPUP

ppPpupPredicateFailure :: Shelley.PpupPredicateFailure era -> PDoc
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

instance PrettyA (Shelley.PpupPredicateFailure era) where
  prettyA = ppPpupPredicateFailure

-- =====================================================
-- Predicate failure for Mary UTXO

ppUtxoPFMary ::
  forall era.
  ( PrettyCore era,
    PrettyA (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Mary.UtxoPredicateFailure era ->
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
    PrettyA (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  PrettyA (Mary.UtxoPredicateFailure era)
  where
  prettyA = ppUtxoPFMary

-- =====================================================
-- Probably should be moved elsewhere

-- LedgerState.hs
ppWitHashes :: WitHashes crypto -> PDoc
ppWitHashes (WitHashes hs) = ppSexp "WitHashes" [ppSet ppKeyHash hs]

instance PrettyA (WitHashes crypto) where
  prettyA = ppWitHashes

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

ppCoreWitnesses :: Proof era -> Core.Witnesses era -> PDoc
ppCoreWitnesses (Alonzo _) x = ppTxWitness x
ppCoreWitnesses (Babbage _) x = ppTxWitness x
ppCoreWitnesses (Mary _) x = ppWitnessSetHKD x
ppCoreWitnesses (Allegra _) x = ppWitnessSetHKD x
ppCoreWitnesses (Shelley _) x = ppWitnessSetHKD x

ppCoreScript :: Proof era -> Core.Script era -> PDoc
ppCoreScript (Babbage _) (PlutusScript _ x) = ppString (show x)
ppCoreScript (Babbage _) (TimelockScript x) = ppTimelock x
ppCoreScript (Alonzo _) (PlutusScript _ x) = ppString (show x)
ppCoreScript (Alonzo _) (TimelockScript x) = ppTimelock x
ppCoreScript (Mary _) x = ppTimelock x
ppCoreScript (Allegra _) x = ppTimelock x
ppCoreScript (Shelley _) x = ppMultiSig x

-- =======================================================

ppLedgersPredicateFailure ::
  PrettyA (PredicateFailure (Core.EraRule "LEDGER" era)) => LedgersPredicateFailure era -> PDoc
ppLedgersPredicateFailure (LedgerFailure x) = ppSexp "LedgerFailure" [prettyA x]

instance
  PrettyA (PredicateFailure (Core.EraRule "LEDGER" era)) =>
  PrettyA (LedgersPredicateFailure era)
  where
  prettyA = ppLedgersPredicateFailure

-- ================
ppBbodyPredicateFailure ::
  PrettyA (PredicateFailure (Core.EraRule "LEDGERS" era)) =>
  BbodyPredicateFailure era ->
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
  PrettyA (PredicateFailure (Core.EraRule "LEDGERS" era)) =>
  PrettyA (BbodyPredicateFailure era)
  where
  prettyA = ppBbodyPredicateFailure

-- ================

ppAlonzoBbodyPredFail ::
  PrettyA (PredicateFailure (Core.EraRule "LEDGERS" era)) =>
  AlonzoBbodyPredFail era ->
  PDoc
ppAlonzoBbodyPredFail (ShelleyInAlonzoPredFail x) =
  ppSexp "ShelleyInAlonzoPredFail" [ppBbodyPredicateFailure x]
ppAlonzoBbodyPredFail (TooManyExUnits e1 e2) =
  ppRecord
    "TooManyExUnits"
    [ ("Computed Sum of ExUnits for all plutus scripts", ppExUnits e1),
      ("Maximum allowed by protocal parameters", ppExUnits e2)
    ]

instance
  PrettyA (PredicateFailure (Core.EraRule "LEDGERS" era)) =>
  PrettyA (AlonzoBbodyPredFail era)
  where
  prettyA = ppAlonzoBbodyPredFail

-- ===================

ppBbodyState ::
  ( PrettyA (Core.TxOut era),
    PrettyA (Core.PParams era),
    PrettyA (State (Core.EraRule "PPUP" era))
  ) =>
  BbodyState era ->
  PDoc
ppBbodyState (BbodyState ls (BlocksMade mp)) =
  ppRecord
    "BbodyState"
    [ ("ledger state", ppLedgerState ls),
      ("blocks made", ppMap ppKeyHash ppNatural mp)
    ]

instance
  ( PrettyA (Core.TxOut era),
    PrettyA (Core.PParams era),
    PrettyA (State (Core.EraRule "PPUP" era))
  ) =>
  PrettyA (BbodyState era)
  where
  prettyA = ppBbodyState

-- =======================================================
-- Summaries

txBodyFieldSummary :: Era era => TxBodyField era -> [(Text, PDoc)]
txBodyFieldSummary txb = case txb of
  (Inputs s) -> [("Inputs", ppInt (Set.size s))]
  (Collateral s) -> [("Collateral", ppInt (Set.size s))]
  (RefInputs s) -> [("RefInputs", ppInt (Set.size s))]
  (Outputs xs) -> [("Outputs", ppInt (length xs))]
  (CollateralReturn (SJust _)) -> [("Collateral Return", ppString "?")]
  (TotalCol c) -> [("TotalCollateral", ppCoin c)]
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

bodySummary :: Era era => Proof era -> Core.TxBody era -> PDoc
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

witnessSummary :: Proof era -> Core.Witnesses era -> PDoc
witnessSummary proof wits =
  ppRecord
    "Witnesses"
    (map witnessFieldSummary (abstractWitnesses proof wits))

txFieldSummary :: Era era => Proof era -> TxField era -> [PDoc]
txFieldSummary proof tx = case tx of
  (Body b) -> [bodySummary proof b]
  (BodyI xs) -> [ppRecord "TxBody" (concat (map txBodyFieldSummary xs))]
  (Witnesses ws) -> [witnessSummary proof ws]
  (WitnessesI ws) -> [ppRecord "Witnesses" (map witnessFieldSummary ws)]
  (AuxData (SJust _)) -> [ppSexp "AuxData" [ppString "?"]]
  (Valid (IsValid b)) -> [ppSexp "IsValid" [ppBool b]]
  _ -> []

txSummary :: Era era => Proof era -> Core.Tx era -> PDoc
txSummary proof tx =
  ppSexp "Tx" (concat (map (txFieldSummary proof) (abstractTx proof tx)))

-- =================================
-- Summary version of UTxO

trim :: PDoc -> PDoc
trim x = ppString (take 10 (show x))

txInSummary :: TxIn era -> PDoc
txInSummary (TxIn (TxId h) n) = ppSexp "TxIn" [trim (ppSafeHash h), ppInt (txIxToInt n)]

txOutSummary :: Proof era -> Core.TxOut era -> PDoc
txOutSummary p@(Babbage _) (Babbage.TxOut addr v d s) =
  ppSexp "TxOut" [addrSummary addr, valueSummary p v, datumSummary d, ppStrictMaybe (scriptSummary p) s]
txOutSummary p@(Alonzo _) (Alonzo.TxOut addr v md) =
  ppSexp "TxOut" [addrSummary addr, valueSummary p v, ppStrictMaybe dataHashSummary md]
txOutSummary p@(Mary _) (Shelley.TxOut addr v) = ppSexp "TxOut" [addrSummary addr, valueSummary p v]
txOutSummary p@(Allegra _) (Shelley.TxOut addr v) = ppSexp "TxOut" [addrSummary addr, valueSummary p v]
txOutSummary p@(Shelley _) (Shelley.TxOut addr v) = ppSexp "TxOut" [addrSummary addr, valueSummary p v]

datumSummary :: Babbage.Datum era -> PDoc
datumSummary Babbage.NoDatum = ppString "NoDatum"
datumSummary (Babbage.DatumHash h) = ppSexp "DHash" [trim (ppSafeHash h)]
datumSummary (Babbage.Datum b) = dataSummary (binaryDataToData b)

dataHashSummary :: DataHash era -> PDoc
dataHashSummary dh = trim (ppSafeHash dh)

dataSummary :: Cardano.Ledger.Alonzo.Data.Data era -> PDoc
dataSummary (Data x) = plutusDataSummary x

plutusDataSummary :: PlutusCore.Data.Data -> PDoc
plutusDataSummary (Constr n _) = ppString ("Constr-" ++ show n)
plutusDataSummary (Map _) = ppString "Map"
plutusDataSummary (List xs) = ppList plutusDataSummary xs
plutusDataSummary (I n) = ppInteger n
plutusDataSummary (B bs) = trim (ppLong bs)

vSummary :: Value c -> PDoc
vSummary (Value n m) = ppSexp "Value" [ppInteger n, ppString ("num tokens = " ++ show (Map.size m))]

valueSummary :: Proof era -> Core.Value era -> PDoc
valueSummary (Babbage _) v = vSummary v
valueSummary (Alonzo _) v = vSummary v
valueSummary (Mary _) v = vSummary v
valueSummary (Allegra _) c = ppCoin c
valueSummary (Shelley _) c = ppCoin c

scriptSummary :: forall era. Proof era -> Core.Script era -> PDoc
scriptSummary (Babbage _) script = scriptHashSummary (hashScript @era script)
scriptSummary (Alonzo _) script = scriptHashSummary (hashScript @era script)
scriptSummary (Mary _) script = scriptHashSummary (hashScript @era script)
scriptSummary (Allegra _) script = scriptHashSummary (hashScript @era script)
scriptSummary (Shelley _) script = scriptHashSummary (hashScript @era script)

networkSummary :: Network -> PDoc
networkSummary Testnet = ppString "Test"
networkSummary Mainnet = ppString "Main"

addrSummary :: Addr crypto -> PDoc
addrSummary (Addr nw pay stk) =
  ppSexp "Addr" [networkSummary nw, credSummary pay, stakeSummary stk]
addrSummary (AddrBootstrap _) = ppString "Bootstrap"

credSummary :: PaymentCredential crypto -> PDoc
credSummary (ScriptHashObj (ScriptHash h)) = ppSexp "Script" [trim (ppHash h)]
credSummary (KeyHashObj (KeyHash kh)) = ppSexp "Key" [trim (ppHash kh)]

stakeSummary :: StakeReference crypto -> PDoc
stakeSummary StakeRefNull = ppString "Null"
stakeSummary (StakeRefPtr _) = ppString "Ptr"
stakeSummary (StakeRefBase x) = ppSexp "Stake" [credSummary (coerceKeyRole x)]

utxoSummary :: Proof era -> UTxO era -> PDoc
utxoSummary proof (UTxO mp) = ppMap txInSummary (txOutSummary proof) (Split.toMap mp)

utxoString :: Proof era -> UTxO era -> String
utxoString proof (UTxO mp) = show (ppMap txInSummary (txOutSummary proof) (Split.toMap mp))

scriptHashSummary :: ScriptHash crypto -> PDoc
scriptHashSummary (ScriptHash h) = trim (ppHash h)
