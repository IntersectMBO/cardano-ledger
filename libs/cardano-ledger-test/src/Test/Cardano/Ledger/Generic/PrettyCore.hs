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

-- import Cardano.Ledger.Babbage(BabbageEra)

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
-- ------------------------------
-- Predicatefailures

-- import qualified  Cardano.Ledger.Shelley.Rules.Utxo as Shelley(UtxoPredicateFailure(..))

import Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError (..))
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo (UtxoPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (TagMismatchDescription (..), UtxosPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail (..))
-- -------------
-- Specific types

-- import Cardano.Ledger.Alonzo.TxWitness(TxWitness (..))

import Cardano.Ledger.Alonzo.Scripts (Script (..))
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (..))
import Cardano.Ledger.Alonzo.TxInfo (FailureDescription (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys (hashKey)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Pretty
import Cardano.Ledger.Pretty.Alonzo
import Cardano.Ledger.Pretty.Mary
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (WitHashes (..))
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (..))
import qualified Cardano.Ledger.Shelley.Rules.Ppup as Shelley (PpupPredicateFailure (..))
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley (UtxoPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (..))
import Cardano.Ledger.Shelley.TxBody (WitVKey (..))
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as Mary (UtxoPredicateFailure (..))
import Control.State.Transition.Extended (PredicateFailure)
import Data.Typeable (Typeable)
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

ppAlonzoPredFail ::
  ( PrettyA (PredicateFailure (Core.EraRule "UTXO" era)),
    PrettyCore era
  ) =>
  AlonzoPredFail era ->
  PDoc
ppAlonzoPredFail (WrappedShelleyEraFailure x) = prettyA x
ppAlonzoPredFail (MissingRedeemers xs) =
  ppSexp "MissingRedeemers" [ppList (ppPair ppScriptPurpose ppScriptHash) xs]
ppAlonzoPredFail (MissingRequiredDatums s1 s2) =
  ppRecord
    "MissingRequiredDatums"
    [ ("missing data hashes", ppSet ppSafeHash s1),
      ("received data hashes", ppSet ppSafeHash s2)
    ]
ppAlonzoPredFail (NonOutputSupplimentaryDatums s1 s2) =
  ppRecord
    "NonOutputSupplimentaryDatums"
    [ ("unallowed data hashes", ppSet ppSafeHash s1),
      ("acceptable data hashes", ppSet ppSafeHash s2)
    ]
ppAlonzoPredFail (PPViewHashesDontMatch h1 h2) =
  ppRecord
    "NonOutputSupplimentaryDatums"
    [ ("PPHash in the TxBody", ppStrictMaybe ppSafeHash h1),
      ("PPHash Computed from the current Protocol Parameters", ppStrictMaybe ppSafeHash h2)
    ]
ppAlonzoPredFail (MissingRequiredSigners x) =
  ppSexp "MissingRequiredSigners" [ppSet ppKeyHash x]
ppAlonzoPredFail (UnspendableUTxONoDatumHash x) =
  ppSexp "UnspendableUTxONoDatumHash" [ppSet ppTxIn x]
ppAlonzoPredFail (ExtraRedeemers x) =
  ppSexp "ExtraRedeemers" [ppList prettyA x]

instance
  ( PrettyA (PredicateFailure (Core.EraRule "UTXO" era)),
    PrettyCore era
  ) =>
  PrettyA (AlonzoPredFail era)
  where
  prettyA = ppAlonzoPredFail

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
ppUtxoPredicateFailure (Alonzo.UtxosFailure subpred) =
  ppSexp "UtxosFailure" [prettyA subpred]
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
  ppRecord "CollectErrors" [("When collecting inputs for twophase scripts, these went wrong.", ppList ppCollectError es)]
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
