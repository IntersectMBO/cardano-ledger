{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxow (
  DijkstraUTXOW,
  DijkstraUtxowPredFailure (..),
  DijkstraUtxoEnv (..),
  conwayToDijkstraUtxowPredFailure,
) where

import Cardano.Crypto.Hash (ByteString)
import Cardano.Ledger.Allegra.Rules (
  AllegraUtxoPredFailure,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent (WrappedShelleyEraEvent),
  AlonzoUtxowPredFailure,
  checkScriptIntegrityHash,
  hasExactSetOfRedeemers,
  missingRequiredDatums,
 )
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded (..),
 )
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
  babbageMissingScripts,
  validateFailedBabbageScripts,
  validateScriptsWellFormed,
 )
import Cardano.Ledger.Babbage.Tx (mkScriptIntegrity)
import Cardano.Ledger.Babbage.UTxO (getReferenceScripts)
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Relation (..),
  ShelleyBase,
  SlotNo,
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
  encodeWord,
  invalidKey,
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayUtxoPredFailure,
  ConwayUtxosPredFailure,
  ConwayUtxowPredFailure,
  alonzoToConwayUtxowPredFailure,
  babbageToConwayUtxowPredFailure,
  shelleyToConwayUtxowPredFailure,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraUTXO, DijkstraUTXOW)
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Keys (VKey)
import Cardano.Ledger.Rules.ValidationMode (runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxO, UTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
  validateNeededWitnesses,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  validateMetadata,
  validateVerifiedWits,
 )
import Cardano.Ledger.State (CertState, EraUTxO (..), ScriptsProvided (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  failureOnNonEmptySet,
  judgmentContext,
  trans,
 )
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

-- ================================

-- Unlike the standard 'UtxoEnv', this carries the original UTxO (before
-- subtransaction processing) and the aggregated scripts provided across
-- all transaction levels.
data DijkstraUtxoEnv era = DijkstraUtxoEnv
  { dueSlot :: !SlotNo
  , duePParams :: !(PParams era)
  , dueCertState :: !(CertState era)
  , dueOriginalUtxo :: !(UTxO era)
  , dueScriptsProvided :: !(ScriptsProvided era)
  }

-- | Predicate failure type for the Dijkstra Era
data DijkstraUtxowPredFailure era
  = UtxoFailure (PredicateFailure (EraRule "UTXO" era))
  | InvalidWitnessesUTXOW (NonEmpty (VKey Witness))
  | -- | witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      -- | witnesses which were needed and not supplied
      (NonEmptySet (KeyHash Witness))
  | -- | missing scripts
    MissingScriptWitnessesUTXOW (NonEmptySet ScriptHash)
  | -- | failed scripts
    ScriptWitnessNotValidatingUTXOW (NonEmptySet ScriptHash)
  | -- | hash of the full metadata
    MissingTxBodyMetadataHash TxAuxDataHash
  | -- | hash of the metadata included in the transaction body
    MissingTxMetadata TxAuxDataHash
  | ConflictingMetadataHash (Mismatch RelEQ TxAuxDataHash)
  | -- | Contains out of range values (string`s too long)
    InvalidMetadata
  | -- | extraneous scripts
    ExtraneousScriptWitnessesUTXOW (NonEmptySet ScriptHash)
  | MissingRedeemers (NonEmpty (PlutusPurpose AsItem era, ScriptHash))
  | MissingRequiredDatums
      -- | Set of missing data hashes
      (NonEmptySet DataHash)
      -- | Set of received data hashes
      (Set DataHash)
  | NotAllowedSupplementalDatums
      -- | Set of unallowed data hashes.
      (NonEmptySet DataHash)
      -- | Set of acceptable supplemental data hashes
      (Set DataHash)
  | PPViewHashesDontMatch
      (Mismatch RelEQ (StrictMaybe ScriptIntegrityHash))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      (NonEmptySet TxIn)
  | -- | List of redeemers not needed
    ExtraRedeemers (NonEmpty (PlutusPurpose AsIx era))
  | -- | Embed UTXO rule failures
    MalformedScriptWitnesses (NonEmptySet ScriptHash)
  | -- | the set of malformed script witnesses
    MalformedReferenceScripts (NonEmptySet ScriptHash)
  | -- | The computed script integrity hash does not match the provided script integrity hash
    ScriptIntegrityHashMismatch
      (Mismatch RelEQ (StrictMaybe ScriptIntegrityHash))
      (StrictMaybe ByteString)
  | -- | Guards required by subtransactions but missing from top-level guards
    MissingRequiredGuards (NonEmptySet (Credential Guard))
  deriving (Generic)

type instance EraRuleFailure "UTXOW" DijkstraEra = DijkstraUtxowPredFailure DijkstraEra

type instance EraRuleEvent "UTXOW" DijkstraEra = AlonzoUtxowEvent DijkstraEra

instance InjectRuleFailure "UTXOW" DijkstraUtxowPredFailure DijkstraEra

instance InjectRuleFailure "UTXOW" ConwayUtxowPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxowPredFailure

instance InjectRuleFailure "UTXOW" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxowPredFailure . babbageToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxowPredFailure . alonzoToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxowPredFailure . shelleyToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

deriving instance
  ( ConwayEraScript era
  , Show (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Show (DijkstraUtxowPredFailure era)

deriving instance
  ( ConwayEraScript era
  , Eq (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Eq (DijkstraUtxowPredFailure era)

instance
  ( ConwayEraScript era
  , NFData (TxCert era)
  , NFData (PredicateFailure (EraRule "UTXO" era))
  ) =>
  NFData (DijkstraUtxowPredFailure era)

--------------------------------------------------------------------------------
-- DijkstraUTXOW STS
--------------------------------------------------------------------------------

dijkstraUtxowTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , DijkstraEraTxBody era
  , EraRule "UTXOW" era ~ DijkstraUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "UTXOW" DijkstraUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (DijkstraUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
  ) =>
  TransitionRule (EraRule "UTXOW" era)
dijkstraUtxowTransition = do
  TRC (DijkstraUtxoEnv slot pp certState originalUtxo scriptsProvided, u, tx) <- judgmentContext

  let txBody = tx ^. bodyTxL
      subTxs = OMap.elems $ txBody ^. subTransactionsTxBodyL
      witsKeyHashes = keyHashWitnessesTxWits (tx ^. witsTxL)

  -- All lookups use originalUtxo.
  -- A subtx may consume a txout that the top-level tx references, so the UTXO threaded in the state may not contain it.

  -- scriptsNeeded for the top-level tx
  let topScriptsNeeded = getScriptsNeeded originalUtxo txBody
      topScriptHashesNeeded = getScriptsHashesNeeded topScriptsNeeded

  -- scriptsNeeded aggregated across all levels
  let allScriptHashesNeeded =
        Set.unions $
          topScriptHashesNeeded
            : [ getScriptsHashesNeeded
                  (getScriptsNeeded originalUtxo (subTx ^. bodyTxL))
              | subTx <- subTxs
              ]

  {- ∀s ∈ (txscripts txw utxo neededHashes ) ∩ Scriptph1 , validateScript s tx -}
  -- Per-level: phase-1 script validation is per-tx (script execution)
  runTest $ validateFailedBabbageScripts tx scriptsProvided topScriptHashesNeeded

  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  -- Aggregated: missing/extraneous scripts across all levels.
  let witnessScripts =
        Map.keysSet (tx ^. witsTxL . scriptTxWitsL)
          <> foldMap (Map.keysSet . (^. witsTxL . scriptTxWitsL)) subTxs
      allRefScriptInputs =
        txBody ^. referenceInputsTxBodyL
          <> txBody ^. inputsTxBodyL
          <> foldMap
            ( \subTx ->
                subTx ^. bodyTxL . referenceInputsTxBodyL
                  <> subTx ^. bodyTxL . inputsTxBodyL
            )
            subTxs
      refScripts = Map.keysSet $ getReferenceScripts originalUtxo allRefScriptInputs
  runTest $ babbageMissingScripts pp allScriptHashesNeeded refScripts witnessScripts

  {-  inputHashes ⊆  dom(txdats txw) ⊆  allowed -}
  -- Per-level: datum check for top-level tx's own spend inputs
  runTest $ missingRequiredDatums originalUtxo tx

  {- dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                          h ↦ s ∈ txscripts txw, s ∈ Scriptph2} -}
  -- Per-level: redeemer indexing is per-tx
  runTest $ hasExactSetOfRedeemers tx scriptsProvided topScriptsNeeded

  -- check VKey witnesses
  {- ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  {- witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes -}
  runTest $ validateNeededWitnesses witsKeyHashes certState originalUtxo txBody

  -- check metadata hash
  {- ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad) -}
  runTestOnSignal $ Shelley.validateMetadata pp tx

  {- ∀x ∈ range(txdats txw) ∪ range(txwitscripts txw) ∪ (⋃ ( , ,d,s) ∈ txouts tx {s, d}),
                       x ∈ Script ∪ Datum ⇒ isWellFormed x -}
  runTest $ validateScriptsWellFormed pp tx

  {- scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw) -}
  -- Per-level: script integrity is per-tx (depends on that tx's redeemers and language views)
  let scriptIntegrity = mkScriptIntegrity pp tx scriptsProvided topScriptHashesNeeded
  runTest $ checkScriptIntegrityHash tx pp scriptIntegrity

  {- concatMapˡ (λ txSub → mapˢ proj₁ (TopLevelGuardsOf txSub)) (SubTransactionsOf txTop) ⊆ GuardsOf txTop -}
  let requiredGuardsBySubTxs =
        foldMap (Map.keysSet . (^. bodyTxL . requiredTopLevelGuardsL)) subTxs
      topLevelGuards = OSet.toSet (txBody ^. guardsTxBodyL)
      missingGuards = requiredGuardsBySubTxs `Set.difference` topLevelGuards
  runTestOnSignal $ failureOnNonEmptySet missingGuards MissingRequiredGuards

  -- Pass through to UTXO sub-rule with standard UtxoEnv (state-based UTXO is correct
  -- for minfee calculation and state update)
  trans @(EraRule "UTXO" era) $ TRC (UtxoEnv slot pp certState, u, tx)

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , DijkstraEraTxBody era
  , EraRule "UTXOW" era ~ DijkstraUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "UTXOW" ConwayUtxowPredFailure era
  , InjectRuleFailure "UTXOW" DijkstraUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (DijkstraUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (DijkstraUTXOW era)
  where
  type State (DijkstraUTXOW era) = UTxOState era
  type Signal (DijkstraUTXOW era) = Tx TopTx era
  type Environment (DijkstraUTXOW era) = DijkstraUtxoEnv era
  type BaseM (DijkstraUTXOW era) = ShelleyBase
  type PredicateFailure (DijkstraUTXOW era) = DijkstraUtxowPredFailure era
  type Event (DijkstraUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [dijkstraUtxowTransition @era]
  initialRules = []

instance
  ( Era era
  , STS (DijkstraUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ DijkstraUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , BaseM (DijkstraUTXOW era) ~ ShelleyBase
  , PredicateFailure (DijkstraUTXOW era) ~ DijkstraUtxowPredFailure era
  , Event (DijkstraUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (DijkstraUTXO era) (DijkstraUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( ConwayEraScript era
  , EncCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  EncCBOR (DijkstraUtxowPredFailure era)
  where
  encCBOR =
    \case
      UtxoFailure x -> encodeListLen 2 <> encodeWord 0 <> encCBOR x
      InvalidWitnessesUTXOW xs -> encodeListLen 2 <> encodeWord 1 <> encCBOR xs
      MissingVKeyWitnessesUTXOW xs -> encodeListLen 2 <> encodeWord 2 <> encCBOR xs
      MissingScriptWitnessesUTXOW xs -> encodeListLen 2 <> encodeWord 3 <> encCBOR xs
      ScriptWitnessNotValidatingUTXOW xs -> encodeListLen 2 <> encodeWord 4 <> encCBOR xs
      MissingTxBodyMetadataHash xs -> encodeListLen 2 <> encodeWord 5 <> encCBOR xs
      MissingTxMetadata xs -> encodeListLen 2 <> encodeWord 6 <> encCBOR xs
      ConflictingMetadataHash mm -> encodeListLen 2 <> encodeWord 7 <> encCBOR mm
      InvalidMetadata -> encodeListLen 1 <> encodeWord 8
      ExtraneousScriptWitnessesUTXOW xs -> encodeListLen 2 <> encodeWord 9 <> encCBOR xs
      MissingRedeemers x -> encodeListLen 2 <> encodeWord 10 <> encCBOR x
      MissingRequiredDatums x y -> encodeListLen 3 <> encodeWord 11 <> encCBOR x <> encCBOR y
      NotAllowedSupplementalDatums x y -> encodeListLen 3 <> encodeWord 12 <> encCBOR x <> encCBOR y
      PPViewHashesDontMatch mm -> encodeListLen 2 <> encodeWord 13 <> encCBOR mm
      UnspendableUTxONoDatumHash x -> encodeListLen 2 <> encodeWord 14 <> encCBOR x
      ExtraRedeemers x -> encodeListLen 2 <> encodeWord 15 <> encCBOR x
      MalformedScriptWitnesses x -> encodeListLen 2 <> encodeWord 16 <> encCBOR x
      MalformedReferenceScripts x -> encodeListLen 2 <> encodeWord 17 <> encCBOR x
      ScriptIntegrityHashMismatch x y -> encodeListLen 3 <> encodeWord 18 <> encCBOR x <> encCBOR y
      MissingRequiredGuards x -> encodeListLen 2 <> encodeWord 19 <> encCBOR x

instance
  ( ConwayEraScript era
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  DecCBOR (DijkstraUtxowPredFailure era)
  where
  decCBOR = decodeRecordSum "ConwayUtxowPred" $ \case
    0 -> fmap (2,) $ UtxoFailure <$> decCBOR
    1 -> fmap (2,) $ InvalidWitnessesUTXOW <$> decCBOR
    2 -> fmap (2,) $ MissingVKeyWitnessesUTXOW <$> decCBOR
    3 -> fmap (2,) $ MissingScriptWitnessesUTXOW <$> decCBOR
    4 -> fmap (2,) $ ScriptWitnessNotValidatingUTXOW <$> decCBOR
    5 -> fmap (2,) $ MissingTxBodyMetadataHash <$> decCBOR
    6 -> fmap (2,) $ MissingTxMetadata <$> decCBOR
    7 -> fmap (2,) $ ConflictingMetadataHash <$> decCBOR
    8 -> pure (1, InvalidMetadata)
    9 -> fmap (2,) $ ExtraneousScriptWitnessesUTXOW <$> decCBOR
    10 -> fmap (2,) $ MissingRedeemers <$> decCBOR
    11 -> fmap (3,) $ MissingRequiredDatums <$> decCBOR <*> decCBOR
    12 -> fmap (3,) $ NotAllowedSupplementalDatums <$> decCBOR <*> decCBOR
    13 -> fmap (2,) $ PPViewHashesDontMatch <$> decCBOR
    14 -> fmap (2,) $ UnspendableUTxONoDatumHash <$> decCBOR
    15 -> fmap (2,) $ ExtraRedeemers <$> decCBOR
    16 -> fmap (2,) $ MalformedScriptWitnesses <$> decCBOR
    17 -> fmap (2,) $ MalformedReferenceScripts <$> decCBOR
    18 -> fmap (3,) $ ScriptIntegrityHashMismatch <$> decCBOR <*> decCBOR
    19 -> fmap (2,) $ MissingRequiredGuards <$> decCBOR
    n -> invalidKey n

-- =====================================================
-- Injecting from one PredicateFailure to another

conwayToDijkstraUtxowPredFailure ::
  forall era.
  ConwayUtxowPredFailure era ->
  DijkstraUtxowPredFailure era
conwayToDijkstraUtxowPredFailure = \case
  Conway.UtxoFailure f -> UtxoFailure f
  Conway.InvalidWitnessesUTXOW ks -> InvalidWitnessesUTXOW ks
  Conway.MissingVKeyWitnessesUTXOW ks -> MissingVKeyWitnessesUTXOW ks
  Conway.MissingScriptWitnessesUTXOW hs -> MissingScriptWitnessesUTXOW hs
  Conway.ScriptWitnessNotValidatingUTXOW hs -> ScriptWitnessNotValidatingUTXOW hs
  Conway.MissingTxBodyMetadataHash dh -> MissingTxBodyMetadataHash dh
  Conway.MissingTxMetadata dh -> MissingTxMetadata dh
  Conway.ConflictingMetadataHash mm -> ConflictingMetadataHash mm
  Conway.InvalidMetadata -> InvalidMetadata
  Conway.ExtraneousScriptWitnessesUTXOW hs -> ExtraneousScriptWitnessesUTXOW hs
  Conway.MissingRedeemers pps -> MissingRedeemers pps
  Conway.MissingRequiredDatums hs1 hs2 -> MissingRequiredDatums hs1 hs2
  Conway.NotAllowedSupplementalDatums hs1 hs2 -> NotAllowedSupplementalDatums hs1 hs2
  Conway.PPViewHashesDontMatch mm -> PPViewHashesDontMatch mm
  Conway.UnspendableUTxONoDatumHash txs -> UnspendableUTxONoDatumHash txs
  Conway.ExtraRedeemers pps -> ExtraRedeemers pps
  Conway.MalformedScriptWitnesses hs -> MalformedScriptWitnesses hs
  Conway.MalformedReferenceScripts hs -> MalformedReferenceScripts hs
  Conway.ScriptIntegrityHashMismatch mm f -> ScriptIntegrityHashMismatch mm f
