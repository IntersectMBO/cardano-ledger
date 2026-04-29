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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxow (
  DijkstraUTXOW,
  DijkstraUtxowPredFailure (..),
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
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
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
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoEnv (..), DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Keys (VKey)
import Cardano.Ledger.Rules.ValidationMode (runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure,
  validateNeededWitnesses,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  validateMetadata,
  validateVerifiedWits,
 )
import Cardano.Ledger.State (EraUTxO (..))
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
  , Environment (EraRule "UTXO" era) ~ DijkstraUtxoEnv era
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

  -- Pass through to UTXO sub-rule, carrying the original UTxO and scriptsProvided
  trans @(EraRule "UTXO" era) $
    TRC (DijkstraUtxoEnv slot pp certState originalUtxo scriptsProvided, u, tx)

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
  , Environment (EraRule "UTXO" era) ~ DijkstraUtxoEnv era
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
  ( STS (DijkstraUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ DijkstraUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
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
    encode . \case
      UtxoFailure x -> Sum UtxoFailure 0 !> To x
      InvalidWitnessesUTXOW xs -> Sum InvalidWitnessesUTXOW 1 !> To xs
      MissingVKeyWitnessesUTXOW xs -> Sum MissingVKeyWitnessesUTXOW 2 !> To xs
      MissingScriptWitnessesUTXOW xs -> Sum MissingScriptWitnessesUTXOW 3 !> To xs
      ScriptWitnessNotValidatingUTXOW xs -> Sum ScriptWitnessNotValidatingUTXOW 4 !> To xs
      MissingTxBodyMetadataHash xs -> Sum MissingTxBodyMetadataHash 5 !> To xs
      MissingTxMetadata xs -> Sum MissingTxMetadata 6 !> To xs
      ConflictingMetadataHash mm -> Sum ConflictingMetadataHash 7 !> To mm
      InvalidMetadata -> Sum InvalidMetadata 8
      ExtraneousScriptWitnessesUTXOW xs -> Sum ExtraneousScriptWitnessesUTXOW 9 !> To xs
      MissingRedeemers x -> Sum MissingRedeemers 10 !> To x
      MissingRequiredDatums x y -> Sum MissingRequiredDatums 11 !> To x !> To y
      NotAllowedSupplementalDatums x y -> Sum NotAllowedSupplementalDatums 12 !> To x !> To y
      PPViewHashesDontMatch mm -> Sum PPViewHashesDontMatch 13 !> To mm
      UnspendableUTxONoDatumHash x -> Sum UnspendableUTxONoDatumHash 14 !> To x
      ExtraRedeemers x -> Sum ExtraRedeemers 15 !> To x
      MalformedScriptWitnesses x -> Sum MalformedScriptWitnesses 16 !> To x
      MalformedReferenceScripts x -> Sum MalformedReferenceScripts 17 !> To x
      ScriptIntegrityHashMismatch x y -> Sum ScriptIntegrityHashMismatch 18 !> To x !> To y
      MissingRequiredGuards x -> Sum MissingRequiredGuards 19 !> To x

instance
  ( ConwayEraScript era
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  DecCBOR (DijkstraUtxowPredFailure era)
  where
  decCBOR = decode . Summands "ConwayUtxowPred" $ \case
    0 -> SumD UtxoFailure <! From
    1 -> SumD InvalidWitnessesUTXOW <! From
    2 -> SumD MissingVKeyWitnessesUTXOW <! From
    3 -> SumD MissingScriptWitnessesUTXOW <! From
    4 -> SumD ScriptWitnessNotValidatingUTXOW <! From
    5 -> SumD MissingTxBodyMetadataHash <! From
    6 -> SumD MissingTxMetadata <! From
    7 -> SumD ConflictingMetadataHash <! From
    8 -> SumD InvalidMetadata
    9 -> SumD ExtraneousScriptWitnessesUTXOW <! From
    10 -> SumD MissingRedeemers <! From
    11 -> SumD MissingRequiredDatums <! From <! From
    12 -> SumD NotAllowedSupplementalDatums <! From <! From
    13 -> SumD PPViewHashesDontMatch <! From
    14 -> SumD UnspendableUTxONoDatumHash <! From
    15 -> SumD ExtraRedeemers <! From
    16 -> SumD MalformedScriptWitnesses <! From
    17 -> SumD MalformedReferenceScripts <! From
    18 -> SumD ScriptIntegrityHashMismatch <! From <! From
    19 -> SumD MissingRequiredGuards <! From
    n -> Invalid n

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
