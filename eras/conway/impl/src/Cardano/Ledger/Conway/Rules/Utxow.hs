{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxow (
  alonzoToConwayUtxowPredFailure,
  babbageToConwayUtxowPredFailure,
  ConwayUTXOW,
  ConwayUtxowPredFailure (..),
  shelleyToConwayUtxowPredFailure,
) where

import Cardano.Crypto.Hash (ByteString)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent (WrappedShelleyEraEvent),
  AlonzoUtxowPredFailure,
  hasExactSetOfRedeemers,
  missingRequiredDatums,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
  babbageMissingScripts,
  validateFailedBabbageScripts,
  validateScriptsWellFormed,
 )
import qualified Cardano.Ledger.Babbage.Rules as Babbage (BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.Tx (ScriptIntegrity (..), hashScriptIntegrity)
import Cardano.Ledger.Babbage.UTxO (getReferenceScripts)
import Cardano.Ledger.BaseTypes (Mismatch (..), ProtVer (..), Relation (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), natVersion)
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayUTXO, ConwayUTXOW)
import Cardano.Ledger.Conway.PParams (getLanguageView)
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Keys (VKey)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley (UTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
  validateNeededWitnesses,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  ShelleyUtxowPredFailure (..),
  UtxoEnv,
  validateMetadata,
  validateVerifiedWits,
 )
import Cardano.Ledger.State (EraUTxO (..), ScriptsProvided (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (StrictMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Validation (failureUnless)

-- ================================

-- | Predicate failure type for the Conway Era
data ConwayUtxowPredFailure era
  = UtxoFailure (PredicateFailure (EraRule "UTXO" era))
  | InvalidWitnessesUTXOW
      [VKey 'Witness]
  | -- | witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      -- | witnesses which were needed and not supplied
      (Set (KeyHash 'Witness))
  | -- | missing scripts
    MissingScriptWitnessesUTXOW
      (Set ScriptHash)
  | -- | failed scripts
    ScriptWitnessNotValidatingUTXOW
      (Set ScriptHash)
  | -- | hash of the full metadata
    MissingTxBodyMetadataHash
      TxAuxDataHash
  | -- | hash of the metadata included in the transaction body
    MissingTxMetadata
      TxAuxDataHash
  | ConflictingMetadataHash
      (Mismatch 'RelEQ TxAuxDataHash)
  | -- | Contains out of range values (string`s too long)
    InvalidMetadata
  | -- | extraneous scripts
    ExtraneousScriptWitnessesUTXOW
      (Set ScriptHash)
  | MissingRedeemers
      [(PlutusPurpose AsItem era, ScriptHash)]
  | MissingRequiredDatums
      -- TODO: Make this NonEmpty #4066

      -- | Set of missing data hashes
      (Set DataHash)
      -- | Set of received data hashes
      (Set DataHash)
  | NotAllowedSupplementalDatums
      -- TODO: Make this NonEmpty #4066

      -- | Set of unallowed data hashes.
      (Set DataHash)
      -- | Set of acceptable supplemental data hashes
      (Set DataHash)
  | PPViewHashesDontMatch
      (Mismatch 'RelEQ (StrictMaybe ScriptIntegrityHash))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      -- TODO: Make this NonEmpty #4066
      (Set TxIn)
  | -- | List of redeemers not needed
    ExtraRedeemers [PlutusPurpose AsIx era]
  | -- | Embed UTXO rule failures
    MalformedScriptWitnesses
      (Set ScriptHash)
  | -- | the set of malformed script witnesses
    MalformedReferenceScripts
      (Set ScriptHash)
  | -- | The computed script integrity hash does not match the provided script integrity hash
    ScriptIntegrityHashMismatch
      (Mismatch 'RelEQ (StrictMaybe ScriptIntegrityHash))
      ByteString
  deriving (Generic)

type instance EraRuleFailure "UTXOW" ConwayEra = ConwayUtxowPredFailure ConwayEra

type instance EraRuleEvent "UTXOW" ConwayEra = AlonzoUtxowEvent ConwayEra

instance InjectRuleFailure "UTXOW" ConwayUtxowPredFailure ConwayEra

instance InjectRuleFailure "UTXOW" BabbageUtxowPredFailure ConwayEra where
  injectFailure = babbageToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure ConwayEra where
  injectFailure = alonzoToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure ConwayEra where
  injectFailure = shelleyToConwayUtxowPredFailure

instance InjectRuleFailure "UTXOW" ConwayUtxoPredFailure ConwayEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" BabbageUtxoPredFailure ConwayEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure ConwayEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure ConwayEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ConwayUtxosPredFailure ConwayEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure ConwayEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure ConwayEra where
  injectFailure = UtxoFailure . injectFailure

deriving instance
  ( ConwayEraScript era
  , Show (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Show (ConwayUtxowPredFailure era)

deriving instance
  ( ConwayEraScript era
  , Eq (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Eq (ConwayUtxowPredFailure era)

deriving via
  InspectHeapNamed "ConwayUtxowPred" (ConwayUtxowPredFailure era)
  instance
    NoThunks (ConwayUtxowPredFailure era)

instance
  ( ConwayEraScript era
  , NFData (TxCert era)
  , NFData (PredicateFailure (EraRule "UTXO" era))
  ) =>
  NFData (ConwayUtxowPredFailure era)

--------------------------------------------------------------------------------
-- ConwayUTXOW STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , ConwayEraTxBody era
  , EraRule "UTXOW" era ~ ConwayUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "UTXOW" ConwayUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (ConwayUTXOW era)
  , Environment (EraRule "UTXO" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXO" era) ~ Shelley.UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (ConwayUTXOW era)
  where
  type State (ConwayUTXOW era) = Shelley.UTxOState era
  type Signal (ConwayUTXOW era) = Tx era
  type Environment (ConwayUTXOW era) = Shelley.UtxoEnv era
  type BaseM (ConwayUTXOW era) = ShelleyBase
  type PredicateFailure (ConwayUTXOW era) = ConwayUtxowPredFailure era
  type Event (ConwayUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [conwayUtxowTransition @era]
  initialRules = []

instance
  ( Era era
  , STS (ConwayUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ ConwayUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , BaseM (ConwayUTXOW era) ~ ShelleyBase
  , PredicateFailure (ConwayUTXOW era) ~ ConwayUtxowPredFailure era
  , Event (ConwayUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (ConwayUTXO era) (ConwayUTXOW era)
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
  EncCBOR (ConwayUtxowPredFailure era)
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
      ConflictingMetadataHash mm -> Sum ConflictingMetadataHash 7 !> ToGroup mm
      InvalidMetadata -> Sum InvalidMetadata 8
      ExtraneousScriptWitnessesUTXOW xs -> Sum ExtraneousScriptWitnessesUTXOW 9 !> To xs
      MissingRedeemers x -> Sum MissingRedeemers 10 !> To x
      MissingRequiredDatums x y -> Sum MissingRequiredDatums 11 !> To x !> To y
      NotAllowedSupplementalDatums x y -> Sum NotAllowedSupplementalDatums 12 !> To x !> To y
      PPViewHashesDontMatch mm -> Sum PPViewHashesDontMatch 13 !> ToGroup mm
      UnspendableUTxONoDatumHash x -> Sum UnspendableUTxONoDatumHash 14 !> To x
      ExtraRedeemers x -> Sum ExtraRedeemers 15 !> To x
      MalformedScriptWitnesses x -> Sum MalformedScriptWitnesses 16 !> To x
      MalformedReferenceScripts x -> Sum MalformedReferenceScripts 17 !> To x
      ScriptIntegrityHashMismatch x y -> Sum ScriptIntegrityHashMismatch 18 !> To x !> To y

instance
  ( ConwayEraScript era
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  DecCBOR (ConwayUtxowPredFailure era)
  where
  decCBOR = decode . Summands "ConwayUtxowPred" $ \case
    0 -> SumD UtxoFailure <! From
    1 -> SumD InvalidWitnessesUTXOW <! From
    2 -> SumD MissingVKeyWitnessesUTXOW <! From
    3 -> SumD MissingScriptWitnessesUTXOW <! From
    4 -> SumD ScriptWitnessNotValidatingUTXOW <! From
    5 -> SumD MissingTxBodyMetadataHash <! From
    6 -> SumD MissingTxMetadata <! From
    7 -> SumD ConflictingMetadataHash <! FromGroup
    8 -> SumD InvalidMetadata
    9 -> SumD ExtraneousScriptWitnessesUTXOW <! From
    10 -> SumD MissingRedeemers <! From
    11 -> SumD MissingRequiredDatums <! From <! From
    12 -> SumD NotAllowedSupplementalDatums <! From <! From
    13 -> SumD PPViewHashesDontMatch <! FromGroup
    14 -> SumD UnspendableUTxONoDatumHash <! From
    15 -> SumD ExtraRedeemers <! From
    16 -> SumD MalformedScriptWitnesses <! From
    17 -> SumD MalformedReferenceScripts <! From
    18 -> SumD ScriptIntegrityHashMismatch <! From <! From
    n -> Invalid n

-- =====================================================
-- Injecting from one PredicateFailure to another

babbageToConwayUtxowPredFailure ::
  forall era.
  BabbageUtxowPredFailure era ->
  ConwayUtxowPredFailure era
babbageToConwayUtxowPredFailure = \case
  Babbage.AlonzoInBabbageUtxowPredFailure x -> alonzoToConwayUtxowPredFailure x
  Babbage.UtxoFailure x -> UtxoFailure x
  Babbage.MalformedScriptWitnesses xs -> MalformedScriptWitnesses xs
  Babbage.MalformedReferenceScripts xs -> MalformedReferenceScripts xs

alonzoToConwayUtxowPredFailure ::
  forall era.
  AlonzoUtxowPredFailure era ->
  ConwayUtxowPredFailure era
alonzoToConwayUtxowPredFailure = \case
  Alonzo.ShelleyInAlonzoUtxowPredFailure f -> shelleyToConwayUtxowPredFailure f
  Alonzo.MissingRedeemers rs -> MissingRedeemers rs
  Alonzo.MissingRequiredDatums mds rds -> MissingRequiredDatums mds rds
  Alonzo.NotAllowedSupplementalDatums uds ads -> NotAllowedSupplementalDatums uds ads
  Alonzo.PPViewHashesDontMatch m -> PPViewHashesDontMatch m
  Alonzo.UnspendableUTxONoDatumHash ins -> UnspendableUTxONoDatumHash ins
  Alonzo.ExtraRedeemers xs -> ExtraRedeemers xs

shelleyToConwayUtxowPredFailure :: ShelleyUtxowPredFailure era -> ConwayUtxowPredFailure era
shelleyToConwayUtxowPredFailure = \case
  Shelley.InvalidWitnessesUTXOW xs -> InvalidWitnessesUTXOW xs
  Shelley.MissingVKeyWitnessesUTXOW xs -> MissingVKeyWitnessesUTXOW xs
  Shelley.MissingScriptWitnessesUTXOW xs -> MissingScriptWitnessesUTXOW xs
  Shelley.ScriptWitnessNotValidatingUTXOW xs -> ScriptWitnessNotValidatingUTXOW xs
  Shelley.UtxoFailure x -> UtxoFailure x
  Shelley.MIRInsufficientGenesisSigsUTXOW _xs ->
    error "Impossible: MIR has been removed in Conway"
  Shelley.MissingTxBodyMetadataHash x -> MissingTxBodyMetadataHash x
  Shelley.MissingTxMetadata x -> MissingTxMetadata x
  Shelley.ConflictingMetadataHash mm -> ConflictingMetadataHash mm
  Shelley.InvalidMetadata -> InvalidMetadata
  Shelley.ExtraneousScriptWitnessesUTXOW xs -> ExtraneousScriptWitnessesUTXOW xs

conwayPPViewHashesMatch ::
  forall era.
  AlonzoEraTx era =>
  Tx era ->
  PParams era ->
  ScriptsProvided era ->
  Set ScriptHash ->
  Test (ConwayUtxowPredFailure era)
conwayPPViewHashesMatch tx pp (ScriptsProvided scriptsProvided) scriptsNeeded = do
  let scriptsUsed = Map.elems $ Map.restrictKeys scriptsProvided scriptsNeeded
      langs = Set.fromList $ plutusScriptLanguage <$> mapMaybe toPlutusScript scriptsUsed
      langViews = Set.map (getLanguageView pp) langs
      txWits = tx ^. witsTxL
      txRedeemers = txWits ^. rdmrsTxWitsL
      txDats = txWits ^. datsTxWitsL
      computedPPhash = hashScriptIntegrity langViews txRedeemers txDats
      bodyPPhash = tx ^. bodyTxL . scriptIntegrityHashTxBodyL
      expectedScriptIntegrity = originalBytes $ ScriptIntegrity @era txRedeemers txDats langViews
  failureUnless
    (bodyPPhash == computedPPhash)
    $ if pp ^. ppProtocolVersionL < ProtVer (natVersion @11) 0
      then
        PPViewHashesDontMatch Mismatch {mismatchSupplied = bodyPPhash, mismatchExpected = computedPPhash}
      else
        ScriptIntegrityHashMismatch
          Mismatch {mismatchSupplied = bodyPPhash, mismatchExpected = computedPPhash}
          expectedScriptIntegrity

-- | UTXOW transition rule that is used in Babbage and Conway era.
conwayUtxowTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , BabbageEraTxBody era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (EraRule "UTXOW" era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , InjectRuleFailure "UTXOW" ConwayUtxowPredFailure era
  ) =>
  TransitionRule (EraRule "UTXOW" era)
conwayUtxowTransition = do
  TRC (utxoEnv@(UtxoEnv _ pp certState), u, tx) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = utxosUtxo u
      txBody = tx ^. bodyTxL
      witsKeyHashes = keyHashWitnessesTxWits (tx ^. witsTxL)
      inputs = (txBody ^. referenceInputsTxBodyL) `Set.union` (txBody ^. inputsTxBodyL)

  -- check scripts
  {- neededHashes := {h | ( , h) ∈ scriptsNeeded utxo txb} -}
  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let scriptsNeeded = getScriptsNeeded utxo txBody
      scriptsProvided = getScriptsProvided utxo tx
      scriptHashesNeeded = getScriptsHashesNeeded scriptsNeeded
  {- ∀s ∈ (txscripts txw utxo neededHashes ) ∩ Scriptph1 , validateScript s tx -}
  -- CHANGED In BABBAGE txscripts depends on UTxO
  runTest $ validateFailedBabbageScripts tx scriptsProvided scriptHashesNeeded

  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let sReceived = Map.keysSet $ tx ^. witsTxL . scriptTxWitsL
      sRefs = Map.keysSet $ getReferenceScripts utxo inputs
  runTest $ babbageMissingScripts pp scriptHashesNeeded sRefs sReceived

  {-  inputHashes ⊆  dom(txdats txw) ⊆  allowed -}
  runTest $ missingRequiredDatums utxo tx

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
  runTest $ hasExactSetOfRedeemers tx scriptsProvided scriptsNeeded

  -- check VKey witnesses
  -- let txbodyHash = hashAnnotated @(Crypto era) txbody
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  runTest $ validateNeededWitnesses witsKeyHashes certState utxo txBody

  -- check metadata hash
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $ Shelley.validateMetadata pp tx

  {- ∀x ∈ range(txdats txw) ∪ range(txwitscripts txw) ∪ (⋃ ( , ,d,s) ∈ txouts tx {s, d}),
                         x ∈ Script ∪ Datum ⇒ isWellFormed x
  -}
  runTest $ validateScriptsWellFormed pp tx
  -- Note that Datum validation is done during deserialization,
  -- as given by the decoders in the Plutus libraray

  {- languages tx utxo ⊆ dom(costmdls pp) -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'.

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  runTest $ conwayPPViewHashesMatch tx pp scriptsProvided scriptHashesNeeded

  trans @(EraRule "UTXO" era) $ TRC (utxoEnv, u, tx)
