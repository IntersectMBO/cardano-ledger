{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Cardano.Ledger.Dijkstra.Rules.SubUtxow (
  SUBUTXOW,
  DijkstraSubUtxowPredFailure (..),
  DijkstraSubUtxowEvent (..),
) where

import Cardano.Crypto.Hash (ByteString)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..), AlonzoScriptsNeeded)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.Babbage.Tx (mkScriptIntegrity)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  SUBUTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.SubUtxo
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.Utxow (
  DijkstraUtxowPredFailure (..),
  conwayToDijkstraUtxowPredFailure,
  validateGuardDatums,
 )
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Keys (VKey)
import Cardano.Ledger.Rules.ValidationMode
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (EraUTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import GHC.Generics (Generic)
import Lens.Micro

data DijkstraSubUtxowPredFailure era
  = SubUtxoFailure (PredicateFailure (EraRule "SUBUTXO" era))
  | SubInvalidWitnessesUTXOW (NonEmpty (VKey Witness))
  | -- | witnesses which failed in verifiedWits function
    SubMissingVKeyWitnessesUTXOW
      -- | witnesses which were needed and not supplied
      (NonEmptySet (KeyHash Witness))
  | -- | failed scripts
    SubScriptWitnessNotValidatingUTXOW (NonEmptySet ScriptHash)
  | -- | hash of the full metadata
    SubMissingTxBodyMetadataHash TxAuxDataHash
  | -- | hash of the metadata included in the transaction body
    SubMissingTxMetadata TxAuxDataHash
  | SubConflictingMetadataHash (Mismatch RelEQ TxAuxDataHash)
  | -- | Contains out of range values (string`s too long)
    SubInvalidMetadata
  | SubMissingRedeemers (NonEmpty (PlutusPurpose AsItem era, ScriptHash))
  | SubMissingRequiredDatums
      -- | Set of missing data hashes
      (NonEmptySet DataHash)
      -- | Set of received data hashes
      (Set DataHash)
  | SubNotAllowedSupplementalDatums
      -- | Set of unallowed data hashes.
      (NonEmptySet DataHash)
      -- | Set of acceptable supplemental data hashes
      (Set DataHash)
  | SubPPViewHashesDontMatch
      (Mismatch RelEQ (StrictMaybe ScriptIntegrityHash))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    SubUnspendableUTxONoDatumHash
      (NonEmptySet TxIn)
  | -- | List of redeemers not needed
    SubExtraRedeemers (NonEmpty (PlutusPurpose AsIx era))
  | -- | Embed UTXO rule failures
    SubMalformedScriptWitnesses (NonEmptySet ScriptHash)
  | -- | the set of malformed script witnesses
    SubMalformedReferenceScripts (NonEmptySet ScriptHash)
  | -- | The computed script integrity hash does not match the provided script integrity hash
    SubScriptIntegrityHashMismatch
      (Mismatch RelEQ (StrictMaybe ScriptIntegrityHash))
      (StrictMaybe ByteString)
  | -- | Guard credentials with incorrect datum presence in requiredTopLevelGuards
    SubMalformedGuardDatums (NonEmptySet (Credential Guard))
  deriving (Generic)

deriving stock instance
  ( ConwayEraScript era
  , Eq (PredicateFailure (EraRule "SUBUTXO" era))
  ) =>
  Eq (DijkstraSubUtxowPredFailure era)

deriving stock instance
  ( ConwayEraScript era
  , Show (PredicateFailure (EraRule "SUBUTXO" era))
  ) =>
  Show (DijkstraSubUtxowPredFailure era)

instance
  ( ConwayEraScript era
  , NFData (TxCert era)
  , NFData (PredicateFailure (EraRule "SUBUTXO" era))
  ) =>
  NFData (DijkstraSubUtxowPredFailure era)

type instance EraRuleFailure "SUBUTXOW" DijkstraEra = DijkstraSubUtxowPredFailure DijkstraEra

type instance EraRuleEvent "SUBUTXOW" DijkstraEra = DijkstraSubUtxowEvent DijkstraEra

instance InjectRuleFailure "SUBUTXOW" DijkstraSubUtxowPredFailure DijkstraEra

instance InjectRuleFailure "SUBUTXOW" DijkstraSubUtxoPredFailure DijkstraEra where
  injectFailure = SubUtxoFailure

instance InjectRuleFailure "SUBUTXOW" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = dijkstraUtxowToDijkstraSubUtxowPredFailure

instance InjectRuleFailure "SUBUTXOW" Conway.ConwayUtxowPredFailure DijkstraEra where
  injectFailure = dijkstraUtxowToDijkstraSubUtxowPredFailure . conwayToDijkstraUtxowPredFailure

instance InjectRuleFailure "SUBUTXOW" Babbage.BabbageUtxowPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxowToDijkstraSubUtxowPredFailure
      . conwayToDijkstraUtxowPredFailure
      . Conway.babbageToConwayUtxowPredFailure

instance InjectRuleFailure "SUBUTXOW" Alonzo.AlonzoUtxowPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxowToDijkstraSubUtxowPredFailure
      . conwayToDijkstraUtxowPredFailure
      . Conway.alonzoToConwayUtxowPredFailure

instance InjectRuleFailure "SUBUTXOW" Shelley.ShelleyUtxowPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxowToDijkstraSubUtxowPredFailure
      . conwayToDijkstraUtxowPredFailure
      . Conway.shelleyToConwayUtxowPredFailure

instance InjectRuleEvent "SUBUTXOW" DijkstraSubUtxowEvent DijkstraEra

newtype DijkstraSubUtxowEvent era = SubUtxo (Event (EraRule "SUBUTXO" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBUTXO" era)) => Eq (DijkstraSubUtxowEvent era)

instance NFData (Event (EraRule "SUBUTXO" era)) => NFData (DijkstraSubUtxowEvent era)

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , BabbageEraTxOut era
  , ConwayEraGov era
  , ConwayEraTxBody era
  , DijkstraEraTxBody era
  , EraPlutusContext era
  , EraRule "SUBUTXO" era ~ SUBUTXO era
  , EraRule "SUBUTXOW" era ~ SUBUTXOW era
  , Embed (EraRule "SUBUTXO" era) (SUBUTXOW era)
  , InjectRuleFailure "SUBUTXOW" Alonzo.AlonzoUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" Shelley.ShelleyUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" Babbage.BabbageUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" DijkstraUtxowPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  STS (SUBUTXOW era)
  where
  type State (SUBUTXOW era) = UTxOState era
  type Signal (SUBUTXOW era) = StAnnTx SubTx era
  type Environment (SUBUTXOW era) = SubUtxoEnv era
  type BaseM (SUBUTXOW era) = ShelleyBase
  type PredicateFailure (SUBUTXOW era) = DijkstraSubUtxowPredFailure era
  type Event (SUBUTXOW era) = DijkstraSubUtxowEvent era

  transitionRules = [dijkstraSubUtxowTransition @era]

dijkstraSubUtxowTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , DijkstraEraTxBody era
  , EraRule "SUBUTXO" era ~ SUBUTXO era
  , EraRule "SUBUTXOW" era ~ SUBUTXOW era
  , Embed (EraRule "SUBUTXO" era) (SUBUTXOW era)
  , InjectRuleFailure "SUBUTXOW" Alonzo.AlonzoUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" Shelley.ShelleyUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" Babbage.BabbageUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" DijkstraUtxowPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  TransitionRule (EraRule "SUBUTXOW" era)
dijkstraSubUtxowTransition = do
  TRC (env@(SubUtxoEnv _ pp certState originalUtxo _), utxoState, stAnnTx) <-
    judgmentContext
  let tx = stAnnTx ^. txStAnnTxG
      txBody = tx ^. bodyTxL
      witsKeyHashes = keyHashWitnessesTxWits (tx ^. witsTxL)
      scriptsProvided = scriptsProvidedStAnnTx stAnnTx

  {- ∀[ (vk , σ) ∈ vKeySigs ] isSigned vk (txidBytes txId) σ -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  let scriptsNeeded = scriptsNeededStAnnTx stAnnTx
      scriptHashesNeeded = getScriptsHashesNeeded scriptsNeeded

  {- ∀[ s ∈ p1ScriptsNeeded ] validP1Script vKeyHashesProvided txVldt s -}
  runTest $ Babbage.validateFailedBabbageScripts tx scriptsProvided scriptHashesNeeded

  {- vKeyHashesNeeded ⊆ vKeyHashesProvided -}
  runTest $ Shelley.validateNeededWitnesses witsKeyHashes certState originalUtxo txBody

  {- dataHashesNeeded ⊆ mapˢ hash dataProvided -}
  runTest $ Alonzo.missingRequiredDatums scriptsProvided originalUtxo tx

  {- txADhash ≡ map hash txAuxData -}
  runTestOnSignal $ Shelley.validateMetadata pp tx

  let scriptIntegrity = mkScriptIntegrity pp tx (plutusLanguagesUsedStAnnTx stAnnTx)
  runTest $ Alonzo.checkScriptIntegrityHash tx pp scriptIntegrity

  runTest $ Alonzo.hasExactSetOfRedeemers tx scriptsProvided scriptsNeeded

  runTest $
    Babbage.validateScriptsWellFormedTxOuts
      pp
      (tx ^. witsTxL . scriptTxWitsL)
      (tx ^. bodyTxL . outputsTxBodyL)

  runTest $ validateGuardDatums scriptsProvided txBody

  trans @(EraRule "SUBUTXO" era) $ TRC (env, utxoState, stAnnTx)

instance
  ( STS (SUBUTXO era)
  , PredicateFailure (EraRule "SUBUTXO" era) ~ DijkstraSubUtxoPredFailure era
  , Event (EraRule "SUBUTXO" era) ~ DijkstraSubUtxoEvent era
  ) =>
  Embed (SUBUTXO era) (SUBUTXOW era)
  where
  wrapFailed = SubUtxoFailure
  wrapEvent = SubUtxo

instance
  ( ConwayEraScript era
  , EncCBOR (PredicateFailure (EraRule "SUBUTXO" era))
  ) =>
  EncCBOR (DijkstraSubUtxowPredFailure era)
  where
  encCBOR =
    encode . \case
      SubUtxoFailure x -> Sum SubUtxoFailure 0 !> To x
      SubInvalidWitnessesUTXOW xs -> Sum SubInvalidWitnessesUTXOW 1 !> To xs
      SubMissingVKeyWitnessesUTXOW xs -> Sum SubMissingVKeyWitnessesUTXOW 2 !> To xs
      SubScriptWitnessNotValidatingUTXOW xs -> Sum SubScriptWitnessNotValidatingUTXOW 3 !> To xs
      SubMissingTxBodyMetadataHash xs -> Sum SubMissingTxBodyMetadataHash 4 !> To xs
      SubMissingTxMetadata xs -> Sum SubMissingTxMetadata 5 !> To xs
      SubConflictingMetadataHash mm -> Sum SubConflictingMetadataHash 6 !> To mm
      SubInvalidMetadata -> Sum SubInvalidMetadata 7
      SubMissingRedeemers x -> Sum SubMissingRedeemers 8 !> To x
      SubMissingRequiredDatums x y -> Sum SubMissingRequiredDatums 9 !> To x !> To y
      SubNotAllowedSupplementalDatums x y -> Sum SubNotAllowedSupplementalDatums 10 !> To x !> To y
      SubPPViewHashesDontMatch mm -> Sum SubPPViewHashesDontMatch 11 !> To mm
      SubUnspendableUTxONoDatumHash x -> Sum SubUnspendableUTxONoDatumHash 12 !> To x
      SubExtraRedeemers x -> Sum SubExtraRedeemers 13 !> To x
      SubMalformedScriptWitnesses x -> Sum SubMalformedScriptWitnesses 14 !> To x
      SubMalformedReferenceScripts x -> Sum SubMalformedReferenceScripts 15 !> To x
      SubScriptIntegrityHashMismatch x y -> Sum SubScriptIntegrityHashMismatch 16 !> To x !> To y
      SubMalformedGuardDatums x -> Sum SubMalformedGuardDatums 17 !> To x

instance
  ( ConwayEraScript era
  , DecCBOR (PredicateFailure (EraRule "SUBUTXO" era))
  ) =>
  DecCBOR (DijkstraSubUtxowPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraUtxowPred" $ \case
    0 -> SumD SubUtxoFailure <! From
    1 -> SumD SubInvalidWitnessesUTXOW <! From
    2 -> SumD SubMissingVKeyWitnessesUTXOW <! From
    3 -> SumD SubScriptWitnessNotValidatingUTXOW <! From
    4 -> SumD SubMissingTxBodyMetadataHash <! From
    5 -> SumD SubMissingTxMetadata <! From
    6 -> SumD SubConflictingMetadataHash <! From
    7 -> SumD SubInvalidMetadata
    8 -> SumD SubMissingRedeemers <! From
    9 -> SumD SubMissingRequiredDatums <! From <! From
    10 -> SumD SubNotAllowedSupplementalDatums <! From <! From
    11 -> SumD SubPPViewHashesDontMatch <! From
    12 -> SumD SubUnspendableUTxONoDatumHash <! From
    13 -> SumD SubExtraRedeemers <! From
    14 -> SumD SubMalformedScriptWitnesses <! From
    15 -> SumD SubMalformedReferenceScripts <! From
    16 -> SumD SubScriptIntegrityHashMismatch <! From <! From
    17 -> SumD SubMalformedGuardDatums <! From
    n -> Invalid n

dijkstraUtxowToDijkstraSubUtxowPredFailure ::
  forall era.
  ( InjectRuleFailure "SUBUTXO" DijkstraUtxoPredFailure era
  , PredicateFailure (EraRule "UTXO" era) ~ DijkstraUtxoPredFailure era
  ) =>
  DijkstraUtxowPredFailure era -> DijkstraSubUtxowPredFailure era
dijkstraUtxowToDijkstraSubUtxowPredFailure = \case
  UtxoFailure f -> SubUtxoFailure (injectFailure @"SUBUTXO" f)
  InvalidWitnessesUTXOW ks -> SubInvalidWitnessesUTXOW ks
  MissingVKeyWitnessesUTXOW ks -> SubMissingVKeyWitnessesUTXOW ks
  MissingScriptWitnessesUTXOW _ -> error "Impossible: `MissingScriptWitnessesUTXOW` for SUBUTXOW"
  ScriptWitnessNotValidatingUTXOW hs -> SubScriptWitnessNotValidatingUTXOW hs
  MissingTxBodyMetadataHash dh -> SubMissingTxBodyMetadataHash dh
  MissingTxMetadata dh -> SubMissingTxMetadata dh
  ConflictingMetadataHash mm -> SubConflictingMetadataHash mm
  InvalidMetadata -> SubInvalidMetadata
  ExtraneousScriptWitnessesUTXOW _ -> error "Impossible: `ExtraneousScriptWitnessesUTXOW` for SUBUTXOW"
  MissingRedeemers pps -> SubMissingRedeemers pps
  MissingRequiredDatums hs1 hs2 -> SubMissingRequiredDatums hs1 hs2
  NotAllowedSupplementalDatums hs1 hs2 -> SubNotAllowedSupplementalDatums hs1 hs2
  PPViewHashesDontMatch mm -> SubPPViewHashesDontMatch mm
  UnspendableUTxONoDatumHash txs -> SubUnspendableUTxONoDatumHash txs
  ExtraRedeemers pps -> SubExtraRedeemers pps
  MalformedScriptWitnesses hs -> SubMalformedScriptWitnesses hs
  MalformedReferenceScripts hs -> SubMalformedReferenceScripts hs
  ScriptIntegrityHashMismatch mm f -> SubScriptIntegrityHashMismatch mm f
  MissingRequiredGuards _ -> error "Impossible: `MissingRequiredGuards` for SUBUTXOW"
  MalformedGuardDatums x -> SubMalformedGuardDatums x
