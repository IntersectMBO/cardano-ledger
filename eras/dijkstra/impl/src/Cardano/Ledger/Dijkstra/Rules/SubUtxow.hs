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
  DijkstraSUBUTXOW,
  DijkstraSubUtxowPredFailure (..),
  DijkstraSubUtxowEvent (..),
) where

import Cardano.Crypto.Hash (ByteString)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxowPredFailure)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (
  checkScriptIntegrityHash,
  hasExactSetOfRedeemers,
  missingRequiredDatums,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..), AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure)
import qualified Cardano.Ledger.Babbage.Rules as Babbage (
  validateFailedBabbageScripts,
  validateScriptsWellFormedTxOuts,
 )
import Cardano.Ledger.Babbage.Tx (mkScriptIntegrity)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  ConwayUtxowPredFailure,
  alonzoToConwayUtxowPredFailure,
  babbageToConwayUtxowPredFailure,
  shelleyToConwayUtxowPredFailure,
 )
import Cardano.Ledger.Credential (Credential, credScriptHash)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBUTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.SubUtxo
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.Utxow (
  DijkstraUtxowPredFailure (..),
  conwayToDijkstraUtxowPredFailure,
 )
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Keys (VKey)
import Cardano.Ledger.Rules.ValidationMode
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  validateMetadata,
  validateNeededWitnesses,
  validateVerifiedWits,
 )
import Cardano.Ledger.State (EraUTxO (..), ScriptsProvided (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
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

instance InjectRuleFailure "SUBUTXOW" ConwayUtxowPredFailure DijkstraEra where
  injectFailure = dijkstraUtxowToDijkstraSubUtxowPredFailure . conwayToDijkstraUtxowPredFailure

instance InjectRuleFailure "SUBUTXOW" BabbageUtxowPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxowToDijkstraSubUtxowPredFailure
      . conwayToDijkstraUtxowPredFailure
      . babbageToConwayUtxowPredFailure

instance InjectRuleFailure "SUBUTXOW" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxowToDijkstraSubUtxowPredFailure
      . conwayToDijkstraUtxowPredFailure
      . alonzoToConwayUtxowPredFailure

instance InjectRuleFailure "SUBUTXOW" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure =
    dijkstraUtxowToDijkstraSubUtxowPredFailure
      . conwayToDijkstraUtxowPredFailure
      . shelleyToConwayUtxowPredFailure

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
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , Embed (EraRule "SUBUTXO" era) (DijkstraSUBUTXOW era)
  , InjectRuleFailure "SUBUTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" DijkstraSubUtxowPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  STS (DijkstraSUBUTXOW era)
  where
  type State (DijkstraSUBUTXOW era) = UTxOState era
  type Signal (DijkstraSUBUTXOW era) = Tx SubTx era
  type Environment (DijkstraSUBUTXOW era) = SubUtxoEnv era
  type BaseM (DijkstraSUBUTXOW era) = ShelleyBase
  type PredicateFailure (DijkstraSUBUTXOW era) = DijkstraSubUtxowPredFailure era
  type Event (DijkstraSUBUTXOW era) = DijkstraSubUtxowEvent era

  transitionRules = [dijkstraSubUtxowTransition @era]

-- | Validate that requiredTopLevelGuards datums are consistent with the credential type:
-- Plutus script credentials must have a datum, key/native script credentials must not.
validateGuardDatums ::
  forall era.
  DijkstraEraTxBody era =>
  ScriptsProvided era ->
  TxBody SubTx era ->
  Test (DijkstraSubUtxowPredFailure era)
validateGuardDatums (ScriptsProvided scripts) txBody =
  failureOnNonEmptySet malformed SubMalformedGuardDatums
  where
    malformed =
      Map.foldlWithKey' accum mempty (txBody ^. requiredTopLevelGuardsL)
    accum acc cred mbDatum =
      case credScriptHash cred of
        Nothing ->
          -- Key hash: datum must be SNothing
          case mbDatum of
            SNothing -> acc
            SJust _ -> Set.insert cred acc
        Just scriptHash ->
          case Map.lookup scriptHash scripts of
            Just script
              | isNativeScript script ->
                  -- Native script: datum must be SNothing
                  case mbDatum of
                    SNothing -> acc
                    SJust _ -> Set.insert cred acc
              | otherwise ->
                  -- Plutus script: datum must be SJust
                  case mbDatum of
                    SJust _ -> acc
                    SNothing -> Set.insert cred acc
            Nothing -> acc

dijkstraSubUtxowTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , DijkstraEraTxBody era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , Embed (EraRule "SUBUTXO" era) (DijkstraSUBUTXOW era)
  , InjectRuleFailure "SUBUTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "SUBUTXOW" DijkstraSubUtxowPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  TransitionRule (EraRule "SUBUTXOW" era)
dijkstraSubUtxowTransition = do
  TRC (env@(SubUtxoEnv _ pp certState scriptsProvided originalUtxo _), utxoState, tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL
      witsKeyHashes = keyHashWitnessesTxWits (tx ^. witsTxL)

  {- ∀[ (vk , σ) ∈ vKeySigs ] isSigned vk (txidBytes txId) σ -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  let scriptsNeeded = getScriptsNeeded originalUtxo txBody
      scriptHashesNeeded = getScriptsHashesNeeded scriptsNeeded

  {- ∀[ s ∈ p1ScriptsNeeded ] validP1Script vKeyHashesProvided txVldt s -}
  runTest $ Babbage.validateFailedBabbageScripts tx scriptsProvided scriptHashesNeeded

  {- vKeyHashesNeeded ⊆ vKeyHashesProvided -}
  runTest $ Shelley.validateNeededWitnesses witsKeyHashes certState originalUtxo txBody

  {- dataHashesNeeded ⊆ mapˢ hash dataProvided -}
  runTest $ Alonzo.missingRequiredDatums originalUtxo tx

  {- txADhash ≡ map hash txAuxData -}
  runTestOnSignal $ Shelley.validateMetadata pp tx

  let scriptIntegrity = mkScriptIntegrity pp tx scriptsProvided scriptHashesNeeded
  runTest $ Alonzo.checkScriptIntegrityHash tx pp scriptIntegrity

  runTest $ Alonzo.hasExactSetOfRedeemers tx scriptsProvided scriptsNeeded

  runTest $
    Babbage.validateScriptsWellFormedTxOuts
      pp
      (tx ^. witsTxL . scriptTxWitsL)
      (tx ^. bodyTxL . outputsTxBodyL)

  runTest $ validateGuardDatums scriptsProvided txBody

  trans @(EraRule "SUBUTXO" era) $ TRC (env, utxoState, tx)

instance
  ( STS (DijkstraSUBUTXO era)
  , PredicateFailure (EraRule "SUBUTXO" era) ~ DijkstraSubUtxoPredFailure era
  , Event (EraRule "SUBUTXO" era) ~ DijkstraSubUtxoEvent era
  ) =>
  Embed (DijkstraSUBUTXO era) (DijkstraSUBUTXOW era)
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
