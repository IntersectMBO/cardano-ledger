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
{-# LANGUAGE TupleSections #-}
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
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxowPredFailure)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (
  checkScriptIntegrityHash,
  hasExactSetOfRedeemers,
  missingRequiredDatums,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..), AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import qualified Cardano.Ledger.Babbage.Rules as Babbage (
  validateFailedBabbageScripts,
  validateScriptsWellFormedTxOuts,
 )
import Cardano.Ledger.Babbage.Tx (mkScriptIntegrity)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
  encodeWord,
 )
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
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  validateMetadata,
  validateNeededWitnesses,
  validateVerifiedWits,
 )
import Cardano.Ledger.State (EraCertState, EraStake, EraUTxO (..), ScriptsProvided (..))
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
  ( EraTx era
  , EraStake era
  , EraCertState era
  , AlonzoEraTxWits era
  , ConwayEraGov era
  , DijkstraEraTxBody era
  , EraPlutusContext era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , InjectRuleFailure "SUBUTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" BabbageUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" DijkstraUtxoPredFailure era
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
    \case
      SubUtxoFailure x -> encodeListLen 2 <> encodeWord 0 <> encCBOR x
      SubInvalidWitnessesUTXOW xs -> encodeListLen 2 <> encodeWord 1 <> encCBOR xs
      SubMissingVKeyWitnessesUTXOW xs -> encodeListLen 2 <> encodeWord 2 <> encCBOR xs
      SubScriptWitnessNotValidatingUTXOW xs -> encodeListLen 2 <> encodeWord 3 <> encCBOR xs
      SubMissingTxBodyMetadataHash xs -> encodeListLen 2 <> encodeWord 4 <> encCBOR xs
      SubMissingTxMetadata xs -> encodeListLen 2 <> encodeWord 5 <> encCBOR xs
      SubConflictingMetadataHash mm -> encodeListLen 2 <> encodeWord 6 <> encCBOR mm
      SubInvalidMetadata -> encodeListLen 1 <> encodeWord 7
      SubMissingRedeemers x -> encodeListLen 2 <> encodeWord 8 <> encCBOR x
      SubMissingRequiredDatums x y -> encodeListLen 3 <> encodeWord 9 <> encCBOR x <> encCBOR y
      SubNotAllowedSupplementalDatums x y -> encodeListLen 3 <> encodeWord 10 <> encCBOR x <> encCBOR y
      SubPPViewHashesDontMatch mm -> encodeListLen 2 <> encodeWord 11 <> encCBOR mm
      SubUnspendableUTxONoDatumHash x -> encodeListLen 2 <> encodeWord 12 <> encCBOR x
      SubExtraRedeemers x -> encodeListLen 2 <> encodeWord 13 <> encCBOR x
      SubMalformedScriptWitnesses x -> encodeListLen 2 <> encodeWord 14 <> encCBOR x
      SubMalformedReferenceScripts x -> encodeListLen 2 <> encodeWord 15 <> encCBOR x
      SubScriptIntegrityHashMismatch x y -> encodeListLen 3 <> encodeWord 16 <> encCBOR x <> encCBOR y
      SubMalformedGuardDatums x -> encodeListLen 2 <> encodeWord 17 <> encCBOR x

instance
  ( ConwayEraScript era
  , DecCBOR (PredicateFailure (EraRule "SUBUTXO" era))
  ) =>
  DecCBOR (DijkstraSubUtxowPredFailure era)
  where
  decCBOR = decodeRecordSum "DijkstraUtxowPred" $ \case
    0 -> fmap (2,) $ SubUtxoFailure <$> decCBOR
    1 -> fmap (2,) $ SubInvalidWitnessesUTXOW <$> decCBOR
    2 -> fmap (2,) $ SubMissingVKeyWitnessesUTXOW <$> decCBOR
    3 -> fmap (2,) $ SubScriptWitnessNotValidatingUTXOW <$> decCBOR
    4 -> fmap (2,) $ SubMissingTxBodyMetadataHash <$> decCBOR
    5 -> fmap (2,) $ SubMissingTxMetadata <$> decCBOR
    6 -> fmap (2,) $ SubConflictingMetadataHash <$> decCBOR
    7 -> pure (1, SubInvalidMetadata)
    8 -> fmap (2,) $ SubMissingRedeemers <$> decCBOR
    9 -> fmap (3,) $ SubMissingRequiredDatums <$> decCBOR <*> decCBOR
    10 -> fmap (3,) $ SubNotAllowedSupplementalDatums <$> decCBOR <*> decCBOR
    11 -> fmap (2,) $ SubPPViewHashesDontMatch <$> decCBOR
    12 -> fmap (2,) $ SubUnspendableUTxONoDatumHash <$> decCBOR
    13 -> fmap (2,) $ SubExtraRedeemers <$> decCBOR
    14 -> fmap (2,) $ SubMalformedScriptWitnesses <$> decCBOR
    15 -> fmap (2,) $ SubMalformedReferenceScripts <$> decCBOR
    16 -> fmap (3,) $ SubScriptIntegrityHashMismatch <$> decCBOR <*> decCBOR
    17 -> fmap (2,) $ SubMalformedGuardDatums <$> decCBOR
    n -> invalidKey n

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
