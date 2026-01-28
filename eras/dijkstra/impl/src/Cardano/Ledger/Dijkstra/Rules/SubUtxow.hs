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
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayUtxoPredFailure, ConwayUtxowPredFailure, UtxoEnv)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBUTXOS,
  DijkstraSUBUTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.SubUtxo
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure (..))
import Cardano.Ledger.Keys (VKey)
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import GHC.Generics (Generic)
import NoThunks.Class (
  InspectHeapNamed (..),
  NoThunks (..),
 )

data DijkstraSubUtxowPredFailure era
  = SubUtxoFailure (PredicateFailure (EraRule "SUBUTXO" era))
  | SubInvalidWitnessesUTXOW (NonEmpty (VKey Witness))
  | -- | witnesses which failed in verifiedWits function
    SubMissingVKeyWitnessesUTXOW
      -- | witnesses which were needed and not supplied
      (NonEmptySet (KeyHash Witness))
  | -- | missing scripts
    SubMissingScriptWitnessesUTXOW (NonEmptySet ScriptHash)
  | -- | failed scripts
    SubScriptWitnessNotValidatingUTXOW (NonEmptySet ScriptHash)
  | -- | hash of the full metadata
    SubMissingTxBodyMetadataHash TxAuxDataHash
  | -- | hash of the metadata included in the transaction body
    SubMissingTxMetadata TxAuxDataHash
  | SubConflictingMetadataHash (Mismatch RelEQ TxAuxDataHash)
  | -- | Contains out of range values (string`s too long)
    SubInvalidMetadata
  | -- | extraneous scripts
    SubExtraneousScriptWitnessesUTXOW (NonEmptySet ScriptHash)
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

deriving via
  InspectHeapNamed "DijkstraSubUtxowPred" (DijkstraSubUtxowPredFailure era)
  instance
    NoThunks (DijkstraSubUtxowPredFailure era)

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

instance InjectRuleFailure "SUBUTXOW" ConwayUtxowPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraSubUtxowPredFailure

instance InjectRuleFailure "SUBUTXOW" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = dijkstraUtxowToDijkstraSubUtxowPredFailure

instance InjectRuleEvent "SUBUTXOW" DijkstraSubUtxowEvent DijkstraEra

newtype DijkstraSubUtxowEvent era = SubUtxo (Event (EraRule "SUBUTXO" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBUTXO" era)) => Eq (DijkstraSubUtxowEvent era)

instance NFData (Event (EraRule "SUBUTXO" era)) => NFData (DijkstraSubUtxowEvent era)

instance
  ( ConwayEraGov era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , Embed (EraRule "SUBUTXO" era) (DijkstraSUBUTXOW era)
  ) =>
  STS (DijkstraSUBUTXOW era)
  where
  type State (DijkstraSUBUTXOW era) = UTxOState era
  type Signal (DijkstraSUBUTXOW era) = Tx SubTx era
  type Environment (DijkstraSUBUTXOW era) = UtxoEnv era
  type BaseM (DijkstraSUBUTXOW era) = ShelleyBase
  type PredicateFailure (DijkstraSUBUTXOW era) = DijkstraSubUtxowPredFailure era
  type Event (DijkstraSUBUTXOW era) = DijkstraSubUtxowEvent era

  transitionRules = [dijkstraSubUtxowTransition @era]

dijkstraSubUtxowTransition ::
  forall era.
  ( EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , Embed (EraRule "SUBUTXO" era) (DijkstraSUBUTXOW era)
  ) =>
  TransitionRule (EraRule "SUBUTXOW" era)
dijkstraSubUtxowTransition = do
  TRC (env, state, signal) <- judgmentContext
  trans @(EraRule "SUBUTXO" era) $ TRC (env, state, signal)

instance
  ( ConwayEraGov era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
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
      SubMissingScriptWitnessesUTXOW xs -> Sum SubMissingScriptWitnessesUTXOW 3 !> To xs
      SubScriptWitnessNotValidatingUTXOW xs -> Sum SubScriptWitnessNotValidatingUTXOW 4 !> To xs
      SubMissingTxBodyMetadataHash xs -> Sum SubMissingTxBodyMetadataHash 5 !> To xs
      SubMissingTxMetadata xs -> Sum SubMissingTxMetadata 6 !> To xs
      SubConflictingMetadataHash mm -> Sum SubConflictingMetadataHash 7 !> To mm
      SubInvalidMetadata -> Sum SubInvalidMetadata 8
      SubExtraneousScriptWitnessesUTXOW xs -> Sum SubExtraneousScriptWitnessesUTXOW 9 !> To xs
      SubMissingRedeemers x -> Sum SubMissingRedeemers 10 !> To x
      SubMissingRequiredDatums x y -> Sum SubMissingRequiredDatums 11 !> To x !> To y
      SubNotAllowedSupplementalDatums x y -> Sum SubNotAllowedSupplementalDatums 12 !> To x !> To y
      SubPPViewHashesDontMatch mm -> Sum SubPPViewHashesDontMatch 13 !> To mm
      SubUnspendableUTxONoDatumHash x -> Sum SubUnspendableUTxONoDatumHash 14 !> To x
      SubExtraRedeemers x -> Sum SubExtraRedeemers 15 !> To x
      SubMalformedScriptWitnesses x -> Sum SubMalformedScriptWitnesses 16 !> To x
      SubMalformedReferenceScripts x -> Sum SubMalformedReferenceScripts 17 !> To x
      SubScriptIntegrityHashMismatch x y -> Sum SubScriptIntegrityHashMismatch 18 !> To x !> To y

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
    3 -> SumD SubMissingScriptWitnessesUTXOW <! From
    4 -> SumD SubScriptWitnessNotValidatingUTXOW <! From
    5 -> SumD SubMissingTxBodyMetadataHash <! From
    6 -> SumD SubMissingTxMetadata <! From
    7 -> SumD SubConflictingMetadataHash <! From
    8 -> SumD SubInvalidMetadata
    9 -> SumD SubExtraneousScriptWitnessesUTXOW <! From
    10 -> SumD SubMissingRedeemers <! From
    11 -> SumD SubMissingRequiredDatums <! From <! From
    12 -> SumD SubNotAllowedSupplementalDatums <! From <! From
    13 -> SumD SubPPViewHashesDontMatch <! From
    14 -> SumD SubUnspendableUTxONoDatumHash <! From
    15 -> SumD SubExtraRedeemers <! From
    16 -> SumD SubMalformedScriptWitnesses <! From
    17 -> SumD SubMalformedReferenceScripts <! From
    18 -> SumD SubScriptIntegrityHashMismatch <! From <! From
    n -> Invalid n

conwayToDijkstraSubUtxowPredFailure ::
  forall era.
  ( InjectRuleFailure "SUBUTXO" ConwayUtxoPredFailure era
  , InjectRuleFailure "SUBUTXO" DijkstraUtxoPredFailure era
  , PredicateFailure (EraRule "UTXO" era) ~ DijkstraUtxoPredFailure era
  ) =>
  ConwayUtxowPredFailure era -> DijkstraSubUtxowPredFailure era
conwayToDijkstraSubUtxowPredFailure = \case
  Conway.UtxoFailure f -> SubUtxoFailure (injectFailure @"SUBUTXO" f)
  Conway.InvalidWitnessesUTXOW ks -> SubInvalidWitnessesUTXOW ks
  Conway.MissingVKeyWitnessesUTXOW ks -> SubMissingVKeyWitnessesUTXOW ks
  Conway.MissingScriptWitnessesUTXOW hs -> SubMissingScriptWitnessesUTXOW hs
  Conway.ScriptWitnessNotValidatingUTXOW hs -> SubScriptWitnessNotValidatingUTXOW hs
  Conway.MissingTxBodyMetadataHash dh -> SubMissingTxBodyMetadataHash dh
  Conway.MissingTxMetadata dh -> SubMissingTxMetadata dh
  Conway.ConflictingMetadataHash mm -> SubConflictingMetadataHash mm
  Conway.InvalidMetadata -> SubInvalidMetadata
  Conway.ExtraneousScriptWitnessesUTXOW hs -> SubExtraneousScriptWitnessesUTXOW hs
  Conway.MissingRedeemers pps -> SubMissingRedeemers pps
  Conway.MissingRequiredDatums hs1 hs2 -> SubMissingRequiredDatums hs1 hs2
  Conway.NotAllowedSupplementalDatums hs1 hs2 -> SubNotAllowedSupplementalDatums hs1 hs2
  Conway.PPViewHashesDontMatch mm -> SubPPViewHashesDontMatch mm
  Conway.UnspendableUTxONoDatumHash txs -> SubUnspendableUTxONoDatumHash txs
  Conway.ExtraRedeemers pps -> SubExtraRedeemers pps
  Conway.MalformedScriptWitnesses hs -> SubMalformedScriptWitnesses hs
  Conway.MalformedReferenceScripts hs -> SubMalformedReferenceScripts hs
  Conway.ScriptIntegrityHashMismatch mm f -> SubScriptIntegrityHashMismatch mm f

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
  MissingScriptWitnessesUTXOW hs -> SubMissingScriptWitnessesUTXOW hs
  ScriptWitnessNotValidatingUTXOW hs -> SubScriptWitnessNotValidatingUTXOW hs
  MissingTxBodyMetadataHash dh -> SubMissingTxBodyMetadataHash dh
  MissingTxMetadata dh -> SubMissingTxMetadata dh
  ConflictingMetadataHash mm -> SubConflictingMetadataHash mm
  InvalidMetadata -> SubInvalidMetadata
  ExtraneousScriptWitnessesUTXOW hs -> SubExtraneousScriptWitnessesUTXOW hs
  MissingRedeemers pps -> SubMissingRedeemers pps
  MissingRequiredDatums hs1 hs2 -> SubMissingRequiredDatums hs1 hs2
  NotAllowedSupplementalDatums hs1 hs2 -> SubNotAllowedSupplementalDatums hs1 hs2
  PPViewHashesDontMatch mm -> SubPPViewHashesDontMatch mm
  UnspendableUTxONoDatumHash txs -> SubUnspendableUTxONoDatumHash txs
  ExtraRedeemers pps -> SubExtraRedeemers pps
  MalformedScriptWitnesses hs -> SubMalformedScriptWitnesses hs
  MalformedReferenceScripts hs -> SubMalformedReferenceScripts hs
  ScriptIntegrityHashMismatch mm f -> SubScriptIntegrityHashMismatch mm f
