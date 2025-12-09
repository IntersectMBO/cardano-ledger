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
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
  babbageUtxowTransition,
 )
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
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraUTXO, DijkstraUTXOW)
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Keys (VKey)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley (UTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  UtxoEnv,
 )
import Cardano.Ledger.State (EraUTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
 )
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import GHC.Generics (Generic)
import NoThunks.Class (
  InspectHeapNamed (..),
  NoThunks (..),
 )

-- ================================

-- | Predicate failure type for the Conway Era
data DijkstraUtxowPredFailure era
  = UtxoFailure (PredicateFailure (EraRule "UTXO" era))
  | InvalidWitnessesUTXOW (NonEmpty (VKey Witness))
  | -- | witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      -- | witnesses which were needed and not supplied
      (Set (KeyHash Witness))
  | -- | missing scripts
    MissingScriptWitnessesUTXOW (Set ScriptHash)
  | -- | failed scripts
    ScriptWitnessNotValidatingUTXOW (Set ScriptHash)
  | -- | hash of the full metadata
    MissingTxBodyMetadataHash TxAuxDataHash
  | -- | hash of the metadata included in the transaction body
    MissingTxMetadata TxAuxDataHash
  | ConflictingMetadataHash (Mismatch RelEQ TxAuxDataHash)
  | -- | Contains out of range values (string`s too long)
    InvalidMetadata
  | -- | extraneous scripts
    ExtraneousScriptWitnessesUTXOW (Set ScriptHash)
  | MissingRedeemers (NonEmpty (PlutusPurpose AsItem era, ScriptHash))
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
      (Mismatch RelEQ (StrictMaybe ScriptIntegrityHash))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      -- TODO: Make this NonEmpty #4066
      (Set TxIn)
  | -- | List of redeemers not needed
    ExtraRedeemers (NonEmpty (PlutusPurpose AsIx era))
  | -- | Embed UTXO rule failures
    MalformedScriptWitnesses (Set ScriptHash)
  | -- | the set of malformed script witnesses
    MalformedReferenceScripts (Set ScriptHash)
  | -- | The computed script integrity hash does not match the provided script integrity hash
    ScriptIntegrityHashMismatch
      (Mismatch RelEQ (StrictMaybe ScriptIntegrityHash))
      (StrictMaybe ByteString)
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

deriving via
  InspectHeapNamed "ConwayUtxowPred" (DijkstraUtxowPredFailure era)
  instance
    NoThunks (DijkstraUtxowPredFailure era)

instance
  ( ConwayEraScript era
  , NFData (TxCert era)
  , NFData (PredicateFailure (EraRule "UTXO" era))
  ) =>
  NFData (DijkstraUtxowPredFailure era)

--------------------------------------------------------------------------------
-- DijkstraUTXOW STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , ConwayEraTxBody era
  , EraRule "UTXOW" era ~ DijkstraUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "UTXOW" ConwayUtxowPredFailure era
  , InjectRuleFailure "UTXOW" DijkstraUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (DijkstraUTXOW era)
  , Environment (EraRule "UTXO" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXO" era) ~ Shelley.UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (DijkstraUTXOW era)
  where
  type State (DijkstraUTXOW era) = Shelley.UTxOState era
  type Signal (DijkstraUTXOW era) = Tx TopTx era
  type Environment (DijkstraUTXOW era) = Shelley.UtxoEnv era
  type BaseM (DijkstraUTXOW era) = ShelleyBase
  type PredicateFailure (DijkstraUTXOW era) = DijkstraUtxowPredFailure era
  type Event (DijkstraUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [babbageUtxowTransition @era]
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
