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

module Cardano.Ledger.Dijkstra.Rules.Utxow (
  alonzoToDijkstraUtxowPredFailure,
  babbageToDijkstraUtxowPredFailure,
  DijkstraUTXOW,
  DijkstraUtxowPredFailure (..),
  shelleyToDijkstraUtxowPredFailure,
)
where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent (WrappedShelleyEraEvent),
  AlonzoUtxowPredFailure,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
  babbageUtxowTransition,
 )
import qualified Cardano.Ledger.Babbage.Rules as Babbage (BabbageUtxowPredFailure (..))
import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraUTXO, DijkstraUTXOW)
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxos (DijkstraUtxosPredFailure)
import Cardano.Ledger.Keys (VKey)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley (UTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  ShelleyUtxowPredFailure (..),
  UtxoEnv,
 )
import Cardano.Ledger.State (EraUTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (Embed (..), STS (..))
import Data.Maybe.Strict (StrictMaybe)
import Data.Set (Set)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

-- ================================

-- | Predicate failure type for the Dijkstra Era
data DijkstraUtxowPredFailure era
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
  deriving (Generic)

type instance EraRuleFailure "UTXOW" DijkstraEra = DijkstraUtxowPredFailure DijkstraEra

type instance EraRuleEvent "UTXOW" DijkstraEra = AlonzoUtxowEvent DijkstraEra

instance InjectRuleFailure "UTXOW" DijkstraUtxowPredFailure DijkstraEra

instance InjectRuleFailure "UTXOW" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = babbageToDijkstraUtxowPredFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = alonzoToDijkstraUtxowPredFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraUtxowPredFailure

instance InjectRuleFailure "UTXOW" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" DijkstraUtxosPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = UtxoFailure . injectFailure

deriving instance
  ( DijkstraEraScript era
  , Show (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Show (DijkstraUtxowPredFailure era)

deriving instance
  ( DijkstraEraScript era
  , Eq (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Eq (DijkstraUtxowPredFailure era)

deriving via
  InspectHeapNamed "DijkstraUtxowPred" (DijkstraUtxowPredFailure era)
  instance
    NoThunks (DijkstraUtxowPredFailure era)

instance
  ( DijkstraEraScript era
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
  , DijkstraEraTxBody era
  , EraRule "UTXOW" era ~ DijkstraUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (DijkstraUTXOW era)
  , Environment (EraRule "UTXO" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXO" era) ~ Shelley.UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (DijkstraUTXOW era)
  where
  type State (DijkstraUTXOW era) = Shelley.UTxOState era
  type Signal (DijkstraUTXOW era) = Tx era
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
  ( DijkstraEraScript era
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

instance
  ( DijkstraEraScript era
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  DecCBOR (DijkstraUtxowPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraUtxowPred" $ \case
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
    n -> Invalid n

-- =====================================================
-- Injecting from one PredicateFailure to another

babbageToDijkstraUtxowPredFailure ::
  forall era.
  BabbageUtxowPredFailure era ->
  DijkstraUtxowPredFailure era
babbageToDijkstraUtxowPredFailure = \case
  Babbage.AlonzoInBabbageUtxowPredFailure x -> alonzoToDijkstraUtxowPredFailure x
  Babbage.UtxoFailure x -> UtxoFailure x
  Babbage.MalformedScriptWitnesses xs -> MalformedScriptWitnesses xs
  Babbage.MalformedReferenceScripts xs -> MalformedReferenceScripts xs

alonzoToDijkstraUtxowPredFailure ::
  forall era.
  AlonzoUtxowPredFailure era ->
  DijkstraUtxowPredFailure era
alonzoToDijkstraUtxowPredFailure = \case
  Alonzo.ShelleyInAlonzoUtxowPredFailure f -> shelleyToDijkstraUtxowPredFailure f
  Alonzo.MissingRedeemers rs -> MissingRedeemers rs
  Alonzo.MissingRequiredDatums mds rds -> MissingRequiredDatums mds rds
  Alonzo.NotAllowedSupplementalDatums uds ads -> NotAllowedSupplementalDatums uds ads
  Alonzo.PPViewHashesDontMatch m -> PPViewHashesDontMatch m
  Alonzo.MissingRequiredSigners _xs ->
    error "Impossible case. It will be removed once we are in Dijkstra. See #3972"
  Alonzo.UnspendableUTxONoDatumHash ins -> UnspendableUTxONoDatumHash ins
  Alonzo.ExtraRedeemers xs -> ExtraRedeemers xs

shelleyToDijkstraUtxowPredFailure :: ShelleyUtxowPredFailure era -> DijkstraUtxowPredFailure era
shelleyToDijkstraUtxowPredFailure = \case
  Shelley.InvalidWitnessesUTXOW xs -> InvalidWitnessesUTXOW xs
  Shelley.MissingVKeyWitnessesUTXOW xs -> MissingVKeyWitnessesUTXOW xs
  Shelley.MissingScriptWitnessesUTXOW xs -> MissingScriptWitnessesUTXOW xs
  Shelley.ScriptWitnessNotValidatingUTXOW xs -> ScriptWitnessNotValidatingUTXOW xs
  Shelley.UtxoFailure x -> UtxoFailure x
  Shelley.MIRInsufficientGenesisSigsUTXOW _xs ->
    error "Impossible: MIR has been removed in Dijkstra"
  Shelley.MissingTxBodyMetadataHash x -> MissingTxBodyMetadataHash x
  Shelley.MissingTxMetadata x -> MissingTxMetadata x
  Shelley.ConflictingMetadataHash mm -> ConflictingMetadataHash mm
  Shelley.InvalidMetadata -> InvalidMetadata
  Shelley.ExtraneousScriptWitnessesUTXOW xs -> ExtraneousScriptWitnessesUTXOW xs
