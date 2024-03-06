{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxow (
  ConwayUTXOW,
  conwayWitsVKeyNeeded,
)
where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent (WrappedShelleyEraEvent),
  AlonzoUtxowPredFailure (..),
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Rules (
  BabbageUTXO,
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure (..),
  babbageUtxowTransition,
 )
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayUTXOW)
import Cardano.Ledger.Conway.Rules.Utxo ()
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.UTxO (getConwayWitsVKeyNeeded)
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
 )
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO)
import Control.State.Transition.Extended (Embed (..), STS (..))
import Data.Set (Set)

conwayWitsVKeyNeeded ::
  (EraTx era, ConwayEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era))
conwayWitsVKeyNeeded = getConwayWitsVKeyNeeded
{-# DEPRECATED conwayWitsVKeyNeeded "In favor of `getConwayWitsVKeyNeeded` or `getWitsVKeyNeeded`" #-}

-- ================================
data ConwayUtxowPredFailure era
  = UtxoFailure (PredicateFailure (EraRule "UTXO" era))
  | InvalidWitnessesUTXOW
      ![VKey 'Witness (EraCrypto era)]
  | -- | witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      -- | witnesses which were needed and not supplied
      !(Set (KeyHash 'Witness (EraCrypto era)))
  | -- | missing scripts
    MissingScriptWitnessesUTXOW
      !(Set (ScriptHash (EraCrypto era)))
  | -- | failed scripts
    ScriptWitnessNotValidatingUTXOW
      !(Set (ScriptHash (EraCrypto era)))
  | -- | hash of the full metadata
    MissingTxBodyMetadataHash
      !(AuxiliaryDataHash (EraCrypto era))
  | -- | hash of the metadata included in the transaction body
    MissingTxMetadata
      !(AuxiliaryDataHash (EraCrypto era))
  | ConflictingMetadataHash
      -- | hash of the metadata included in the transaction body
      !(AuxiliaryDataHash (EraCrypto era))
      -- | expected hash of the full metadata
      !(AuxiliaryDataHash (EraCrypto era))
  | -- | Contains out of range values (string`s too long)
    InvalidMetadata
  | -- | extraneous scripts
    ExtraneousScriptWitnessesUTXOW
      !(Set (ScriptHash (EraCrypto era)))
  | MissingRedeemers
      ![(PlutusPurpose AsItem era, ScriptHash (EraCrypto era))]
  | MissingRequiredDatums
      -- | Set of missing data hashes
      !(Set (DataHash (EraCrypto era)))
      -- | Set of received data hashes
      !(Set (DataHash (EraCrypto era)))
  | NotAllowedSupplementalDatums
      -- | Set of unallowed data hashes
      !(Set (DataHash (EraCrypto era)))
      -- | Set of acceptable supplemental data hashes
      !(Set (DataHash (EraCrypto era)))
  | PPViewHashesDontMatch
      -- | The PPHash in the TxBody
      !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
      -- | Computed from the current Protocol Parameters
      !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      (Set (TxIn (EraCrypto era)))
  | -- | List of redeemers not needed
    ExtraRedeemers ![PlutusPurpose AsIx era]
  | -- | Embed UTXO rule failures
    MalformedScriptWitnesses
      !(Set (ScriptHash (EraCrypto era)))
  | -- | the set of malformed script witnesses
    MalformedReferenceScripts
      !(Set (ScriptHash (EraCrypto era)))
  deriving (Generic)

type instance EraRuleFailure "UTXOW" (ConwayEra c) = ConwayUtxowPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXOW" BabbageUtxowPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxowPredFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxowPredFailure . ShelleyInAlonzoUtxowPredFailure

instance InjectRuleFailure "UTXOW" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , ConwayEraTxBody era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , EraRule "UTXOW" era ~ ConwayUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (ConwayUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (ConwayUTXOW era)
  where
  type State (ConwayUTXOW era) = UTxOState era
  type Signal (ConwayUTXOW era) = Tx era
  type Environment (ConwayUTXOW era) = UtxoEnv era
  type BaseM (ConwayUTXOW era) = ShelleyBase
  type PredicateFailure (ConwayUTXOW era) = ConwayUtxowPredFailure era
  type Event (ConwayUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [babbageUtxowTransition @era]
  initialRules = []

instance
  ( Era era
  , STS (BabbageUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ BabbageUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , BaseM (ConwayUTXOW era) ~ ShelleyBase
  , PredicateFailure (ConwayUTXOW era) ~ ConwayUtxowPredFailure era
  , Event (ConwayUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (BabbageUTXO era) (ConwayUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent
