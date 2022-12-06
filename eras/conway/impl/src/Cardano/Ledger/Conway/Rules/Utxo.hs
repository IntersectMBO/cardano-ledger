{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxo (ConwayUTXO) where

import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoEvent (..), AlonzoUtxoPredFailure (..), AlonzoUtxosEvent, AlonzoUtxosPredFailure, AlonzoUtxowEvent (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage (BabbageTxOut)
import Cardano.Ledger.Babbage.PParams (BabbagePParamsUpdate)
import Cardano.Ledger.Babbage.Rules (BabbageUTXOW, BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (UtxoFailure))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Era (ConwayUTXO, ConwayUTXOS)
import Cardano.Ledger.Conway.Rules.Utxos ()
import Cardano.Ledger.Core
  ( EraPParams (..),
    EraRule,
    EraScript (..),
    EraTx,
    EraTxOut (..),
    Value,
  )
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Rules.ValidationMode (Inject)
import Cardano.Ledger.Shelley.API (PPUPState (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.Rules (ShelleyPpupPredFailure, ShelleyUtxowEvent (UtxoEvent), UtxoEnv (..))
import Control.State.Transition.Extended (Embed (..), STS (..))

instance
  ( Era era,
    EraTx era,
    Value era ~ MaryValue (EraCrypto era),
    TxOut era ~ BabbageTxOut era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    PredicateFailure (EraRule "UTXOS" era) ~ AlonzoUtxosPredFailure era,
    PredicateFailure (EraRule "UTXO" era) ~ BabbageUtxoPredFailure era,
    PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era
  ) =>
  STS (ConwayUTXO era)
  where
  type State (ConwayUTXO era) = Shelley.UTxOState era
  type Signal (ConwayUTXO era) = AlonzoTx era
  type Environment (ConwayUTXO era) = UtxoEnv era
  type BaseM (ConwayUTXO era) = ShelleyBase
  type PredicateFailure (ConwayUTXO era) = BabbageUtxoPredFailure era
  type Event (ConwayUTXO era) = AlonzoUtxoEvent era

  initialRules = []
  transitionRules = []

instance
  ( EraScript era,
    Era era,
    PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era,
    PredicateFailure (EraRule "UTXOS" era) ~ AlonzoUtxosPredFailure era,
    Event (EraRule "UTXOS" era) ~ AlonzoUtxosEvent era,
    Script era ~ AlonzoScript era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Value era ~ MaryValue (EraCrypto era),
    TxOut era ~ BabbageTxOut era,
    PParamsUpdate era ~ BabbagePParamsUpdate era,
    Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Embed (ConwayUTXOS era) (ConwayUTXO era)
  where
  wrapFailed = AlonzoInBabbageUtxoPredFailure . UtxosFailure
  wrapEvent = UtxosEvent

instance
  ( Era era,
    STS (ConwayUTXO era),
    PredicateFailure (EraRule "UTXO" era) ~ BabbageUtxoPredFailure era,
    Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era,
    BaseM (EraRule "UTXOW" era) ~ ShelleyBase,
    PredicateFailure (EraRule "UTXOW" era) ~ BabbageUtxowPredFailure era,
    Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (ConwayUTXO era) (BabbageUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent
