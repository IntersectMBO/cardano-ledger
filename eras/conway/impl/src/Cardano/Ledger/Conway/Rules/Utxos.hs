{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxos (ConwayUTXOS) where

import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosEvent, AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Era (ConwayUTXOS)
import Cardano.Ledger.Conway.PParams (BabbagePParamsUpdate)
import Cardano.Ledger.Core (EraPParams (..), EraRule, EraTx (..), Script)
import Cardano.Ledger.Rules.ValidationMode (Inject)
import Cardano.Ledger.Shelley.LedgerState (PPUPState (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules (ShelleyPpupPredFailure, UtxoEnv (..))
import Control.State.Transition.Extended (STS (..))

instance
  ( EraTx era
  , PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era
  , Script era ~ AlonzoScript era
  , State (EraRule "PPUP" era) ~ PPUPState era
  , PParamsUpdate era ~ BabbagePParamsUpdate era
  , Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (ConwayUTXOS era)
  where
  type BaseM (ConwayUTXOS era) = ShelleyBase
  type Environment (ConwayUTXOS era) = UtxoEnv era
  type State (ConwayUTXOS era) = UTxOState era
  type Signal (ConwayUTXOS era) = AlonzoTx era
  type PredicateFailure (ConwayUTXOS era) = AlonzoUtxosPredFailure era
  type Event (ConwayUTXOS era) = AlonzoUtxosEvent era

  transitionRules = []
