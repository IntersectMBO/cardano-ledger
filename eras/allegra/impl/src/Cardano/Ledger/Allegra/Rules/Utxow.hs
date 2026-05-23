{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Utxow (AllegraUTXOW) where

import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Era (AllegraEra, AllegraUTXOW)
import Cardano.Ledger.Allegra.Rules.Utxo (AllegraUTXO, AllegraUtxoPredFailure)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded)
import Cardano.Ledger.State (
  EraCertState (..),
  EraUTxO (..),
 )
import Control.State.Transition.Extended

type instance EraRuleFailure "UTXOW" AllegraEra = Shelley.ShelleyUtxowPredFailure AllegraEra

instance InjectRuleFailure "UTXOW" Shelley.ShelleyUtxowPredFailure AllegraEra

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure AllegraEra where
  injectFailure = Shelley.UtxoFailure

instance InjectRuleFailure "UTXOW" Shelley.ShelleyUtxoPredFailure AllegraEra where
  injectFailure = Shelley.UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" Shelley.ShelleyPpupPredFailure AllegraEra where
  injectFailure = Shelley.UtxoFailure . injectFailure

--------------------------------------------------------------------------------
-- UTXOW STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( EraTx era
  , EraUTxO era
  , ShelleyEraTxBody era
  , ScriptsNeeded era ~ ShelleyScriptsNeeded era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (AllegraUTXOW era)
  , Environment (EraRule "UTXO" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ StAnnTx TopTx era
  , EraRule "UTXOW" era ~ AllegraUTXOW era
  , InjectRuleFailure "UTXOW" Shelley.ShelleyUtxowPredFailure era
  , EraCertState era
  ) =>
  STS (AllegraUTXOW era)
  where
  type State (AllegraUTXOW era) = UTxOState era
  type Signal (AllegraUTXOW era) = StAnnTx TopTx era
  type Environment (AllegraUTXOW era) = Shelley.UtxoEnv era
  type BaseM (AllegraUTXOW era) = ShelleyBase
  type PredicateFailure (AllegraUTXOW era) = Shelley.ShelleyUtxowPredFailure era
  type Event (AllegraUTXOW era) = Shelley.ShelleyUtxowEvent era

  transitionRules = [Shelley.transitionRulesUTXOW]

  -- The Allegra Era uses the same PredicateFailure type
  -- as Shelley, so the 'embed' function is identity
  initialRules = []

instance
  ( Era era
  , STS (AllegraUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ AllegraUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ Event (AllegraUTXO era)
  ) =>
  Embed (AllegraUTXO era) (AllegraUTXOW era)
  where
  wrapFailed = Shelley.UtxoFailure
  wrapEvent = Shelley.UtxoEvent

instance
  ( Era era
  , STS (AllegraUTXOW era)
  , PredicateFailure (EraRule "UTXOW" era) ~ Shelley.ShelleyUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ Event (AllegraUTXOW era)
  ) =>
  Embed (AllegraUTXOW era) (Shelley.LEDGER era)
  where
  wrapFailed = Shelley.UtxowFailure
  wrapEvent = Shelley.UtxowEvent
