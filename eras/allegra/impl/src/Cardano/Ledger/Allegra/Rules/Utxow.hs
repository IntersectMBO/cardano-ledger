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
import Cardano.Ledger.Allegra.Era (AllegraUTXOW)
import Cardano.Ledger.Allegra.Rules.Utxo (AllegraUTXO, AllegraUtxoPredFailure)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxowEvent (..),
  ShelleyUtxowPredFailure (..),
  UtxoEnv,
  transitionRulesUTXOW,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded)
import Cardano.Ledger.UTxO (EraUTxO (..))
import Control.State.Transition.Extended

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
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  ) =>
  STS (AllegraUTXOW era)
  where
  type State (AllegraUTXOW era) = UTxOState era
  type Signal (AllegraUTXOW era) = Tx era
  type Environment (AllegraUTXOW era) = UtxoEnv era
  type BaseM (AllegraUTXOW era) = ShelleyBase
  type PredicateFailure (AllegraUTXOW era) = ShelleyUtxowPredFailure era
  type Event (AllegraUTXOW era) = ShelleyUtxowEvent era

  transitionRules = [transitionRulesUTXOW]

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
  wrapFailed = UtxoFailure
  wrapEvent = UtxoEvent

instance
  ( Era era
  , STS (AllegraUTXOW era)
  , PredicateFailure (EraRule "UTXOW" era) ~ ShelleyUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ Event (AllegraUTXOW era)
  ) =>
  Embed (AllegraUTXOW era) (Shelley.ShelleyLEDGER era)
  where
  wrapFailed = Shelley.UtxowFailure
  wrapEvent = Shelley.UtxowEvent
