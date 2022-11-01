{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.Rules.Utxow (ShelleyMAUTXOW) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import Cardano.Ledger.Shelley.Rules
  ( ShelleyUtxowEvent (..),
    ShelleyUtxowPredFailure (..),
    UtxoEnv,
    transitionRulesUTXOW,
  )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody)
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits)
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded)
import Cardano.Ledger.ShelleyMA.Era (ShelleyMAUTXOW)
import Cardano.Ledger.ShelleyMA.Rules.Utxo (ShelleyMAUTXO, ShelleyMAUtxoPredFailure)
import Cardano.Ledger.UTxO (EraUTxO (..))
import Control.State.Transition.Extended
import GHC.Records

-- ==============================================================================
--   We want to reuse the same rules for Mary and Allegra. We accomplish this
--   by adding: HasField "minted" (TxBody era) (Set (ScriptHash (Crypto era)))
--   to the (WellFormed era) constraint, and adjusting UTxO.(ScriptsNeeded) to
--   add this set to its output. In the Shelley and Allegra Era, this is the empty set.
--   With this generalization, Cardano.Ledger.Shelley.Rules.Utxow(shelleyStyleWitness)
--   can still be used in Allegra and Mary, because they use the same Shelley style rules.

--------------------------------------------------------------------------------
-- UTXOW STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( EraTx era,
    EraUTxO era,
    ShelleyEraTxBody era,
    TxWits era ~ ShelleyTxWits era,
    ScriptsNeeded era ~ ShelleyScriptsNeeded era,
    -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (ShelleyMAUTXOW era),
    Environment (EraRule "UTXO" era) ~ UtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era,
    Signal (EraRule "UTXO" era) ~ Tx era,
    HasField "_protocolVersion" (PParams era) ProtVer,
    DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  ) =>
  STS (ShelleyMAUTXOW era)
  where
  type State (ShelleyMAUTXOW era) = UTxOState era
  type Signal (ShelleyMAUTXOW era) = Tx era
  type Environment (ShelleyMAUTXOW era) = UtxoEnv era
  type BaseM (ShelleyMAUTXOW era) = ShelleyBase
  type PredicateFailure (ShelleyMAUTXOW era) = ShelleyUtxowPredFailure era
  type Event (ShelleyMAUTXOW era) = ShelleyUtxowEvent era

  transitionRules = [transitionRulesUTXOW]

  -- The ShelleyMA Era uses the same PredicateFailure type
  -- as Shelley, so the 'embed' function is identity
  initialRules = []

instance
  ( Era era,
    STS (ShelleyMAUTXO era),
    PredicateFailure (EraRule "UTXO" era) ~ ShelleyMAUtxoPredFailure era,
    Event (EraRule "UTXO" era) ~ Event (ShelleyMAUTXO era)
  ) =>
  Embed (ShelleyMAUTXO era) (ShelleyMAUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = UtxoEvent

instance
  ( Era era,
    STS (ShelleyMAUTXOW era),
    PredicateFailure (EraRule "UTXOW" era) ~ ShelleyUtxowPredFailure era,
    Event (EraRule "UTXOW" era) ~ Event (ShelleyMAUTXOW era)
  ) =>
  Embed (ShelleyMAUTXOW era) (Shelley.ShelleyLEDGER era)
  where
  wrapFailed = Shelley.UtxowFailure
  wrapEvent = Shelley.UtxowEvent
