{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA.Rules.Utxow where

import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley.LedgerState
  ( UTxOState,
    witsVKeyNeeded,
  )
import Cardano.Ledger.Shelley.PParams (Update)
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Shelley
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv)
import Cardano.Ledger.Shelley.Rules.Utxow
  ( ShelleyUtxowNeeds,
    UtxowDeltaS (..),
    UtxowEvent (..),
    UtxowPredicateFailure (..),
    genericShelleyUtxow,
  )
import Cardano.Ledger.Shelley.Tx (TxIn, WitnessSet)
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.ShelleyMA.Rules.Utxo (UTXO, UtxoPredicateFailure)
import Control.State.Transition.Extended
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set
import GHC.Records (HasField (getField))

-- ==============================================================================
--   We want to reuse the same rules for Mary and Allegra. We accomplish this
--   by adding: HasField "minted" (Core.TxBody era) (Set (ScriptHash (Crypto era)))
--   to the (WellFormed era) constraint, and adjusting UTxO.(ScriptsNeeded) to
--   add this set to its output. In the Shelley and Allegra Era, this is the empty set.
--   With this generalization, Cardano.Ledger.Shelley.Rules.Utxow(genericShelleyUtxow)
--   can still be used in Allegra and Mary, because they use the same Shelley style rules.

--------------------------------------------------------------------------------
-- UTXOW STS
--------------------------------------------------------------------------------

data UTXOW era

instance
  forall era.
  ( -- Fix Core.Witnesses to the Allegra and Mary Era
    Core.Witnesses era ~ WitnessSet era,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (UTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ Core.Tx era,
    -- Supply the HasField and Validate instances for Mary and Allegra (which match Shelley)
    ShelleyUtxowNeeds era
  ) =>
  STS (UTXOW era)
  where
  type State (UTXOW era) = UTxOState era
  type Signal (UTXOW era) = Core.Tx era
  type Environment (UTXOW era) = UtxoEnv era
  type BaseM (UTXOW era) = ShelleyBase
  type PredicateFailure (UTXOW era) = UtxowPredicateFailure era
  type Event (UTXOW era) = UtxowEvent era

  transitionRules = [genericShelleyUtxow maUtxowDelta]

  -- The ShelleyMA Era uses the same PredicateFailure type
  -- as Shelley, so the 'embed' function is identity
  initialRules = []

maUtxowDelta ::
  ( Era era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set.Set (TxIn (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Ord (Core.Script era)
  ) =>
  UtxowDeltaS UTXOW era
maUtxowDelta = UtxowDeltaS witsVKeyNeeded id txscripts (const Set.empty)
  where
    txscripts wits _utxo = Set.fromList (Map.elems (getField @"scriptWits" wits))

instance
  ( Era era,
    STS (UTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ UtxoPredicateFailure era,
    Event (Core.EraRule "UTXO" era) ~ Event (UTXO era)
  ) =>
  Embed (UTXO era) (UTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = UtxoEvent

instance
  ( Era era,
    STS (UTXOW era),
    PredicateFailure (Core.EraRule "UTXOW" era) ~ UtxowPredicateFailure era,
    Event (Core.EraRule "UTXOW" era) ~ Event (UTXOW era)
  ) =>
  Embed (UTXOW era) (Shelley.LEDGER era)
  where
  wrapFailed = Shelley.UtxowFailure
  wrapEvent = Shelley.UtxowEvent
