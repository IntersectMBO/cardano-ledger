{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Babbage.Rules.Utxos
  ( BabbageUTXOS,
    UTXOS,
  )
where

import Cardano.Ledger.Alonzo.Language (Language)
import Cardano.Ledger.Alonzo.Rules.Utxos
  ( UTXOS,
    UtxosDelta (UtxosDelta),
    UtxosEvent (..),
    UtxosPredicateFailure (..),
    genericUtxosTransition,
  )
import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.Tx (CostModel, DataHash)
import Cardano.Ledger.Babbage.Collateral (collBalance)
import Cardano.Ledger.Babbage.PParams (PParams, PParamsUpdate)
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..))
import Cardano.Ledger.Babbage.TxBody (TxBody, TxOut)
import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase)
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Mary.Value (Value)
import Cardano.Ledger.Shelley.LedgerState (PPUPState (..), UTxOState (..))
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Ppup (PPUP, PPUPEnv (..), PpupPredicateFailure)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val as Val
import Control.State.Transition.Extended (Embed (..), STS (..))
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Records (HasField (getField))

-- ====================================================
-- Specialize Utxos to Babbage

babbageUtxosDelta ::
  ( Era era,
    Core.TxBody era ~ TxBody era
  ) =>
  UtxosDelta BabbageUTXOS era
babbageUtxosDelta = UtxosDelta collBalFee id
  where
    collBalFee txb utxo = (UTxO utxoKeep, UTxO utxoDel, coin_)
      where
        !(!utxoKeep, !utxoDel) =
          SplitMap.extractKeysSet
            (unUTxO utxo)
            (getField @"collateral" txb)
        !coin_ = Val.coin (collBalance txb (UTxO utxoDel))

-- =======================================

-- | The uninhabited type that marks the Babbage UTXOS rule
data BabbageUTXOS era

instance
  ( ValidateScript era,
    -- Babbage specific types
    Core.TxBody era ~ TxBody era,
    Core.TxOut era ~ TxOut era,
    Core.Value era ~ Value (Crypto era),
    Core.PParams era ~ PParams era,
    Core.Script era ~ Script era,
    Core.PParamsDelta era ~ PParamsUpdate era,
    -- we need to call PPUP
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    Core.EraRule "PPUP" era ~ PPUP era,
    HasField "_protocolVersion" (Core.PParamsDelta era) (StrictMaybe ProtVer),
    -- Substructural properties
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_costmdls" (Core.PParams era) (Map.Map Language CostModel),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "mint" (Core.TxBody era) (Value (Crypto era)),
    HasField "reqSignerHashes" (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  STS (BabbageUTXOS era)
  where
  type BaseM (BabbageUTXOS era) = ShelleyBase
  type Environment (BabbageUTXOS era) = UtxoEnv era
  type State (BabbageUTXOS era) = UTxOState era
  type Signal (BabbageUTXOS era) = ValidatedTx era
  type PredicateFailure (BabbageUTXOS era) = UtxosPredicateFailure era
  type Event (BabbageUTXOS era) = UtxosEvent era
  initialRules = []
  transitionRules = [genericUtxosTransition babbageUtxosDelta]

instance
  ( Era era,
    STS (PPUP era),
    PredicateFailure (Core.EraRule "PPUP" era) ~ PpupPredicateFailure era,
    Event (Core.EraRule "PPUP" era) ~ Event (PPUP era)
  ) =>
  Embed (PPUP era) (BabbageUTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = UpdateEvent
