{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Interface to the Shelley ledger for the purposes of managing a Shelley
-- mempool.
module Cardano.Ledger.Shelley.API.Mempool
  ( MempoolEnv,
    MempoolState,
    mkMempoolEnv,
    mkMempoolState,
    ApplyTxError (..),
    applyTxs,
  )
where

import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Ledger.Shelley.API.Validation
import Cardano.Ledger.Shelley.Crypto
import Control.Arrow (left)
import Control.Monad.Except
import Control.State.Transition (PredicateFailure, TRC (..), applySTS)
import Data.Sequence (Seq)
import qualified LedgerState
import STS.Ledgers (LEDGERS)
import qualified STS.Ledgers as Ledgers
import Slot (Slot)
import TxData (Tx)
import qualified TxData as Tx

type MempoolEnv = Ledgers.LedgersEnv

type MempoolState = LedgerState.LedgerState

-- | Construct the environment used to validate transactions from the full
-- ledger state. Note that this also requires the slot in which transactions are
-- being submitted.
mkMempoolEnv ::
  ShelleyState crypto ->
  Slot ->
  MempoolEnv
mkMempoolEnv
  LedgerState.NewEpochState
    { LedgerState.nesEs
    }
  slot =
    Ledgers.LedgersEnv
      { Ledgers.ledgersSlot = slot,
        Ledgers.ledgersPp = LedgerState.esPp nesEs,
        Ledgers.ledgersReserves =
          LedgerState._reserves $
            LedgerState.esAccountState nesEs
      }

-- | Construct a mempool state from the wider ledger state.
--
--   The given mempool state may then be evolved using 'applyTxs', but should be
--   regenerated when the ledger state gets updated (e.g. through application of
--   a new block).
mkMempoolState :: ShelleyState crypto -> MempoolState crypto
mkMempoolState LedgerState.NewEpochState {LedgerState.nesEs} =
  LedgerState.esLState nesEs

data ApplyTxError crypto = ApplyTxError [PredicateFailure (LEDGERS crypto)]

applyTxs ::
  forall crypto m.
  ( Crypto crypto,
    MonadError (ApplyTxError crypto) m,
    DSIGN.Signable (DSIGN crypto) (Tx.TxBody crypto)
  ) =>
  MempoolEnv ->
  MempoolState crypto ->
  Seq (Tx crypto) ->
  m (MempoolState crypto)
applyTxs env state txs =
  liftEither
    . left (ApplyTxError . join)
    . applySTS @(LEDGERS crypto)
    $ TRC (env, state, txs)
