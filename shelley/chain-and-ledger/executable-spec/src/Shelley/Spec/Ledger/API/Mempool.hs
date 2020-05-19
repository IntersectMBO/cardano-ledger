{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Interface to the Shelley ledger for the purposes of managing a Shelley
-- mempool.
module Shelley.Spec.Ledger.API.Mempool
  ( MempoolEnv,
    MempoolState,
    mkMempoolEnv,
    mkMempoolState,
    ApplyTxError (..),
    applyTxs,
    overShelleyState,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.BaseTypes (Globals)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Keys
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS)
import qualified Shelley.Spec.Ledger.STS.Ledgers as Ledgers
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx)
import qualified Shelley.Spec.Ledger.Tx as Tx

type MempoolEnv = Ledgers.LedgersEnv

type MempoolState = LedgerState.LedgerState

-- | Construct the environment used to validate transactions from the full
-- ledger state.
--
-- Note that this function also takes a slot. During slot validation, the slot
-- given here is the slot of the block containing the transactions. This slot is
-- used for quite a number of things, but in general these do not determine the
-- validity of the transaction. There are two exceptions:
--
-- - Each transaction has a ttl (time-to-live) value. If the slot is beyond this
--   value, then the transaction is invalid.
-- - If the transaction contains a protocol update proposal, then it may only be
--   included until a certain number of slots before the end of the epoch. A
--   protocol update proposal submitted after this is considered invalid.
mkMempoolEnv ::
  ShelleyState crypto ->
  SlotNo ->
  MempoolEnv
mkMempoolEnv
  LedgerState.NewEpochState
    { LedgerState.nesEs
    }
  slot =
    Ledgers.LedgersEnv
      { Ledgers.ledgersSlotNo = slot,
        Ledgers.ledgersPp = LedgerState.esPp nesEs,
        Ledgers.ledgersAccount = LedgerState.esAccountState nesEs
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
  deriving (Eq, Show)

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (ApplyTxError crypto)
  where
  toCBOR (ApplyTxError es) = toCBOR es

instance
  (Crypto crypto) =>
  FromCBOR (ApplyTxError crypto)
  where
  fromCBOR = ApplyTxError <$> fromCBOR

applyTxs ::
  forall crypto m.
  ( Crypto crypto,
    MonadError (ApplyTxError crypto) m,
    DSignable crypto (Hash crypto (Tx.TxBody crypto))
  ) =>
  Globals ->
  MempoolEnv ->
  Seq (Tx crypto) ->
  MempoolState crypto ->
  m (MempoolState crypto)
applyTxs globals env txs state =
  let res =
        flip runReader globals
          . applySTS @(LEDGERS crypto)
          $ TRC (env, state, txs)
   in liftEither
        . left (ApplyTxError . join)
        $ res

-- | Transform a function over mempool states to one over the full Shelley
-- state.
overShelleyState ::
  Applicative f =>
  (MempoolState c -> f (MempoolState c)) ->
  ShelleyState c ->
  f (ShelleyState c)
overShelleyState f st = do
  res <- f $ mkMempoolState st
  pure $
    st
      { LedgerState.nesEs =
          (LedgerState.nesEs st) {LedgerState.esLState = res}
      }
