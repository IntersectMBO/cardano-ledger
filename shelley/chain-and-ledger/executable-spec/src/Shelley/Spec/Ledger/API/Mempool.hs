{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Cardano.Ledger.Shelley (ShelleyBased)
import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
  ( PredicateFailure,
    STS,
    TRC (..),
    applySTS,
  )
import Data.Sequence (Seq)
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.BaseTypes (Globals)
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS)
import qualified Shelley.Spec.Ledger.STS.Ledgers as Ledgers
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx)

type MempoolEnv era = Ledgers.LedgersEnv era

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
  ShelleyState era ->
  SlotNo ->
  MempoolEnv era
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
mkMempoolState :: ShelleyState era -> MempoolState era
mkMempoolState LedgerState.NewEpochState {LedgerState.nesEs} =
  LedgerState.esLState nesEs

data ApplyTxError era = ApplyTxError [PredicateFailure (LEDGERS era)]

deriving stock instance
  (Eq (PredicateFailure (LEDGERS era))) =>
  Eq (ApplyTxError era)

deriving stock instance
  (Show (PredicateFailure (LEDGERS era))) =>
  Show (ApplyTxError era)

instance
  ( ShelleyBased era,
    ToCBOR (PredicateFailure (LEDGERS era))
  ) =>
  ToCBOR (ApplyTxError era)
  where
  toCBOR (ApplyTxError es) = toCBOR es

instance
  ( ShelleyBased era,
    FromCBOR (PredicateFailure (LEDGERS era))
  ) =>
  FromCBOR (ApplyTxError era)
  where
  fromCBOR = ApplyTxError <$> fromCBOR

applyTxs ::
  forall era m.
  ( STS (LEDGERS era),
    MonadError (ApplyTxError era) m
  ) =>
  Globals ->
  MempoolEnv era ->
  Seq (Tx era) ->
  MempoolState era ->
  m (MempoolState era)
applyTxs globals env txs state =
  let res =
        flip runReader globals
          . applySTS @(LEDGERS era)
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
