{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
  ( ApplyTx (..),
    ApplyTxError (..),

    -- * Exports for testing
    MempoolEnv,
    MempoolState,
    applyTxsTransition,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Core (AnnotatedData, ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
  ( BaseM,
    Environment,
    PredicateFailure,
    STS,
    Signal,
    State,
    TRC (..),
    applySTS,
  )
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.BaseTypes (Globals, ShelleyBase)
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.STS.Ledgers (LedgersEnv, LedgersPredicateFailure)
import qualified Shelley.Spec.Ledger.STS.Ledgers as Ledgers
import Shelley.Spec.Ledger.Slot (SlotNo)

-- TODO #1304: add reapplyTxs
class
  ( ChainData (Core.Tx era),
    AnnotatedData (Core.Tx era),
    Eq (ApplyTxError era),
    Show (ApplyTxError era),
    Typeable (ApplyTxError era),
    SerialisableData (ApplyTxError era),
    STS (Core.EraRule "LEDGERS" era),
    BaseM (Core.EraRule "LEDGERS" era) ~ ShelleyBase,
    Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (Core.EraRule "LEDGERS" era) ~ MempoolState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Core.Tx era),
    PredicateFailure (Core.EraRule "LEDGERS" era) ~ LedgersPredicateFailure era
  ) =>
  ApplyTx era
  where
  applyTxs ::
    MonadError (ApplyTxError era) m =>
    Globals ->
    SlotNo ->
    Seq (Core.Tx era) ->
    NewEpochState era ->
    m (NewEpochState era)
  default applyTxs ::
    (MonadError (ApplyTxError era) m) =>
    Globals ->
    SlotNo ->
    Seq (Core.Tx era) ->
    NewEpochState era ->
    m (NewEpochState era)
  applyTxs globals slot txs state =
    overNewEpochState (applyTxsTransition globals mempoolEnv txs) state
    where
      mempoolEnv = mkMempoolEnv state slot

instance PraosCrypto c => ApplyTx (ShelleyEra c)

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
  NewEpochState era ->
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
mkMempoolState :: NewEpochState era -> MempoolState era
mkMempoolState LedgerState.NewEpochState {LedgerState.nesEs} =
  LedgerState.esLState nesEs

data ApplyTxError era = ApplyTxError [PredicateFailure (Core.EraRule "LEDGERS" era)]

deriving stock instance
  (Eq (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  Eq (ApplyTxError era)

deriving stock instance
  (Show (PredicateFailure (Core.EraRule "LEDGERS" era))) =>
  Show (ApplyTxError era)

instance
  ( ShelleyBased era,
    ToCBOR (PredicateFailure (Core.EraRule "LEDGERS" era))
  ) =>
  ToCBOR (ApplyTxError era)
  where
  toCBOR (ApplyTxError es) = toCBOR es

instance
  ( ShelleyBased era,
    FromCBOR (PredicateFailure (Core.EraRule "LEDGERS" era))
  ) =>
  FromCBOR (ApplyTxError era)
  where
  fromCBOR = ApplyTxError <$> fromCBOR

applyTxsTransition ::
  forall era m.
  ( STS (Core.EraRule "LEDGERS" era),
    BaseM (Core.EraRule "LEDGERS" era) ~ ShelleyBase,
    Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (Core.EraRule "LEDGERS" era) ~ MempoolState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Core.Tx era),
    PredicateFailure (Core.EraRule "LEDGERS" era) ~ LedgersPredicateFailure era,
    MonadError (ApplyTxError era) m
  ) =>
  Globals ->
  MempoolEnv era ->
  Seq (Core.Tx era) ->
  MempoolState era ->
  m (MempoolState era)
applyTxsTransition globals env txs state =
  let res =
        flip runReader globals
          . applySTS @(Core.EraRule "LEDGERS" era)
          $ TRC (env, state, txs)
   in liftEither
        . left (ApplyTxError . join)
        $ res

-- | Transform a function over mempool states to one over the full
-- 'NewEpochState'.
overNewEpochState ::
  Applicative f =>
  (MempoolState era -> f (MempoolState era)) ->
  NewEpochState era ->
  f (NewEpochState era)
overNewEpochState f st = do
  res <- f $ mkMempoolState st
  pure $
    st
      { LedgerState.nesEs =
          (LedgerState.nesEs st) {LedgerState.esLState = res}
      }
