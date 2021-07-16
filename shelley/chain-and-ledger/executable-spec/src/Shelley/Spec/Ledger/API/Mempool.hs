{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
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

    -- * Exports for compatibility
    applyTxs,
    mkMempoolEnv,
    mkMempoolState,
    overNewEpochState,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.BaseTypes (Globals, ShelleyBase)
import Cardano.Ledger.Core (AnnotatedData, ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, TxInBlock)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import Cardano.Ledger.Slot (SlotNo)
import Control.Arrow (ArrowChoice (right), left)
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
import Data.Functor ((<&>))
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv, LedgerPredicateFailure)
import qualified Shelley.Spec.Ledger.STS.Ledger as Ledger

class
  ( ChainData (Core.Tx era),
    AnnotatedData (Core.Tx era),
    Eq (ApplyTxError era),
    Show (ApplyTxError era),
    Typeable (ApplyTxError era),
    SerialisableData (ApplyTxError era),
    STS (Core.EraRule "LEDGER" era),
    BaseM (Core.EraRule "LEDGER" era) ~ ShelleyBase,
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ MempoolState era,
    Signal (Core.EraRule "LEDGER" era) ~ TxInBlock era,
    PredicateFailure (Core.EraRule "LEDGER" era) ~ LedgerPredicateFailure era
  ) =>
  ApplyTx era
  where
  -- | Validate a transaction against a mempool state, and return both the new
  -- mempool state and a "validated" 'TxInBlock'.
  --
  -- The meaning of being "validated" depends on the era. In general, a
  -- 'TxInBlock' has had all checks run, and can now only fail due to checks
  -- which depend on the state; most notably, that UTxO inputs disappear.
  applyTx ::
    MonadError (ApplyTxError era) m =>
    Globals ->
    MempoolEnv era ->
    MempoolState era ->
    Core.Tx era ->
    m (MempoolState era, TxInBlock era)
  default applyTx ::
    Core.Tx era ~ TxInBlock era =>
    MonadError (ApplyTxError era) m =>
    Globals ->
    MempoolEnv era ->
    MempoolState era ->
    Core.Tx era ->
    m (MempoolState era, TxInBlock era)
  applyTx globals env state tx =
    let res =
          flip runReader globals
            . applySTS @(Core.EraRule "LEDGER" era)
            $ TRC (env, state, tx)
     in liftEither
          . left ApplyTxError
          . right (,tx)
          $ res

  -- | Reapply a 'TxInBlock'.
  --
  --   This applies the (validated) transaction to a new mempool state. It may
  --   fail due to the mempool state changing (for example, a needed output
  --   having already been spent). It should not fail due to any static check
  --   (such as cryptographic checks).
  --
  --   Implementations of this function may optionally skip the performance of
  --   any static checks. This is not required, but strongly encouraged since
  --   this function will be called each time the mempool revalidates
  --   transactions against a new mempool state.
  applyTxInBlock ::
    MonadError (ApplyTxError era) m =>
    Globals ->
    MempoolEnv era ->
    MempoolState era ->
    TxInBlock era ->
    m (MempoolState era)
  default applyTxInBlock ::
    Core.Tx era ~ TxInBlock era =>
    MonadError (ApplyTxError era) m =>
    Globals ->
    MempoolEnv era ->
    MempoolState era ->
    TxInBlock era ->
    m (MempoolState era)
  applyTxInBlock globals env state tx =
    let res =
          flip runReader globals
            . applySTS @(Core.EraRule "LEDGER" era)
            $ TRC (env, state, tx)
     in liftEither
          . left ApplyTxError
          $ res

  -- | Extract the underlying `Tx` from the `TxInBlock`.
  extractTx :: TxInBlock era -> Core.Tx era
  default extractTx ::
    Core.Tx era ~ TxInBlock era =>
    TxInBlock era ->
    Core.Tx era
  extractTx = id

instance PraosCrypto c => ApplyTx (ShelleyEra c)

type MempoolEnv era = Ledger.LedgerEnv era

type MempoolState era =
  ( LedgerState.UTxOState era,
    LedgerState.DPState (Crypto era)
  )

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
    Ledger.LedgerEnv
      { Ledger.ledgerSlotNo = slot,
        Ledger.ledgerIx = 0,
        Ledger.ledgerPp = LedgerState.esPp nesEs,
        Ledger.ledgerAccount = LedgerState.esAccountState nesEs
      }

-- | Construct a mempool state from the wider ledger state.
--
--   The given mempool state may then be evolved using 'applyTxs', but should be
--   regenerated when the ledger state gets updated (e.g. through application of
--   a new block).
mkMempoolState :: NewEpochState era -> MempoolState era
mkMempoolState LedgerState.NewEpochState {LedgerState.nesEs} =
  (_utxoState, _delegationState)
  where
    LedgerState.LedgerState
      { LedgerState._utxoState,
        LedgerState._delegationState
      } = LedgerState.esLState nesEs

data ApplyTxError era = ApplyTxError [PredicateFailure (Core.EraRule "LEDGER" era)]

deriving stock instance
  (Eq (PredicateFailure (Core.EraRule "LEDGER" era))) =>
  Eq (ApplyTxError era)

deriving stock instance
  (Show (PredicateFailure (Core.EraRule "LEDGER" era))) =>
  Show (ApplyTxError era)

instance
  ( ShelleyBased era,
    ToCBOR (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  ToCBOR (ApplyTxError era)
  where
  toCBOR (ApplyTxError es) = toCBOR es

instance
  ( ShelleyBased era,
    FromCBOR (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  FromCBOR (ApplyTxError era)
  where
  fromCBOR = ApplyTxError <$> fromCBOR

-- | Old 'applyTxs'
applyTxs ::
  ApplyTx era =>
  MonadError (ApplyTxError era) m =>
  Globals ->
  SlotNo ->
  Seq (Core.Tx era) ->
  NewEpochState era ->
  m (NewEpochState era)
applyTxs
  globals
  slot
  txs
  state =
    overNewEpochState (applyTxsTransition globals mempoolEnv txs) state
    where
      mempoolEnv = mkMempoolEnv state slot

applyTxsTransition ::
  forall era m.
  ( ApplyTx era,
    MonadError (ApplyTxError era) m
  ) =>
  Globals ->
  MempoolEnv era ->
  Seq (Core.Tx era) ->
  MempoolState era ->
  m (MempoolState era)
applyTxsTransition globals env txs state =
  foldM
    (\st tx -> fst <$> applyTx globals env st tx)
    state
    txs

-- | Transform a function over mempool states to one over the full
-- 'NewEpochState'.
overNewEpochState ::
  Applicative f =>
  (MempoolState era -> f (MempoolState era)) ->
  NewEpochState era ->
  f (NewEpochState era)
overNewEpochState f st = do
  f (mkMempoolState st)
    <&> \(us, ds) ->
      st
        { LedgerState.nesEs =
            (LedgerState.nesEs st)
              { LedgerState.esLState =
                  LedgerState.LedgerState us ds
              }
        }
