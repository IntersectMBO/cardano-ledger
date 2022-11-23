{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interface to the Shelley ledger for the purposes of managing a Shelley
-- mempool.
module Cardano.Ledger.Shelley.API.Mempool
  ( ApplyTx (..),
    ApplyTxError (..),
    Validated,
    extractTx,
    coerceValidated,
    translateValidated,

    -- * Exports for testing
    MempoolEnv,
    MempoolState,
    applyTxsTransition,
    unsafeMakeValidated,

    -- * Exports for compatibility
    applyTxs,
    mkMempoolEnv,
    mkMempoolState,
    overNewEpochState,
  )
where

import Cardano.Ledger.BaseTypes (Globals, ShelleyBase)
import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Core
  ( Era,
    EraIndependentTxBody,
    EraRule,
    EraTx (Tx),
    PreviousEra,
    TranslateEra (..),
    TranslationContext,
  )
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Rules ()
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerEnv, ShelleyLedgerPredFailure)
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import Cardano.Ledger.Slot (SlotNo)
import Control.Arrow (ArrowChoice (right), left)
import Control.DeepSeq (NFData)
import Control.Monad.Except (Except, MonadError, foldM, liftEither)
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
import Data.Coerce (Coercible, coerce)
import Data.Functor ((<&>))
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks)

-- | A newtype which indicates that a transaction has been validated against
-- some chain state.
newtype Validated tx = Validated tx
  deriving (Eq, NoThunks, Show, NFData)

-- | Extract the underlying unvalidated Tx.
extractTx :: Validated tx -> tx
extractTx (Validated tx) = tx

coerceValidated :: Coercible a b => Validated a -> Validated b
coerceValidated (Validated a) = Validated $ coerce a

-- Don't use this except in Testing to make Arbitrary instances, etc.
unsafeMakeValidated :: tx -> Validated tx
unsafeMakeValidated = Validated

-- | Translate a validated transaction across eras.
--
-- This is not a `TranslateEra` instance since `Validated` is not itself
-- era-parametrised.
translateValidated ::
  forall era f.
  (TranslateEra era f) =>
  TranslationContext era ->
  Validated (f (PreviousEra era)) ->
  Except (TranslationError era f) (Validated (f era))
translateValidated ctx (Validated tx) = Validated <$> translateEra @era ctx tx

class
  ( EraTx era,
    Eq (ApplyTxError era),
    Show (ApplyTxError era),
    Typeable (ApplyTxError era),
    STS (EraRule "LEDGER" era),
    BaseM (EraRule "LEDGER" era) ~ ShelleyBase,
    Environment (EraRule "LEDGER" era) ~ LedgerEnv era,
    State (EraRule "LEDGER" era) ~ MempoolState era,
    Signal (EraRule "LEDGER" era) ~ Tx era,
    PredicateFailure (EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era
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
    Tx era ->
    m (MempoolState era, Validated (Tx era))
  applyTx globals env state tx =
    let res =
          flip runReader globals
            . applySTS @(EraRule "LEDGER" era)
            $ TRC (env, state, tx)
     in liftEither
          . left ApplyTxError
          . right (,Validated tx)
          $ res

  -- | Reapply a previously validated 'Tx'.
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
  reapplyTx ::
    MonadError (ApplyTxError era) m =>
    Globals ->
    MempoolEnv era ->
    MempoolState era ->
    Validated (Tx era) ->
    m (MempoolState era)
  reapplyTx globals env state (Validated tx) =
    let res =
          flip runReader globals
            . applySTS @(EraRule "LEDGER" era)
            $ TRC (env, state, tx)
     in liftEither
          . left ApplyTxError
          $ res

instance
  ( CC.Crypto c,
    DSignable c (Hash c EraIndependentTxBody)
  ) =>
  ApplyTx (ShelleyEra c)

type MempoolEnv era = Ledger.LedgerEnv era

type MempoolState era = LedgerState.LedgerState era

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
        Ledger.ledgerIx = minBound,
        Ledger.ledgerPp = LedgerState.esPp nesEs,
        Ledger.ledgerAccount = LedgerState.esAccountState nesEs
      }

-- | Construct a mempool state from the wider ledger state.
--
--   The given mempool state may then be evolved using 'applyTxs', but should be
--   regenerated when the ledger state gets updated (e.g. through application of
--   a new block).
mkMempoolState :: NewEpochState era -> MempoolState era
mkMempoolState LedgerState.NewEpochState {LedgerState.nesEs} = LedgerState.esLState nesEs

newtype ApplyTxError era = ApplyTxError [PredicateFailure (EraRule "LEDGER" era)]

deriving stock instance
  (Eq (PredicateFailure (EraRule "LEDGER" era))) =>
  Eq (ApplyTxError era)

deriving stock instance
  (Show (PredicateFailure (EraRule "LEDGER" era))) =>
  Show (ApplyTxError era)

instance
  ( Era era,
    ToCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  ToCBOR (ApplyTxError era)
  where
  toCBOR (ApplyTxError es) = toCBOR es

instance
  ( Era era,
    FromCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  FromCBOR (ApplyTxError era)
  where
  fromCBOR = ApplyTxError <$> fromCBOR

-- | Old 'applyTxs'
applyTxs ::
  (ApplyTx era, MonadError (ApplyTxError era) m) =>
  Globals ->
  SlotNo ->
  Seq (Tx era) ->
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
  Seq (Tx era) ->
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
  Functor f =>
  (MempoolState era -> f (MempoolState era)) ->
  NewEpochState era ->
  f (NewEpochState era)
overNewEpochState f st = do
  f (mkMempoolState st)
    <&> \ls ->
      st
        { LedgerState.nesEs =
            (LedgerState.nesEs st)
              { LedgerState.esLState = ls
              }
        }
