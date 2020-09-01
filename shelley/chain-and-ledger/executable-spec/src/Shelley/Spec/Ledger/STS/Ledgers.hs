{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Ledgers
  ( LEDGERS,
    LedgersEnv (..),
    LedgersPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Era (Era)
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Monad (foldM)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    LedgerState (..),
    emptyLedgerState,
    _delegationState,
    _utxoState,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx, TxBody)

data LEDGERS era

data LedgersEnv = LedgersEnv
  { ledgersSlotNo :: SlotNo,
    ledgersPp :: PParams,
    ledgersAccount :: AccountState
  }

data LedgersPredicateFailure era
  = LedgerFailure (PredicateFailure (LEDGER era)) -- Subtransition Failures
  deriving (Show, Eq, Generic)

instance (Era era) => NoUnexpectedThunks (LedgersPredicateFailure era)

instance
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  STS (LEDGERS era)
  where
  type State (LEDGERS era) = LedgerState era
  type Signal (LEDGERS era) = Seq (Tx era)
  type Environment (LEDGERS era) = LedgersEnv
  type BaseM (LEDGERS era) = ShelleyBase
  type PredicateFailure (LEDGERS era) = LedgersPredicateFailure era

  initialRules = [pure emptyLedgerState]
  transitionRules = [ledgersTransition]

instance
  (Typeable era, Era era) =>
  ToCBOR (LedgersPredicateFailure era)
  where
  toCBOR (LedgerFailure e) = toCBOR e

instance
  (Era era) =>
  FromCBOR (LedgersPredicateFailure era)
  where
  fromCBOR = LedgerFailure <$> fromCBOR

ledgersTransition ::
  forall era.
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  TransitionRule (LEDGERS era)
ledgersTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  let (u, dp) = (_utxoState ls, _delegationState ls)
  (u'', dp'') <-
    foldM
      ( \(u', dp') (ix, tx) ->
          trans @(LEDGER era) $
            TRC (LedgerEnv slot ix pp account, (u', dp'), tx)
      )
      (u, dp)
      $ zip [0 ..] $
        toList txwits

  pure $ LedgerState u'' dp''

instance
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = LedgerFailure
