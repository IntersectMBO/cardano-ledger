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
    PredicateFailure (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
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
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Crypto (Crypto)
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
import Shelley.Spec.Ledger.Value

data LEDGERS crypto v

data LedgersEnv = LedgersEnv
  { ledgersSlotNo :: SlotNo,
    ledgersPp :: PParams,
    ledgersAccount :: AccountState
  }

instance
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  STS (LEDGERS crypto v)
  where
  type State (LEDGERS crypto v) = LedgerState crypto v
  type Signal (LEDGERS crypto v) = Seq (Tx crypto v)
  type Environment (LEDGERS crypto v) = LedgersEnv
  type BaseM (LEDGERS crypto v) = ShelleyBase
  data PredicateFailure (LEDGERS crypto v)
    = LedgerFailure (PredicateFailure (LEDGER crypto v)) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = [pure emptyLedgerState]
  transitionRules = [ledgersTransition]

instance (CV crypto v) => NoUnexpectedThunks (PredicateFailure (LEDGERS crypto v))

instance
  (CV crypto v) =>
  ToCBOR (PredicateFailure (LEDGERS crypto v))
  where
  toCBOR (LedgerFailure e) = toCBOR e

instance
  (CV crypto v) =>
  FromCBOR (PredicateFailure (LEDGERS crypto v))
  where
  fromCBOR = LedgerFailure <$> fromCBOR

ledgersTransition ::
  forall crypto v.
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  TransitionRule (LEDGERS crypto v)
ledgersTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  let (u, dp) = (_utxoState ls, _delegationState ls)
  (u'', dp'') <-
    foldM
      ( \(u', dp') (ix, tx) ->
          trans @(LEDGER crypto v) $
            TRC (LedgerEnv slot ix pp account, (u', dp'), tx)
      )
      (u, dp)
      $ zip [0 ..] $
        toList txwits

  pure $ LedgerState u'' dp''

instance
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  Embed (LEDGER crypto v) (LEDGERS crypto v)
  where
  wrapFailed = LedgerFailure
