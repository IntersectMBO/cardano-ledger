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
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    LedgerState (..),
    _delegationState,
    _utxoState,
    emptyLedgerState,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx, TxBody)

data LEDGERS crypto

data LedgersEnv = LedgersEnv
  { ledgersSlotNo :: SlotNo,
    ledgersPp :: PParams,
    ledgersAccount :: AccountState
  }

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  STS (LEDGERS crypto)
  where
  type State (LEDGERS crypto) = LedgerState crypto
  type Signal (LEDGERS crypto) = Seq (Tx crypto)
  type Environment (LEDGERS crypto) = LedgersEnv
  type BaseM (LEDGERS crypto) = ShelleyBase
  data PredicateFailure (LEDGERS crypto)
    = LedgerFailure (PredicateFailure (LEDGER crypto)) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = [pure emptyLedgerState]
  transitionRules = [ledgersTransition]

instance (Crypto crypto) => NoUnexpectedThunks (PredicateFailure (LEDGERS crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (LEDGERS crypto))
  where
  toCBOR (LedgerFailure e) = toCBOR e

instance
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (LEDGERS crypto))
  where
  fromCBOR = LedgerFailure <$> fromCBOR

ledgersTransition ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  TransitionRule (LEDGERS crypto)
ledgersTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  let (u, dp) = (_utxoState ls, _delegationState ls)
  (u'', dp'') <-
    foldM
      ( \(u', dp') (ix, tx) ->
          trans @(LEDGER crypto) $
            TRC (LedgerEnv slot ix pp account, (u', dp'), tx)
      )
      (u, dp)
      $ zip [0 ..]
      $ toList txwits

  pure $ LedgerState u'' dp''

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Embed (LEDGER crypto) (LEDGERS crypto)
  where
  wrapFailed = LedgerFailure
