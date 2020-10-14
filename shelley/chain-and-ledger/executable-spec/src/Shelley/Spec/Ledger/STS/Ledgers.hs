{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
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
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState,
    LedgerState (..),
    UTxOState,
    emptyLedgerState,
    _delegationState,
    _utxoState,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx, TxBody)

data LEDGERS era

data LedgersEnv era = LedgersEnv
  { ledgersSlotNo :: SlotNo,
    ledgersPp :: PParams era,
    ledgersAccount :: AccountState
  }

data LedgersPredicateFailure era
  = LedgerFailure (PredicateFailure (LEDGER era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( ShelleyBased era,
    Show (PredicateFailure (LEDGER era))
  ) =>
  Show (LedgersPredicateFailure era)

deriving stock instance
  ( ShelleyBased era,
    Eq (PredicateFailure (LEDGER era))
  ) =>
  Eq (LedgersPredicateFailure era)

instance
  ( ShelleyBased era,
    NoThunks (PredicateFailure (LEDGER era))
  ) =>
  NoThunks (LedgersPredicateFailure era)

instance
  ( ShelleyBased era,
    ToCBOR (PredicateFailure (LEDGER era))
  ) =>
  ToCBOR (LedgersPredicateFailure era)
  where
  toCBOR (LedgerFailure e) = toCBOR e

instance
  ( ShelleyBased era,
    FromCBOR (PredicateFailure (LEDGER era))
  ) =>
  FromCBOR (LedgersPredicateFailure era)
  where
  fromCBOR = LedgerFailure <$> fromCBOR

instance
  ( Crypto c,
    DSignable (ShelleyEra c) (Hash (ShelleyEra c) (TxBody (ShelleyEra c)))
  ) =>
  STS (LEDGERS (ShelleyEra c))
  where
  type State (LEDGERS (ShelleyEra c)) = LedgerState (ShelleyEra c)
  type Signal (LEDGERS (ShelleyEra c)) = Seq (Tx (ShelleyEra c))
  type Environment (LEDGERS (ShelleyEra c)) = LedgersEnv (ShelleyEra c)
  type BaseM (LEDGERS (ShelleyEra c)) = ShelleyBase
  type PredicateFailure (LEDGERS (ShelleyEra c)) = LedgersPredicateFailure (ShelleyEra c)

  initialRules = [pure emptyLedgerState]
  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( Embed (LEDGER era) (LEDGERS era),
    Environment (LEDGERS era) ~ LedgersEnv era,
    State (LEDGERS era) ~ LedgerState era,
    Signal (LEDGERS era) ~ Seq (Tx era),
    Environment (LEDGER era) ~ LedgerEnv era,
    State (LEDGER era) ~ (UTxOState era, DPState era),
    Signal (LEDGER era) ~ Tx era
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
  ( Crypto c,
    DSignable (ShelleyEra c) (Hash (ShelleyEra c) (TxBody (ShelleyEra c)))
  ) =>
  Embed (LEDGER (ShelleyEra c)) (LEDGERS (ShelleyEra c))
  where
  wrapFailed = LedgerFailure
