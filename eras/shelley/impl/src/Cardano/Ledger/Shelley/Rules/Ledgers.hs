{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Ledgers
  ( LEDGERS,
    LedgersEnv (..),
    LedgersPredicateFailure (..),
    LedgersEvent (..),
    PredicateFailure,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.LedgerState (AccountState, LedgerState)
import Cardano.Ledger.Shelley.Rules.Ledger
  ( LEDGER,
    LedgerEnv (..),
    LedgerEvent,
    LedgerPredicateFailure,
  )
import Cardano.Ledger.Slot (SlotNo)
import Control.Monad (foldM)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Default.Class (Default)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data LEDGERS era

data LedgersEnv era = LedgersEnv
  { ledgersSlotNo :: SlotNo,
    ledgersPp :: PParams era,
    ledgersAccount :: AccountState
  }

newtype LedgersPredicateFailure era
  = LedgerFailure (PredicateFailure (EraRule "LEDGER" era)) -- Subtransition Failures
  deriving (Generic)

newtype LedgersEvent era
  = LedgerEvent (Event (EraRule "LEDGER" era))

deriving stock instance
  ( Era era,
    Show (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Show (LedgersPredicateFailure era)

deriving stock instance
  ( Era era,
    Eq (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Eq (LedgersPredicateFailure era)

instance
  ( Era era,
    NoThunks (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  NoThunks (LedgersPredicateFailure era)

instance
  ( Era era,
    ToCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  ToCBOR (LedgersPredicateFailure era)
  where
  toCBOR (LedgerFailure e) = toCBOR e

instance
  ( Era era,
    FromCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  FromCBOR (LedgersPredicateFailure era)
  where
  fromCBOR = LedgerFailure <$> fromCBOR

instance
  ( Era era,
    Embed (EraRule "LEDGER" era) (LEDGERS era),
    Environment (EraRule "LEDGER" era) ~ LedgerEnv era,
    State (EraRule "LEDGER" era) ~ LedgerState era,
    Signal (EraRule "LEDGER" era) ~ Tx era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Default (LedgerState era)
  ) =>
  STS (LEDGERS era)
  where
  type State (LEDGERS era) = LedgerState era
  type Signal (LEDGERS era) = Seq (Tx era)
  type Environment (LEDGERS era) = LedgersEnv era
  type BaseM (LEDGERS era) = ShelleyBase
  type PredicateFailure (LEDGERS era) = LedgersPredicateFailure era
  type Event (LEDGERS era) = LedgersEvent era

  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( Embed (EraRule "LEDGER" era) (LEDGERS era),
    Environment (EraRule "LEDGER" era) ~ LedgerEnv era,
    State (EraRule "LEDGER" era) ~ LedgerState era,
    Signal (EraRule "LEDGER" era) ~ Tx era
  ) =>
  TransitionRule (LEDGERS era)
ledgersTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  foldM
    ( \ !ls' (ix, tx) ->
        trans @(EraRule "LEDGER" era) $
          TRC (LedgerEnv slot ix pp account, ls', tx)
    )
    ls
    $ zip [minBound ..] $ toList txwits

instance
  ( Era era,
    STS (LEDGER era),
    PredicateFailure (EraRule "LEDGER" era) ~ LedgerPredicateFailure era,
    Event (EraRule "LEDGER" era) ~ LedgerEvent era
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent
