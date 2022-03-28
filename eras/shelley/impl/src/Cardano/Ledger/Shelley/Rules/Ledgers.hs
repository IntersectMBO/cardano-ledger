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
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    DPState,
    LedgerState (..),
    UTxOState,
    lsDPState,
    lsUTxOState,
  )
import Cardano.Ledger.Shelley.Rules.Ledger
  ( LEDGER,
    LedgerEnv (..),
    LedgerEvent,
    LedgerPredicateFailure,
  )
import Cardano.Ledger.Shelley.TxBody (EraIndependentTxBody)
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
    ledgersPp :: Core.PParams era,
    ledgersAccount :: AccountState
  }

newtype LedgersPredicateFailure era
  = LedgerFailure (PredicateFailure (Core.EraRule "LEDGER" era)) -- Subtransition Failures
  deriving (Generic)

newtype LedgersEvent era
  = LedgerEvent (Event (Core.EraRule "LEDGER" era))

deriving stock instance
  ( Era era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  Show (LedgersPredicateFailure era)

deriving stock instance
  ( Era era,
    Eq (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  Eq (LedgersPredicateFailure era)

instance
  ( Era era,
    NoThunks (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  NoThunks (LedgersPredicateFailure era)

instance
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  ToCBOR (LedgersPredicateFailure era)
  where
  toCBOR (LedgerFailure e) = toCBOR e

instance
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  FromCBOR (LedgersPredicateFailure era)
  where
  fromCBOR = LedgerFailure <$> fromCBOR

instance
  ( Era era,
    Embed (Core.EraRule "LEDGER" era) (LEDGERS era),
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ (UTxOState era, DPState (Crypto era)),
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Default (LedgerState era)
  ) =>
  STS (LEDGERS era)
  where
  type State (LEDGERS era) = LedgerState era
  type Signal (LEDGERS era) = Seq (Core.Tx era)
  type Environment (LEDGERS era) = LedgersEnv era
  type BaseM (LEDGERS era) = ShelleyBase
  type PredicateFailure (LEDGERS era) = LedgersPredicateFailure era
  type Event (LEDGERS era) = LedgersEvent era

  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( Embed (Core.EraRule "LEDGER" era) (LEDGERS era),
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ (UTxOState era, DPState (Crypto era)),
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era
  ) =>
  TransitionRule (LEDGERS era)
ledgersTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  let (u, dp) = (lsUTxOState ls, lsDPState ls)
  (u'', dp'') <-
    foldM
      ( \(u', dp') (ix, tx) ->
          trans @(Core.EraRule "LEDGER" era) $
            TRC (LedgerEnv slot ix pp account, (u', dp'), tx)
      )
      (u, dp)
      $ zip [minBound ..] $
        toList txwits

  pure $ LedgerState u'' dp''

instance
  ( Era era,
    STS (LEDGER era),
    PredicateFailure (Core.EraRule "LEDGER" era) ~ LedgerPredicateFailure era,
    Event (Core.EraRule "LEDGER" era) ~ LedgerEvent era
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent
