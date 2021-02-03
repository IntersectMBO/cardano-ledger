{-# LANGUAGE DataKinds #-}
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
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
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
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState,
    LedgerState (..),
    UTxOState,
    _delegationState,
    _utxoState,
  )
import Shelley.Spec.Ledger.STS.Ledger
  ( LEDGER,
    LedgerEnv (..),
    LedgerPredicateFailure,
  )
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx)
import Shelley.Spec.Ledger.TxBody (EraIndependentTxBody)

data LEDGERS era

data LedgersEnv era = LedgersEnv
  { ledgersSlotNo :: SlotNo,
    ledgersPp :: Core.PParams era,
    ledgersAccount :: AccountState
  }

data LedgersPredicateFailure era
  = LedgerFailure (PredicateFailure (Core.EraRule "LEDGER" era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( ShelleyBased era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  Show (LedgersPredicateFailure era)

deriving stock instance
  ( ShelleyBased era,
    Eq (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  Eq (LedgersPredicateFailure era)

instance
  ( ShelleyBased era,
    NoThunks (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  NoThunks (LedgersPredicateFailure era)

instance
  ( ShelleyBased era,
    ToCBOR (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  ToCBOR (LedgersPredicateFailure era)
  where
  toCBOR (LedgerFailure e) = toCBOR e

instance
  ( ShelleyBased era,
    FromCBOR (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  FromCBOR (LedgersPredicateFailure era)
  where
  fromCBOR = LedgerFailure <$> fromCBOR

instance
  ( Era era,
    ShelleyBased era,
    Embed (Core.EraRule "LEDGER" era) (LEDGERS era),
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ (UTxOState era, DPState (Crypto era)),
    Signal (Core.EraRule "LEDGER" era) ~ Tx era,
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

  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( Embed (Core.EraRule "LEDGER" era) (LEDGERS era),
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ (UTxOState era, DPState (Crypto era)),
    Signal (Core.EraRule "LEDGER" era) ~ Tx era
  ) =>
  TransitionRule (LEDGERS era)
ledgersTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  let (u, dp) = (_utxoState ls, _delegationState ls)
  (u'', dp'') <-
    foldM
      ( \(u', dp') (ix, tx) ->
          trans @(Core.EraRule "LEDGER" era) $
            TRC (LedgerEnv slot ix pp account, (u', dp'), tx)
      )
      (u, dp)
      $ zip [0 ..] $
        toList txwits

  pure $ LedgerState u'' dp''

instance
  ( Era era,
    STS (LEDGER era),
    PredicateFailure (Core.EraRule "LEDGER" era) ~ LedgerPredicateFailure era
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = LedgerFailure
