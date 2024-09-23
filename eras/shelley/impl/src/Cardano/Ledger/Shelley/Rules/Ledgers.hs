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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Ledgers (
  ShelleyLEDGERS,
  ShelleyLedgersEnv (..),
  ShelleyLedgersPredFailure (..),
  ShelleyLedgersEvent (..),
  PredicateFailure,
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyLEDGERS)
import Cardano.Ledger.Shelley.LedgerState (AccountState, LedgerState)
import Cardano.Ledger.Shelley.Rules.Deleg (ShelleyDelegPredFailure)
import Cardano.Ledger.Shelley.Rules.Delegs (ShelleyDelegsPredFailure)
import Cardano.Ledger.Shelley.Rules.Delpl (ShelleyDelplPredFailure)
import Cardano.Ledger.Shelley.Rules.Ledger (
  LedgerEnv (..),
  ShelleyLEDGER,
  ShelleyLedgerEvent,
  ShelleyLedgerPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Pool (ShelleyPoolPredFailure)
import Cardano.Ledger.Shelley.Rules.Ppup (ShelleyPpupPredFailure)
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUtxoPredFailure)
import Cardano.Ledger.Shelley.Rules.Utxow (ShelleyUtxowPredFailure)
import Cardano.Ledger.Slot (SlotNo)
import Control.Monad (foldM)
import Control.State.Transition (
  Embed (..),
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

data ShelleyLedgersEnv era = LedgersEnv
  { ledgersSlotNo :: SlotNo
  , ledgersPp :: PParams era
  , ledgersAccount :: AccountState
  }

newtype ShelleyLedgersPredFailure era
  = LedgerFailure (PredicateFailure (EraRule "LEDGER" era)) -- Subtransition Failures
  deriving (Generic)

type instance EraRuleFailure "LEDGERS" (ShelleyEra c) = ShelleyLedgersPredFailure (ShelleyEra c)

type instance EraRuleEvent "LEDGERS" (ShelleyEra c) = ShelleyLedgersEvent (ShelleyEra c)

instance InjectRuleFailure "LEDGERS" ShelleyLedgersPredFailure (ShelleyEra c)

instance InjectRuleFailure "LEDGERS" ShelleyLedgerPredFailure (ShelleyEra c) where
  injectFailure = LedgerFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxowPredFailure (ShelleyEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxoPredFailure (ShelleyEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPpupPredFailure (ShelleyEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelegsPredFailure (ShelleyEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelplPredFailure (ShelleyEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPoolPredFailure (ShelleyEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelegPredFailure (ShelleyEra c) where
  injectFailure = LedgerFailure . injectFailure

newtype ShelleyLedgersEvent era
  = LedgerEvent (Event (EraRule "LEDGER" era))

deriving stock instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Show (ShelleyLedgersPredFailure era)

deriving stock instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Eq (ShelleyLedgersPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  NoThunks (ShelleyLedgersPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  EncCBOR (ShelleyLedgersPredFailure era)
  where
  encCBOR (LedgerFailure e) = encCBOR e

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  DecCBOR (ShelleyLedgersPredFailure era)
  where
  decCBOR = LedgerFailure <$> decCBOR

instance
  ( Era era
  , Embed (EraRule "LEDGER" era) (ShelleyLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Default (LedgerState era)
  ) =>
  STS (ShelleyLEDGERS era)
  where
  type State (ShelleyLEDGERS era) = LedgerState era
  type Signal (ShelleyLEDGERS era) = Seq (Tx era)
  type Environment (ShelleyLEDGERS era) = ShelleyLedgersEnv era
  type BaseM (ShelleyLEDGERS era) = ShelleyBase
  type PredicateFailure (ShelleyLEDGERS era) = ShelleyLedgersPredFailure era
  type Event (ShelleyLEDGERS era) = ShelleyLedgersEvent era

  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( Embed (EraRule "LEDGER" era) (ShelleyLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  ) =>
  TransitionRule (ShelleyLEDGERS era)
ledgersTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  foldM
    ( \ !ls' (ix, tx) ->
        trans @(EraRule "LEDGER" era) $
          TRC (LedgerEnv slot ix pp account False, ls', tx)
    )
    ls
    $ zip [minBound ..]
    $ toList txwits

instance
  ( Era era
  , STS (ShelleyLEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ ShelleyLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ShelleyLedgerEvent era
  ) =>
  Embed (ShelleyLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent
