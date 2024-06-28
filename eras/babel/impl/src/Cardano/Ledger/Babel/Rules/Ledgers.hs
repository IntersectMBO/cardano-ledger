{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Ledgers (BabelLEDGERS, BabelLedgersEnv (..)) where

import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure)
import Cardano.Ledger.Babel.Era (BabelEra, BabelLEDGERS)
import Cardano.Ledger.Babel.LedgerState.Types (BabelLedgerState)
import Cardano.Ledger.Babel.Rules.Ledger (BabelLEDGER, BabelLedgerEvent, BabelLedgerPredFailure)
import Cardano.Ledger.Babel.Rules.Pool ()
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure)
import Cardano.Ledger.Babel.Rules.Utxos (BabelUtxosPredFailure)
import Cardano.Ledger.Babel.Rules.Utxow (BabelUtxowPredFailure)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Core (EraGov)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.API.Types (AccountState, LedgerEnv (LedgerEnv), LedgerState)
import Cardano.Ledger.Shelley.LedgerState (EraLedgerState, HasLedgerState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEvent (LedgerEvent),
  ShelleyLedgersPredFailure (..),
 )
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.TxIn (TxIx)
import Control.Monad (foldM)
import Control.State.Transition (
  Embed (wrapEvent, wrapFailed),
  STS (..),
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Default.Class (Default)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import GHC.Generics (Generic)

data BabelLedgersEnv era = BabelLedgersEnv
  { ledgerSlotNo :: !SlotNo
  , ledgerIxStart :: !TxIx
  , ledgerPp :: !(PParams era)
  , ledgerAccount :: !AccountState
  }
  deriving (Generic)

type instance EraRuleFailure "LEDGERS" (BabelEra c) = ShelleyLedgersPredFailure (BabelEra c)

type instance EraRuleEvent "LEDGERS" (BabelEra c) = ShelleyLedgersEvent (BabelEra c)

instance InjectRuleFailure "LEDGERS" ShelleyLedgersPredFailure (BabelEra c)

instance InjectRuleFailure "LEDGERS" BabelLedgerPredFailure (BabelEra c) where
  injectFailure = LedgerFailure

instance InjectRuleFailure "LEDGERS" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance
  ( Era era
  , Embed (EraRule "LEDGER" era) (BabelLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ BabelLedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Default (LedgerState era)
  , HasLedgerState era
  , EraGov era
  ) =>
  STS (BabelLEDGERS era)
  where
  type State (BabelLEDGERS era) = BabelLedgerState era
  type Signal (BabelLEDGERS era) = Seq (Tx era)
  type Environment (BabelLEDGERS era) = BabelLedgersEnv era
  type BaseM (BabelLEDGERS era) = ShelleyBase
  type PredicateFailure (BabelLEDGERS era) = ShelleyLedgersPredFailure era
  type Event (BabelLEDGERS era) = ShelleyLedgersEvent era

  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( Embed (EraRule "LEDGER" era) (BabelLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ BabelLedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  ) =>
  TransitionRule (BabelLEDGERS era)
ledgersTransition = do
  TRC (BabelLedgersEnv slot ixStart pp account, ls, txwits) <-
    judgmentContext
  foldM
    ( \ !ls' (ix, tx) ->
        trans @(EraRule "LEDGER" era) $
          TRC (LedgerEnv slot ix pp account, ls', tx)
    )
    ls
    $ zip [ixStart ..]
    $ toList txwits

instance
  ( Era era
  , STS (BabelLEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ BabelLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ BabelLedgerEvent era
  ) =>
  Embed (BabelLEDGER era) (BabelLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent