{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Ledgers (LEDGERS) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Babbage.Era (BabbageEra, LEDGER, LEDGERS)
import Cardano.Ledger.Babbage.Rules.Ledger ()
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPredFailure)
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.State
import Cardano.Ledger.BaseTypes (ShelleyBase, epochInfo, systemStart)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (..))
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..))
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
 )
import Data.Default (Default)
import Data.Foldable (toList)
import Data.Sequence (Seq)

type instance EraRuleFailure "LEDGERS" BabbageEra = Shelley.ShelleyLedgersPredFailure BabbageEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgersPredFailure BabbageEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgerPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxowPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxowPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxoPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxoPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxosPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPpupPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Allegra.AllegraUtxoPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegsPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelplPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance
  ( ApplyTx era
  , EraGov era
  , EraStake era
  , Default (CertState era)
  , Embed (EraRule "LEDGER" era) (LEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , Default (LedgerState era)
  ) =>
  STS (LEDGERS era)
  where
  type State (LEDGERS era) = LedgerState era
  type Signal (LEDGERS era) = Seq (Tx TopTx era)
  type Environment (LEDGERS era) = Shelley.ShelleyLedgersEnv era
  type BaseM (LEDGERS era) = ShelleyBase
  type PredicateFailure (LEDGERS era) = Shelley.ShelleyLedgersPredFailure era
  type Event (LEDGERS era) = Shelley.ShelleyLedgersEvent era

  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( ApplyTx era
  , EraGov era
  , EraStake era
  , Default (CertState era)
  , Embed (EraRule "LEDGER" era) (LEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  ) =>
  TransitionRule (LEDGERS era)
ledgersTransition = do
  TRC (Shelley.LedgersEnv slot epochNo pp account, ls, txs) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sysStart <- liftSTS $ asks systemStart
  foldM
    ( \ !ls' (ix, tx) ->
        let utxo = utxosUtxo (lsUTxOState ls')
            stAnnTx = mkStAnnTx ei sysStart pp utxo tx
         in trans @(EraRule "LEDGER" era) $
              TRC (Shelley.LedgerEnv slot (Just epochNo) ix pp account, ls', stAnnTx)
    )
    ls
    $ zip [minBound ..]
    $ toList txs

instance
  ( Era era
  , STS (LEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ Shelley.ShelleyLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ Shelley.ShelleyLedgerEvent era
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
