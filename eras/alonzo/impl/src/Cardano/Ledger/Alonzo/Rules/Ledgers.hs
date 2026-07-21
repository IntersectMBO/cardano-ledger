{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Ledgers (
  LEDGERS,
) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.Era (AlonzoEra, LEDGER, LEDGERS)
import Cardano.Ledger.Alonzo.Rules.Ledger ()
import Cardano.Ledger.Alonzo.Rules.Utxo (AlonzoUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxos (AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUtxowPredFailure)
import Cardano.Ledger.BaseTypes (ShelleyBase, epochInfo, systemStart)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (..))
import Cardano.Ledger.Shelley.Core (EraGov)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..))
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (CertState, EraStake)
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
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

type instance EraRuleFailure "LEDGERS" AlonzoEra = Shelley.ShelleyLedgersPredFailure AlonzoEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgersPredFailure AlonzoEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgerPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxowPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxoPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxosPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPpupPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Allegra.AllegraUtxoPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegsPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelplPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure AlonzoEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegPredFailure AlonzoEra where
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

  transitionRules = [alonzoLedgersTransition]

alonzoLedgersTransition ::
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
alonzoLedgersTransition = do
  TRC (Shelley.LedgersEnv slot epochNo pp account, ls, txs) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sysStart <- liftSTS $ asks systemStart
  foldM
    ( \ !ls' (ix, tx) ->
        let utxo = utxosUtxo (lsUTxOState ls')
            stAnnTx = mkStAnnTx (unsafeLinearExtendEpochInfo slot ei) sysStart pp utxo tx
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
