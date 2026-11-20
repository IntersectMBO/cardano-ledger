{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Ledgers (LEDGERS) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes (ShelleyBase, epochInfo, systemStart)
import Cardano.Ledger.Conway.Era (ConwayEra, LEDGER, LEDGERS)
import Cardano.Ledger.Conway.Rules.Cert (ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Certs (ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerEvent, ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (..))
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  LedgerStateWithLimits (..),
  lswlLedgerStateL,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import Data.Default (Default)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Lens.Micro ((^.))

type instance EraRuleFailure "LEDGERS" ConwayEra = Shelley.ShelleyLedgersPredFailure ConwayEra

type instance EraRuleEvent "LEDGERS" ConwayEra = Shelley.ShelleyLedgersEvent ConwayEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgersPredFailure ConwayEra

instance InjectRuleFailure "LEDGERS" ConwayLedgerPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxowPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxowPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxowPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxoPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxoPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxoPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxosPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxosPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Allegra.AllegraUtxoPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertsPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayDelegPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovCertPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovPredFailure ConwayEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance
  ( ApplyTx era
  , EraBlockBody era
  , EraGov era
  , EraStake era
  , Default (CertState era)
  , Embed (EraRule "LEDGER" era) (LEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerStateWithLimits era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
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
  , EraBlockBody era
  , EraGov era
  , EraStake era
  , Default (CertState era)
  , Embed (EraRule "LEDGER" era) (LEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerStateWithLimits era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  ) =>
  TransitionRule (LEDGERS era)
ledgersTransition = do
  TRC (Shelley.LedgersEnv slot epochNo pp account, initLedgerState, txs) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sysStart <- liftSTS $ asks systemStart
  (LedgerStateWithLimits finalLedgerState _, _) <-
    foldM -- cache is lazy on purpose, since it is unused when validation is off
      ( \(!curLedgerState, stAnnTxCache) (ix, tx) -> do
          let
            utxo = curLedgerState ^. lswlLedgerStateL . utxoL
            stAnnTx = mkStAnnTx ei sysStart pp utxo stAnnTxCache tx
          newLedgerState <-
            trans @(EraRule "LEDGER" era) $
              TRC (Shelley.LedgerEnv slot (Just epochNo) ix pp account, curLedgerState, stAnnTx)
          pure (newLedgerState, stAnnTx ^. cacheStAnnTxG)
      )
      (LedgerStateWithLimits initLedgerState mempty, mempty)
      $ zip [minBound ..]
      $ toList txs
  pure finalLedgerState

instance
  ( STS (LEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ ConwayLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ConwayLedgerEvent era
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
