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

module Cardano.Ledger.Dijkstra.Rules.Ledgers (LEDGERS) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes (ShelleyBase, epochInfo, systemStart)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, LEDGER, LEDGERS)
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerEvent, DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
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
import Lens.Micro ((^.))

type instance EraRuleFailure "LEDGERS" DijkstraEra = Shelley.ShelleyLedgersPredFailure DijkstraEra

type instance EraRuleEvent "LEDGERS" DijkstraEra = Shelley.ShelleyLedgersEvent DijkstraEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgersPredFailure DijkstraEra

instance InjectRuleFailure "LEDGERS" DijkstraLedgerPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure

instance InjectRuleFailure "LEDGERS" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxowPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxoPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayUtxosPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayCertPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayGovPredFailure DijkstraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraGovPredFailure DijkstraEra where
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
  , PredicateFailure (EraRule "LEDGER" era) ~ DijkstraLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ DijkstraLedgerEvent era
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent
