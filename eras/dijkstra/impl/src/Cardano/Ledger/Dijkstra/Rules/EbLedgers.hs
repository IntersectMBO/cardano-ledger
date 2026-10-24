{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.EbLedgers (EBLEDGERS) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Dijkstra.Block.Endorser (
  EbTransactions (..),
  EraEndorserBlock (..),
 )
import Cardano.Ledger.Dijkstra.Core (EraGov, EraRule, EraTx (..), EraTxOut, TxLevel (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.State (CanSetUTxO (..), EraStake)
import Cardano.Ledger.Shelley.API (
  ApplyTx (..),
  Globals (..),
  LedgerEnv (..),
  ShelleyLedgersEnv (..),
 )
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed,
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
 )
import Data.Foldable (Foldable (..))
import Lens.Micro ((^.))

data EBLEDGERS era

data EbLedgersPredFailure era
  deriving (Eq, Ord, Show)

instance
  ( EraTxOut era
  , EraStake era
  , EraGov era
  , EraEndorserBlock era
  , ApplyTx era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , Embed (EraRule "LEDGER" era) (EBLEDGERS era)
  ) =>
  STS (EBLEDGERS era)
  where
  type Environment (EBLEDGERS era) = ShelleyLedgersEnv era
  type State (EBLEDGERS era) = LedgerState era
  type Signal (EBLEDGERS era) = EndorserBlock era

  type PredicateFailure (EBLEDGERS era) = EbLedgersPredFailure era
  type BaseM (EBLEDGERS era) = ShelleyBase

  initialRules = []
  transitionRules = [ebLedgersTransition]

ebLedgersTransition ::
  forall era.
  ( ApplyTx era
  , EraGov era
  , EraStake era
  , Embed (EraRule "LEDGER" era) (EBLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , EraEndorserBlock era
  ) =>
  TransitionRule (EBLEDGERS era)
ebLedgersTransition = do
  TRC (LedgersEnv slot epochNo pp account, initLedgerState, endorserBlock) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sysStart <- liftSTS $ asks systemStart
  foldM
    ( \ !curLedgerState (ix, tx) ->
        let utxo = curLedgerState ^. utxoL
            stAnnTx = mkStAnnTx ei sysStart pp utxo mempty tx
         in trans @(EraRule "LEDGER" era) $
              TRC
                ( LedgerEnv slot (Just epochNo) ix pp account
                , curLedgerState
                , stAnnTx
                )
    )
    initLedgerState
    $ zip [minBound ..] (toList . unEbTransactions $ endorserBlock ^. transactionsEndorserBlockL)

type instance EraRule "EBLEDGERS" DijkstraEra = EBLEDGERS DijkstraEra
