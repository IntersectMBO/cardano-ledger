{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Cardano.Ledger.Dijkstra.Rules.EbLedgers (EBLEDGERS, EbLedgersPredFailure (..)) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Dijkstra.Block.Endorser (
  EbTransactions (..),
  EraEndorserBlock (..),
 )
import Cardano.Ledger.Dijkstra.Core (
  Era,
  EraBlockBody (..),
  EraGov,
  EraRule,
  EraTx (..),
  EraTxOut,
  TxLevel (..),
 )
import Cardano.Ledger.Dijkstra.Era (EBLEDGERS, LEDGER)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerEvent, DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.State (CanSetUTxO (..), EraStake)
import Cardano.Ledger.Shelley.API (
  ApplyTx (..),
  Globals (..),
  LedgerEnv (..),
  ShelleyLedgersEnv (..),
 )
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState,
  LedgerStateWithLimits (..),
  lswlLedgerStateL,
 )
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
 )
import Data.Foldable (Foldable (..))
import GHC.Generics (Generic)
import Lens.Micro ((^.))

newtype EbLedgersPredFailure era = LedgerFailureInEbLedgers (PredicateFailure (EraRule "LEDGER" era))
  deriving (Generic)

deriving instance Eq (PredicateFailure (EraRule "LEDGER" era)) => Eq (EbLedgersPredFailure era)

deriving instance Ord (PredicateFailure (EraRule "LEDGER" era)) => Ord (EbLedgersPredFailure era)

deriving instance Show (PredicateFailure (EraRule "LEDGER" era)) => Show (EbLedgersPredFailure era)

deriving instance
  NFData (PredicateFailure (EraRule "LEDGER" era)) => NFData (EbLedgersPredFailure era)

instance
  EncCBOR (PredicateFailure (EraRule "LEDGER" era)) =>
  EncCBOR (EbLedgersPredFailure era)
  where
  encCBOR (LedgerFailureInEbLedgers x) = encode $ Sum (LedgerFailureInEbLedgers @era) 0 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  DecCBOR (EbLedgersPredFailure era)
  where
  decCBOR = decode $ Summands "EbLedgersPredFailure" $ \case
    0 -> SumD LedgerFailureInEbLedgers <! From
    k -> Invalid k

instance
  ( EraTxOut era
  , EraStake era
  , EraGov era
  , EraEndorserBlock era
  , ApplyTx era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , State (EraRule "LEDGER" era) ~ LedgerStateWithLimits era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , Embed (EraRule "LEDGER" era) (EBLEDGERS era)
  , Monoid (BlockBodyLimits era)
  ) =>
  STS (EBLEDGERS era)
  where
  type Environment (EBLEDGERS era) = ShelleyLedgersEnv era
  type State (EBLEDGERS era) = LedgerState era
  type Signal (EBLEDGERS era) = EndorserBlock era

  type PredicateFailure (EBLEDGERS era) = EbLedgersPredFailure era
  type BaseM (EBLEDGERS era) = ShelleyBase
  type Event (EBLEDGERS era) = DijkstraLedgerEvent era

  initialRules = []
  transitionRules = [ebLedgersTransition]

ebLedgersTransition ::
  forall era.
  ( ApplyTx era
  , EraGov era
  , EraStake era
  , Embed (EraRule "LEDGER" era) (EBLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerStateWithLimits era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , EraEndorserBlock era
  , Monoid (BlockBodyLimits era)
  ) =>
  TransitionRule (EBLEDGERS era)
ebLedgersTransition = do
  TRC (LedgersEnv slot epochNo pp account, initLedgerState, endorserBlock) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sysStart <- liftSTS $ asks systemStart
  (LedgerStateWithLimits finalLedgerState _, _) <-
    foldM
      ( \(!curLedgerState, stAnnTxCache) (ix, tx) -> do
          let utxo = curLedgerState ^. lswlLedgerStateL . utxoL
              stAnnTx = mkStAnnTx ei sysStart pp utxo stAnnTxCache tx
          newLedgerState <-
            trans @(EraRule "LEDGER" era) $
              TRC (LedgerEnv slot (Just epochNo) ix pp account, curLedgerState, stAnnTx)
          pure (newLedgerState, stAnnTx ^. cacheStAnnTxG)
      )
      (LedgerStateWithLimits initLedgerState mempty, mempty)
      $ zip [minBound ..] (toList . unEbTransactions $ endorserBlock ^. transactionsEndorserBlockL)
  pure finalLedgerState

instance
  ( STS (LEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ DijkstraLedgerPredFailure era
  ) =>
  Embed (LEDGER era) (EBLEDGERS era)
  where
  wrapFailed = LedgerFailureInEbLedgers
