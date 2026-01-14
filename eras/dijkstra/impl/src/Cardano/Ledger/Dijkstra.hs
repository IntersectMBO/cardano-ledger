{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra (DijkstraEra, ApplyTxError (..)) where

import Cardano.Ledger.BaseTypes (Inject (inject))
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Conway.Governance (RunConwayRatify)
import Cardano.Ledger.Dijkstra.BlockBody ()
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Genesis ()
import Cardano.Ledger.Dijkstra.Governance ()
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraLedgerPredFailure,
  DijkstraMempoolPredFailure (LedgerFailure),
 )
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Dijkstra.State.CertState ()
import Cardano.Ledger.Dijkstra.State.Stake ()
import Cardano.Ledger.Dijkstra.Transition ()
import Cardano.Ledger.Dijkstra.Translation ()
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Dijkstra.TxBody ()
import Cardano.Ledger.Dijkstra.TxInfo ()
import Cardano.Ledger.Dijkstra.TxWits ()
import Cardano.Ledger.Dijkstra.UTxO ()
import Cardano.Ledger.Shelley.API (ApplyBlock, ApplyTx (..), ruleApplyTxValidation)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)

instance ApplyTx DijkstraEra where
  newtype ApplyTxError DijkstraEra = DijkstraApplyTxError (NonEmpty (DijkstraMempoolPredFailure DijkstraEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup)
  applyTxValidation validationPolicy globals env state tx =
    first DijkstraApplyTxError $
      ruleApplyTxValidation @"MEMPOOL" validationPolicy globals env state tx

instance ApplyBlock DijkstraEra

instance RunConwayRatify DijkstraEra

instance Inject (NonEmpty (DijkstraMempoolPredFailure DijkstraEra)) (ApplyTxError DijkstraEra) where
  inject = DijkstraApplyTxError

instance Inject (NonEmpty (DijkstraLedgerPredFailure DijkstraEra)) (ApplyTxError DijkstraEra) where
  inject = DijkstraApplyTxError . fmap LedgerFailure
