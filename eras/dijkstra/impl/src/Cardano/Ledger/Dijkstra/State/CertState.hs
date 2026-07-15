{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.State.CertState (
  dijkstraCertsTotalRefundsTxBody,
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.State (
  ConwayCertState,
  ConwayEraCertState (..),
  EraAccounts,
  EraCertState (..),
  conwayCertDStateL,
  conwayCertPStateL,
  conwayCertVStateL,
  conwayCertsTotalDepositsTxBody,
  conwayCertsTotalRefundsTxBody,
  conwayObligationCertState,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.State.Account ()
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Val ((<+>))
import Data.Foldable (foldMap')
import Lens.Micro ((^.))

instance EraCertState DijkstraEra where
  type CertState DijkstraEra = ConwayCertState DijkstraEra

  certDStateL = conwayCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = conwayCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = conwayObligationCertState

  certsTotalDepositsTxBody = conwayCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = dijkstraCertsTotalRefundsTxBody

instance ConwayEraCertState DijkstraEra where
  certVStateL = conwayCertVStateL
  {-# INLINE certVStateL #-}

-- | Batch-aware total refunds for a transaction
dijkstraCertsTotalRefundsTxBody ::
  forall era l.
  ( EraTx era
  , EraAccounts era
  , DijkstraEraTxBody era
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  PParams era ->
  ConwayCertState era ->
  TxBody l era ->
  Coin
dijkstraCertsTotalRefundsTxBody pp certState txBody =
  -- TODO: restrict to TopTx
  withBothTxLevels
    txBody
    ( \topTxBody ->
        conwayCertsTotalRefundsTxBody pp certState topTxBody
          <+> foldMap'
            (conwayCertsTotalRefundsTxBody pp certState . (^. bodyTxL))
            (topTxBody ^. subTransactionsTxBodyL)
    )
    (conwayCertsTotalRefundsTxBody pp certState)
