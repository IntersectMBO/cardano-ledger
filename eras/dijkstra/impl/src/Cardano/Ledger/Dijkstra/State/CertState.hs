{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.State.CertState () where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.State.Account ()
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
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

-- | Total refunds for a transaction, summed across the top-level tx and its subtransactions
dijkstraCertsTotalRefundsTxBody ::
  forall era l.
  ( EraTx era
  , DijkstraEraTxBody era
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  PParams era ->
  Accounts era ->
  TxBody l era ->
  Coin
dijkstraCertsTotalRefundsTxBody pp _ txBody =
  getTotalRefundsTxCerts pp (const Nothing) $
    withBothTxLevels
      txBody
      ( \topTxBody ->
          (topTxBody ^. certsTxBodyL)
            <> foldMap' (^. bodyTxL . certsTxBodyL) (topTxBody ^. subTransactionsTxBodyL)
      )
      (^. certsTxBodyL)
