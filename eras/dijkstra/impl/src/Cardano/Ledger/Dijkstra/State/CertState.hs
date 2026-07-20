{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.State.CertState () where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxBody (conwayProposalsDeposits)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.State.Account ()
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Val ((<+>))
import Data.Foldable (fold, foldMap')
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))

instance EraCertState DijkstraEra where
  type CertState DijkstraEra = ConwayCertState DijkstraEra

  certDStateL = conwayCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = conwayCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = conwayObligationCertState

  certsTotalDepositsTxBody = dijkstraCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = dijkstraCertsTotalRefundsTxBody

instance ConwayEraCertState DijkstraEra where
  certVStateL = conwayCertVStateL
  {-# INLINE certVStateL #-}

-- | Total deposits for a transaction, summed across the top-level tx and its subtransactions
dijkstraCertsTotalDepositsTxBody ::
  forall era l.
  ( EraTx era
  , DijkstraEraTxBody era
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  PParams era ->
  ConwayCertState era ->
  TxBody l era ->
  Coin
dijkstraCertsTotalDepositsTxBody pp certState = \txBody ->
  -- TODO: restrict to TopTx, once certsTotalDepositsTxBody is restricted to TopTx
  withBothTxLevels
    txBody
    ( \topTxBody ->
        let subTxs = topTxBody ^. subTransactionsTxBodyL
            batchTxCerts =
              foldMap' (^. bodyTxL . certsTxBodyL) subTxs
                <> (topTxBody ^. certsTxBodyL)
         in getTotalDepositsTxCerts pp isPoolReg batchTxCerts
              <+> depositPerTxBody topTxBody
              <+> foldMap' (depositPerTxBody . (^. bodyTxL)) subTxs
    )
    (getTotalDepositsTxBody pp isPoolReg)
  where
    isPoolReg = (`Map.member` psStakePools (conwayCertPState certState))
    depositPerTxBody :: TxBody ll era -> Coin
    depositPerTxBody body =
      conwayProposalsDeposits pp body
        <+> fold (unDirectDeposits (body ^. directDepositsTxBodyL))

-- | Total refunds for a transaction, summed across the top-level tx and its subtransactions
dijkstraCertsTotalRefundsTxBody ::
  forall era l.
  ( EraTx era
  , EraAccounts era
  , DijkstraEraTxBody era
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  PParams era ->
  Accounts era ->
  TxBody l era ->
  Coin
dijkstraCertsTotalRefundsTxBody pp accounts txBody =
  withBothTxLevels
    txBody
    ( \topTxBody ->
        shelleyCertsTotalRefundsTxBody pp accounts topTxBody
          <+> foldMap'
            (shelleyCertsTotalRefundsTxBody pp accounts . (^. bodyTxL))
            (topTxBody ^. subTransactionsTxBodyL)
    )
    (shelleyCertsTotalRefundsTxBody pp accounts)
