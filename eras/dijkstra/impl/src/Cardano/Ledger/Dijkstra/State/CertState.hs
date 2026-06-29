{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.State.CertState () where

import Cardano.Ledger.Conway.State (
  ConwayCertState,
  ConwayEraCertState (..),
  EraCertState (..),
  conwayCertDStateL,
  conwayCertPStateL,
  conwayCertVStateL,
  conwayCertsTotalDepositsTxBody,
  conwayCertsTotalRefundsTxBody,
  conwayObligationCertState,
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.State.Account ()

instance EraCertState DijkstraEra where
  type CertState DijkstraEra = ConwayCertState DijkstraEra

  certDStateL = conwayCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = conwayCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = conwayObligationCertState

  certsTotalDepositsTxBody = conwayCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = conwayCertsTotalRefundsTxBody

instance ConwayEraCertState DijkstraEra where
  certVStateL = conwayCertVStateL
  {-# INLINE certVStateL #-}
