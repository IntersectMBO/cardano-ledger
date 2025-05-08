{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.State.Stake () where

import Cardano.Ledger.Conway.State (
  ConwayInstantStake,
  EraStake (..),
  addConwayInstantStake,
  conwayInstantStakeCredentialsL,
  deleteConwayInstantStake,
  resolveConwayInstantStake,
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut ()

instance EraStake DijkstraEra where
  type InstantStake DijkstraEra = ConwayInstantStake DijkstraEra
  instantStakeCredentialsL = conwayInstantStakeCredentialsL
  addInstantStake = addConwayInstantStake
  deleteInstantStake = deleteConwayInstantStake
  resolveInstantStake = resolveConwayInstantStake
