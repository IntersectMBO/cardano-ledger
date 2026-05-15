{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Pool where

import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (SpecRep, toSpecRep),
  toSpecRepMap,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Base ()

instance SpecTranslate DijkstraEra (PState DijkstraEra) where
  type SpecRep DijkstraEra (PState DijkstraEra) = Agda.PState

  toSpecRep PState {..} =
    Agda.MkPState
      <$> toSpecRepMap (Map.mapWithKey (stakePoolStateToStakePoolParams Testnet) psStakePools)
      <*> toSpecRepMap psFutureStakePoolParams
      <*> toSpecRepMap psRetiring
      <*> toSpecRepMap (fromCompact . spsDeposit <$> psStakePools)

instance SpecTranslate DijkstraEra PoolCert where
  type SpecRep DijkstraEra PoolCert = Agda.DCert

  toSpecRep (RegPool p@StakePoolParams {sppId = KeyHash ppHash}) =
    Agda.Regpool
      <$> toSpecRep ppHash
      <*> toSpecRep p
  toSpecRep (RetirePool (KeyHash ppHash) e) =
    Agda.Retirepool
      <$> toSpecRep ppHash
      <*> toSpecRep e
