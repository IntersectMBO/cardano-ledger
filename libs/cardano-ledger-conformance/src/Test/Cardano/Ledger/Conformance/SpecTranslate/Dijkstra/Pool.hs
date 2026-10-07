{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Pool () where

import Cardano.Crypto.Util (bytesToNatural)
import Cardano.Ledger.BaseTypes (Network, strictMaybeToMaybe)
import Cardano.Ledger.Binary (FixedSizeCodec (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTransM,
  SpecTranslate (..),
  askSpecTransM,
  toSpecRepMap,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Base ()

instance SpecTranslate DijkstraEra (PState DijkstraEra) where
  type SpecRep DijkstraEra (PState DijkstraEra) = Agda.PState

  type SpecContext DijkstraEra (PState DijkstraEra) = Network

  toSpecRep PState {..} = do
    netId <- askSpecTransM
    pools <- Map.traverseWithKey (stakePoolStateToSpec netId) psStakePools
    withCtxSpecTransM () $
      Agda.MkPState
        <$> (Agda.MkHSMap <$> traverse (\(key, value) -> (,value) <$> toSpecRep key) (Map.toList pools))
        <*> toSpecRepMap psFutureStakePoolParams
        <*> toSpecRepMap psRetiring
        <*> toSpecRepMap (fromCompact . spsDeposit <$> psStakePools)

instance SpecTranslate DijkstraEra (PoolCert DijkstraEra) where
  type SpecRep DijkstraEra (PoolCert DijkstraEra) = Agda.DCert

  toSpecRep (RegPool p@StakePoolParams {sppId = KeyHash ppHash}) =
    Agda.Regpool
      <$> toSpecRep ppHash
      <*> toSpecRep p
  toSpecRep (RetirePool (KeyHash ppHash) e) =
    Agda.Retirepool
      <$> toSpecRep ppHash
      <*> toSpecRep e

stakePoolStateToSpec ::
  Network -> KeyHash StakePool -> StakePoolState -> SpecTransM DijkstraEra Network Agda.StakePoolState
stakePoolStateToSpec netId poolId sps =
  withCtxSpecTransM () $ do
    let StakePoolParams {..} = stakePoolStateToStakePoolParams @DijkstraEra netId poolId sps
    bls <-
      traverse
        ( \BlsKeyState {bksKey = BlsKey {..}, ..} -> (,) (toInteger (bytesToNatural (rawEncodeFixedSized blsPubKey))) <$> toSpecRep bksRegisteredIn
        )
        (strictMaybeToMaybe (spsBlsKey sps))
    Agda.StakePoolState
      <$> toSpecRep sppOwners
      <*> toSpecRep sppCost
      <*> toSpecRep sppMargin
      <*> toSpecRep sppPledge
      <*> toSpecRep sppAccountAddress
      <*> toSpecRep sppVrf
      <*> pure bls
