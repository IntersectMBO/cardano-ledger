{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Snap (
  ShelleySNAP,
  PredicateFailure,
  ShelleySnapPredFailure,
  SnapEvent (..),
  SnapEnv (..),
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.EpochBoundary (
  SnapShot (ssDelegations, ssStake),
  SnapShots (..),
  Stake (unStake),
  calculatePoolDistr,
  emptySnapShots,
 )
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.Era (ShelleySNAP)
import Cardano.Ledger.Shelley.LedgerState (
  DPState (..),
  LedgerState (..),
  UTxOState (..),
  incrementalStakeDistr,
 )
import Control.State.Transition (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  tellEvent,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- ======================================================

data ShelleySnapPredFailure era -- No predicate failures
  deriving (Show, Generic, Eq)

instance NoThunks (ShelleySnapPredFailure era)

newtype SnapEvent era
  = StakeDistEvent (Map (Credential 'Staking (EraCrypto era)) (Coin, KeyHash 'StakePool (EraCrypto era)))

data SnapEnv era = SnapEnv !(LedgerState era) !(PParams era)

instance EraTxOut era => STS (ShelleySNAP era) where
  type State (ShelleySNAP era) = SnapShots (EraCrypto era)
  type Signal (ShelleySNAP era) = ()
  type Environment (ShelleySNAP era) = SnapEnv era
  type BaseM (ShelleySNAP era) = ShelleyBase
  type PredicateFailure (ShelleySNAP era) = ShelleySnapPredFailure era
  type Event (ShelleySNAP era) = SnapEvent era
  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

-- | The stake distribution was previously computed as in the spec:
--
-- @
--  stakeDistr @era utxo dstate pstate
-- @
--
-- but is now computed incrementally. We leave the comment as a historical note about
-- where important changes were made to the source code.
snapTransition ::
  forall era.
  TransitionRule (ShelleySNAP era)
snapTransition = do
  TRC (snapEnv, s, _) <- judgmentContext

  let SnapEnv (LedgerState (UTxOState _utxo _ fees _ incStake) (DPState dstate pstate)) _ = snapEnv
      -- stakeSnap = stakeDistr @era utxo dstate pstate  -EraCrypto- HISTORICAL NOTE
      istakeSnap = incrementalStakeDistr @(EraCrypto era) incStake dstate pstate

  tellEvent $
    let stMap :: Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin)
        stMap = VMap.toMap . unStake $ ssStake istakeSnap

        stakeCoinMap :: Map (Credential 'Staking (EraCrypto era)) Coin
        stakeCoinMap = fmap fromCompact stMap

        stakePoolMap :: Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
        stakePoolMap = VMap.toMap $ ssDelegations istakeSnap

        stakeMap :: Map (Credential 'Staking (EraCrypto era)) (Coin, KeyHash 'StakePool (EraCrypto era))
        stakeMap = Map.intersectionWith (,) stakeCoinMap stakePoolMap
     in StakeDistEvent stakeMap

  pure $
    SnapShots
      { ssStakeMark = istakeSnap
      , ssStakeMarkPoolDistr = calculatePoolDistr istakeSnap
      , -- ssStakeMarkPoolDistr exists for performance reasons, see ADR-7
        ssStakeSet = ssStakeMark s
      , ssStakeGo = ssStakeSet s
      , ssFee = fees
      }
