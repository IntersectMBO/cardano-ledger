{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dijkstra's SNAP rule. Like Shelley's, but the fresh mark snapshot records
-- the epoch and @leiosCommitteeSize@ protocol parameter, from which the Leios
-- voting committee (CIP-0164) is seated when the snapshot rotates into the set
-- position.
module Cardano.Ledger.Dijkstra.Rules.Snap () where

import Cardano.Ledger.BaseTypes (EpochNo, ShelleyBase, unNonZero)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (SNAP)
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams, ppLeiosCommitteeSizeL)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules (SnapEnv (..), SnapEvent (..))
import Cardano.Ledger.State (
  EraCertState,
  EraStake,
  MarkSnapShot (..),
  SnapShot (..),
  SnapShots (..),
  calculatePoolDistr,
  certDStateL,
  certPStateL,
  emptySnapShots,
  instantStakeG,
  mkGoSnapShot,
  mkSetSnapShot,
  snapShotFromInstantStake,
  swdDelegation,
  swdStake,
  unActiveStake,
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
import Data.Void (Void)
import Lens.Micro ((^.))

instance
  (EraTxOut era, EraStake era, EraCertState era, DijkstraEraPParams era) =>
  STS (SNAP era)
  where
  type State (SNAP era) = SnapShots era
  type Signal (SNAP era) = EpochNo
  type Environment (SNAP era) = SnapEnv era
  type BaseM (SNAP era) = ShelleyBase
  type PredicateFailure (SNAP era) = Void
  type Event (SNAP era) = SnapEvent era
  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

snapTransition ::
  (EraStake era, EraCertState era, DijkstraEraPParams era) =>
  TransitionRule (SNAP era)
snapTransition = do
  TRC (snapEnv, s, eNo) <- judgmentContext

  let SnapEnv ls@(LedgerState (UTxOState _utxo _ fees _ _ _) certState) pp = snapEnv
      instantStake = ls ^. instantStakeG
      istakeSnap =
        snapShotFromInstantStake
          instantStake
          (certState ^. certDStateL)
          (certState ^. certPStateL)

  tellEvent $
    let stakeMap :: Map (Credential Staking) (Coin, KeyHash StakePool)
        stakeMap =
          Map.map
            (\swd -> (fromCompact $ unNonZero $ swdStake swd, swdDelegation swd))
            (VMap.toMap $ unActiveStake $ ssActiveStake istakeSnap)
     in StakeDistEvent stakeMap

  pure $
    SnapShots
      { -- The mark records the committee size; the Leios committee is seated
        -- from it when this snapshot rotates into the set position (CIP-0164).
        ssStakeMark = MarkSnapShot istakeSnap eNo (pp ^. ppLeiosCommitteeSizeL)
      , ssStakeMarkPoolDistr = calculatePoolDistr istakeSnap
      , -- ssStakeMarkPoolDistr exists for performance reasons, see ADR-7
        ssStakeSet = mkSetSnapShot (ssStakeMarkPoolDistr s) (ssStakeMark s)
      , ssStakeGo = mkGoSnapShot (ssStakeSet s)
      , ssFee = fees
      }
