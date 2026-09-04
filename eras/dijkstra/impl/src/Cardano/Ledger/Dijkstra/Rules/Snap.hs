{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dijkstra's SNAP rule. Like Shelley's, but the fresh mark snapshot records
-- the epoch, @leiosCommitteeSize@ protocol parameter, and the maximum honoured
-- voting key age, from which the Leios voting committee (CIP-0164) is seated
-- when the snapshot rotates into the set position, carrying whichever
-- registered BLS keys are still honoured.
module Cardano.Ledger.Dijkstra.Rules.Snap (
  maxKeyAgeEpochs,
) where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochSize (..),
  Globals (..),
  ShelleyBase,
  addEpochInterval,
  epochInfoPure,
  unNonZero,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (SNAP)
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams, ppLeiosCommitteeSizeL)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules (SnapEnv (..), SnapEvent (..))
import Cardano.Ledger.Slot (EpochNo)
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
import Cardano.Slotting.EpochInfo (epochInfoSize)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  tellEvent,
 )
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
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
  (EraTxOut era, EraStake era, EraCertState era, DijkstraEraPParams era) =>
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
      -- The fresh mark snapshot becomes the active stake distribution two epoch
      -- boundaries from now; its committee is judged for that epoch, so a voting
      -- key is honoured against the epoch it will actually vote in (CIP-0164).
      activeEpoch = addEpochInterval eNo (EpochInterval 2)
  -- 'maxKeyAge' is derived from 'Globals', which the pure snapshot rotation
  -- cannot read, so compute it here and record it on the mark. The committee
  -- itself is seated when the mark rotates into the set position.
  maxKeyAge <- liftSTS $ asks (`maxKeyAgeEpochs` activeEpoch)

  tellEvent $
    let stakeMap :: Map (Credential Staking) (Coin, KeyHash StakePool)
        stakeMap =
          Map.map
            (\swd -> (fromCompact $ unNonZero $ swdStake swd, swdDelegation swd))
            (VMap.toMap $ unActiveStake $ ssActiveStake istakeSnap)
     in StakeDistEvent stakeMap

  pure $
    SnapShots
      { -- The mark records the committee size and honoured key age; the Leios
        -- committee is seated from them when this snapshot rotates into the set
        -- position (CIP-0164).
        ssStakeMark = MarkSnapShot istakeSnap eNo (pp ^. ppLeiosCommitteeSizeL) maxKeyAge
      , ssStakeMarkPoolDistr = calculatePoolDistr istakeSnap
      , -- ssStakeMarkPoolDistr exists for performance reasons, see ADR-7
        ssStakeSet = mkSetSnapShot (ssStakeMarkPoolDistr s) (ssStakeMark s)
      , ssStakeGo = mkGoSnapShot (ssStakeSet s)
      , ssFee = fees
      }

-- | Maximum age of a registered Leios voting key (CIP-0164): the KES key
-- lifetime rounded up to whole epochs, plus two epochs of activation delay — a
-- registered key enters the mark snapshot at the next epoch boundary and the
-- active committee at the one after. Deriving the bound from the KES setup keeps
-- voting key rotation in step with the operational key rotation pools do anyway,
-- instead of governing a second cadence through a parameter.
maxKeyAgeEpochs :: Globals -> EpochNo -> EpochInterval
maxKeyAgeEpochs globals e =
  EpochInterval $
    ceiling ((maxKESEvo * slotsPerKESPeriod) % slotsPerEpoch) + 2
  where
    EpochSize slotsPerEpoch = runIdentity $ epochInfoSize (epochInfoPure globals) e

    Globals {maxKESEvo, slotsPerKESPeriod} = globals
