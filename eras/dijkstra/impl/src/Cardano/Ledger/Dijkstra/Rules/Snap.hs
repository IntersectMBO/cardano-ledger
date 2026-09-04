{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dijkstra's SNAP rule. Like Shelley's, but as it builds the fresh mark
-- snapshot it seats the Leios voting committee (CIP-0164) on it: the top
-- @leiosCommitteeSize@ pools by stake, carrying whichever registered BLS keys
-- are still honoured. The committee travels with the snapshot so consensus can
-- read it two epochs later, when the snapshot becomes the active stake
-- distribution.
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
  SnapShot (..),
  SnapShots (..),
  calculatePoolDistr,
  certDStateL,
  certPStateL,
  emptySnapShots,
  instantStakeG,
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
      -- The fresh mark snapshot becomes the active stake distribution two epoch
      -- boundaries from now; its committee is judged for that epoch, so a voting
      -- key is honoured against the epoch it will actually vote in (CIP-0164).
      activeEpoch = addEpochInterval eNo (EpochInterval 2)
  maxKeyAge <- liftSTS $ asks (`maxKeyAgeEpochs` activeEpoch)

  let
    -- The committee is seated here, on the fresh mark snapshot, sized by the
    -- Leios committee-size parameter and honouring keys against @activeEpoch@
    -- (CIP-0164).
    istakeSnap =
      snapShotFromInstantStake
        activeEpoch
        maxKeyAge
        (pp ^. ppLeiosCommitteeSizeL)
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
      { ssStakeMark = istakeSnap
      , ssStakeMarkPoolDistr = calculatePoolDistr istakeSnap
      , -- ssStakeMarkPoolDistr exists for performance reasons, see ADR-7
        ssStakeSet = ssStakeMark s
      , ssStakeGo = ssStakeSet s
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
