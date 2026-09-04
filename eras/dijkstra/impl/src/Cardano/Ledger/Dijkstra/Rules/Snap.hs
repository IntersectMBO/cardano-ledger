{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dijkstra's SNAP rule runs Shelley's snapshot rotation and then seats the
-- Leios voting committee (CIP-0164) on the fresh mark snapshot: the top
-- @leiosCommitteeSize@ pools by stake, carrying their registered BLS keys. The
-- committee travels with the snapshot so consensus can read it two epochs later
-- when the snapshot becomes active.
module Cardano.Ledger.Dijkstra.Rules.Snap () where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (SNAP)
import Cardano.Ledger.Dijkstra.PParams (DijkstraEraPParams, ppLeiosCommitteeSizeL)
import Cardano.Ledger.Shelley.Rules (SnapEnv (..), SnapEvent)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (
  EraCertState,
  EraStake,
  SnapShots (..),
  emptySnapShots,
  selectLeiosCommittee,
  ssLeiosCommitteeL,
  ssStakeMarkL,
  ssStakePoolsSnapShot,
 )
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Void (Void, absurd)
import Lens.Micro ((&), (.~), (^.))

instance
  (EraTxOut era, EraStake era, EraCertState era, DijkstraEraPParams era) =>
  STS (SNAP era)
  where
  type State (SNAP era) = SnapShots era
  type Signal (SNAP era) = ()
  type Environment (SNAP era) = SnapEnv era
  type BaseM (SNAP era) = ShelleyBase
  type PredicateFailure (SNAP era) = Void
  type Event (SNAP era) = SnapEvent era
  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

-- | Run Shelley's SNAP to rotate the snapshots, then seat the committee on the
-- fresh mark snapshot it produced.
snapTransition ::
  forall era.
  (EraTxOut era, EraStake era, EraCertState era, DijkstraEraPParams era) =>
  TransitionRule (SNAP era)
snapTransition = do
  TRC (snapEnv@(SnapEnv _ pp), s, sig) <- judgmentContext
  snaps <- trans @(Shelley.SNAP era) $ TRC (snapEnv, s, sig)
  let committee =
        selectLeiosCommittee
          (pp ^. ppLeiosCommitteeSizeL)
          (snaps ^. ssStakeMarkL & ssStakePoolsSnapShot)
  pure $ snaps & ssStakeMarkL . ssLeiosCommitteeL .~ committee

instance
  (EraTxOut era, EraStake era, EraCertState era) =>
  Embed (Shelley.SNAP era) (SNAP era)
  where
  wrapFailed = absurd
  wrapEvent = id
