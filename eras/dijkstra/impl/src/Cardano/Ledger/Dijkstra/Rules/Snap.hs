{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dijkstra's SNAP rule. Like Shelley's, but as it builds the fresh mark
-- snapshot it seats the Leios voting committee (CIP-0164) on it, sized by
-- 'leiosCommitteeSize'. The committee travels with the snapshot so consensus
-- can read it two epochs later, when the snapshot becomes the active stake
-- distribution.
module Cardano.Ledger.Dijkstra.Rules.Snap (
  leiosCommitteeSize,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase, unNonZero)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (SNAP)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules (SnapEnv (..), SnapEvent (..))
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
import Control.State.Transition (
  Embed (..),
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
import Data.Word (Word16)
import Lens.Micro ((^.))

-- | Number of top-stake pools seated on the epoch's voting committee, @N_c@ in
-- CIP-0164.
--
-- TODO: replace with the @leiosCommitteeSize@ protocol parameter once the
-- Leios protocol parameters are available on this branch.
leiosCommitteeSize :: Word16
leiosCommitteeSize = 900

instance
  (EraTxOut era, EraStake era, EraCertState era) =>
  STS (SNAP era)
  where
  type State (SNAP era) = SnapShots
  type Signal (SNAP era) = ()
  type Environment (SNAP era) = SnapEnv era
  type BaseM (SNAP era) = ShelleyBase
  type PredicateFailure (SNAP era) = Void
  type Event (SNAP era) = SnapEvent era
  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

snapTransition ::
  (EraStake era, EraCertState era) =>
  TransitionRule (SNAP era)
snapTransition = do
  TRC (snapEnv, s, _) <- judgmentContext

  let SnapEnv ls@(LedgerState (UTxOState _utxo _ fees _ _ _) certState) _pp = snapEnv
      instantStake = ls ^. instantStakeG
      -- The committee is seated here, on the fresh mark snapshot (CIP-0164).
      istakeSnap =
        snapShotFromInstantStake
          leiosCommitteeSize
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

instance
  ( EraTxOut era
  , EraStake era
  , EraCertState era
  , Event (EraRule "SNAP" era) ~ SnapEvent era
  ) =>
  Embed (SNAP era) (Conway.EPOCH era)
  where
  wrapFailed = \case {}
  wrapEvent = Conway.SnapEvent
