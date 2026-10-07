{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dijkstra's SNAP rule. Like Shelley's, but the fresh mark snapshot records
-- the epoch, @leiosCommitteeSize@ protocol parameter, and the maximum honoured
-- voting key age, from which the Leios voting committee (CIP-0164) is selected
-- and memoized in the mark snapshot, carrying whichever registered BLS keys are
-- still honoured. The committee is moved into the set position when the mark
-- snapshot rotates.
module Cardano.Ledger.Dijkstra.Rules.Snap (
  maxKeyAgeEpochs,
  kesMaxKeyAgeEpochs,
) where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochSize (..),
  Globals (..),
  ShelleyBase,
  epochInfoPure,
  unNonZero,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Rules (ConwayTickfEvent (..), TICKF)
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
  certDStateL,
  certPStateL,
  emptySnapShots,
  instantStakeG,
  mkGoSnapShot,
  mkMarkSnapShot,
  mkSetSnapShot,
  snapShotFromInstantStake,
  swdDelegation,
  swdStake,
  unActiveStake,
 )
import Cardano.Slotting.EpochInfo (epochInfoSize)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Embed (..),
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
import Data.Word (Word64)
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

instance
  ( EraTxOut era
  , EraStake era
  , EraCertState era
  , DijkstraEraPParams era
  , Event (EraRule "SNAP" era) ~ SnapEvent era
  ) =>
  Embed (SNAP era) (TICKF era)
  where
  wrapFailed = \case {}
  wrapEvent = TickfSnapEvent

snapTransition ::
  (EraTxOut era, EraStake era, EraCertState era, DijkstraEraPParams era) =>
  TransitionRule (SNAP era)
snapTransition = do
  TRC (snapEnv, s, eNo) <- judgmentContext

  -- 'maxKeyAge' is derived from 'Globals', which the pure snapshot construction
  -- cannot read, so compute it here and memoize in the fresh mark snapshot the
  -- committee it will seat once it rotates into the set position. Measure against
  -- @eNo@ (the epoch we are entering), not a later one: this only needs an epoch
  -- /length/ to turn the KES lifetime into a count of epochs, and a future
  -- epoch's length is past the forecast horizon whenever the stability window is
  -- shorter than an epoch. Note that this measures the key age against the epoch
  -- the mark is created in, rather than the one after it, which is totally fine,
  -- because it cannot be changed without major changes to Ledger and Consensus.
  -- This value depends solely on a config option from 'Globals', which was set
  -- when we entered Shelley era.
  maxKeyAge <- liftSTS $ asks (`maxKeyAgeEpochs` eNo)

  let SnapEnv ls@(LedgerState (UTxOState _utxo _ fees _ _ _) certState) pp = snapEnv
      instantStake = ls ^. instantStakeG
      istakeSnap =
        snapShotFromInstantStake instantStake (certState ^. certDStateL) (certState ^. certPStateL)
      markSnapShot = mkMarkSnapShot istakeSnap eNo (pp ^. ppLeiosCommitteeSizeL) maxKeyAge

  tellEvent $
    let stakeMap :: Map (Credential Staking) (Coin, KeyHash StakePool)
        stakeMap =
          Map.map
            (\swd -> (fromCompact $ unNonZero $ swdStake swd, swdDelegation swd))
            (VMap.toMap $ unActiveStake $ ssActiveStake istakeSnap)
     in StakeDistEvent stakeMap

  pure $
    SnapShots
      { -- The mark memoizes the Leios committee (CIP-0164), which is moved into
        -- the 'set' position together with the snapshot upon rotation.
        ssStakeMark = markSnapShot
      , ssStakeSet = mkSetSnapShot (ssStakeMark s)
      , ssStakeGo = mkGoSnapShot (ssStakeSet s)
      , ssFee = fees
      }

-- | Maximum age of a registered Leios voting key (CIP-0164): the KES key
-- lifetime rounded up to whole epochs, plus two epochs of activation delay — a
-- registered key enters the mark snapshot at the next epoch boundary and the
-- active committee at the one after. Deriving the bound from the KES setup keeps
-- voting key rotation in step with the operational key rotation pools do anyway,
-- instead of governing a second cadence through a parameter.
-- The epoch argument only fixes the epoch /length/ used for the conversion, so
-- pass one that is already known -- asking for a future epoch's size can fall
-- past the hard-fork forecast horizon and throw.
maxKeyAgeEpochs :: Globals -> EpochNo -> EpochInterval
maxKeyAgeEpochs globals e = kesMaxKeyAgeEpochs maxKESEvo slotsPerKESPeriod epochSize
  where
    -- Safe against the forecast horizon as long as @e@ is an already-known
    -- epoch (see the note above); 'epochInfoPure' is the only handle on the
    -- epoch length 'Globals' offers.
    epochSize = runIdentity $ epochInfoSize (epochInfoPure globals) e

    Globals {maxKESEvo, slotsPerKESPeriod} = globals

-- | Same as 'maxKeyAgeEpochs', but computed directly from the KES parameters and the epoch
-- length, for when 'Globals' are not available, e.g. at genesis.
kesMaxKeyAgeEpochs ::
  -- | Maximum number of KES key evolutions
  Word64 ->
  -- | Number of slots per KES period
  Word64 ->
  -- | Epoch length
  EpochSize ->
  EpochInterval
kesMaxKeyAgeEpochs maxKESEvo slotsPerKESPeriod (EpochSize slotsPerEpoch) =
  EpochInterval $
    ceiling ((maxKESEvo * slotsPerKESPeriod) % slotsPerEpoch) + 2
