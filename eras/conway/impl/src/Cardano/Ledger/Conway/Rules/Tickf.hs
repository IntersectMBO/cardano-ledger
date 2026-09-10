{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Like TICK, called only by consensus. But, ticks ledger state to a __future__ slot.
module Cardano.Ledger.Conway.Rules.Tickf (
  TICKF,
  ConwayTickfEvent,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase, SlotNo)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (
  SnapShots (ssStakeGo, ssStakeMark, ssStakeMarkPoolDistr, ssStakeSet),
 )
import Control.State.Transition
import Data.Void (Void)
import Lens.Micro ((&), (.~), (^.))

data ConwayTickfEvent era

instance
  EraGov era =>
  STS (TICKF era)
  where
  type State (TICKF era) = NewEpochState era
  type Signal (TICKF era) = SlotNo
  type Environment (TICKF era) = ()
  type BaseM (TICKF era) = ShelleyBase
  type PredicateFailure (TICKF era) = Void
  type Event (TICKF era) = ConwayTickfEvent era

  initialRules = []
  transitionRules = pure $ do
    TRC ((), nes0, slot) <- judgmentContext
    -- This whole function is a specialization of an inlined 'NEWEPOCH'.
    --
    -- The ledger view, 'LedgerView', is built entirely from the 'nesPd' and 'esPp' and
    -- 'dsGenDelegs', so the correctness of 'validatingTickTransitionFORECAST' only
    -- depends on getting these three fields correct.

    (curEpochNo, nes) <- liftSTS $ Shelley.solidifyNextEpochPParams nes0 slot

    let es = nesEs nes
        ss = esSnapshots es

    -- the relevant 'NEWEPOCH' logic
    let pd' = ssStakeMarkPoolDistr ss

    if curEpochNo /= succ (nesEL nes)
      then pure nes
      else do
        let govState = nes ^. newEpochStateGovStateL
        -- We can skip most of 'SNAP'. Its cheap half is the snapshot rotation,
        -- which 'ss'' below does; all that is then left out is its expensive
        -- half, constructing a new mark snapshot by aggregating the whole
        -- instant stake. A forecast never reads that new mark snapshot, since
        -- it governs the epoch after the one being forecast into.
        --
        -- Historically the rotation was left out too, and 'nesPd' was patched
        -- by hand from the cached 'ssStakeMarkPoolDistr' instead. That sufficed
        -- because no forecast projection read the snapshots at all: through
        -- Conway they read only 'nesPd', the current protocol parameters and
        -- (Shelley) the genesis delegates. So the stale snapshots were not
        -- merely unnoticed, they were unreachable.
        --
        -- Dijkstra's forecast is the first to read one: it takes the Leios
        -- voting committee from 'ssStakeSet'. Without the rotation, a forecast
        -- across an epoch boundary would report the anchor epoch's committee
        -- rather than the target epoch's.
        --
        -- Note that forecasting the committee across an epoch boundary does not
        -- necessarily mean a Leios certificate is certifying a Leios
        -- announcement from the previous epoch. It merely means the committee
        -- is being acquired from an earlier ledger state than that of the
        -- announcing block.
        let ss' = ss {ssStakeSet = ssStakeMark ss, ssStakeGo = ssStakeSet ss}

        -- We can skip 'POOLREAP';
        -- we don't need to do the checks:
        -- if the checks would fail, then the node will fail in the 'TICK' rule
        -- if it ever then node tries to validate blocks for which the
        -- return value here was used to validate their headers.

        pure $!
          nes {nesPd = pd', nesEs = es {esSnapshots = ss'}}
            & newEpochStateGovStateL . curPParamsGovStateL .~ nextEpochPParams govState
            & newEpochStateGovStateL . prevPParamsGovStateL .~ (govState ^. curPParamsGovStateL)
            & newEpochStateGovStateL . futurePParamsGovStateL .~ NoPParamsUpdate
