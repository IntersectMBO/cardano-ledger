{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Tickf (
  DijkstraTICKF,
  DijkstraTickfPredFailure,
  DijkstraTickfEvent,
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase, SlotNo)
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (solidifyNextEpochPParams)
import Cardano.Ledger.State (SnapShots (ssStakeMarkPoolDistr))
import Control.State.Transition
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import NoThunks.Class (NoThunks (..))

-- ==================================================

{------------------------------------------------------------------------------
-- TICKF transition

-- This is a variant on the TICK transition called only by the consensus layer
to tick the ledger state to a future slot.
------------------------------------------------------------------------------}

data DijkstraTickfPredFailure era
  deriving (Generic)

deriving instance
  Era era =>
  Show (DijkstraTickfPredFailure era)

deriving instance
  Era era =>
  Eq (DijkstraTickfPredFailure era)

instance NoThunks (DijkstraTickfPredFailure era)

data DijkstraTickfEvent era

instance
  EraGov era =>
  STS (DijkstraTICKF era)
  where
  type State (DijkstraTICKF era) = NewEpochState era
  type Signal (DijkstraTICKF era) = SlotNo
  type Environment (DijkstraTICKF era) = ()
  type BaseM (DijkstraTICKF era) = ShelleyBase
  type PredicateFailure (DijkstraTICKF era) = DijkstraTickfPredFailure era
  type Event (DijkstraTICKF era) = DijkstraTickfEvent era

  initialRules = []
  transitionRules = pure $ do
    TRC ((), nes0, slot) <- judgmentContext
    -- This whole function is a specialization of an inlined 'NEWEPOCH'.
    --
    -- The ledger view, 'LedgerView', is built entirely from the 'nesPd' and 'esPp' and
    -- 'dsGenDelegs', so the correctness of 'validatingTickTransitionFORECAST' only
    -- depends on getting these three fields correct.

    (curEpochNo, nes) <- liftSTS $ solidifyNextEpochPParams nes0 slot

    let es = nesEs nes
        ss = esSnapshots es

    -- the relevant 'NEWEPOCH' logic
    let pd' = ssStakeMarkPoolDistr ss

    if curEpochNo /= succ (nesEL nes)
      then pure nes
      else do
        let govState = nes ^. newEpochStateGovStateL
        -- We can skip 'SNAP'; we already have the equivalent pd'.

        -- We can skip 'POOLREAP';
        -- we don't need to do the checks:
        -- if the checks would fail, then the node will fail in the 'TICK' rule
        -- if it ever then node tries to validate blocks for which the
        -- return value here was used to validate their headers.

        pure $!
          nes {nesPd = pd'}
            & newEpochStateGovStateL . curPParamsGovStateL .~ nextEpochPParams govState
            & newEpochStateGovStateL . prevPParamsGovStateL .~ (govState ^. curPParamsGovStateL)
            & newEpochStateGovStateL . futurePParamsGovStateL .~ NoPParamsUpdate
