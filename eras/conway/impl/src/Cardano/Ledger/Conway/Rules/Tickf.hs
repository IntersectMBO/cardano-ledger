{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Tickf (
  ConwayTICKF,
  ConwayTickfPredFailure,
  ConwayTickfEvent,
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase, SlotNo, epochInfoPure)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Core
import Cardano.Ledger.EpochBoundary (SnapShots (ssStakeMarkPoolDistr))
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Slot (epochInfoEpoch)
import Control.Monad.Trans.Reader (asks)
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

data ConwayTickfPredFailure era
  deriving (Generic)

deriving instance
  Era era =>
  Show (ConwayTickfPredFailure era)

deriving instance
  Era era =>
  Eq (ConwayTickfPredFailure era)

instance NoThunks (ConwayTickfPredFailure era)

data ConwayTickfEvent era

instance
  EraGov era =>
  STS (ConwayTICKF era)
  where
  type State (ConwayTICKF era) = NewEpochState era
  type Signal (ConwayTICKF era) = SlotNo
  type Environment (ConwayTICKF era) = ()
  type BaseM (ConwayTICKF era) = ShelleyBase
  type PredicateFailure (ConwayTICKF era) = ConwayTickfPredFailure era
  type Event (ConwayTICKF era) = ConwayTickfEvent era

  initialRules = []
  transitionRules = pure $ do
    TRC ((), nes, slot) <- judgmentContext
    -- This whole function is a specialization of an inlined 'NEWEPOCH'.
    --
    -- The ledger view, 'LedgerView', is built entirely from the 'nesPd' and 'esPp' and
    -- 'dsGenDelegs', so the correctness of 'validatingTickTransitionFORECAST' only
    -- depends on getting these three fields correct.

    epoch <- liftSTS $ do
      ei <- asks epochInfoPure
      epochInfoEpoch ei slot

    let es = nesEs nes
        ss = esSnapshots es

    -- the relevant 'NEWEPOCH' logic
    let pd' = ssStakeMarkPoolDistr ss

    if epoch /= succ (nesEL nes)
      then pure nes
      else do
        let govState = nes ^. newEpochStateGovStateL
            -- TICKF is only ever called at most one stability window into the future,
            -- which means that `futurePParamsGovStateG` will be known at this point at
            -- will be O(1) lookup.
            newPParams =
              case govState ^. futurePParamsGovStateG of
                Nothing -> govState ^. curPParamsGovStateL
                Just futurePParams -> futurePParams
        -- We can skip 'SNAP'; we already have the equivalent pd'.

        -- We can skip 'POOLREAP';
        -- we don't need to do the checks:
        -- if the checks would fail, then the node will fail in the 'TICK' rule
        -- if it ever then node tries to validate blocks for which the
        -- return value here was used to validate their headers.

        pure $!
          nes {nesPd = pd'} & newEpochStateGovStateL . curPParamsGovStateL .~ newPParams
