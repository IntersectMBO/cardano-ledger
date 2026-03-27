{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Like TICK, called only by consensus. But, ticks ledger state to a __future__ slot.
module Cardano.Ledger.Conway.Rules.Tickf (
  ConwayTICKF,
  ConwayTickfEvent,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase, SlotNo)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (solidifyNextEpochPParams)
import Cardano.Ledger.State (SnapShots (ssStakeMarkPoolDistr))
import Control.State.Transition
import Data.Void (Void)
import Lens.Micro ((&), (.~), (^.))

data ConwayTickfEvent era

instance
  EraGov era =>
  STS (ConwayTICKF era)
  where
  type State (ConwayTICKF era) = NewEpochState era
  type Signal (ConwayTICKF era) = SlotNo
  type Environment (ConwayTICKF era) = ()
  type BaseM (ConwayTICKF era) = ShelleyBase
  type PredicateFailure (ConwayTICKF era) = Void
  type Event (ConwayTICKF era) = ConwayTickfEvent era

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
