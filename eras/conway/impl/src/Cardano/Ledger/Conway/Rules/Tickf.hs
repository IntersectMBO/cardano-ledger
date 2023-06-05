{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Tickf (
  ConwayTICKF,
  ConwayTickfPredFailure,
  ConwayTickfEvent,
)
where

import Cardano.Ledger.BaseTypes (ShelleyBase, SlotNo)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
import Control.State.Transition
import GHC.Generics (Generic)
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
  ( Era era
  ) =>
  Show (ConwayTickfPredFailure era)

deriving instance
  ( Era era
  ) =>
  Eq (ConwayTickfPredFailure era)

instance NoThunks (ConwayTickfPredFailure era)

data ConwayTickfEvent era

instance
  ( Era era
  ) =>
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
    TRC ((), nes, _) <- judgmentContext
    pure nes
