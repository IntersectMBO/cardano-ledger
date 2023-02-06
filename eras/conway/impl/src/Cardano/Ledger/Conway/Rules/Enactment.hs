{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Enactment (
  ConwayENACTMENT,
)
where

import Cardano.Ledger.BaseTypes (EpochNo (..), ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayENACTMENT)
import Cardano.Ledger.Shelley.LedgerState (EpochState (..))
import Control.State.Transition.Extended (STS (..))
import Data.Void (Void)

instance
  ( EraPParams era
  , EraGovernance era
  ) =>
  STS (ConwayENACTMENT era)
  where
  type Environment (ConwayENACTMENT era) = ()
  type PredicateFailure (ConwayENACTMENT era) = Void
  type Signal (ConwayENACTMENT era) = EpochNo
  type State (ConwayENACTMENT era) = EpochState era
  type BaseM (ConwayENACTMENT era) = ShelleyBase

  transitionRules = undefined -- TODO once the specification is done
