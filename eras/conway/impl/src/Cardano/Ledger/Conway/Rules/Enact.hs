{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Enact (
  ConwayENACT,
  EnactState (..),
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayENACT)
import Cardano.Ledger.Conway.Governance (
  EnactState (..),
  GovernanceAction (..),
 )
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
 )
import Data.Void (Void)

instance
  ( EraPParams era
  , EraGovernance era
  ) =>
  STS (ConwayENACT era)
  where
  type Environment (ConwayENACT era) = ()
  type PredicateFailure (ConwayENACT era) = Void
  type Signal (ConwayENACT era) = GovernanceAction era
  type State (ConwayENACT era) = EnactState era
  type BaseM (ConwayENACT era) = ShelleyBase

  initialRules = []
  transitionRules = [enactmentTransition]

enactmentTransition :: EraPParams era => TransitionRule (ConwayENACT era)
enactmentTransition = do
  TRC ((), st, act) <- judgmentContext
  case act of
    ParameterChange ppup -> pure $ st {ensPParams = newPP}
      where
        newPP = ensPParams st `applyPPUpdates` ppup
    HardForkInitiation pv -> pure $ st {ensProtVer = pv}
    TreasuryWithdrawals _ -> undefined -- TODO implement when added to spec
    NoConfidence -> pure $ st {ensCC = SNothing}
    NewCommittee mems q -> pure $ st {ensCC = SJust (mems, q)}
    NewConstitution c -> pure $ st {ensConstitution = c}
