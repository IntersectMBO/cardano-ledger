{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Enact (
  ConwayENACT,
  EnactState (..),
  EnactPredFailure (..),
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayENACT)
import Cardano.Ledger.Conway.Governance (
  EnactState (..),
  GovernanceAction (..),
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Rules.ValidationMode (Inject (..), runTest)
import Cardano.Ledger.Val (Val (..))
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
 )
import Data.Map.Strict (Map, foldr')
import qualified Data.Map.Strict as Map
import Validation (failureUnless)

data EnactPredFailure era
  = EnactTreasuryInsufficientFunds !(Map (Credential 'Staking (EraCrypto era)) Coin) !Coin
  deriving (Show, Eq)

instance
  ( EraPParams era
  , EraGovernance era
  ) =>
  STS (ConwayENACT era)
  where
  type Environment (ConwayENACT era) = ()
  type PredicateFailure (ConwayENACT era) = EnactPredFailure era
  type Signal (ConwayENACT era) = GovernanceAction era
  type State (ConwayENACT era) = EnactState era
  type BaseM (ConwayENACT era) = ShelleyBase

  initialRules = []
  transitionRules = [enactmentTransition]

enactmentTransition :: forall era. EraPParams era => TransitionRule (ConwayENACT era)
enactmentTransition = do
  TRC ((), st, act) <- judgmentContext
  case act of
    ParameterChange ppup -> pure $ st {ensPParams = newPP}
      where
        newPP = ensPParams st `applyPPUpdates` ppup
    HardForkInitiation pv -> pure $ st {ensProtVer = pv}
    TreasuryWithdrawals wdrl -> do
      let newWdrls = foldr' (<>) (Coin 0) $ ensWithdrawals st
      runTest
        . failureUnless (newWdrls <= ensTreasury st)
        $ EnactTreasuryInsufficientFunds @era wdrl (ensTreasury st)
      pure
        st
          { ensWithdrawals = Map.unionWith (<>) wdrl $ ensWithdrawals st
          , ensTreasury = ensTreasury st <-> newWdrls
          }
    NoConfidence -> pure $ st {ensCommittee = SNothing}
    NewCommittee mems q -> pure $ st {ensCommittee = SJust (mems, q)}
    NewConstitution c -> pure $ st {ensConstitution = c}
    InfoAction -> pure st

instance Inject (EnactPredFailure era) (EnactPredFailure era) where
  inject = id
