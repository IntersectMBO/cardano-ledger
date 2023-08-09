{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Enact (
  ConwayENACT,
  EnactState (..),
  EnactPredFailure (..),
) where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayENACT)
import Cardano.Ledger.Conway.Gov (
  EnactState (..),
  GovAction (..),
 )
import Cardano.Ledger.Rules.ValidationMode (Inject (..), runTest)
import Cardano.Ledger.Val (Val (..))
import Control.State.Transition.Extended (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
 )
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Validation (failureUnless)

data EnactPredFailure era
  = EnactTreasuryInsufficientFunds !(Map (RewardAcnt (EraCrypto era)) Coin) !Coin
  deriving (Show, Eq)

instance EraGov era => STS (ConwayENACT era) where
  type Environment (ConwayENACT era) = ()
  type PredicateFailure (ConwayENACT era) = EnactPredFailure era
  type Signal (ConwayENACT era) = GovAction era
  type State (ConwayENACT era) = EnactState era
  type BaseM (ConwayENACT era) = ShelleyBase

  initialRules = []
  transitionRules = [enactmentTransition]

enactmentTransition :: forall era. EraPParams era => TransitionRule (ConwayENACT era)
enactmentTransition = do
  TRC ((), st, act) <- judgmentContext
  case act of
    ParameterChange _prevGovActionId ppup -> pure $ st {ensPParams = newPP}
      where
        newPP = ensPParams st `applyPPUpdates` ppup
    HardForkInitiation _prevGovActionId pv -> pure $ st {ensProtVer = pv}
    TreasuryWithdrawals wdrls -> do
      let wdrlsAmount = fold wdrls
          -- TODO: validate NetworkId in all
          -- We can drop NetworkIds after their validation
          wdrlsNoNetworkId = Map.mapKeys getRwdCred wdrls
      runTest
        . failureUnless (wdrlsAmount <= ensTreasury st)
        $ EnactTreasuryInsufficientFunds @era wdrls (ensTreasury st)
      pure
        st
          { ensWithdrawals = Map.unionWith (<>) wdrlsNoNetworkId $ ensWithdrawals st
          , ensTreasury = ensTreasury st <-> wdrlsAmount
          }
    NoConfidence _prevGovActionId -> pure $ st {ensCommittee = SNothing}
    NewCommittee _prevGovActionId _ committee -> pure $ st {ensCommittee = SJust committee} -- TODO: check old members
    NewConstitution _prevGovActionId c -> pure $ st {ensConstitution = c}
    InfoAction -> pure st

instance Inject (EnactPredFailure era) (EnactPredFailure era) where
  inject = id
