{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Ledger.Conway.Imp (
  spec,
) where

import Cardano.Ledger.Conway.Governance (ConwayGovState, GovState)
import Cardano.Ledger.Conway.Rules (
  ConwayGOV,
  ConwayLEDGER,
 )
import Cardano.Ledger.Core (EraRule)
import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Conway.Governance (ConwayGovState, GovState)
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure (..))
import Cardano.Ledger.Core (EraRule, EraPParams (..))
import Control.State.Transition.Extended (PredicateFailure)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp.EnactSpec as Enact
import qualified Test.Cardano.Ledger.Conway.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Conway.Imp.GovSpec as Gov
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp, withImpState)

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , EraRule "GOV" era ~ ConwayGOV era
  , EraRule "LEDGER" era ~ ConwayLEDGER era
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  Spec
spec = do
  describe "ConwayImpSpec" $ withImpState @era $ do
    Enact.spec @era
    Epoch.spec @era
    Gov.spec @era
