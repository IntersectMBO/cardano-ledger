{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp (spec) where

import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (ConwayGovState)
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp.EnactSpec as Enact
import qualified Test.Cardano.Ledger.Conway.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Conway.Imp.GovSpec as Gov
import qualified Test.Cardano.Ledger.Conway.Imp.UtxoSpec as Utxo
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp, withImpState)

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  Spec
spec = do
  describe "ConwayImpSpec" $ withImpState @era $ do
    Enact.spec @era
    Epoch.spec @era
    Gov.spec @era
    Utxo.spec @era
