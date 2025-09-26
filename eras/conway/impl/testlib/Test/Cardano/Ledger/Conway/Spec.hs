{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Spec (spec) where

import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayEpochEvent,
  ConwayHardForkEvent,
  ConwayNewEpochEvent,
 )
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Control.State.Transition (STS (..))
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Babbage.TxInfoSpec as BabbageTxInfo
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Binary.Regression as Regression
import qualified Test.Cardano.Ledger.Conway.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Conway.CommitteeRatifySpec as CommitteeRatify
import qualified Test.Cardano.Ledger.Conway.DRepRatifySpec as DRepRatify
import qualified Test.Cardano.Ledger.Conway.Imp as Imp
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp, EraSpecificSpec)
import qualified Test.Cardano.Ledger.Conway.Proposals as Proposals
import qualified Test.Cardano.Ledger.Conway.SPORatifySpec as SPORatifySpec
import Test.Cardano.Ledger.Core.Binary.RoundTrip (RuleListEra)
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)

spec ::
  forall era.
  ( RuleListEra era
  , ConwayEraImp era
  , EraSpecificSpec era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  Spec
spec =
  describe "Conway features" $ do
    Proposals.spec @era
    Binary.spec @era
    DRepRatify.spec @era
    CommitteeRatify.spec @era
    SPORatifySpec.spec @era
    roundTripJsonEraSpec @era
    describe "Imp" $ do
      Imp.spec @era
    describe "CostModels" $ do
      CostModelsSpec.spec @era
    describe "TxWits" $ do
      TxWitsSpec.spec @era
    Regression.spec @era
    describe "TxInfo" $ do
      BabbageTxInfo.spec @era
      describe "PlutusV3" $
        BabbageTxInfo.txInfoSpec @era SPlutusV3
