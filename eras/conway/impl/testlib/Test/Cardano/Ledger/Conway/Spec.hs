{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Spec (spec) where

import Cardano.Ledger.Plutus.Language (SLanguage (..))
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Babbage.TxInfoSpec as BabbageTxInfo
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Binary.Golden as Golden
import qualified Test.Cardano.Ledger.Conway.Binary.Regression as Regression
import qualified Test.Cardano.Ledger.Conway.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Conway.CommitteeRatifySpec as CommitteeRatify
import qualified Test.Cardano.Ledger.Conway.DRepRatifySpec as DRepRatify
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp)
import qualified Test.Cardano.Ledger.Conway.Proposals as Proposals
import qualified Test.Cardano.Ledger.Conway.SPORatifySpec as SPORatifySpec
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)

spec :: forall era. ConwayEraImp era => Spec
spec =
  describe "Conway features" $ do
    Proposals.spec @era
    Binary.spec @era
    DRepRatify.spec @era
    CommitteeRatify.spec @era
    SPORatifySpec.spec @era
    roundTripJsonEraSpec @era
    Golden.spec @era
    Golden.goldenListRedeemers @era
    describe "CostModels" $ do
      CostModelsSpec.spec @era
    describe "TxWits" $ do
      TxWitsSpec.spec @era
    Regression.spec @era
    describe "TxInfo" $ do
      BabbageTxInfo.spec @era
      describe "PlutusV3" $
        BabbageTxInfo.txInfoSpec @era SPlutusV3
