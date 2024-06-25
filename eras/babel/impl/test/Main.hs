{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babel (Babel)
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Alonzo.Imp as AlonzoImp
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Babel.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Babel.Binary.Regression as Regression
import qualified Test.Cardano.Ledger.Babel.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Babel.CommitteeRatifySpec as CommitteeRatify
import qualified Test.Cardano.Ledger.Babel.DRepRatifySpec as DRepRatify
import qualified Test.Cardano.Ledger.Babel.GenesisSpec as Genesis
import qualified Test.Cardano.Ledger.Babel.GovActionReorderSpec as GovActionReorder
import qualified Test.Cardano.Ledger.Babel.Imp as BabelImp
import Test.Cardano.Ledger.Babel.Plutus.PlutusSpec as PlutusSpec
import qualified Test.Cardano.Ledger.Babel.Proposals as Proposals
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

main :: IO ()
main =
  ledgerTestMain $
    describe "Babel" $ do
      Proposals.spec
      Binary.spec
      Cddl.spec
      DRepRatify.spec
      CommitteeRatify.spec
      Genesis.spec
      GovActionReorder.spec
      roundTripJsonEraSpec @Babel
      describe "Imp" $ do
        AlonzoImp.spec @Babel
        BabelImp.spec @Babel
        ShelleyImp.spec @Babel
      describe "CostModels" $ do
        CostModelsSpec.spec @Babel
      describe "TxWits" $ do
        TxWitsSpec.spec @Babel
      describe "Plutus" $ do
        PlutusSpec.spec
      Regression.spec @Babel
