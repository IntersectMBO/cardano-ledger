{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.BinarySpec (spec) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Genesis
import Cardano.Ledger.Conway.Governance
import Data.Default (def)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Binary.RoundTrip (roundTripConwayCommonSpec)
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraSpec)

spec :: Spec
spec = do
  specUpgrade @ConwayEra def
  describe "RoundTrip" $ do
    roundTripCborSpec @GovActionId
    roundTripCborSpec @(GovPurposeId 'PParamUpdatePurpose ConwayEra)
    roundTripCborSpec @(GovPurposeId 'HardForkPurpose ConwayEra)
    roundTripCborSpec @(GovPurposeId 'CommitteePurpose ConwayEra)
    roundTripCborSpec @(GovPurposeId 'ConstitutionPurpose ConwayEra)
    roundTripCborSpec @Vote
    roundTripCborSpec @Voter
    roundTripConwayCommonSpec @ConwayEra
    -- ConwayGenesis only makes sense in Conway era
    roundTripEraSpec @ConwayEra @ConwayGenesis
