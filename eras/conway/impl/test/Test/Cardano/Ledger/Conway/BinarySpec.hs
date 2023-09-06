{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.BinarySpec (spec) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Genesis
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Crypto
import Data.Default.Class (def)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Binary.RoundTrip (roundTripConwayCommonSpec)
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraSpec)

spec :: Spec
spec = do
  specUpgrade @Conway def
  describe "RoundTrip" $ do
    roundTripCborSpec @(GovActionId StandardCrypto)
    roundTripCborSpec @(PrevGovActionId 'PParamUpdatePurpose StandardCrypto)
    roundTripCborSpec @(PrevGovActionId 'HardForkPurpose StandardCrypto)
    roundTripCborSpec @(PrevGovActionId 'CommitteePurpose StandardCrypto)
    roundTripCborSpec @(PrevGovActionId 'ConstitutionPurpose StandardCrypto)
    roundTripCborSpec @Vote
    roundTripCborSpec @(Voter StandardCrypto)
    roundTripConwayCommonSpec @Conway
    -- ConwayGenesis only makes sense in Conway era
    roundTripEraSpec @Conway @(ConwayGenesis StandardCrypto)
