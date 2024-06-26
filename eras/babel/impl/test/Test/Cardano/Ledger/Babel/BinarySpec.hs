{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babel.BinarySpec (spec) where

import Cardano.Ledger.Babel
import Cardano.Ledger.Babel.Genesis
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Crypto
import Data.Default.Class (def)
import Test.Cardano.Ledger.Babel.Arbitrary ()
import Test.Cardano.Ledger.Babel.Binary.RoundTrip (roundTripBabelCommonSpec)
import Test.Cardano.Ledger.Babel.TreeDiff ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraSpec)

spec :: Spec
spec = do
  specUpgrade @Babel def
  describe "RoundTrip" $ do
    roundTripCborSpec @(GovActionId StandardCrypto)
    roundTripCborSpec @(GovPurposeId 'PParamUpdatePurpose Babel)
    roundTripCborSpec @(GovPurposeId 'HardForkPurpose Babel)
    roundTripCborSpec @(GovPurposeId 'CommitteePurpose Babel)
    roundTripCborSpec @(GovPurposeId 'ConstitutionPurpose Babel)
    roundTripCborSpec @Vote
    roundTripCborSpec @(Voter StandardCrypto)
    roundTripBabelCommonSpec @Babel
    -- BabelGenesis only makes sense in Babel era
    roundTripEraSpec @Babel @(BabelGenesis StandardCrypto)
