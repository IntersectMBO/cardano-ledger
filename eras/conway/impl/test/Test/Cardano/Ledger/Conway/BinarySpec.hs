{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.BinarySpec (spec) where

import Cardano.Ledger.Babbage
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Credential
import Cardano.Ledger.Shelley.LedgerState
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Lens.Micro
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
    describe "Regression" $ do
      prop "Drop Ptrs from Incrementasl Stake" $ \(ls :: LedgerState BabbageEra) conwayGenesis slotNo testCoin -> do
        let
          badPtr = Ptr slotNo (TxIx maxBound) (CertIx maxBound)
          lsBabbage :: LedgerState BabbageEra
          lsBabbage = ls & lsUTxOStateL . utxosStakeDistrL . ptrMapL <>~ Map.singleton badPtr testCoin
          lsConway :: LedgerState ConwayEra
          lsConway = translateEra' conwayGenesis lsBabbage
          v = eraProtVerLow @ConwayEra
          expectNoBadPtr :: LedgerState ConwayEra -> LedgerState ConwayEra -> Expectation
          expectNoBadPtr x y = x `shouldBe` (y & lsUTxOStateL . utxosStakeDistrL . ptrMapL .~ mempty)
        embedTripExpectation v v (mkTrip encCBOR decNoShareCBOR) expectNoBadPtr lsConway
