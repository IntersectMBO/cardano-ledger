{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.BinarySpec (spec) where

import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Cardano.Ledger.Binary
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Genesis
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Core
import Data.Default (def)
import Data.Proxy
import Data.Typeable (typeRep)
import Test.Cardano.Ledger.Binary (decoderEquivalenceExpectation)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Conway.Binary.RoundTrip (roundTripConwayCommonSpec)
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Core.Binary as Binary (decoderEquivalenceCoreEraTypesSpec, txSizeSpec)
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
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @ConwayEra
    decoderEquivalenceLenientSpec @(TxDats ConwayEra)
    decoderEquivalenceLenientSpec @(Redeemers ConwayEra)
  Binary.txSizeSpec @ConwayEra
  where
    -- The expectation used in this spec allows for the deserialization to fail, in which case
    -- it only checks that it fails for both decoders.
    -- This is necessary because for some arbitrarily generated values, the deserialization fails
    -- starting with Conway (for example: empty TxDats or Redeemers)
    decoderEquivalenceLenientSpec ::
      forall t. (Arbitrary t, Eq t, EncCBOR t, DecCBOR t, DecCBOR (Annotator t), Show t) => Spec
    decoderEquivalenceLenientSpec =
      prop (show (typeRep $ Proxy @t)) $ property $ \(x :: t) ->
        forM_ [eraProtVerLow @ConwayEra .. eraProtVerHigh @ConwayEra] $ \v ->
          decoderEquivalenceExpectation @t v (serialize v x)
