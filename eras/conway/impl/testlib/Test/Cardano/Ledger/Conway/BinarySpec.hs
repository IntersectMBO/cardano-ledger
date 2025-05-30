{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.BinarySpec (spec) where

import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Cardano.Ledger.Binary
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core (AlonzoEraScript (..), AsIx)
import Cardano.Ledger.Conway.Genesis
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses)
import Data.Default (def)
import Data.Proxy
import Data.Typeable (typeRep)
import Test.Cardano.Ledger.Binary (decoderEquivalenceExpectation)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Conway.Binary.RoundTrip (roundTripConwayCommonSpec)
import Test.Cardano.Ledger.Conway.Era (BabbageEraTest)
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp)
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Core.Binary as Binary (decoderEquivalenceCoreEraTypesSpec, txSizeSpec)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (RuleListEra, roundTripEraSpec)

spec ::
  forall era.
  ( BabbageEraTest (PreviousEra era)
  , ConwayEraImp era
  , DecCBOR (TxAuxData era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxBody era)
  , DecCBOR (Tx era)
  , Arbitrary (PlutusPurpose AsIx era)
  , RuleListEra era
  , StashedAVVMAddresses era ~ ()
  , SafeToHash (TxWits era)
  ) =>
  Spec
spec = do
  specUpgrade @era def
  describe "RoundTrip" $ do
    roundTripCborSpec @GovActionId
    roundTripCborSpec @(GovPurposeId 'PParamUpdatePurpose)
    roundTripCborSpec @(GovPurposeId 'HardForkPurpose)
    roundTripCborSpec @(GovPurposeId 'CommitteePurpose)
    roundTripCborSpec @(GovPurposeId 'ConstitutionPurpose)
    roundTripCborSpec @Vote
    roundTripCborSpec @Voter
    roundTripConwayCommonSpec @era
    -- ConwayGenesis only makes sense in Conway era
    roundTripEraSpec @era @ConwayGenesis
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @era
    decoderEquivalenceLenientSpec @(TxDats era)
    decoderEquivalenceLenientSpec @(Redeemers era)
  Binary.txSizeSpec @era
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
