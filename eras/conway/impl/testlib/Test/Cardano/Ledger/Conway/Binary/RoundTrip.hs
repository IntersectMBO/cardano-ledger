{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Binary.RoundTrip (
  roundTripConwayCommonSpec,
  roundTripConwayEraTypesSpec,
) where

import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
import Test.Cardano.Ledger.Alonzo.Arbitrary (FlexibleCostModels (..))
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.RoundTrip

roundTripConwayCommonSpec ::
  forall era.
  ( EraTx era
  , EraGov era
  , StashedAVVMAddresses era ~ ()
  , Arbitrary (Tx era)
  , Arbitrary (TxBody era)
  , Arbitrary (TxOut era)
  , Arbitrary (TxCert era)
  , Arbitrary (TxWits era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (Value era)
  , Arbitrary (CompactForm (Value era))
  , Arbitrary (Script era)
  , Arbitrary (GovState era)
  , Arbitrary (PParams era)
  , Arbitrary (PParamsUpdate era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Spec
roundTripConwayCommonSpec = do
  roundTripConwayEraTypesSpec @era
  roundTripAlonzoCommonSpec @era

roundTripConwayEraTypesSpec ::
  forall era.
  ( Arbitrary (PParams era)
  , Arbitrary (PParamsUpdate era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , EraPParams era
  ) =>
  Spec
roundTripConwayEraTypesSpec = do
  describe "Conway Transaction Types" $ do
    roundTripEraTypeSpec @era @GovAction
    roundTripEraTypeSpec @era @VotingProcedure
    roundTripEraTypeSpec @era @VotingProcedures
    roundTripEraTypeSpec @era @ProposalProcedure
    -- Conway adds ability to serialize unknown cost models, i.e. FlexibleCostModels
    prop "CostModels" $ roundTripEraExpectation @era . unFlexibleCostModels
  describe "Conway State Types" $ do
    roundTripShareEraTypeSpec @era @EnactState
    roundTripShareEraTypeSpec @era @GovActionState
    roundTripShareEraTypeSpec @era @Proposals
    roundTripShareEraTypeSpec @era @DRepPulsingState
    roundTripShareEraTypeSpec @era @PulsingSnapshot
    roundTripShareEraTypeSpec @era @RatifyState
