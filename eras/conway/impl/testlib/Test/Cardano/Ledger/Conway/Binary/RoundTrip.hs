{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Binary.RoundTrip (
  roundTripConwayCommonSpec,
  roundTripConwayEraTypesSpec,
) where

import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus (CostModels)
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Era (ConwayEraTest)
import Test.Cardano.Ledger.Core.Binary.RoundTrip

roundTripConwayCommonSpec ::
  forall era.
  ( ConwayEraTest era
  , RuleListEra era
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
  , Arbitrary (InstantStake era)
  , EraPParams era
  , EraStake era
  , ConwayEraAccounts era
  ) =>
  Spec
roundTripConwayEraTypesSpec = do
  describe (eraName @era <> " Transaction Types") $ do
    roundTripEraTypeSpec @era @GovAction
    roundTripEraTypeSpec @era @VotingProcedure
    roundTripEraTypeSpec @era @VotingProcedures
    roundTripEraTypeSpec @era @ProposalProcedure
    roundTripEraTypeSpec @era @Constitution
    prop "CostModels" $ roundTripEraExpectation @era @CostModels
  describe (eraName @era <> " State Types") $ do
    roundTripShareEraTypeSpec @era @EnactState
    roundTripShareEraTypeSpec @era @GovActionState
    roundTripShareEraTypeSpec @era @Proposals
    roundTripShareEraTypeSpec @era @Drep
    roundTripShareEraTypeSpec @era @DrepState
    roundTripShareEraTypeSpec @era @DRepPulsingState
    roundTripShareEraTypeSpec @era @PulsingSnapshot
    roundTripShareEraTypeSpec @era @RatifyState
    roundTripShareEraTypeSpec @era @VState

instance RuleListEra ConwayEra where
  type
    EraRules ConwayEra =
      '[ "BBODY"
       , "CERT"
       , "CERTS"
       , "DELEG"
       , "GOVCERT"
       , "GOV"
       , "LEDGER"
       , "LEDGERS"
       , "POOL"
       , "UTXO"
       , "UTXOS"
       , "UTXOW"
       ]
