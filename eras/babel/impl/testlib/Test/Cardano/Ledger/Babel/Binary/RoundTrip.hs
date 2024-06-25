{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babel.Binary.RoundTrip (
  roundTripBabelCommonSpec,
  roundTripBabelEraTypesSpec,
) where

import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway.Governance (
  Constitution,
  DRepPulsingState,
  EnactState,
  EraGov,
  GovAction,
  GovActionState,
  GovState,
  ProposalProcedure,
  Proposals,
  PulsingSnapshot,
  RatifyState,
  VotingProcedure,
  VotingProcedures,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.LedgerState
import Test.Cardano.Ledger.Alonzo.Arbitrary (FlexibleCostModels (..))
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Babel.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.RoundTrip

roundTripBabelCommonSpec ::
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
  , RuleListEra era
  ) =>
  Spec
roundTripBabelCommonSpec = do
  roundTripBabelEraTypesSpec @era
  roundTripAlonzoCommonSpec @era

roundTripBabelEraTypesSpec ::
  forall era.
  ( Arbitrary (PParams era)
  , Arbitrary (PParamsUpdate era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , EraPParams era
  ) =>
  Spec
roundTripBabelEraTypesSpec = do
  describe "Babel Transaction Types" $ do
    roundTripEraTypeSpec @era @GovAction
    roundTripEraTypeSpec @era @VotingProcedure
    roundTripEraTypeSpec @era @VotingProcedures
    roundTripEraTypeSpec @era @ProposalProcedure
    roundTripEraTypeSpec @era @Constitution
    -- Babel adds ability to serialize unknown cost models, i.e. FlexibleCostModels
    prop "CostModels" $ roundTripEraExpectation @era . unFlexibleCostModels
  describe "Babel State Types" $ do
    roundTripShareEraTypeSpec @era @EnactState
    roundTripShareEraTypeSpec @era @GovActionState
    roundTripShareEraTypeSpec @era @Proposals
    roundTripShareEraTypeSpec @era @DRepPulsingState
    roundTripShareEraTypeSpec @era @PulsingSnapshot
    roundTripShareEraTypeSpec @era @RatifyState

instance Crypto c => RuleListEra (BabelEra c) where
  type
    EraRules (BabelEra c) =
      '[ "GOV"
       , "UTXOS"
       , "LEDGER"
       , "CERTS"
       , "CERT"
       , "DELEG"
       , "GOVCERT"
       , "UTXOW"
       , "UTXO"
       , "LEDGERS"
       , "POOL"
       ]
