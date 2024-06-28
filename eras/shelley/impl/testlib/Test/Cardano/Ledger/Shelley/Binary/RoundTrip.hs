{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Binary.RoundTrip (
  roundTripShelleyCommonSpec,
  roundTripStateEraTypesSpec,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Arbitrary ()

roundTripShelleyCommonSpec ::
  forall era.
  ( EraTx era
  , EraGov era
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , EncCBOR (StashedAVVMAddresses era)
  , DecCBOR (StashedAVVMAddresses era)
  , Arbitrary (StashedAVVMAddresses era)
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
  , RuleListEra era
  , HasLedgerState era
  , Arbitrary (EraLedgerState era)
  ) =>
  Spec
roundTripShelleyCommonSpec = do
  roundTripCoreEraTypesSpec @era
  roundTripStateEraTypesSpec @era
  roundTripAllPredicateFailures @era

roundTripStateEraTypesSpec ::
  forall era.
  ( EraTxOut era
  , EraGov era
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , EncCBOR (StashedAVVMAddresses era)
  , DecCBOR (StashedAVVMAddresses era)
  , Arbitrary (StashedAVVMAddresses era)
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (PParams era)
  , Arbitrary (GovState era)
  , HasLedgerState era
  , Arbitrary (EraLedgerState era)
  ) =>
  Spec
roundTripStateEraTypesSpec = do
  describe "State Types Families" $ do
    roundTripShareEraSpec @era @(GovState era)
  describe "State Types" $ do
    roundTripShareEraTypeSpec @era @UTxOState
    roundTripEraTypeSpec @era @EpochState
    roundTripEraTypeSpec @era @NewEpochState

instance Crypto c => RuleListEra (ShelleyEra c) where
  type
    EraRules (ShelleyEra c) =
      '[ "DELEG"
       , "DELEGS"
       , "DELPL"
       , "LEDGER"
       , "LEDGERS"
       , "POOL"
       , "PPUP"
       , "UTXO"
       , "UTXOW"
       ]
