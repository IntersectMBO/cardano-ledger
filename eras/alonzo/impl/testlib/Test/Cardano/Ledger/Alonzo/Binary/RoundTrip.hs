{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (
  roundTripAlonzoCommonSpec,
  roundTripAlonzoEraTypesSpec,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AsIx (..),
 )
import Cardano.Ledger.Alonzo.State
import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Cardano.Ledger.Binary
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.LedgerState
import Test.Cardano.Ledger.Alonzo.Arbitrary (genNonEmptyRedeemers, genNonEmptyTxDats)
import Test.Cardano.Ledger.Alonzo.Binary.Annotator ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (genValidCostModels)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (
  RuleListEra (..),
  roundTripAnnEraTypeSpec,
  roundTripEraExpectation,
  roundTripEraTypeExpectation,
  roundTripEraTypeSpec,
 )
import Test.Cardano.Ledger.Shelley.Binary.RoundTrip (roundTripShelleyCommonSpec)

roundTripAlonzoCommonSpec ::
  forall era.
  ( EraTx era
  , EraGov era
  , EraStake era
  , EraCertState era
  , AlonzoEraScript era
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
  , Arbitrary (PlutusPurpose AsIx era)
  , Arbitrary (PParams era)
  , Arbitrary (PParamsUpdate era)
  , Arbitrary (CertState era)
  , Arbitrary (InstantStake era)
  , DecCBOR (Script era)
  , DecCBOR (TxAuxData era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxBody era)
  , DecCBOR (Tx era)
  , RuleListEra era
  ) =>
  Spec
roundTripAlonzoCommonSpec = do
  roundTripAlonzoEraTypesSpec @era
  roundTripShelleyCommonSpec @era

roundTripAlonzoEraTypesSpec ::
  forall era.
  (AlonzoEraScript era, Arbitrary (PlutusPurpose AsIx era)) =>
  Spec
roundTripAlonzoEraTypesSpec = do
  describe "Alonzo era types" $ do
    roundTripAnnEraTypeSpec @era @Data
    roundTripEraTypeSpec @era @Data
    roundTripEraTypeSpec @era @BinaryData
    -- CostModel serialization changes drastically for Conway, which requires a different
    -- QuickCheck generator, hence Arbitrary can't be reused
    prop "CostModels" $
      forAll (genValidCostModels [PlutusV1, PlutusV2]) $
        roundTripEraExpectation @era
    xdescribe "Datum doesn't roundtrip" $ do
      -- TODO: Adjust Datum implementation somehow to avoid this situtaiton
      -- It doesn't roundtrip because we do not en/decode NoDatum
      -- Possibly use peekAvailable, but haven't looked into the issue too deeply
      roundTripEraTypeSpec @era @Datum
    prop "TxDats" $
      forAll genNonEmptyTxDats $
        roundTripEraTypeExpectation @era @TxDats
    prop "Redeemers" $
      forAll genNonEmptyRedeemers $
        roundTripEraTypeExpectation @era @Redeemers

instance RuleListEra AlonzoEra where
  type
    EraRules AlonzoEra =
      '[ "DELEG"
       , "DELEGS"
       , "DELPL"
       , "LEDGER"
       , "LEDGERS"
       , "POOL"
       , "PPUP"
       , "UTXO"
       , "UTXOW"
       , "UTXOS"
       ]
