{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Binary.RoundTrip (roundTripShelleyCommonSpec)

roundTripAlonzoCommonSpec ::
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
  , RuleListEra era
  , HasLedgerState era
  , Arbitrary (EraLedgerState era)
  ) =>
  Spec
roundTripAlonzoCommonSpec = do
  roundTripAlonzoEraTypesSpec @era
  roundTripShelleyCommonSpec @era

roundTripAlonzoEraTypesSpec ::
  forall era.
  Era era =>
  Spec
roundTripAlonzoEraTypesSpec = do
  describe "Alonzo era types" $ do
    roundTripAnnEraTypeSpec @era @Data
    roundTripEraTypeSpec @era @BinaryData
    xdescribe "Datum doesn't roundtrip" $ do
      -- TODO: Adjust Datum implementation somehow to avoid this situtaiton
      -- It doesn't roundtrip because we do not en/decode NoDatum
      -- Possibly use peekAvailable, but haven't looked into the issue too deeply
      roundTripEraTypeSpec @era @Datum

instance Crypto c => RuleListEra (AlonzoEra c) where
  type
    EraRules (AlonzoEra c) =
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
