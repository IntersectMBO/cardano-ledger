{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.HuddleSpec (babbageCDDL)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (Data, Datum)
import Test.Cardano.Ledger.Babbage.Binary.Annotator ()
import Test.Cardano.Ledger.Binary.Cuddle (
  huddleDecoderEquivalenceSpec,
  huddleRoundTripAnnCborSpec,
  huddleRoundTripCborSpec,
  specWithHuddle,
 )
import Test.Cardano.Ledger.Common

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerHigh @BabbageEra
    describe "Huddle" $ specWithHuddle babbageCDDL $ do
      huddleRoundTripCborSpec @(Value BabbageEra) v "coin"
      huddleRoundTripAnnCborSpec @(TxBody TopTx BabbageEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody TopTx BabbageEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData BabbageEra) v "auxiliary_data"
      huddleRoundTripCborSpec @(TxAuxData BabbageEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock BabbageEra) v "native_script"
      huddleRoundTripCborSpec @(Timelock BabbageEra) v "native_script"
      huddleRoundTripAnnCborSpec @(Data BabbageEra) v "plutus_data"
      huddleRoundTripCborSpec @(Data BabbageEra) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut BabbageEra) v "transaction_output"
      huddleRoundTripAnnCborSpec @(Script BabbageEra) v "script"
      huddleRoundTripCborSpec @(Script BabbageEra) v "script"
      huddleRoundTripCborSpec @(Datum BabbageEra) v "datum_option"
      huddleRoundTripAnnCborSpec @(TxWits BabbageEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(TxWits BabbageEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate BabbageEra) v "protocol_param_update"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripAnnCborSpec @(Redeemers BabbageEra) v "redeemers"
      huddleRoundTripCborSpec @(Redeemers BabbageEra) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx TopTx BabbageEra) v "transaction"
      huddleRoundTripCborSpec @(Tx TopTx BabbageEra) v "transaction"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @(TxBody TopTx BabbageEra) v "transaction_body"
        huddleDecoderEquivalenceSpec @(TxAuxData BabbageEra) v "auxiliary_data"
        huddleDecoderEquivalenceSpec @(Timelock BabbageEra) v "native_script"
        huddleDecoderEquivalenceSpec @(Data BabbageEra) v "plutus_data"
        huddleDecoderEquivalenceSpec @(Script BabbageEra) v "script"
        huddleDecoderEquivalenceSpec @(TxWits BabbageEra) v "transaction_witness_set"
        huddleDecoderEquivalenceSpec @(Redeemers BabbageEra) v "redeemers"
        huddleDecoderEquivalenceSpec @(Tx TopTx BabbageEra) v "transaction"
