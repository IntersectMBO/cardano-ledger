{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.HuddleSpec (alonzoCDDL)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits, Redeemers)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (Data)
import Test.Cardano.Ledger.Alonzo.Binary.Annotator ()
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
    let v = eraProtVerHigh @AlonzoEra
    describe "Huddle" $ specWithHuddle alonzoCDDL $ do
      huddleRoundTripCborSpec @(Value AlonzoEra) v "coin"
      huddleRoundTripAnnCborSpec @(TxBody TopTx AlonzoEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody TopTx AlonzoEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData AlonzoEra) v "auxiliary_data"
      huddleRoundTripCborSpec @(TxAuxData AlonzoEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock AlonzoEra) v "native_script"
      huddleRoundTripCborSpec @(Timelock AlonzoEra) v "native_script"
      huddleRoundTripAnnCborSpec @(Data AlonzoEra) v "plutus_data"
      huddleRoundTripCborSpec @(Data AlonzoEra) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut AlonzoEra) v "transaction_output"
      huddleRoundTripAnnCborSpec @(AlonzoTxWits AlonzoEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(AlonzoTxWits AlonzoEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate AlonzoEra) v "protocol_param_update"
      huddleRoundTripAnnCborSpec @(Redeemers AlonzoEra) v "redeemers"
      huddleRoundTripCborSpec @(Redeemers AlonzoEra) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx TopTx AlonzoEra) v "transaction"
      huddleRoundTripCborSpec @(Tx TopTx AlonzoEra) v "transaction"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @(TxBody TopTx AlonzoEra) v "transaction_body"
        huddleDecoderEquivalenceSpec @(TxAuxData AlonzoEra) v "auxiliary_data"
        huddleDecoderEquivalenceSpec @(Timelock AlonzoEra) v "native_script"
        huddleDecoderEquivalenceSpec @(Data AlonzoEra) v "plutus_data"
        huddleDecoderEquivalenceSpec @(AlonzoTxWits AlonzoEra) v "transaction_witness_set"
        huddleDecoderEquivalenceSpec @(Redeemers AlonzoEra) v "redeemers"
        huddleDecoderEquivalenceSpec @(Tx TopTx AlonzoEra) v "transaction"
