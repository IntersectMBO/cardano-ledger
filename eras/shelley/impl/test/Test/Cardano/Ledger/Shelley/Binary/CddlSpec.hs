{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.CddlSpec (spec) where

import Cardano.Ledger.BaseTypes (UnitInterval)
import Cardano.Ledger.Address (AccountAddress, Addr)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (
  Credential,
  MultiSig,
  ProposedPPUpdates,
  Update,
 )
import Cardano.Ledger.Shelley.HuddleSpec (shelleyCDDL)
import Cardano.Ledger.State (StakePoolRelay)
import Cardano.Ledger.TxIn (TxIn)
import Test.Cardano.Ledger.Binary.Cuddle (
  huddleDecoderEquivalenceSpec,
  huddleRoundTripAnnCborSpec,
  huddleRoundTripArbitraryValidate,
  huddleRoundTripCborSpec,
  specWithHuddle,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Binary.Annotator ()

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerLow @ShelleyEra
    specWithHuddle shelleyCDDL 100 $ do
      huddleRoundTripCborSpec @Addr v "address"
      huddleRoundTripArbitraryValidate @Addr v "address"
      huddleRoundTripAnnCborSpec @BootstrapWitness v "bootstrap_witness"
      huddleRoundTripArbitraryValidate @BootstrapWitness v "bootstrap_witness"
      huddleRoundTripCborSpec @BootstrapWitness v "bootstrap_witness"
      huddleRoundTripCborSpec @AccountAddress v "reward_account"
      huddleRoundTripCborSpec @(Credential Staking) v "stake_credential"
      huddleRoundTripAnnCborSpec @(TxBody TopTx ShelleyEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody TopTx ShelleyEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxOut ShelleyEra) v "transaction_output"
      huddleRoundTripCborSpec @StakePoolRelay v "relay"
      huddleRoundTripCborSpec @(TxCert ShelleyEra) v "certificate"
      huddleRoundTripCborSpec @TxIn v "transaction_input"
      huddleRoundTripAnnCborSpec @(TxAuxData ShelleyEra) v "metadata"
      huddleRoundTripCborSpec @(TxAuxData ShelleyEra) v "metadata"
      huddleRoundTripAnnCborSpec @(MultiSig ShelleyEra) v "native_script"
      huddleRoundTripCborSpec @(MultiSig ShelleyEra) v "native_script"
      huddleRoundTripCborSpec @(Update ShelleyEra) v "update"
      huddleRoundTripCborSpec @(ProposedPPUpdates ShelleyEra) v "proposed_protocol_parameter_updates"
      huddleRoundTripCborSpec @(PParamsUpdate ShelleyEra) v "protocol_param_update"
      huddleRoundTripAnnCborSpec @(Tx TopTx ShelleyEra) v "transaction"
      huddleRoundTripCborSpec @(Tx TopTx ShelleyEra) v "transaction"
      huddleRoundTripAnnCborSpec @(TxWits ShelleyEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(TxWits ShelleyEra) v "transaction_witness_set"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @BootstrapWitness v "bootstrap_witness"
        huddleDecoderEquivalenceSpec @(TxBody TopTx ShelleyEra) v "transaction_body"
        huddleDecoderEquivalenceSpec @(TxAuxData ShelleyEra) v "metadata"
        huddleDecoderEquivalenceSpec @(MultiSig ShelleyEra) v "native_script"
        huddleDecoderEquivalenceSpec @(Tx TopTx ShelleyEra) v "transaction"
