{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Binary.CddlSpec (spec) where

import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Conway.Governance (GovAction, ProposalProcedure, VotingProcedure)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.HuddleSpec (dijkstraCDDL)
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceInterval, AccountBalanceIntervals)
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import Cardano.Ledger.Plutus.Data (Data, Datum)
import Test.Cardano.Ledger.Alonzo.Arbitrary (genDatumPresent, genNonEmptyRedeemers)
import Test.Cardano.Ledger.Binary.Cuddle (
  noTwiddle,
  specWithHuddle,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary (
  fullAnnCddlSpec,
  fullAnnGenCddlSpec,
  fullCddlSpec,
  fullGenCddlSpec,
 )
import Test.Cardano.Ledger.Dijkstra.Arbitrary (genNonEmptyAccountBalanceIntervals)
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()

spec :: Spec
spec = do
  describe "CDDL" $ do
    let v = eraProtVerHigh @DijkstraEra
    describe "Huddle" $ specWithHuddle dijkstraCDDL . noTwiddle $ do
      -- BlockBody
      xdescribe "fix transaction" $
        fullAnnCddlSpec @(BlockBody DijkstraEra) v "block_body"
      -- AccountBalanceInterval
      fullCddlSpec @(AccountBalanceInterval DijkstraEra) v "account_balance_interval"
      -- AccountBalanceIntervals
      fullGenCddlSpec @(AccountBalanceIntervals DijkstraEra)
        genNonEmptyAccountBalanceIntervals
        v
        "account_balance_intervals"
      -- Value
      fullCddlSpec @(Value DijkstraEra) v "value"
      -- TxBody TopTx
      xdescribe "fix TxBody" $ do
        fullAnnCddlSpec @(TxBody TopTx DijkstraEra) v "transaction_body"
      -- TxBody SubTx
      xdescribe "fix TxBody" $ do
        fullAnnCddlSpec @(TxBody SubTx DijkstraEra) v "sub_transaction_body"
      -- TxAuxData
      fullAnnCddlSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
      -- NativeScript
      fullAnnCddlSpec @(NativeScript DijkstraEra) v "native_script"
      -- Data
      fullAnnCddlSpec @(Data DijkstraEra) v "plutus_data"
      -- TxOut
      fullCddlSpec @(TxOut DijkstraEra) v "transaction_output"
      -- Script
      fullAnnCddlSpec @(Script DijkstraEra) v "script"
      -- Datum
      fullGenCddlSpec @(Datum DijkstraEra) genDatumPresent v "datum_option"
      -- TxWits
      xdescribe "fix plutus_v4_script" $ do
        fullAnnCddlSpec @(TxWits DijkstraEra) v "transaction_witness_set"
      -- PParamsUpdate
      fullCddlSpec @(PParamsUpdate DijkstraEra) v "protocol_param_update"
      -- CostModels
      fullCddlSpec @CostModels v "cost_models"
      -- Redeemers
      fullAnnGenCddlSpec @(Redeemers DijkstraEra) genNonEmptyRedeemers v "redeemers"
      -- Tx
      xdescribe "fix Transaction" $ do
        fullAnnCddlSpec @(Tx TopTx DijkstraEra) v "transaction"
      -- VotingProcedure
      fullCddlSpec @(VotingProcedure DijkstraEra) v "voting_procedure"
      -- ProposalProcedure
      fullCddlSpec @(ProposalProcedure DijkstraEra) v "proposal_procedure"
      -- GovAction
      fullCddlSpec @(GovAction DijkstraEra) v "gov_action"
      -- TxCert
      fullCddlSpec @(TxCert DijkstraEra) v "certificate"
