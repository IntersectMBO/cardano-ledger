{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Binary.CddlSpec (spec) where

import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Conway.Governance (
  GovAction,
  ProposalProcedure,
  VotingProcedure,
  VotingProcedures,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.HuddleSpec (dijkstraCDDL)
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceInterval, AccountBalanceIntervals)
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import Cardano.Ledger.Plutus.Data (Data, Datum)
import Data.OSet.Strict (OSet)
import Test.Cardano.Ledger.Alonzo.Arbitrary (genDatumPresent, genNonEmptyRedeemers)
import Test.Cardano.Ledger.Binary.Cuddle (
  noTwiddle,
  specWithHuddle,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary (genNonEmptyVotingProcedures)
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
      fullGenCddlSpec @(AccountBalanceIntervals DijkstraEra)
        genNonEmptyAccountBalanceIntervals
        v
        "account_balance_intervals"
      fullCddlSpec @(Value DijkstraEra) v "value"
      fullAnnCddlSpec @(TxBody TopTx DijkstraEra) v "transaction_body"
      fullAnnCddlSpec @(TxBody SubTx DijkstraEra) v "sub_transaction_body"
      fullAnnCddlSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
      fullAnnCddlSpec @(NativeScript DijkstraEra) v "native_script"
      fullAnnCddlSpec @(Data DijkstraEra) v "plutus_data"
      fullCddlSpec @(TxOut DijkstraEra) v "transaction_output"
      fullAnnCddlSpec @(Script DijkstraEra) v "script"
      fullGenCddlSpec @(Datum DijkstraEra) genDatumPresent v "datum_option"
      xdescribe "fix plutus_v4_script" $ do
        fullAnnCddlSpec @(TxWits DijkstraEra) v "transaction_witness_set"
      fullCddlSpec @(PParamsUpdate DijkstraEra) v "protocol_param_update"
      fullCddlSpec @CostModels v "cost_models"
      fullAnnGenCddlSpec @(Redeemers DijkstraEra) genNonEmptyRedeemers v "redeemers"
      fullAnnCddlSpec @(Tx TopTx DijkstraEra) v "transaction"
      fullCddlSpec @(VotingProcedure DijkstraEra) v "voting_procedure"
      fullCddlSpec @(ProposalProcedure DijkstraEra) v "proposal_procedure"
      fullCddlSpec @(GovAction DijkstraEra) v "gov_action"
      fullCddlSpec @(TxCert DijkstraEra) v "certificate"
      fullCddlSpec @(OSet (TxCert DijkstraEra)) v "certificates"
      fullCddlSpec @(OSet (ProposalProcedure DijkstraEra)) v "proposal_procedures"
      fullCddlSpec @(OSet (Credential Guard)) v "guards"
      fullGenCddlSpec @(VotingProcedures DijkstraEra) genNonEmptyVotingProcedures v "voting_procedures"
