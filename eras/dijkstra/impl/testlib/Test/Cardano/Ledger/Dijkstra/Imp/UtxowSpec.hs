{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.ImpTest

spec ::
  forall era.
  ( DijkstraEraImp era
  , InjectRuleFailure "LEDGER" ConwayUtxowPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec =
  describe "RequireGuard native scripts" $ do
    it "Spending inputs locked by script requiring a keyhash guard" $ do
      guardKeyHash <- KeyHashObj <$> freshKeyHash
      scriptHash <- impAddNativeScript (RequireGuard guardKeyHash)
      txIn <- produceScript scriptHash
      let tx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [txIn])
      submitFailingTx tx [injectFailure $ ScriptWitnessNotValidatingUTXOW [scriptHash]]
      submitTx_ $ tx & bodyTxL . guardsTxBodyL .~ [guardKeyHash]

    it "A native script required as guard needs to be witnessed " $ do
      let guardScript = RequireAllOf []
      let guardScriptHash = hashScript @era $ fromNativeScript guardScript
      scriptHash <- impAddNativeScript $ RequireGuard (ScriptHashObj guardScriptHash)
      tx <- mkTokenMintingTx scriptHash
      submitFailingTx tx [injectFailure $ ScriptWitnessNotValidatingUTXOW [scriptHash]]

      let txWithGuards = tx & bodyTxL . guardsTxBodyL .~ [ScriptHashObj guardScriptHash]
      submitFailingTx txWithGuards [injectFailure $ MissingScriptWitnessesUTXOW [guardScriptHash]]
      submitTx_ $ txWithGuards & witsTxL . hashScriptTxWitsL .~ [fromNativeScript guardScript]

    it "A failing native script required as guard results in a predicate failure" $ do
      let guardScriptFailing = RequireAnyOf []
      let guardScriptHash = hashScript @era $ fromNativeScript guardScriptFailing
      scriptHash <- impAddNativeScript $ RequireGuard (ScriptHashObj guardScriptHash)
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegDepositTxCert (ScriptHashObj scriptHash) expectedDeposit]
              & bodyTxL . guardsTxBodyL .~ [ScriptHashObj guardScriptHash]
              & witsTxL . hashScriptTxWitsL .~ [fromNativeScript guardScriptFailing]
      submitFailingTx tx [injectFailure $ ScriptWitnessNotValidatingUTXOW [guardScriptHash]]

    it "A redundant guard is ignored" $ do
      guardKeyHash <- KeyHashObj <$> freshKeyHash
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . guardsTxBodyL .~ [guardKeyHash]
      submitTx_ tx

    it "Nested RequiredGuard scripts" $ do
      guardKeyHash <- KeyHashObj <$> freshKeyHash
      let guardScript = RequireGuard guardKeyHash
      let guardScriptHash = hashScript @era $ fromNativeScript guardScript

      scriptHash <- impAddNativeScript $ RequireGuard (ScriptHashObj guardScriptHash)

      tx <- mkTokenMintingTx scriptHash
      submitFailingTx tx [injectFailure $ ScriptWitnessNotValidatingUTXOW [scriptHash]]
      submitTx_ $
        tx
          & bodyTxL . guardsTxBodyL .~ [ScriptHashObj guardScriptHash, guardKeyHash]
          & witsTxL . hashScriptTxWitsL .~ [fromNativeScript guardScript]
