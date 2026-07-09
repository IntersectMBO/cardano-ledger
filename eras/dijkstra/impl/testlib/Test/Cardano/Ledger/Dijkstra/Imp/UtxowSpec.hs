{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxowSpec (spec) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraUtxowPredFailure (..))
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Plutus (Data, Language (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts
import qualified Data.OMap.Strict as OMap
import qualified Data.Set.NonEmpty as NES
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  DijkstraEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOW" $ do
  describe "RequireGuard native scripts" $ do
    it "Spending inputs locked by script requiring a keyhash guard" $ do
      guardKeyHash <- KeyHashObj <$> freshKeyHash
      scriptHash <- impAddNativeScript (RequireGuard guardKeyHash)
      txIn <- produceScript scriptHash
      let tx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [txIn])
      submitFailingTx
        tx
        [injectFailure $ Conway.ScriptWitnessNotValidatingUTXOW $ NES.singleton scriptHash]
      submitTx_ $ tx & bodyTxL . guardsTxBodyL .~ [guardKeyHash]

    it "A native script required as guard needs to be witnessed " $ do
      let guardScript = RequireAllOf []
      let guardScriptHash = hashScript @era $ fromNativeScript guardScript
      scriptHash <- impAddNativeScript $ RequireGuard (ScriptHashObj guardScriptHash)
      tx <- mkTokenMintingTx scriptHash
      submitFailingTx
        tx
        [injectFailure $ Conway.ScriptWitnessNotValidatingUTXOW $ NES.singleton scriptHash]

      let txWithGuards = tx & bodyTxL . guardsTxBodyL .~ [ScriptHashObj guardScriptHash]
      submitFailingTx
        txWithGuards
        [injectFailure $ Conway.MissingScriptWitnessesUTXOW $ NES.singleton guardScriptHash]
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
      submitFailingTx
        tx
        [injectFailure $ Conway.ScriptWitnessNotValidatingUTXOW $ NES.singleton guardScriptHash]

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
      submitFailingTx
        tx
        [injectFailure $ Conway.ScriptWitnessNotValidatingUTXOW $ NES.singleton scriptHash]
      submitTx_ $
        tx
          & bodyTxL . guardsTxBodyL .~ [ScriptHashObj guardScriptHash, guardKeyHash]
          & witsTxL . hashScriptTxWitsL .~ [fromNativeScript guardScript]

  describe "Required top-level guards" $ do
    describe "MissingRequiredGuards" $ do
      it "A top-level required guard absent from the guards set is a predicate failure" $ do
        guardKeyHash <- KeyHashObj <$> freshKeyHash
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardKeyHash, SNothing)]
        submitFailingTx
          tx
          [injectFailure $ MissingRequiredGuards $ NES.singleton guardKeyHash]
        submitTx_ $ tx & bodyTxL . guardsTxBodyL .~ [guardKeyHash]

      it "A guard required by a sub-transaction must be present in the top-level guards" $ do
        guardKeyHash <- KeyHashObj <$> freshKeyHash
        let subTx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardKeyHash, SNothing)]
            tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . subTransactionsTxBodyL .~ OMap.singleton subTx
        result <- trySubmitTx tx
        let present = case result of
              Left (predFailures, _) ->
                injectFailure (MissingRequiredGuards (NES.singleton guardKeyHash)) `elem` predFailures
              Right _ -> False
        present `shouldBe` True

    describe "MalformedGuardDatums" $ do
      it "A key-hash guard carrying a datum is a predicate failure" $ do
        guardKeyHash <- KeyHashObj <$> freshKeyHash
        datum <- arbitrary @(Data era)
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ [guardKeyHash]
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardKeyHash, SJust datum)]
        submitFailingTx
          tx
          [injectFailure $ MalformedGuardDatums $ NES.singleton guardKeyHash]
        submitTx_ $ tx & bodyTxL . requiredTopLevelGuardsL .~ [(guardKeyHash, SNothing)]

      it "A native-script guard carrying a datum is a predicate failure" $ do
        datum <- arbitrary @(Data era)
        let guardScript = RequireAllOf []
            guardScriptHash = hashScript @era $ fromNativeScript guardScript
            guardCred = ScriptHashObj guardScriptHash
            tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ [guardCred]
                & witsTxL . hashScriptTxWitsL .~ [fromNativeScript guardScript]
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardCred, SJust datum)]
        submitFailingTx
          tx
          [injectFailure $ MalformedGuardDatums $ NES.singleton guardCred]
        submitTx_ $ tx & bodyTxL . requiredTopLevelGuardsL .~ [(guardCred, SNothing)]

      it "A Plutus-script guard's datum presence is validated" $ do
        datum <- arbitrary @(Data era)
        let guardScript = alwaysSucceeds @'PlutusV3 3
            guardCred = ScriptHashObj (hashScript @era guardScript)
            malformed = injectFailure (MalformedGuardDatums (NES.singleton guardCred))
            mkTx mDatum =
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ [guardCred]
                & witsTxL . hashScriptTxWitsL .~ [guardScript]
                & bodyTxL . requiredTopLevelGuardsL .~ [(guardCred, mDatum)]
            hasMalformed tx = do
              result <- trySubmitTx tx
              pure $ case result of
                Left (predFailures, _) -> malformed `elem` predFailures
                Right _ -> False
        hasMalformed (mkTx SNothing) `shouldReturn` True
        hasMalformed (mkTx (SJust datum)) `shouldReturn` False
