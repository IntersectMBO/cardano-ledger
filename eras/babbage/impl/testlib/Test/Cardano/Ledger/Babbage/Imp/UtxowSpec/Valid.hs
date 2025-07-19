{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Valid (spec) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Plutus (
  Language (..),
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
 )
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "Valid" $ do
  it "Native reference scripts must be witnessed" $ do
    addr <- freshKeyAddr_
    let
      nativeScript = fromNativeScript @era $ RequireAllOf []
      txOut =
        mkCoinTxOut addr (Coin 15_000_000)
          & referenceScriptTxOutL .~ SJust nativeScript
    tx0 <-
      submitTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ [txOut]
    let
      txIn = txInAt 0 tx0
      tx1 =
        mkBasicTx mkBasicTxBody
          & bodyTxL . referenceInputsTxBodyL .~ [txIn]
    submitTx_ tx1

  forM_ @[] [PlutusV2 .. eraMaxLanguage @era] $ \slang -> do
    describe (show slang) $ do
      withSLanguage slang $ \lang -> do
        it "Inline datum" $ do
          let scriptHash = hashPlutusScript $ alwaysSucceedsWithDatum lang
          txIn <- produceScript scriptHash
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . referenceInputsTxBodyL .~ [txIn]

        it "Reference script" $ do
          const $ pendingWith "not implemented yet"

        it "Inline datum and ref script" $ do
          const $ pendingWith "not implemented yet"

        it "Reference input with data hash, no data witness" $ do
          const $ pendingWith "not implemented yet"

        it "Reference input with data hash, with data witness" $ do
          const $ pendingWith "not implemented yet"

        it "Reference script to authorize delegation certificate" $ do
          const $ pendingWith "not implemented yet"

        it "Reference script in output" $ do
          const $ pendingWith "not implemented yet"

        it "Spend simple script output with reference script" $ do
          addr <- freshKeyAddr_
          let
            nativeScript = fromNativeScript @era $ RequireAllOf []
            nativeScriptAddr = mkAddr (hashScript nativeScript) StakeRefNull
            txOutScript =
              mkCoinTxOut nativeScriptAddr (Coin 12_000_000)
            txOutRef =
              mkCoinTxOut addr (Coin 15_000_000)
                & referenceScriptTxOutL .~ SJust nativeScript
          tx0 <-
            submitTx $
              mkBasicTx mkBasicTxBody
                & bodyTxL . outputsTxBodyL .~ [txOutScript, txOutRef]
          let
            txInScript = txInAt 0 tx0
            txInRef = txInAt 1 tx0
            tx1 =
              mkBasicTx $
                mkBasicTxBody
                  & inputsTxBodyL .~ [txInScript]
                  & referenceInputsTxBodyL .~ [txInRef]
          submitTx_ tx1
