{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Valid (spec, babbageEraSpecificSpec) where

import Cardano.Ledger.Alonzo.TxWits (unTxDatsL)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Plutus (
  Data (..),
  Datum (..),
  Language (..),
  hashData,
  hashPlutusScript,
  mkInlineDatum,
  withSLanguage,
 )
import Lens.Micro
import Lens.Micro.GHC ()
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples
import Test.Cardano.Ledger.Shelley.Era (nativeAlwaysFails, nativeAlwaysSucceeds)

spec ::
  forall era.
  BabbageEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "Valid" $ do
  it "Native reference scripts must not be witnessed" $ do
    addr <- freshKeyAddr_
    let
      -- Tx would fail if reference scripts require a witness
      script = nativeAlwaysFails
      txOutRef =
        mkBasicTxOut addr mempty
          & referenceScriptTxOutL .~ SJust script
    txInitial <-
      submitTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ [txOutRef]
    submitTx_ $
      mkBasicTx mkBasicTxBody
        & bodyTxL . referenceInputsTxBodyL .~ [txInAt 0 txInitial]

  it "Spend native script output with reference script" $ do
    addr <- freshKeyAddr_
    let
      script = nativeAlwaysSucceeds
      txOut =
        mkBasicTxOut (mkAddr (hashScript script) StakeRefNull) mempty
      txOutRef =
        mkBasicTxOut addr mempty
          & referenceScriptTxOutL .~ SJust script
    txInitial <-
      submitTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ [txOut, txOutRef]
    submitTx_ $
      mkBasicTx $
        mkBasicTxBody
          & inputsTxBodyL .~ [txInAt 0 txInitial]
          & referenceInputsTxBodyL .~ [txInAt 1 txInitial]

  it "Reference input with data hash, no data witness" $ do
    addr <- freshKeyAddr_
    let
      datumValue = Data @era $ PV1.B "abcde"
      datumHash = hashData datumValue
      txOut =
        mkBasicTxOut addr mempty
      txOutDatum =
        mkBasicTxOut addr mempty
          & datumTxOutL .~ DatumHash datumHash
    txInitial <-
      submitTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ [txOut, txOutDatum]
    tx <-
      submitTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ [txInAt 0 txInitial]
          & bodyTxL . referenceInputsTxBodyL .~ [txInAt 1 txInitial]
    -- Verify that a witness wasn't added by fixup
    tx ^. witsTxL . datsTxWitsL . unTxDatsL . at datumHash `shouldBe` Nothing

  it "Reference input with data hash, with data witness" $ do
    addr <- freshKeyAddr_
    let
      datumValue = Data @era $ PV1.B "abcde"
      datumHash = hashData datumValue
      txOut =
        mkBasicTxOut addr mempty
      txOutDatum =
        mkBasicTxOut addr mempty
          & datumTxOutL .~ DatumHash datumHash
    txInitial <-
      submitTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ [txOut, txOutDatum]
    tx <-
      submitTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ [txInAt 0 txInitial]
          & bodyTxL . referenceInputsTxBodyL .~ [txInAt 1 txInitial]
          & witsTxL . datsTxWitsL . unTxDatsL . at datumHash .~ Just datumValue
    -- Verify that fixup didn't remove our witness
    tx ^. witsTxL . datsTxWitsL . unTxDatsL . at datumHash `shouldBe` Just datumValue

  forM_ @[] [PlutusV2 .. eraMaxLanguage @era] $ \slang -> do
    describe (show slang) $ do
      withSLanguage slang $ \lang -> do
        describe "Spend outputs locked by scripts" $ do
          it "With an inline datum" $ do
            let
              scriptHash = hashPlutusScript $ alwaysSucceedsWithDatum lang
              txOut =
                mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
                  & datumTxOutL .~ mkInlineDatum (PV1.I 0)
            txInitial <-
              submitTx $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . outputsTxBodyL .~ [txOut]
            submitTx_ $
              mkBasicTx mkBasicTxBody
                & bodyTxL . referenceInputsTxBodyL .~ [txInAt 0 txInitial]

          it "Passed as a reference script" $ do
            addr <- freshKeyAddr_
            plutus <- mkPlutusScript $ alwaysSucceedsWithDatum lang
            let
              script = fromPlutusScript plutus
              txOut =
                mkBasicTxOut (mkAddr (hashScript script) StakeRefNull) mempty
              txOutRef =
                mkBasicTxOut addr mempty
                  & referenceScriptTxOutL .~ SJust script
            txInitial <-
              submitTx $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . outputsTxBodyL .~ [txOut, txOutRef]
            submitTx_ $
              mkBasicTx mkBasicTxBody
                & bodyTxL . inputsTxBodyL .~ [txInAt 0 txInitial]
                & bodyTxL . referenceInputsTxBodyL .~ [txInAt 1 txInitial]

          it "Passed as a reference script and with an inline datum" $ do
            addr <- freshKeyAddr_
            plutus <- mkPlutusScript $ alwaysSucceedsWithDatum lang
            let
              script = fromPlutusScript plutus
              txOut =
                mkBasicTxOut (mkAddr (hashScript script) StakeRefNull) mempty
                  & datumTxOutL .~ mkInlineDatum (PV1.I 0)
              txOutRef =
                mkBasicTxOut addr mempty
                  & referenceScriptTxOutL .~ SJust script
            txInitial <-
              submitTx $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . outputsTxBodyL .~ [txOut, txOutRef]
            submitTx_ $
              mkBasicTx mkBasicTxBody
                & bodyTxL . inputsTxBodyL .~ [txInAt 0 txInitial]
                & bodyTxL . referenceInputsTxBodyL .~ [txInAt 1 txInitial]

babbageEraSpecificSpec ::
  forall era.
  ( BabbageEraImp era
  , ShelleyEraTxCert era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
babbageEraSpecificSpec = describe "Valid" $ do
  forM_ @[] [PlutusV2 .. eraMaxLanguage @era] $ \slang -> do
    describe (show slang) $ do
      withSLanguage slang $ \lang -> do
        it "Use a reference script to authorize a delegation certificate" $ do
          addr <- freshKeyAddr_
          plutus <- mkPlutusScript $ alwaysSucceedsNoDatum lang
          let
            script = fromPlutusScript plutus
            txOut =
              mkBasicTxOut addr mempty
            txOutRef =
              mkBasicTxOut addr mempty
                & referenceScriptTxOutL .~ SJust script
          txInitial <-
            submitTx $
              mkBasicTx mkBasicTxBody
                & bodyTxL . outputsTxBodyL .~ [txOut, txOutRef]
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txInAt 0 txInitial]
              & bodyTxL . referenceInputsTxBodyL .~ [txInAt 1 txInitial]
              & bodyTxL . certsTxBodyL
                .~ [RegTxCert . ScriptHashObj $ hashScript script]
