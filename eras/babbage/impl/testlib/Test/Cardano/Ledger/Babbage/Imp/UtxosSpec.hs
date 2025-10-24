{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxosSpec (spec, babbageEraSpecificSpec) where

import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (BadTranslation))
import Cardano.Ledger.Alonzo.Plutus.TxInfo (
  TxOutSource (TxOutFromOutput),
 )
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (CollectErrors))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core (dataHashTxOutL, fromPlutusScript, referenceInputsTxBodyL)
import Cardano.Ledger.Babbage.TxInfo (
  BabbageContextError (
    ReferenceInputsNotSupported,
    ReferenceScriptsNotSupported
  ),
 )
import Cardano.Ledger.Babbage.TxOut (referenceScriptTxOutL)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), TxIx (..), inject, maybeToStrictMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  bodyTxL,
  eraProtVerHigh,
  eraProtVerLow,
  fromNativeScript,
  hashScript,
  injectFailure,
  inputsTxBodyL,
  mkBasicTx,
  mkBasicTxBody,
  mkBasicTxOut,
  mkCoinTxOut,
  outputsTxBodyL,
  txIdTx,
 )
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Plutus (Data (..), Language (..), hashData, hashPlutusScript, withSLanguage)
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.TxIn (mkTxInPartial)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as P1
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

spec :: forall era. BabbageEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $ do
  describe "PlutusV1 with references" $ do
    let inBabbage = eraProtVerLow @era <= eraProtVerHigh @BabbageEra
        behavior = if inBabbage then "fails" else "succeeds"
        submitBabbageFailingTx tx failures =
          if inBabbage then submitFailingTx tx failures else submitTx_ tx

    it (behavior <> " with a reference script") $ do
      let plutusScriptHash = withSLanguage PlutusV1 $ hashPlutusScript . alwaysSucceedsWithDatum
          nativeScript = fromNativeScript @era $ RequireAllOf []
      txIn <- produceScript plutusScriptHash
      addr <- freshKeyAddr_
      let txOut =
            mkCoinTxOut addr (inject $ Coin 5_000_000)
              & referenceScriptTxOutL .~ SJust nativeScript
          tx =
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & outputsTxBodyL .~ [txOut]
      submitBabbageFailingTx
        tx
        [ injectFailure $
            CollectErrors
              [ BadTranslation . inject $
                  ReferenceScriptsNotSupported @era (TxOutFromOutput (TxIx 0))
              ]
        ]

    it (behavior <> " with a reference input") $ do
      let plutusScriptHash = withSLanguage PlutusV1 $ hashPlutusScript . alwaysSucceedsWithDatum
          nativeScriptHash = hashScript . fromNativeScript @era $ RequireAllOf []
      txIn <- produceScript plutusScriptHash
      refIn <- produceScript nativeScriptHash
      let tx =
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & referenceInputsTxBodyL .~ [refIn]
      submitBabbageFailingTx
        tx
        [ injectFailure $
            CollectErrors
              [ BadTranslation . inject $
                  ReferenceInputsNotSupported @era [refIn]
              ]
        ]

babbageEraSpecificSpec :: forall era. BabbageEraImp era => SpecWith (ImpInit (LedgerSpec era))
babbageEraSpecificSpec =
  describe "UTXOS" $ do
    it "succeeds with same txIn in regular inputs and reference inputs (PlutusV2)" $ do
      let
        setupRefTx = do
          let shSpending = withSLanguage PlutusV2 $ hashPlutusScript . redeemerSameAsDatum
          refTxOut <- mkRefTxOut shSpending
          fmap txIdTx . submitTxAnn "Producing transaction" $
            mkBasicTx mkBasicTxBody
              & bodyTxL . outputsTxBodyL
                .~ [ refTxOut
                   , scriptLockedTxOut shSpending
                   , scriptLockedTxOut shSpending
                   ]
        scriptLockedTxOut shSpending =
          mkBasicTxOut
            (mkAddr shSpending StakeRefNull)
            mempty
            & dataHashTxOutL .~ SJust (hashData $ Data @era $ P1.I 3)
        mkRefTxOut sh = do
          addr <- freshKeyAddr_
          let mbyPlutusScript = impLookupPlutusScript sh
          pure $
            mkBasicTxOut addr mempty
              & referenceScriptTxOutL .~ maybeToStrictMaybe (fromPlutusScript <$> mbyPlutusScript)
      producingTx <- setupRefTx
      let
        consumingTx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL
              .~ [ mkTxInPartial producingTx 0
                 , mkTxInPartial producingTx 1
                 ]
            & bodyTxL . referenceInputsTxBodyL .~ [mkTxInPartial producingTx 0]
      submitTxAnn_ "Consuming transaction" consumingTx
