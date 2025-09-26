{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (BadTranslation))
import Cardano.Ledger.Alonzo.Plutus.TxInfo (
  TxOutSource (TxOutFromOutput),
 )
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (CollectErrors))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody, referenceInputsTxBodyL)
import Cardano.Ledger.Babbage.TxInfo (
  BabbageContextError (
    ReferenceInputsNotSupported,
    ReferenceScriptsNotSupported
  ),
 )
import Cardano.Ledger.Babbage.TxOut (referenceScriptTxOutL)
import Cardano.Ledger.BaseTypes (Inject, StrictMaybe (..), TxIx (..), inject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  eraProtVerHigh,
  eraProtVerLow,
  fromNativeScript,
  hashScript,
  injectFailure,
  inputsTxBodyL,
  mkBasicTx,
  mkBasicTxBody,
  mkCoinTxOut,
  outputsTxBodyL,
 )
import Cardano.Ledger.Plutus (Language (..), hashPlutusScript, withSLanguage)
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

spec :: forall era. BabbageEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $ do
  describe "Plutus V1 with references" $ do
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
