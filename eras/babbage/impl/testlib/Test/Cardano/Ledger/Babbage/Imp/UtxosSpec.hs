{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (BadTranslation))
import Cardano.Ledger.Alonzo.Plutus.TxInfo (
  TxOutSource (TxOutFromOutput),
 )
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (CollectErrors))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core (
  collateralInputsTxBodyL,
  collateralReturnTxBodyL,
  datumTxOutL,
  referenceInputsTxBodyL,
  totalCollateralTxBodyL,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (
  BabbageContextError (
    ReferenceInputsNotSupported,
    ReferenceScriptsNotSupported
  ),
 )
import Cardano.Ledger.Babbage.TxOut (referenceScriptTxOutL)
import Cardano.Ledger.BaseTypes (ProtVer (..), TxIx (..), inject, natVersion)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Core (
  ProtVerHigh,
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
 )
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Plutus (Language (..), hashPlutusScript, mkInlineDatum, withSLanguage)
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Core.Utils (txInAt)
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
              & referenceScriptTxOutL .~ pure nativeScript
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

  describe "PlutusV2 with references" $ do
    it "succeeds with same txIn in regular inputs and reference inputs" $ do
      let
        scriptHash = withSLanguage PlutusV2 $ hashPlutusScript . inputsOverlapsWithRefInputs
        txOut =
          mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
            & datumTxOutL .~ mkInlineDatum (PV1.I 0)
      tx <-
        submitTx $
          mkBasicTx $
            mkBasicTxBody & outputsTxBodyL .~ [txOut]
      let txIn = txInAt 0 tx
      majorVer <- pvMajor <$> getProtVer
      when (majorVer <= natVersion @(ProtVerHigh BabbageEra) || majorVer >= natVersion @11) $
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ [txIn]
            & bodyTxL . referenceInputsTxBodyL .~ [txIn]

  it "Incorrect collateral total" $ do
    let
      scriptHash = withSLanguage PlutusV2 (hashPlutusScript . alwaysSucceedsWithDatum)
      txOut =
        mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
          & datumTxOutL .~ mkInlineDatum (PV1.I 1)
    tx <-
      submitTx $
        mkBasicTx $
          mkBasicTxBody & outputsTxBodyL .~ [txOut]
    let txIn = txInAt 0 tx
    addr <- freshKeyAddrNoPtr_
    coll <- sendCoinTo addr $ Coin 5_000_000
    let
      collReturn = mkBasicTxOut addr . inject $ Coin 2_000_000
      tx2 =
        mkBasicTx $
          mkBasicTxBody
            & inputsTxBodyL .~ [txIn]
            & collateralInputsTxBodyL .~ [coll]
            & collateralReturnTxBodyL .~ pure collReturn
            & totalCollateralTxBodyL .~ pure (Coin 1_000_000)
    submitFailingTx
      tx2
      [injectFailure (IncorrectTotalCollateralField (DeltaCoin 3_000_000) (Coin 1_000_000))]
