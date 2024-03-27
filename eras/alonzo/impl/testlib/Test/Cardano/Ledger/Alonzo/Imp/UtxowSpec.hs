{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Alonzo.Core (
  AlonzoEraTxWits (..),
  scriptIntegrityHashTxBodyL,
 )
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..), AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxAuxData
import Cardano.Ledger.Alonzo.TxOut (dataHashTxOutL)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.LedgerState (esLStateL, lsUTxOStateL, nesEsL, utxosUtxoL)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Data.Map as Map
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Allegra.Imp.UtxowSpec (genInvalidMetadatum)
import Test.Cardano.Ledger.Allegra.ImpTest (produceScript)
import Test.Cardano.Ledger.Alonzo.ImpTest (AlonzoEraImp, fixupPPHash)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (guessTheNumber3)
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ( AlonzoEraImp era
  , TxAuxData era ~ AlonzoTxAuxData era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , Arbitrary (Data era)
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOW" $ do
  it "MissingRedeemers" $ do
    let lang = eraMaxLanguage @era
    let scriptHash = withSLanguage lang (hashPlutusScript . guessTheNumber3)
    txIn <- produceScript scriptHash
    let txToSubmit = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    let fixup tx =
          fixupResetAddrWits $ tx & witsTxL . rdmrsTxWitsL .~ Redeemers mempty
    let missingRedeemer = mkSpendingPurpose $ AsItem txIn
    withPostFixup fixup $
      submitFailingTx
        txToSubmit
        [ injectFailure $
            MissingRedeemers [(missingRedeemer, scriptHash)]
        , injectFailure $
            CollectErrors [NoRedeemer missingRedeemer]
        ]

  it "MissingRequiredDatums" $ do
    let lang = eraMaxLanguage @era
    let scriptHash = withSLanguage lang (hashPlutusScript . guessTheNumber3)
    txIn <- produceScript scriptHash
    let txToSubmit = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    let fixup tx =
          fixupResetAddrWits $ tx & witsTxL . datsTxWitsL .~ mempty
    let missingDatum = hashData @era (Data (P.I 3))
    withPostFixup fixup $
      submitFailingTx
        txToSubmit
        [ injectFailure $
            MissingRequiredDatums [missingDatum] []
        ]

  it "NotAllowedSupplementalDatums" $ do
    let lang = eraMaxLanguage @era
    let scriptHash = withSLanguage lang (hashPlutusScript . guessTheNumber3)
    txIn <- produceScript scriptHash
    let extraDatumHash = hashData @era (Data (P.I 30))
    let extraDatum = Data (P.I 30)
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ [txIn]
            & witsTxL . datsTxWitsL .~ TxDats (Map.singleton extraDatumHash extraDatum)
    submitFailingTx
      tx
      [ injectFailure $
          NotAllowedSupplementalDatums [extraDatumHash] []
      ]

  it "PPViewHashesDontMatch" $ do
    let lang = eraMaxLanguage @era
    let scriptHash = withSLanguage lang (hashPlutusScript . guessTheNumber3)
    txIn <- produceScript scriptHash
    tx <- fixupTx $ mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    void $ impAnn "Mismatched " $ do
      wrongIntegrityHash <- arbitrary
      wrongIntegrityHashTx <-
        resetAddrWits $ tx & bodyTxL . scriptIntegrityHashTxBodyL .~ SJust wrongIntegrityHash
      withNoFixup $
        submitFailingTx
          wrongIntegrityHashTx
          [ injectFailure $
              PPViewHashesDontMatch (SJust wrongIntegrityHash) (tx ^. bodyTxL . scriptIntegrityHashTxBodyL)
          ]
    void $ impAnn "Missing" $ do
      missingIntegrityHashTx <-
        resetAddrWits $ tx & bodyTxL . scriptIntegrityHashTxBodyL .~ SNothing
      withNoFixup $
        submitFailingTx
          missingIntegrityHashTx
          [ injectFailure $
              PPViewHashesDontMatch SNothing (tx ^. bodyTxL . scriptIntegrityHashTxBodyL)
          ]

  it "UnspendableUTxONoDatumHash" $ do
    let lang = eraMaxLanguage @era
    let scriptHash = withSLanguage lang (hashPlutusScript . guessTheNumber3)
    txIn <- produceScript scriptHash
    let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    let resetDatum out = out & dataHashTxOutL .~ SNothing
    modifyNES $ \nes ->
      nes
        & nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
          %~ ( \(UTxO utxo) ->
                UTxO (Map.adjust resetDatum txIn utxo)
             )
    submitFailingTx
      tx
      [ injectFailure $
          UnspendableUTxONoDatumHash [txIn]
      ]

  it "ExtraRedeemers" $ do
    let scriptHash = withSLanguage PlutusV1 (hashPlutusScript . guessTheNumber3)
    txIn <- produceScript scriptHash
    let prp = MintingPurpose (AsIx 0)
    dt <- arbitrary
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ [txIn]
            & witsTxL . rdmrsTxWitsL
              %~ ( \(Redeemers m) ->
                    Redeemers (Map.insert prp (dt, ExUnits 0 0) m)
                 )
    submitFailingTx
      tx
      [ injectFailure $
          ExtraRedeemers [prp]
      ]

  it "InvalidMetadata" $ do
    invalidMetadatum <- genInvalidMetadatum
    let auxData = AlonzoTxAuxData @era invalidMetadatum [] []
    let auxDataHash = hashTxAuxData auxData
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . auxDataHashTxBodyL .~ SJust auxDataHash
            & auxDataTxL .~ SJust auxData
    submitFailingTx tx [injectFailure InvalidMetadata]
  where
    fixupResetAddrWits = fixupPPHash >=> resetAddrWits
    resetAddrWits tx = updateAddrTxWits $ tx & witsTxL . addrTxWitsL .~ []
