{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Invalid (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..), AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxWits (TxDats (..), hashDataTxWitsL, unRedeemersL)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Plutus (
  Data (..),
  Datum (..),
  Language (..),
  TxOutSource (TxOutFromInput),
  asSLanguage,
  hashData,
  hashPlutusScript,
  mkInlineDatum,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (ExtraneousScriptWitnessesUTXOW))
import qualified Data.Map.Strict as Map
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "Invalid" $ do
  it "Inline datum with Plutus V1" $ do
    let scriptHash = withSLanguage PlutusV1 $ hashPlutusScript . alwaysSucceedsWithDatum
        txOut =
          mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
            & datumTxOutL .~ mkInlineDatum (PV1.I 0)
        tx = mkBasicTx mkBasicTxBody & bodyTxL . outputsTxBodyL .~ [txOut]
    txIn <- txInAt 0 <$> submitTx tx
    submitFailingTx
      (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn])
      [ injectFailure $
          CollectErrors
            [ BadTranslation . inject $
                InlineDatumsNotSupported @era (TxOutFromInput txIn)
            ]
      ]

  forM_ @[] [PlutusV2 .. eraMaxLanguage @era] $ \slang -> do
    describe (show slang) $ do
      withSLanguage slang $ \lang -> do
        it "MalformedScriptWitnesses" $ do
          let scriptHash = hashPlutusScript $ asSLanguage lang malformedPlutus
          txIn <- produceScript scriptHash
          let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
          submitFailingTx
            tx
            [ injectFailure $
                MalformedScriptWitnesses [scriptHash]
            ]

        it "MalformedReferenceScripts" $ do
          plutus <- mkPlutusScript $ asSLanguage lang malformedPlutus
          let script = fromPlutusScript plutus
              scriptHash = hashScript script
          addr <- freshKeyAddr_
          let tx =
                mkBasicTx mkBasicTxBody
                  & bodyTxL . outputsTxBodyL
                    .~ [ mkBasicTxOut addr mempty & referenceScriptTxOutL .~ SJust script
                       ]
          submitFailingTx
            tx
            [ injectFailure $
                MalformedReferenceScripts [scriptHash]
            ]

        -- There's already an ExtraRedeemers test for PlutusV1 in Alonzo, thus it's OK to start with PlutusV2
        it "ExtraRedeemers/RedeemerPointerPointsToNothing" $ do
          let scriptHash = hashPlutusScript $ redeemerSameAsDatum lang
          txIn <- produceScript scriptHash
          let prp = MintingPurpose (AsIx 2)
          dt <- arbitrary
          let tx =
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL .~ [txIn]
                  & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert prp (dt, ExUnits 0 0)
          submitFailingTx
            tx
            [ injectFailure $ ExtraRedeemers [prp]
            , injectFailure $
                CollectErrors [BadTranslation (inject $ RedeemerPointerPointsToNothing prp)]
            ]

        it "Inline datum with a failing script" $ do
          let scriptHash = hashPlutusScript $ evenDatum lang
              txOut =
                mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
                  & datumTxOutL .~ mkInlineDatum (PV1.I 1)
              tx = mkBasicTx mkBasicTxBody & bodyTxL . outputsTxBodyL .~ [txOut]
          txIn <- txInAt 0 <$> submitTx tx
          submitPhase2Invalid_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]

        it "Use a collateral output" $ do
          let scriptHash = hashPlutusScript $ alwaysFailsWithDatum lang
              datum = Data @era $ PV1.B "abcde"
              datumHash = hashData datum
              txOut =
                mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
                  & datumTxOutL .~ DatumHash datumHash
              tx = mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [txOut]
          txIn <- txInAt 0 <$> submitTx tx
          addr <- freshKeyAddr_
          coll <- sendCoinTo addr $ Coin 5_000_000
          let collReturn = mkBasicTxOut addr . inject $ Coin 2_000_000
          submitPhase2Invalid_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txIn]
              & bodyTxL . collateralInputsTxBodyL .~ [coll]
              & bodyTxL . collateralReturnTxBodyL .~ SJust collReturn
              & witsTxL . datsTxWitsL .~ TxDats [(datumHash, datum)]

        -- Spend a UTxO that has an inline datum, using a reference script,
        -- and also redundantly supply the script witness.
        it "Inline datum and ref script and redundant script witness" $ do
          addr <- freshKeyAddr_
          plutus <- mkPlutusScript $ alwaysSucceedsWithDatum lang
          let script = fromPlutusScript plutus
              scriptHash = hashScript script
              txOutDatum =
                mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
                  & datumTxOutL .~ mkInlineDatum (PV1.B "abcde")
              txOutScript =
                mkBasicTxOut addr mempty
                  & referenceScriptTxOutL .~ SJust script
          tx <-
            submitTx $
              mkBasicTx mkBasicTxBody
                & bodyTxL . outputsTxBodyL .~ [txOutDatum, txOutScript]
          let txInDatum = txInAt 0 tx
              txInScript = txInAt 1 tx
          submitFailingTx
            ( mkBasicTx mkBasicTxBody
                & bodyTxL . inputsTxBodyL .~ [txInDatum]
                & bodyTxL . referenceInputsTxBodyL .~ [txInScript]
                & witsTxL . hashScriptTxWitsL .~ [script]
            )
            [ injectFailure $
                ExtraneousScriptWitnessesUTXOW [scriptHash]
            ]

        it "Inline datum with redundant datum witness" $ do
          let scriptHash = hashPlutusScript $ alwaysSucceedsWithDatum lang
              txOut =
                mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
                  & datumTxOutL .~ mkInlineDatum (PV1.B "abcde")
              tx = mkBasicTx mkBasicTxBody & bodyTxL . outputsTxBodyL .~ [txOut]
          txIn <- txInAt 0 <$> submitTx tx
          let redundantDatum = Data @era $ PV1.I 1
          submitFailingTx
            ( mkBasicTx mkBasicTxBody
                & bodyTxL . inputsTxBodyL .~ [txIn]
                & witsTxL . hashDataTxWitsL .~ [redundantDatum]
            )
            [ injectFailure $
                NotAllowedSupplementalDatums [hashData redundantDatum] mempty
            ]

        -- There is no such thing as a "reference datum". In other words, you cannot
        -- include a reference input that contains an inline datum and have it count
        -- for the datum witness where ever it is needed.
        it "No such thing as a reference datum" $ do
          addr <- freshKeyAddr_

          let scriptHash = hashPlutusScript $ alwaysFailsWithDatum lang
              datum = PV1.B "abcde"
              datumHash = hashData $ Data @era datum
              txOutInline =
                mkBasicTxOut addr mempty
                  & datumTxOutL .~ mkInlineDatum datum
              txOutHash =
                mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
                  & datumTxOutL .~ DatumHash datumHash

          tx <-
            submitTx $
              mkBasicTx mkBasicTxBody
                & bodyTxL . outputsTxBodyL .~ [txOutInline, txOutHash]
          let txInInline = txInAt 0 tx
              txInHash = txInAt 1 tx

          -- The reference input has the required inline datum, but that doesn't
          -- witness the datum hash for the Plutus script in the other input
          submitFailingTx
            ( mkBasicTx mkBasicTxBody
                & bodyTxL . referenceInputsTxBodyL .~ [txInInline]
                & bodyTxL . inputsTxBodyL .~ [txInHash]
            )
            [ injectFailure $
                MissingRequiredDatums [datumHash] mempty
            ]
