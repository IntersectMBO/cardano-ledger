{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Invalid (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusTxInfo, mkSupportedPlutusScript)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..), AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Plutus
import qualified Data.Map.Strict as Map
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (redeemerSameAsDatum)

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , EraPlutusTxInfo PlutusV2 era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "Invalid" $ do
  it "Inline datum with Plutus V1" $ do
    const $ pendingWith "not implemented yet"

  it "MalformedScriptWitnesses" $ do
    let scriptHash = hashPlutusScript @PlutusV2 malformedPlutus
    txIn <- produceScript scriptHash
    let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    submitFailingTx
      tx
      [ injectFailure $
          MalformedScriptWitnesses [scriptHash]
      ]

  it "MalformedReferenceScripts" $ do
    let script = fromPlutusScript $ mkSupportedPlutusScript @PlutusV2 @era malformedPlutus
    let scriptHash = hashScript script
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

  it "ExtraRedeemers/RedeemerPointerPointsToNothing" $
    -- There is ExtraRedeemers test for PlutusV1 in Alonzo, thus we start with PlutusV2
    forM_ ([PlutusV2 .. eraMaxLanguage @era] :: [Language]) $ \lang -> do
      logString $ "Testing for " <> show lang
      let scriptHash = withSLanguage lang (hashPlutusScript . redeemerSameAsDatum)
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

  it "Inline datum failing script" $ do
    const $ pendingWith "not implemented yet"

  it "Use a collateral output" $ do
    const $ pendingWith "not implemented yet"

  it "Inline datum and ref script and redundant script witness" $ do
    const $ pendingWith "not implemented yet"

  it "Inline datum with redundant datum witness" $ do
    const $ pendingWith "not implemented yet"

  it "No such thing as a reference datum" $ do
    const $ pendingWith "not implemented yet"
