{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..), AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (mkPlutusScript')
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (redeemerSameAsDatum)

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxOut era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOW" $ do
  it "MalformedScriptWitnesses" $ do
    let scriptHash = hashPlutusScript (malformedPlutus @'PlutusV2)
    txIn <- produceScript scriptHash
    let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    submitFailingTx
      tx
      [ injectFailure $
          MalformedScriptWitnesses [scriptHash]
      ]

  it "MalformedReferenceScripts" $ do
    let script = mkPlutusScript' @era (malformedPlutus @'PlutusV2)
    let scriptHash = hashScript script
    addr <- freshKeyAddr_
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . outputsTxBodyL
              .~ [ mkBasicTxOut addr (inject (Coin 10)) & referenceScriptTxOutL .~ SJust script
                 ]
    submitFailingTx
      tx
      [ injectFailure $
          MalformedReferenceScripts [scriptHash]
      ]

  it "ExtraRedeemers/RedeemerPointerPointsToNothing" $
    -- There is ExtraRedeemers test for PlutusV1 in Alonzo, thus we start with PlutusV2
    forM_ ([PlutusV2 .. eraMaxLanguage @era] :: [Language]) $ \lang -> do
      logEntry $ "Testing for " ++ show lang
      let scriptHash = withSLanguage lang (hashPlutusScript . redeemerSameAsDatum)
      txIn <- produceScript scriptHash
      let prp = MintingPurpose (AsIx 2)
      dt <- arbitrary
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txIn]
              & witsTxL . rdmrsTxWitsL <>~ Redeemers [(prp, (dt, ExUnits 0 0))]
      let submit =
            submitFailingTx tx $
              [ injectFailure $ ExtraRedeemers [prp]
              , injectFailure $
                  CollectErrors [BadTranslation (inject $ RedeemerPointerPointsToNothing prp)]
              ]
      if eraProtVerLow @era < natVersion @9
        then -- PlutusPurpose serialization was fixed in Conway
          withCborRoundTripFailures submit
        else submit
