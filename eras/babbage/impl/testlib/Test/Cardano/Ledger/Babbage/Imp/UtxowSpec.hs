{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusTxInfo, mkSupportedPlutusScript)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..), AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.Scripts (pattern RequireAnyOf)
import Cardano.Ledger.TxIn (mkTxInPartial)
import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (redeemerSameAsDatum)

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , EraPlutusTxInfo 'PlutusV2 era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
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
    let script = fromPlutusScript (mkSupportedPlutusScript (malformedPlutus @'PlutusV2))
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

  it "P1 reference scripts must be witnessed" $ do
    (_, addr) <- freshKeyAddr
    let
      timelock = fromNativeScript @era $ RequireAnyOf []
      txOut =
        mkCoinTxOut addr (inject $ Coin 15_000_000)
          & referenceScriptTxOutL .~ SJust timelock
    tx0 <-
      submitTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ SSeq.singleton txOut
    let
      txIn = mkTxInPartial (txIdTx tx0) 0
      tx1 =
        mkBasicTx mkBasicTxBody
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton txIn
    res <- trySubmitTx tx1
    res `shouldSatisfyExpr` isRight
