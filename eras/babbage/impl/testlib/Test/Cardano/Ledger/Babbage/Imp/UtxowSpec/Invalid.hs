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
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Plutus
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
  , EraPlutusTxInfo PlutusV2 era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "Invalid" $ do
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
      let submit =
            submitFailingTx
              tx
              [ injectFailure $ ExtraRedeemers [prp]
              , injectFailure $
                  CollectErrors [BadTranslation (inject $ RedeemerPointerPointsToNothing prp)]
              ]
      if eraProtVerLow @era < natVersion @9
        then -- PlutusPurpose serialization was fixed in Conway
          withCborRoundTripFailures submit
        else submit

  it "Inline datum failing script" $ do
    let scriptHash = withSLanguage PlutusV2 $ hashPlutusScript . evenDatum
        txOut =
          mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
            & datumTxOutL .~ mkDatum (PV1.I 1)
        tx = mkBasicTx mkBasicTxBody & bodyTxL . outputsTxBodyL .~ [txOut]
    txIn <- txInAt (0 :: Int) <$> submitTx tx
    submitPhase2Invalid_ $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]

  it "Use a collateral output" $ do
    const $ pendingWith "not implemented yet"

  it "Incorrect collateral total" $ do
    const $ pendingWith "not implemented yet"

  it "Inline datum and ref script and redundant script witness" $ do
    const $ pendingWith "not implemented yet"

  it "Inline datum with redundant datum witness" $ do
    const $ pendingWith "not implemented yet"

  it "Inline datum with Plutus V1" $ do
    const $ pendingWith "not implemented yet"

  it "Min-utxo value with output too large" $ do
    const $ pendingWith "not implemented yet"

  it "No such thing as a reference datum" $ do
    const $ pendingWith "not implemented yet"

mkDatum :: Era era => PV1.Data -> Datum era
mkDatum = Datum . dataToBinaryData . Data
