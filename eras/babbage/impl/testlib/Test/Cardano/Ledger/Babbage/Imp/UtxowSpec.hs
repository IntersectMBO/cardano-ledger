{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut, referenceScriptTxOutL)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (mkPlutusScript')
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxOut era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOW" $ do
  it "MalformedScriptWitnesses" $ do
    let scriptHash = hashPlutusScript @'PlutusV2 malformedPlutus
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
    addr <- snd <$> freshKeyAddr
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
