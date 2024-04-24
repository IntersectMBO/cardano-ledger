{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Cardano.Ledger.Shelley.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure (..))
import Cardano.Ledger.Val (inject)
import Data.Sequence.Strict (StrictSeq (..))
import Lens.Micro
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  ( ShelleyEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXO" $ do
  describe "ShelleyUtxoPredFailure" $ do
    it "ValueNotConservedUTxO" $ do
      (_, addr1) <- freshKeyAddr
      let txAmount = Coin 1000000
      txIn <- sendCoinTo addr1 txAmount
      (_, addr2) <- freshKeyAddr
      (_, rootTxOut) <- lookupImpRootTxOut
      let extra = Coin 3
          rootTxOutValue = rootTxOut ^. valueTxOutL
          txBody =
            mkBasicTxBody
              & inputsTxBodyL .~ [txIn]
              & outputsTxBodyL .~ [mkBasicTxOut addr2 (inject (Coin 200000))]
          adjustTxOut = \case
            Empty -> error "Unexpected empty sequence of outputs"
            txOut :<| outs -> (txOut & coinTxOutL %~ (<> extra)) :<| outs
          adjustFirstTxOut tx =
            tx
              & bodyTxL . outputsTxBodyL %~ adjustTxOut
              & witsTxL .~ mkBasicTxWits
      res <- withPostFixup (updateAddrTxWits . adjustFirstTxOut) $ trySubmitTx (mkBasicTx txBody)

      predFailures <- expectLeftDeep res
      predFailures
        `shouldBe` [ injectFailure $
                      ValueNotConservedUTxO
                        (rootTxOutValue <> inject txAmount)
                        (rootTxOutValue <> inject (txAmount <> extra))
                   ]
