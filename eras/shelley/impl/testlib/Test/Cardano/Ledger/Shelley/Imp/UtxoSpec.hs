{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Shelley.Imp.UtxoSpec (spec) where

import Cardano.Ledger.BaseTypes (Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure (..))
import Cardano.Ledger.Val (inject)
import Data.Sequence.Strict (StrictSeq (..))
import Lens.Micro
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec :: ShelleyEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  describe "ShelleyUtxoPredFailure" $ do
    it "ValueNotConservedUTxO" $ do
      addr1 <- freshKeyAddr_
      let txAmount = Coin 2000000
      txIn <- sendCoinTo addr1 txAmount
      addr2 <- freshKeyAddr_
      (_, rootTxOut) <- getImpRootTxOut
      let extra = Coin 3
          rootTxOutValue = rootTxOut ^. valueTxOutL
          txBody =
            mkBasicTxBody
              & inputsTxBodyL .~ [txIn]
              & outputsTxBodyL .~ [mkBasicTxOut addr2 mempty]
          adjustTxOut = \case
            Empty -> error "Unexpected empty sequence of outputs"
            txOut :<| outs -> (txOut & coinTxOutL %~ (<> extra)) :<| outs
          adjustFirstTxOut tx =
            tx
              & bodyTxL . outputsTxBodyL %~ adjustTxOut
              & witsTxL .~ mkBasicTxWits
      withPostFixup (updateAddrTxWits . adjustFirstTxOut) $
        submitFailingTx
          (mkBasicTx txBody)
          [ injectFailure $
              ValueNotConservedUTxO $
                Mismatch
                  (rootTxOutValue <> inject txAmount)
                  (rootTxOutValue <> inject (txAmount <> extra))
          ]
