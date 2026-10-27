{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Shelley.Imp.UtxoSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Tools
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set.NonEmpty as NES
import Lens.Micro
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec :: ShelleyEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  describe "ShelleyUtxoPredFailure" $ do
    it "InputSetEmptyUTxO" $ do
      (_, rootTxOut) <- getImpRootTxOut
      -- Without the root input that the fixup balanced the transaction with,
      -- nothing pays for the fee and the change output
      withPostFixup (rederiveAddrTxWits . (bodyTxL . inputsTxBodyL .~ mempty)) $
        submitFailingTx
          (mkBasicTx mkBasicTxBody)
          [ injectFailure InputSetEmptyUTxO
          , injectFailure . ValueNotConservedUTxO $ Mismatch mempty (rootTxOut ^. valueTxOutL)
          ]

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

    it "WrongNetwork" $ do
      addr <- freshMainnetKeyAddr_
      submitFailingTx
        (mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [mkBasicTxOut addr mempty])
        [injectFailure . WrongNetwork Testnet $ NES.singleton addr]

    it "OutputBootAddrAttrsTooBig" $ do
      pp <- getsPParams id
      bootAddr <- freshBootstrapAddressOversizedPayload
      let txOut = setMinCoinTxOut pp $ mkBasicTxOut (AddrBootstrap bootAddr) mempty
      submitFailingTx
        (mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [txOut])
        [injectFailure $ OutputBootAddrAttrsTooBig [txOut]]
