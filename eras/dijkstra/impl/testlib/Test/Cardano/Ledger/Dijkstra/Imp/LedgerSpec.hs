{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.LedgerSpec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.TxIn (mkTxInPartial)
import qualified Data.OMap.Strict as OMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "LEDGER" $ do
  describe "Spending sub-transaction outputs" $ do
    it "Fails when top-level transaction spends output from its own sub-transaction" $ do
      txIn <- (`sendCoinTo` Coin 10_000_000) =<< freshKeyAddr_
      subTxIn <- (`sendCoinTo` Coin 5_000_000) =<< freshKeyAddr_

      let subTx :: Tx SubTx era
          -- consume an input, to avoid the fixup adding one, which would throw off the test conditions
          subTx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [subTxIn])
          subTxId = txIdTx subTx -- now stable through fixup
          badInput = mkTxInPartial subTxId 0
          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txIn, badInput]
              & bodyTxL . subTransactionsTxBodyL .~ [subTx]

      submitFailingTx
        tx
        -- the failure is produced twice - checking against the origin and threaded state, respectively
        [ injectFailure $ BadInputsUTxO $ NES.singleton badInput
        , injectFailure $ BadInputsUTxO $ NES.singleton badInput
        ]

    it "Fails when sub-transaction spends output from another sub-transaction" $ do
      (_, addr1) <- freshKeyAddr
      txIn1 <- sendCoinTo addr1 (Coin 10_000_000)
      (_, addr2) <- freshKeyAddr
      txIn2 <- sendCoinTo addr2 (Coin 10_000_000)

      let subTx1 :: Tx SubTx era
          subTx1 =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txIn1]
          subTx1Id = txIdTx subTx1

          badInput = mkTxInPartial subTx1Id 0
          subTx2 :: Tx SubTx era
          subTx2 =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txIn2, badInput]

          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . subTransactionsTxBodyL .~ [subTx1, subTx2]

      submitFailingTx
        tx
        [ injectFailure $ SubBadInputsUTxO $ NES.singleton badInput
        , injectFailure $ SubBadInputsUTxO $ NES.singleton badInput
        ]

    it "Succeeds when inputs don't reference sub-transaction outputs" $ do
      (_, addr1) <- freshKeyAddr
      txIn1 <- sendCoinTo addr1 (Coin 10_000_000)
      (_, addr2) <- freshKeyAddr
      txIn2 <- sendCoinTo addr2 (Coin 10_000_000)

      let subTx :: Tx SubTx era
          subTx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton txIn1

          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton txIn2
              & bodyTxL . subTransactionsTxBodyL .~ OMap.singleton subTx

      submitTx_ tx
