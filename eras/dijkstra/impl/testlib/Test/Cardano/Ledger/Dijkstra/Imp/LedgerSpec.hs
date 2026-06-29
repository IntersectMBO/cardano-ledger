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
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraLedgerPredFailure (..),
  DijkstraUtxoPredFailure (BadInputsUTxO),
 )
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import qualified Data.Map.NonEmpty as NEM
import qualified Data.OMap.Strict as OMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "Spending sub-transaction outputs" $ do
    it "Fails when top-level transaction spends output from its own sub-transaction" $ do
      (_, addr) <- freshKeyAddr
      txIn <- sendCoinTo addr (Coin 10_000_000)

      let subTx :: Tx SubTx era
          subTx = mkBasicTx mkBasicTxBody
          subTxId = txIdTx subTx

          badInput = mkTxInPartial subTxId 0
          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.fromList [txIn, badInput]
              & bodyTxL . subTransactionsTxBodyL .~ OMap.singleton subTx

      submitFailingTxM tx $ \txFixed ->
        pure
          [ injectFailure $ BadInputsUTxO $ NES.singleton badInput
          , injectFailure $
              DijkstraSpendingOutputFromSameTx $
                NEM.singleton (txIdTx txFixed) $
                  NES.singleton badInput
          ]

    it "Fails when sub-transaction spends output from another sub-transaction" $ do
      (_, addr1) <- freshKeyAddr
      txIn1 <- sendCoinTo addr1 (Coin 10_000_000)
      (_, addr2) <- freshKeyAddr
      txIn2 <- sendCoinTo addr2 (Coin 10_000_000)

      let subTx1 :: Tx SubTx era
          subTx1 =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton txIn1
          subTx1Id = txIdTx subTx1

          badInput = mkTxInPartial subTx1Id 0
          subTx2 :: Tx SubTx era
          subTx2 =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.fromList [txIn2, badInput]
          subTx2Id = txIdTx subTx2

          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable ([subTx1, subTx2] :: [Tx SubTx era])

      submitFailingTx
        tx
        [injectFailure $ DijkstraSpendingOutputFromSameTx $ NEM.singleton subTx2Id $ NES.singleton badInput]

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
