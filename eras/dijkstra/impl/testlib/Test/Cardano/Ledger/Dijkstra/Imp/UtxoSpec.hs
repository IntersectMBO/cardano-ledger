{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxoSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraUtxoPredFailure (..))
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Mary.Value (
  AssetName,
  MaryValue (..),
  MultiAsset,
  PolicyID,
  multiAssetFromList,
 )
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  lsCertStateL,
  nesEsL,
 )
import Cardano.Ledger.Shelley.UTxO (produced)
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val
import Data.Typeable (Typeable)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  DijkstraEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  describe "Collaterals" $ do
    it "Fails to submit a transaction containing a Ptr in collateral return" $ do
      cred <- KeyHashObj <$> freshKeyHash
      ptr <- arbitrary
      pp <- getsPParams id
      let
        ptrAddr = Addr Testnet cred (StakeRefPtr ptr)
        ptrOutput = ensureMinCoinTxOut pp $ mkBasicTxOut ptrAddr . inject $ Coin 100
        tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . collateralReturnTxBodyL .~ SJust ptrOutput
      submitFailingTx tx [injectFailure $ PtrPresentInCollateralReturn ptrOutput]

  describe "value produced by a transaction" $ do
    it "counts each new pool deposit at most once across the batch" $ do
      poolDeposit <- (Coin 1 <>) <$> arbitrary
      modifyPParams $ ppPoolDepositL .~ poolDeposit
      cert <- RegPoolTxCert <$> arbitrary
      txIn1 <- arbitrary
      txIn2 <- arbitrary
      let topTx =
            mkBasicTx $
              mkBasicTxBody
                & certsTxBodyL .~ [cert]
                -- distinguish subs by an input
                & subTransactionsTxBodyL .~ [mkSubTx txIn1 cert, mkSubTx txIn2 cert]
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      -- just the pool deposits are in `produced` because the transaction is not fixed up
      produced pp pState (topTx ^. bodyTxL) `shouldBe` inject poolDeposit

    it "counts distinct pool deposits in top and sub separately" $ do
      poolDeposit <- (Coin 1 <>) <$> arbitrary
      modifyPParams $ ppPoolDepositL .~ poolDeposit
      poolA <- RegPoolTxCert <$> arbitrary
      poolB <- RegPoolTxCert <$> arbitrary
      txIn1 <- arbitrary
      txIn2 <- arbitrary
      txIn3 <- arbitrary
      let topTx =
            mkBasicTx $
              mkBasicTxBody
                & certsTxBodyL .~ [poolB, poolA, poolB]
                & subTransactionsTxBodyL .~ [mkSubTx txIn1 poolA, mkSubTx txIn2 poolA, mkSubTx txIn3 poolB]
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (topTx ^. bodyTxL) `shouldBe` inject ((2 :: Int) <×> poolDeposit)

    it "includes sub-tx cert deposits when top has no certs" $ do
      poolDeposit <- (Coin 1 <>) <$> arbitrary
      modifyPParams $ ppPoolDepositL .~ poolDeposit
      poolParams <- arbitrary
      let subTx = mkBasicTx $ mkBasicTxBody & certsTxBodyL .~ [RegPoolTxCert poolParams]
          topTx =
            mkBasicTx $
              mkBasicTxBody
                & subTransactionsTxBodyL .~ [subTx]
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (topTx ^. bodyTxL) `shouldBe` inject poolDeposit

    it "does not count re-registrations of an already-registered pool across the batch" $ do
      poolDeposit <- (Coin 1 <>) <$> arbitrary
      modifyPParams $ ppPoolDepositL .~ poolDeposit
      kh <- freshKeyHash
      poolParams <- registerAccountAddress >>= freshPoolParams kh
      let cert = RegPoolTxCert poolParams
          regTx :: forall l. Typeable l => Tx l era
          regTx = mkBasicTx $ mkBasicTxBody & certsTxBodyL .~ [cert]
          topTx = regTx & bodyTxL . subTransactionsTxBodyL .~ [regTx :: Tx SubTx era]
      submitTx_ (regTx :: Tx TopTx era)
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (topTx ^. bodyTxL) `shouldBe` mempty

    it "dedupes across multiple subtransactions registering the same fresh pool" $ do
      poolDeposit <- (Coin 1 <>) <$> arbitrary
      modifyPParams $ ppPoolDepositL .~ poolDeposit
      cert <- RegPoolTxCert <$> arbitrary
      txInA <- arbitrary @TxIn
      txInB <- arbitrary @TxIn
      let topTx =
            mkBasicTx $
              mkBasicTxBody
                & subTransactionsTxBodyL
                  .~ [mkSubTx txInA cert, mkSubTx txInB cert]
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (topTx ^. bodyTxL) `shouldBe` inject poolDeposit

    it "sums outputs, fee, treasury donations, pool/DRep deposits and burned assets across the batch" $ do
      poolDeposit <- (Coin 1 <>) <$> arbitrary
      dRepDeposit <- (Coin 1 <>) <$> arbitrary
      modifyPParams $ (ppPoolDepositL .~ poolDeposit) . (ppDRepDepositL .~ dRepDeposit)
      poolParams <- arbitrary
      drepA <- arbitrary
      drepB <- arbitrary
      topAddr <- arbitrary
      subAddr <- arbitrary
      policyA <- arbitrary @PolicyID
      asset <- arbitrary @AssetName
      topOutValue <- arbitrary
      subOutValue <- arbitrary
      topFee <- arbitrary
      topTreasury <- arbitrary
      subTreasury <- arbitrary
      topBurnAmount <- getPositive <$> arbitrary
      subBurnAmount <- getPositive <$> arbitrary
      let topMint = multiAssetFromList [(policyA, asset, -topBurnAmount)]
          subMint = multiAssetFromList [(policyA, asset, -subBurnAmount)]
          expectedBurned :: MultiAsset
          expectedBurned =
            multiAssetFromList
              [ (policyA, asset, topBurnAmount + subBurnAmount)
              ]
          regPool :: TxCert era
          regPool = RegPoolTxCert poolParams
          topOut = mkBasicTxOut topAddr (inject topOutValue)
          subOut = mkBasicTxOut subAddr (inject subOutValue)
          subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody
                & outputsTxBodyL .~ [subOut]
                & certsTxBodyL
                  .~ [regPool, RegDRepTxCert drepB dRepDeposit SNothing]
                & treasuryDonationTxBodyL .~ subTreasury
                & mintTxBodyL .~ subMint
          topTx :: Tx TopTx era
          topTx =
            mkBasicTx $
              mkBasicTxBody
                & outputsTxBodyL .~ [topOut]
                & feeTxBodyL .~ topFee
                & certsTxBodyL
                  .~ [regPool, RegDRepTxCert drepA dRepDeposit SNothing]
                & treasuryDonationTxBodyL .~ topTreasury
                & mintTxBodyL .~ topMint
                & subTransactionsTxBodyL .~ [subTx]
          expectedCoin =
            topOutValue
              <> subOutValue
              <> topFee
              <> topTreasury
              <> subTreasury
              <> poolDeposit
              <> ((2 :: Int) <×> dRepDeposit)
          expected = MaryValue expectedCoin expectedBurned
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (topTx ^. bodyTxL) `shouldBe` expected
  where
    mkSubTx i cert = mkBasicTx $ mkBasicTxBody & certsTxBodyL .~ [cert] & inputsTxBodyL .~ [i]
