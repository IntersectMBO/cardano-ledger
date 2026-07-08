{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.UtxoSpec (spec) where

import Cardano.Ledger.BaseTypes (Inject (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Mary.Value (
  AssetName,
  MaryValue (..),
  MultiAsset,
  PolicyID,
  multiAssetFromList,
 )
import Cardano.Ledger.State (EraUTxO (..), StakePoolParams (..))
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Mary.Arbitrary ()

spec :: Spec
spec = describe "getProducedValue" $ do
  it "counts each new pool deposit at most once across the batch" $ do
    -- Top and sub register the SAME fresh pool. A per-tx (recursive)
    -- accumulator would charge the deposit twice; a batch-wide shared
    -- accumulator charges once.
    poolParams <- generate arbitrary
    let cert = RegPoolTxCert poolParams
        pp = emptyPParams @DijkstraEra & ppPoolDepositL .~ poolDeposit
        subBody = mkBasicTxBody & certsTxBodyL .~ SSeq.singleton cert
        subTx = mkBasicTx subBody
        topBody =
          mkBasicTxBody
            & certsTxBodyL .~ [cert]
            & subTransactionsTxBodyL .~ [subTx]
    getProducedValue pp (const False) topBody `shouldBe` inject poolDeposit

  it "counts distinct pool deposits in top and sub separately" $ do
    -- Different pools in top and sub. Post-fix the batch view must still
    -- include sub-tx cert deposits; test fails if sub certs are ignored.
    poolA <- generate arbitrary
    poolB <- generate $ arbitrary `suchThat` \p -> sppId p /= sppId poolA
    let pp = emptyPParams @DijkstraEra & ppPoolDepositL .~ poolDeposit
        subBody = mkBasicTxBody & certsTxBodyL .~ SSeq.singleton (RegPoolTxCert poolA)
        subTx = mkBasicTx subBody
        topBody =
          mkBasicTxBody
            & certsTxBodyL .~ [RegPoolTxCert poolB]
            & subTransactionsTxBodyL .~ [subTx]
    getProducedValue pp (const False) topBody `shouldBe` inject (Coin 1000)

  it "includes sub-tx cert deposits when top has no certs" $ do
    -- Only the sub-tx has a cert. Tests that sub-cert contribution is
    -- included at all in the batch.
    poolParams <- generate arbitrary
    let pp = emptyPParams @DijkstraEra & ppPoolDepositL .~ poolDeposit
        subBody = mkBasicTxBody & certsTxBodyL .~ [RegPoolTxCert poolParams]
        subTx = mkBasicTx subBody
        topBody =
          mkBasicTxBody
            & subTransactionsTxBodyL .~ [subTx]
    getProducedValue pp (const False) topBody `shouldBe` inject poolDeposit

  it "charges zero for re-registration of an already-registered pool across the batch" $ do
    -- Pool P is already registered on-chain (isRegPoolId P = True). Both
    -- top and sub attempt to re-register it. No new pool → 0 deposit.
    poolParams <- generate arbitrary
    let cert = RegPoolTxCert poolParams
        pp = emptyPParams @DijkstraEra & ppPoolDepositL .~ poolDeposit
        subBody = mkBasicTxBody & certsTxBodyL .~ [cert]
        subTx = mkBasicTx subBody
        -- topBody :: TxBody TopTx DijkstraEra
        topBody =
          mkBasicTxBody
            & certsTxBodyL .~ [cert]
            & subTransactionsTxBodyL .~ [subTx]
    getProducedValue pp (\kh -> sppId poolParams == kh) topBody
      `shouldBe` inject (Coin 0)

  it "dedupes across multiple sub-txs registering the same fresh pool" $ do
    -- Two sub-txs each register the same fresh pool; top has no certs.
    -- Batch view must count the deposit exactly once. A per-tx recursive
    -- accumulator would give 2 * poolDeposit.
    poolParams <- generate arbitrary
    txInA <- generate (arbitrary :: Gen TxIn)
    txInB <- generate (arbitrary :: Gen TxIn)
    let cert = RegPoolTxCert poolParams
        pp = emptyPParams @DijkstraEra & ppPoolDepositL .~ poolDeposit
        -- Distinguish subs by an input (inputs don't contribute to produced).
        mkSub i =
          mkBasicTx $
            mkBasicTxBody
              & certsTxBodyL .~ [cert]
              & inputsTxBodyL .~ [i]
        topBody =
          mkBasicTxBody
            & subTransactionsTxBodyL
              .~ [mkSub txInA, mkSub txInB]
    getProducedValue pp (const False) topBody `shouldBe` inject poolDeposit

  it "sums outputs, fee, treasury donations, pool/DRep deposits and burned assets across the batch" $ do
    -- Positive mints contribute to consumed, not produced; we test the burn side here.
    poolParams <- generate arbitrary
    drepA <- generate arbitrary
    drepB <- generate $ arbitrary `suchThat` (/= drepA)
    topAddr <- generate arbitrary
    subAddr <- generate arbitrary
    policyA <- generate (arbitrary :: Gen PolicyID)
    asset <- generate (arbitrary :: Gen AssetName)
    let drepDeposit = Coin 100
        topOutValue = Coin 1000
        subOutValue = Coin 2000
        topFee = Coin 70
        topTreasury = Coin 30
        subTreasury = Coin 20
        topBurnAmount = 5
        subBurnAmount = 3
        topMint = multiAssetFromList [(policyA, asset, -topBurnAmount)]
        subMint = multiAssetFromList [(policyA, asset, -subBurnAmount)]
        expectedBurned :: MultiAsset
        expectedBurned =
          multiAssetFromList
            [ (policyA, asset, topBurnAmount + subBurnAmount)
            ]
        pp =
          emptyPParams @DijkstraEra
            & ppPoolDepositL .~ poolDeposit
            & ppDRepDepositL .~ drepDeposit
        regPool = RegPoolTxCert poolParams
        topOut = mkBasicTxOut topAddr (inject topOutValue)
        subOut = mkBasicTxOut subAddr (inject subOutValue)
        subBody =
          mkBasicTxBody
            & outputsTxBodyL .~ [subOut]
            & certsTxBodyL
              .~ [regPool, RegDRepTxCert drepB drepDeposit SNothing]
            & treasuryDonationTxBodyL .~ subTreasury
            & mintTxBodyL .~ subMint
        subTx = mkBasicTx subBody
        topBody =
          mkBasicTxBody
            & outputsTxBodyL .~ [topOut]
            & feeTxBodyL .~ topFee
            & certsTxBodyL
              .~ [regPool, RegDRepTxCert drepA drepDeposit SNothing]
            & treasuryDonationTxBodyL .~ topTreasury
            & mintTxBodyL .~ topMint
            & subTransactionsTxBodyL .~ [subTx]
        expectedCoin =
          topOutValue -- outputs
            <> subOutValue
            <> topFee -- fee (top only)
            <> topTreasury -- treasury donations
            <> subTreasury
            <> poolDeposit -- 1 unique fresh pool (top + sub dedup)
            <> drepDeposit -- DRep deposits: 2 distinct DReps
            <> drepDeposit
        expected = MaryValue expectedCoin expectedBurned
    getProducedValue pp (const False) topBody `shouldBe` expected
  where
    poolDeposit = Coin 500
