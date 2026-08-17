{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
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
  PolicyID (..),
  multiAssetFromList,
 )
import qualified Cardano.Ledger.Shelley.AdaPots as AdaPots
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts (pattern RequireSignature)
import Cardano.Ledger.Shelley.UTxO (produced)
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Cardano.Ledger.Val
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  DijkstraEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  describe "Collaterals" $ do
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/1264
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "Fails to submit a transaction containing a Ptr in collateral return" $ do
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
      poolKh <- freshKeyHash
      tx <- registerPoolTxWithSubTxs [poolKh] [[poolKh], [poolKh]]
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      -- just the pool deposits are in `produced` because the transaction is not fixed up
      produced pp pState (tx ^. bodyTxL) `shouldBe` inject (pp ^. ppPoolDepositL)
      submitTx_ tx

    it "counts distinct pool deposits in top and sub separately" $ do
      poolA <- freshKeyHash
      poolB <- freshKeyHash
      tx <- registerPoolTxWithSubTxs [poolB, poolA, poolB] [[poolA, poolA, poolB], [poolA, poolB]]

      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (tx ^. bodyTxL) `shouldBe` inject ((2 :: Int) <×> (pp ^. ppPoolDepositL))
      submitTx_ tx

    it "includes sub-tx cert deposits when top has no certs" $ do
      poolKh <- freshKeyHash
      tx <- registerPoolTxWithSubTxs [] [[poolKh]]

      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (tx ^. bodyTxL) `shouldBe` inject (pp ^. ppPoolDepositL)
      submitTx_ tx

    it "does not count re-registrations of an already-registered pool across the batch" $ do
      poolKh <- freshKeyHash
      registerPool poolKh
      tx <- registerPoolTxWithSubTxs [poolKh] [[poolKh]]
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (tx ^. bodyTxL) `shouldBe` mempty
      submitTx_ tx

    it "dedupes across multiple subtransactions registering the same fresh pool" $ do
      poolKh <- freshKeyHash
      tx <- registerPoolTxWithSubTxs [] [[poolKh], [poolKh]]
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (tx ^. bodyTxL) `shouldBe` inject (pp ^. ppPoolDepositL)
      submitTx_ tx

    it "sums outputs, fee, treasury donations and deposits across the batch" $ do
      pp <- getsPParams id
      let poolDeposit = pp ^. ppPoolDepositL
          dRepDeposit = pp ^. ppDRepDepositL

      let freshPoolCert = do
            poolKh <- freshKeyHash
            pps <- freshPoolParams poolKh =<< registerAccountAddress
            pure $ RegPoolTxCert @era pps
      topPoolCert <- freshPoolCert
      subPoolCert <- freshPoolCert

      let freshDRepCert = do
            kh <- freshKeyHash
            pure $ RegDRepTxCert @era (KeyHashObj kh) dRepDeposit SNothing
      topDRepCert <- freshDRepCert
      subDRepCert <- freshDRepCert

      topDDAccount <- registerAccountAddress
      subDDAccount <- registerAccountAddress
      topDDAmount <- (Coin 1 <>) <$> arbitrary
      subDDAmount <- (Coin 1 <>) <$> arbitrary

      topOut <- freshTxOut
      subOut <- freshTxOut
      topTreasury <- arbitrary
      subTreasury <- arbitrary
      -- we are setting the fee manually in order to verify the `produced` value before the fixup.
      topFee <- (Coin 1_000_000 <>) <$> arbitrary

      -- Mint upfront the tokens that the batch is going to burn: one output for the top
      -- transaction to spend and one for the sub transaction.
      policyId <- PolicyID <$> (impAddNativeScript . RequireSignature =<< freshKeyHash)
      assetName <- arbitrary @AssetName
      topBurnAmount <- getPositive <$> arbitrary
      subBurnAmount <- getPositive <$> arbitrary
      let tokens n = multiAssetFromList [(policyId, assetName, n)]
      tokenAddr <- freshKeyAddr_
      mintTx <-
        submitTx $
          mkBasicTx $
            mkBasicTxBody
              & mintTxBodyL .~ tokens (topBurnAmount + subBurnAmount)
              & outputsTxBodyL
                .~ [ mkBasicTxOut tokenAddr (MaryValue mempty (tokens topBurnAmount))
                   , mkBasicTxOut tokenAddr (MaryValue mempty (tokens subBurnAmount))
                   ]

      let subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txInAt (1 :: Int) mintTx]
                & outputsTxBodyL .~ [subOut]
                & certsTxBodyL
                  .~ [subPoolCert, subDRepCert]
                & treasuryDonationTxBodyL .~ subTreasury
                & mintTxBodyL .~ tokens (negate subBurnAmount)
                & directDepositsTxBodyL .~ DirectDeposits [(subDDAccount, subDDAmount)]
          topTx :: Tx TopTx era
          topTx =
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txInAt (0 :: Int) mintTx]
                & outputsTxBodyL .~ [topOut]
                & feeTxBodyL .~ topFee
                & certsTxBodyL
                  .~ [topPoolCert, topDRepCert]
                & treasuryDonationTxBodyL .~ topTreasury
                & mintTxBodyL .~ tokens (negate topBurnAmount)
                & directDepositsTxBodyL .~ DirectDeposits [(topDDAccount, topDDAmount)]
                & subTransactionsTxBodyL .~ [subTx]
          expectedCoin =
            (topOut ^. coinTxOutL)
              <> (subOut ^. coinTxOutL)
              <> topFee
              <> topTreasury
              <> subTreasury
              <> ((2 :: Int) <×> poolDeposit)
              <> ((2 :: Int) <×> dRepDeposit)
              <> topDDAmount
              <> subDDAmount
          expected = MaryValue expectedCoin (tokens (topBurnAmount + subBurnAmount))
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (topTx ^. bodyTxL) `shouldBe` expected
      checkDepositCalculation
        (topTx ^. bodyTxL)
        (((2 :: Int) <×> poolDeposit) <> ((2 :: Int) <×> dRepDeposit))
        (poolDeposit <> dRepDeposit)
      submitTx_ topTx
  where
    registerPoolTxWithSubTxs ::
      [KeyHash StakePool] -> -- top's pool certs
      [[KeyHash StakePool]] -> -- one sub-tx per inner list, with one pool cert per key
      ImpTestM era (Tx TopTx era)
    registerPoolTxWithSubTxs topKhs subKhs = do
      top <- registerPoolTx @TopTx topKhs
      subs <- traverse (registerPoolTx @SubTx) subKhs
      pure $ top & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable subs
    registerPoolTx :: forall l. Typeable l => [KeyHash StakePool] -> ImpTestM era (Tx l era)
    registerPoolTx khPools = do
      certs <-
        traverse
          ( \khPool ->
              RegPoolTxCert @era <$> (freshPoolParams khPool =<< registerAccountAddress)
          )
          khPools
      pure $ mkBasicTx mkBasicTxBody & bodyTxL . certsTxBodyL .~ StrictSeq.fromList certs

    -- Check that `certsTotalDepositsTxBody` (used to set deposits in `UTxOState` and `AdaPots` calculations)
    -- returns the batch deposits, while `getTotalDepositsTxBody` returns the top-level deposits
    checkDepositCalculation topBody batchDeposits topLevelDeposits = do
      pp <- getsPParams id
      certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
      AdaPots.proDeposits (AdaPots.producedTxBody topBody pp certState)
        `shouldBe` batchDeposits
      let isPoolReg = (`Map.member` (certState ^. certPStateL . psStakePoolsL))
      getTotalDepositsTxBody pp isPoolReg topBody `shouldBe` topLevelDeposits
    freshTxOut = do
      pp <- getsPParams id
      addr <- freshKeyAddr_
      amount <- arbitrary @Coin
      pure $ ensureMinCoinTxOut pp (mkBasicTxOut addr (inject amount))
