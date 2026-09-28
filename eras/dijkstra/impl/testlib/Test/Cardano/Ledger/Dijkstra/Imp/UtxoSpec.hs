{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
import Cardano.Ledger.Dijkstra.UTxO (dijkstraConsumed)
import Cardano.Ledger.Mary.Value (
  AssetName,
  MaryValue (..),
  PolicyID (..),
  multiAssetFromList,
 )
import Cardano.Ledger.Plutus
import qualified Cardano.Ledger.Shelley.AdaPots as AdaPots
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts (pattern RequireSignature)
import Cardano.Ledger.Shelley.UTxO (produced)
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Cardano.Ledger.TxIn
import Cardano.Ledger.Val
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Lens.Micro
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysFailsWithDatum, alwaysSucceedsWithDatum)

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
      pp <- getsPParams id
      poolKh <- freshKeyHash
      tx <- registerPoolTxWithSubTxs [poolKh] [[poolKh], [poolKh]]
      -- just the pool deposits are in `produced` because the transaction is not fixed up
      expectProduced tx $ inject (pp ^. ppPoolDepositL)
      submitInAllModes tx

    it "counts distinct pool deposits in top and sub separately" $ do
      pp <- getsPParams id
      poolA <- freshKeyHash
      poolB <- freshKeyHash
      tx <- registerPoolTxWithSubTxs [poolB, poolA, poolB] [[poolA, poolA, poolB], [poolA, poolB]]
      expectProduced tx $ inject ((2 :: Int) <×> (pp ^. ppPoolDepositL))
      submitInAllModes tx

    it "includes sub-tx cert deposits when top has no certs" $ do
      pp <- getsPParams id
      poolKh <- freshKeyHash
      tx <- registerPoolTxWithSubTxs [] [[poolKh]]
      expectProduced tx $ inject (pp ^. ppPoolDepositL)
      submitInAllModes tx

    it "does not count re-registrations of an already-registered pool across the batch" $ do
      poolKh <- freshKeyHash
      registerPool poolKh
      tx <- registerPoolTxWithSubTxs [poolKh] [[poolKh]]
      expectProduced tx mempty
      submitInAllModes tx

    it "dedupes across multiple subtransactions registering the same fresh pool" $ do
      pp <- getsPParams id
      poolKh <- freshKeyHash
      tx <- registerPoolTxWithSubTxs [] [[poolKh], [poolKh]]
      expectProduced tx $ inject (pp ^. ppPoolDepositL)
      submitInAllModes tx

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

      subDDAccount <- registerAccountAddress
      subDDAmount <- (Coin 1 <>) <$> arbitrary

      topOut <- freshTxOut
      subOut <- freshTxOut
      topTreasury <- arbitrary
      subTreasury <- arbitrary
      -- we are setting the fee manually in order to verify the `produced` value before the fixup.
      topFee <- (Coin 3_000_000 <>) <$> arbitrary

      let subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody
                & outputsTxBodyL .~ [subOut]
                & certsTxBodyL
                  .~ [subPoolCert, subDRepCert]
                & treasuryDonationTxBodyL .~ subTreasury
                & directDepositsTxBodyL .~ DirectDeposits [(subDDAccount, subDDAmount)]
          topTx :: Tx TopTx era
          topTx =
            mkBasicTx $
              mkBasicTxBody
                & outputsTxBodyL .~ [topOut]
                & feeTxBodyL .~ topFee
                & certsTxBodyL
                  .~ [topPoolCert, topDRepCert]
                & treasuryDonationTxBodyL .~ topTreasury
                & subTransactionsTxBodyL .~ [subTx]
          -- we're not adding direct deposits at the top level
          -- in order to be able to submit this transaction when switched to legacy mode
          -- (which doesn't support direct deposits)
          expectedCoin =
            (topOut ^. coinTxOutL)
              <> (subOut ^. coinTxOutL)
              <> topFee
              <> topTreasury
              <> subTreasury
              <> ((2 :: Int) <×> poolDeposit)
              <> ((2 :: Int) <×> dRepDeposit)
              <> subDDAmount
      expectProduced topTx $ inject expectedCoin
      checkDepositCalculation
        (topTx ^. bodyTxL)
        (((2 :: Int) <×> poolDeposit) <> ((2 :: Int) <×> dRepDeposit))
        (poolDeposit <> dRepDeposit)

      submitInAllModes topTx

    disableInConformanceIt "sums assets burned by the top and the sub transaction" $ do
      -- Mint upfront the tokens that the batch is going to burn: one output for the top
      -- transaction to spend and one for the sub transaction.
      policyId <- PolicyID <$> (impAddNativeScript . RequireSignature =<< freshKeyHash)
      assetName <- arbitrary @AssetName
      topBurnAmount <- getPositive <$> arbitrary
      subBurnAmount <- getPositive <$> arbitrary
      tokenAddr <- freshKeyAddr_
      let tokens n = multiAssetFromList [(policyId, assetName, n)]
      mintTx <-
        submitTopTx $
          mkBasicTx $
            mkBasicTxBody
              & mintTxBodyL .~ tokens (topBurnAmount + subBurnAmount)
              & outputsTxBodyL
                .~ [ mkBasicTxOut tokenAddr (MaryValue mempty (tokens topBurnAmount))
                   , mkBasicTxOut tokenAddr (MaryValue mempty (tokens subBurnAmount))
                   ]
      topOut <- freshTxOut
      subOut <- freshTxOut
      topFee <- (Coin 3_000_000 <>) <$> arbitrary
      let subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txInAt (1 :: Int) mintTx]
                & outputsTxBodyL .~ [subOut]
                & mintTxBodyL .~ tokens (negate subBurnAmount)
          topTx :: Tx TopTx era
          topTx =
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txInAt (0 :: Int) mintTx]
                & outputsTxBodyL .~ [topOut]
                & feeTxBodyL .~ topFee
                & mintTxBodyL .~ tokens (negate topBurnAmount)
                & subTransactionsTxBodyL .~ [subTx]
          expected =
            MaryValue
              ((topOut ^. coinTxOutL) <> (subOut ^. coinTxOutL) <> topFee)
              (tokens (topBurnAmount + subBurnAmount))
      expectProduced topTx expected
      submitInAllModes topTx

  describe "value consumed by a transaction" $ do
    it "sums inputs, withdrawals and refunds across the batch" $ do
      let genTx = do
            keyDeposit <- getsPParams ppKeyDepositL
            dRepDeposit <- getsPParams ppDRepDepositL

            -- accounts and DReps that the batch unregisters, one of each in the top
            -- transaction and one of each in the sub-transaction
            topCred <- freshRegisteredStakeCred
            subCred <- freshRegisteredStakeCred
            topDRep <- KeyHashObj <$> registerDRep
            subDRep <- KeyHashObj <$> registerDRep

            -- accounts that the batch withdraws from. They are distinct from the ones
            -- above, because an account with a non-zero balance cannot be unregistered.
            (topAccount, topWithdrawal) <- freshFundedAccount
            (subAccount, subWithdrawal) <- freshFundedAccount

            topInAmount <- Coin <$> choose (1_000_000, 2_000_000)
            topIn <- txInWithFunds topInAmount
            subInAmount <- Coin <$> choose (1_000_000, 2_000_000)
            subIn <- txInWithFunds subInAmount

            let subTx :: Tx SubTx era
                subTx =
                  mkBasicTx $
                    mkBasicTxBody
                      & inputsTxBodyL .~ [subIn]
                      & withdrawalsTxBodyL .~ Withdrawals [(subAccount, subWithdrawal)]
                      & certsTxBodyL
                        .~ [ UnRegDepositTxCert subCred keyDeposit
                           , UnRegDRepTxCert subDRep dRepDeposit
                           ]
                topTx :: Tx TopTx era
                topTx =
                  mkBasicTx $
                    mkBasicTxBody
                      & inputsTxBodyL .~ [topIn]
                      & withdrawalsTxBodyL .~ Withdrawals [(topAccount, topWithdrawal)]
                      & certsTxBodyL
                        .~ [ UnRegDepositTxCert topCred keyDeposit
                           , UnRegDRepTxCert topDRep dRepDeposit
                           ]
                      & subTransactionsTxBodyL .~ [subTx]
                batchRefunds = ((2 :: Int) <×> keyDeposit) <> ((2 :: Int) <×> dRepDeposit)
                expectedCoin =
                  topInAmount
                    <> subInAmount
                    <> topWithdrawal
                    <> subWithdrawal
                    <> batchRefunds
            expectConsumed topTx $ inject expectedCoin
            checkRefundCalculation (topTx ^. bodyTxL) batchRefunds (keyDeposit <> dRepDeposit)
            pure topTx
      submitInAllModes genTx

    it "includes sub-tx cert refunds when top has no certs" $ do
      let genTx = do
            keyDeposit <- getsPParams ppKeyDepositL
            dRepDeposit <- getsPParams ppDRepDepositL
            subCred <- freshRegisteredStakeCred
            subDRep <- KeyHashObj <$> registerDRep
            let subTx :: Tx SubTx era
                subTx =
                  mkBasicTx $
                    mkBasicTxBody
                      & certsTxBodyL
                        .~ [ UnRegDepositTxCert subCred keyDeposit
                           , UnRegDRepTxCert subDRep dRepDeposit
                           ]
                topTx = mkTopTxWithSubTxs [subTx]
            expectConsumed topTx $ inject (keyDeposit <> dRepDeposit)
            checkRefundCalculation (topTx ^. bodyTxL) (keyDeposit <> dRepDeposit) mempty
            pure topTx
      submitInAllModes genTx

    -- Refunds are collected from the values in the certificates, rather than from the
    -- state, which is why a deposit that is only paid within the same batch can still be
    -- refunded by it.
    it "refunds deposits that are paid earlier in the same batch" $ do
      keyDeposit <- getsPParams ppKeyDepositL
      dRepDeposit <- getsPParams ppDRepDepositL
      cred <- KeyHashObj <$> freshKeyHash
      dRep <- KeyHashObj <$> freshKeyHash
      let subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody
                & certsTxBodyL
                  .~ [ RegDepositTxCert cred keyDeposit
                     , RegDRepTxCert dRep dRepDeposit SNothing
                     ]
          -- the batch can unregister what it has just registered
          topTx :: Tx TopTx era
          topTx =
            mkBasicTx $
              mkBasicTxBody
                & certsTxBodyL
                  .~ [ UnRegDepositTxCert cred keyDeposit
                     , UnRegDRepTxCert dRep dRepDeposit
                     ]
                & subTransactionsTxBodyL .~ [subTx]
      expectConsumed topTx $ inject (keyDeposit <> dRepDeposit)
      expectProduced topTx $ inject (keyDeposit <> dRepDeposit)
      checkRefundCalculation (topTx ^. bodyTxL) (keyDeposit <> dRepDeposit) (keyDeposit <> dRepDeposit)

      depositedBefore <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL
      submitTx_ topTx
      expectStakeCredNotRegistered cred
      depositedAfter <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL
      depositedAfter `shouldBe` depositedBefore

    it "refunds the deposit in the certificate, not the one in the protocol parameters" $ do
      keyDeposit <- getsPParams ppKeyDepositL
      dRepDeposit <- getsPParams ppDRepDepositL
      topCred <- freshRegisteredStakeCred
      subCred <- freshRegisteredStakeCred
      topDRep <- KeyHashObj <$> registerDRep
      subDRep <- KeyHashObj <$> registerDRep
      -- Overwrite the deposit protocol parameters in order to ensure they do not affect
      -- the refunds that the batch collects
      modifyPParams $ \pp ->
        pp
          & ppKeyDepositL .~ Coin 1
          & ppDRepDepositL .~ Coin 2
      let subTx :: Tx SubTx era
          subTx =
            mkBasicTx $
              mkBasicTxBody
                & certsTxBodyL
                  .~ [ UnRegDepositTxCert subCred keyDeposit
                     , UnRegDRepTxCert subDRep dRepDeposit
                     ]
          topTx :: Tx TopTx era
          topTx =
            mkBasicTx $
              mkBasicTxBody
                & certsTxBodyL
                  .~ [ UnRegDepositTxCert topCred keyDeposit
                     , UnRegDRepTxCert topDRep dRepDeposit
                     ]
                & subTransactionsTxBodyL .~ [subTx]
          batchRefunds = ((2 :: Int) <×> keyDeposit) <> ((2 :: Int) <×> dRepDeposit)
      expectConsumed topTx $ inject batchRefunds
      checkRefundCalculation (topTx ^. bodyTxL) batchRefunds (keyDeposit <> dRepDeposit)
      submitTx_ topTx

  describe "Value preservation" $ do
    let mkSubTx :: BatchAmounts -> ImpTestM era (Tx SubTx era)
        mkSubTx BatchAmounts {..} = do
          txIn <- txInWithFunds baSubTxIn
          txOut <- mkTxOut baSubTxOut
          account <- registerAccountAddress
          pure $
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & outputsTxBodyL .~ [txOut]
                & directDepositsTxBodyL .~ DirectDeposits [(account, baSubDirectDeposit)]

    let mkTopTx :: BatchAmounts -> ImpTestM era (Tx TopTx era)
        mkTopTx amounts@BatchAmounts {..} = do
          txIn <- txInWithFunds baTopTxIn
          txOut <- mkTxOut baTopTxOut
          account <- registerAccountAddress
          fundAccountBalance account baTopWithdrawal
          subTx <- mkSubTx amounts
          pure $
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & outputsTxBodyL .~ [txOut]
                & feeTxBodyL .~ baFee
                & withdrawalsTxBodyL .~ Withdrawals [(account, baTopWithdrawal)]
                & subTransactionsTxBodyL .~ OMap.singleton subTx

    let mkTopTxLegacyMode :: BatchAmounts -> Tx TopTx era -> ImpTestM era (Tx TopTx era)
        mkTopTxLegacyMode BatchAmounts {..} tx = do
          scriptTxIn <- produceScriptAt (hashPlutusScript $ alwaysSucceedsWithDatum SPlutusV3) baScriptTxIn
          pure $
            tx
              & bodyTxL . inputsTxBodyL <>~ Set.singleton scriptTxIn
              & bodyTxL . feeTxBodyL <>~ baScriptTxIn

    let mkTopTxLegacyModePhase2Invalid :: BatchAmounts -> Tx TopTx era -> ImpTestM era (Tx TopTx era)
        mkTopTxLegacyModePhase2Invalid BatchAmounts {..} tx = do
          scriptTxIn <- produceScriptAt (hashPlutusScript $ alwaysFailsWithDatum SPlutusV3) baScriptTxIn
          pure $
            tx
              & bodyTxL . inputsTxBodyL <>~ Set.singleton scriptTxIn
              & bodyTxL . feeTxBodyL <>~ baScriptTxIn

    it "tx balanced across the batch and at the top level - normal mode" $ do
      amounts <- genFullyBalancedAmounts
      topTx <- mkTopTx amounts
      withFixup noBalanceFixup $ submitTopTx_ topTx

    it "tx balanced across the batch and at the top level - legacy mode" $ do
      amounts <- genFullyBalancedAmounts
      topTx <- mkTopTx amounts
      topTxLegacy <- mkTopTxLegacyMode amounts topTx
      withFixup noBalanceFixup $ submitTopTx_ topTxLegacy

    it "tx balanced across the batch and at the top level - legacy mode, phase2 invalid" $ do
      amounts <- genFullyBalancedAmounts
      topTxInvalid <- mkTopTxLegacyModePhase2Invalid amounts =<< mkTopTx amounts
      withFixup noBalanceFixup $ submitPhase2Invalid_ topTxInvalid

    it "tx balanced across the batch and unbalanced at the top level - normal mode" $ do
      amounts <- genBatchOnlyBalancedAmounts
      topTx <- mkTopTx amounts
      withFixup noBalanceFixup $ submitTopTx_ topTx

    it "tx balanced across the batch and unbalanced at the top level - legacy mode" $ do
      amounts <- genBatchOnlyBalancedAmounts
      topTx <- mkTopTx amounts
      topTxLegacy <- mkTopTxLegacyMode amounts topTx
      let balances = batchBalances True amounts
      withFixup noBalanceFixup $
        submitFailingTx
          topTxLegacy
          [ injectFailure $
              ValueNotConservedInLegacyMode
                Mismatch
                  { mismatchSupplied = inject (bbTopConsumed balances)
                  , mismatchExpected = inject (bbTopProduced balances)
                  }
          ]

    it "tx balanced at the top level and unbalanced across the batch - normal mode" $ do
      amounts <- genTopOnlyBalancedAmounts
      topTx <- mkTopTx amounts
      let balances = batchBalances False amounts
      withFixup noBalanceFixup $
        submitFailingTx
          topTx
          [ injectFailure $
              ValueNotConservedUTxO
                Mismatch
                  { mismatchSupplied = inject (bbBatchConsumed balances)
                  , mismatchExpected = inject (bbBatchProduced balances)
                  }
          ]
    it "tx balanced at the top level and unbalanced across the batch - legacy mode" $ do
      amounts <- genTopOnlyBalancedAmounts
      topTx <- mkTopTx amounts
      topTxLegacy <- mkTopTxLegacyMode amounts topTx
      let balances = batchBalances True amounts
      withFixup noBalanceFixup $
        submitFailingTx
          topTxLegacy
          [ injectFailure $
              ValueNotConservedUTxO
                Mismatch
                  { mismatchSupplied = inject (bbBatchConsumed balances)
                  , mismatchExpected = inject (bbBatchProduced balances)
                  }
          ]

    it "tx unbalanced across the batch and at the top level - normal mode" $ do
      amounts <- genFullyUnbalancedAmounts
      topTx <- mkTopTx amounts
      let balances = batchBalances False amounts
      withFixup noBalanceFixup $
        submitFailingTx
          topTx
          [ injectFailure $
              ValueNotConservedUTxO
                Mismatch
                  { mismatchSupplied = inject (bbBatchConsumed balances)
                  , mismatchExpected = inject (bbBatchProduced balances)
                  }
          ]

    it "tx unbalanced across the batch and at the top level - legacy mode" $ do
      amounts <- genFullyUnbalancedAmounts
      topTx <- mkTopTx amounts
      topTxLegacy <- mkTopTxLegacyMode amounts topTx
      let balances = batchBalances True amounts
      withFixup noBalanceFixup $
        submitFailingTx
          topTxLegacy
          [ injectFailure $
              ValueNotConservedInLegacyMode
                Mismatch
                  { mismatchSupplied = inject (bbTopConsumed balances)
                  , mismatchExpected = inject (bbTopProduced balances)
                  }
          , injectFailure $
              ValueNotConservedUTxO
                Mismatch
                  { mismatchSupplied = inject (bbBatchConsumed balances)
                  , mismatchExpected = inject (bbBatchProduced balances)
                  }
          ]

    it "a failing phase-1 check suppresses the script failure" $ do
      pp <- getsPParams id
      amounts <- do
        balanced <- genFullyBalancedAmounts
        pure balanced {baTopTxIn = baTopTxIn balanced <> pp ^. ppPoolDepositL}

      poolKh <- freshKeyHash
      poolParams <- freshPoolParams poolKh =<< registerAccountAddress
      let addPoolCert :: forall l. Tx l era -> Tx l era
          addPoolCert = bodyTxL . certsTxBodyL .~ [RegPoolTxCert poolParams]

      topTx <- mkTopTx amounts
      -- the sub-transaction registers the pool, the top-level transaction re-registers it
      withCerts <- traverseSubTxs (pure . addPoolCert) (addPoolCert topTx)
      withFailingScript <- mkTopTxLegacyModePhase2Invalid amounts withCerts

      -- exactly one failure: no `ValidationTagMismatch` alongside it
      let balances = batchBalances True amounts
      withFixup noBalanceFixup $
        submitFailingTx
          withFailingScript
          [ injectFailure $
              ValueNotConservedInLegacyMode
                Mismatch
                  { mismatchSupplied = inject (bbTopConsumed balances)
                  , mismatchExpected = inject (bbTopProduced balances)
                  }
          ]

    describe "fixup function for balancing subtransactions" $ do
      it "top-only balanced - normal mode" $ do
        amounts <- genTopOnlyBalancedAmounts
        topTx <- mkTopTx amounts
        balanced <- balanceSubTransactions topTx
        withFixup noBalanceFixup $ submitTopTx_ balanced

      it "top-only balanced - legacy mode" $ do
        amounts <- genTopOnlyBalancedAmounts
        topTx <- mkTopTx amounts
        topTxLegacy <- mkTopTxLegacyMode amounts topTx
        balanced <- balanceSubTransactions topTxLegacy
        withFixup noBalanceFixup $ submitTopTx_ balanced

      it "balanced on both levels keeps it balanced" $ do
        amounts <- genFullyBalancedAmounts
        topTx <- mkTopTx amounts
        balanced <- balanceSubTransactions topTx
        withFixup noBalanceFixup $ submitTopTx_ balanced
  where
    submitInAllModes :: HasCallStack => Tx TopTx era -> ImpTestM era ()
    submitInAllModes tx = do
      simulateThenRestore $ submitTopTx_ tx
      simulateThenRestore $ submitTopTx_ =<< switchTxToLegacyMode tx
      submitPhase2Invalid_ =<< switchTxToPhase2InvalidLegacyMode tx
    -- TODO add switchTxToFailing, after Plutus V4 support is complete
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
    expectProduced :: Tx TopTx era -> Value era -> ImpTestM era ()
    expectProduced tx expected = do
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (tx ^. bodyTxL) `shouldBe` expected

    expectConsumed :: Tx TopTx era -> Value era -> ImpTestM era ()
    expectConsumed tx expected = do
      pp <- getsPParams id
      utxo <- getUTxO
      dijkstraConsumed pp utxo (tx ^. bodyTxL) `shouldBe` expected

    -- Check that `certsTotalDepositsTxBody` (used to set deposits in `UTxOState` and `AdaPots` calculations)
    -- returns the batch deposits, while `getTotalDepositsTxBody` returns the top-level deposits
    checkDepositCalculation topBody batchDeposits topLevelDeposits = do
      pp <- getsPParams id
      certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
      AdaPots.proDeposits (AdaPots.producedTxBody topBody pp certState)
        `shouldBe` batchDeposits
      let isPoolReg = (`Map.member` (certState ^. certPStateL . psStakePoolsL))
      getTotalDepositsTxBody pp isPoolReg topBody `shouldBe` topLevelDeposits

    -- Check that `certsTotalRefundsTxBody` (used to update the deposits in `UTxOState` and in
    -- `AdaPots` calculations) returns the batch refunds, while `getTotalRefundsTxBody` returns
    -- the top-level refunds
    checkRefundCalculation topBody batchRefunds topLevelRefunds = do
      pp <- getsPParams id
      certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
      utxo <- getUTxO
      AdaPots.conRefunds (AdaPots.consumedTxBody topBody pp certState utxo)
        `shouldBe` batchRefunds
      -- refunds do not depend on the state, hence the deposit lookup is irrelevant
      getTotalRefundsTxBody pp (const Nothing) topBody `shouldBe` topLevelRefunds

    freshRegisteredStakeCred = do
      cred <- KeyHashObj <$> freshKeyHash
      cred <$ registerStakeCredential cred

    -- An account with a freshly funded, non-zero balance
    freshFundedAccount = do
      account <- registerAccountAddress
      amount <- (Coin 1 <>) <$> arbitrary
      fundAccountBalance account amount
      pure (account, amount)

    freshTxOut = do
      pp <- getsPParams id
      addr <- freshKeyAddr_
      amount <- arbitrary @Coin
      pure $ ensureMinCoinTxOut pp (mkBasicTxOut addr (inject amount))
    fundAccountBalance :: AccountAddress -> Coin -> ImpTestM era ()
    fundAccountBalance account amount = do
      submitTx_ $
        mkBasicTx $
          mkBasicTxBody
            & directDepositsTxBodyL .~ DirectDeposits [(account, amount)]
    txInWithFunds :: Coin -> ImpTestM era TxIn
    txInWithFunds amount = freshKeyAddr_ >>= \a -> sendCoinTo a amount
    mkTxOut :: Coin -> ImpTestM era (TxOut era)
    mkTxOut amount = freshKeyAddr_ >>= \a -> pure $ mkBasicTxOut a (inject amount)
    produceScriptAt :: ScriptHash -> Coin -> ImpTestM era TxIn
    produceScriptAt scriptHash amount = do
      let addr = mkAddr scriptHash StakeRefNull
      let
        tx :: forall l. Typeable l => Tx l era
        tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut addr (inject amount)]
      txInAt 0 <$> submitTopTx tx

noBalanceFixup ::
  ( HasCallStack
  , DijkstraEraImp era
  ) =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
noBalanceFixup =
  fixupSubTransactions
    >=> addNativeScriptTxWits
    >=> fixupAuxDataHash
    >=> addCollateralInput
    >=> fixupScriptWits
    >=> fixupOutputDatums
    >=> fixupDatums
    >=> fixupRedeemerIndices
    >=> fixupTxOuts
    >=> fixupCollateralReturn
    >=> fixupRedeemers
    >=> fixupPPHash
    >=> updateAddrTxWits

-- A template for creating a transaction with exactly one subtransaction,
-- with values for different fields that contribute to consumed and produced.
data BatchAmounts = BatchAmounts
  { baSubTxIn :: Coin
  , baSubTxOut :: Coin
  , baSubDirectDeposit :: Coin
  , baTopTxIn :: Coin
  , baTopWithdrawal :: Coin
  , baTopTxOut :: Coin
  , baFee :: Coin
  , baScriptTxIn :: Coin
  }

genBatchOnlyBalancedAmounts :: ImpTestM era BatchAmounts
genBatchOnlyBalancedAmounts = do
  -- we are restricted in the lower bound by min utxo size
  -- and in the upper bound by the hardcoded collateral in `makeCollateralInput`
  m <- Coin <$> choose (1_000_000, 2_000_000)
  pure $ mkAmounts m
  where
    mkAmounts m =
      -- These values create an unbalanced sub-transaction, with:
      --      consumed = subTxIn   = 1
      --      produced = subTxOut + subDirectDeposit  =  2 + 3
      -- and an unbalanced top transaction, with:
      --      consumed = topTxIn + topWithdrawal = 8 + 5
      --      produced = topTxOut + fee    = 6 + 3
      -- Legacy variant adds scriptTxIn on both sides (input + fee)
      -- On the batch level, the transaction is balancing out.
      let amounts =
            BatchAmounts
              { baSubTxIn = (1 :: Int) <×> m
              , baSubTxOut = (2 :: Int) <×> m
              , baSubDirectDeposit = (3 :: Int) <×> m
              , baTopTxIn = (8 :: Int) <×> m
              , baTopWithdrawal = (5 :: Int) <×> m
              , baTopTxOut = (6 :: Int) <×> m
              , baFee = (3 :: Int) <×> m
              , baScriptTxIn = (4 :: Int) <×> m
              }
       in assertBatchBalanced amounts

-- Amounts for a transaction that balances out both at batch level, and at top level
genFullyBalancedAmounts :: ImpTestM era BatchAmounts
genFullyBalancedAmounts = do
  batchBalanced@BatchAmounts {..} <- genBatchOnlyBalancedAmounts
  let BatchBalances {..} = batchBalances False batchBalanced
      mismatch = bbTopConsumed <-> bbTopProduced
      fullyBalanced =
        batchBalanced
          { -- because the batch is balanced, we can fix both top and sub balances with the same `mismatch`
            baTopTxOut = baTopTxOut <> mismatch
          , baSubTxIn = baSubTxIn <> mismatch
          }
  pure $
    fullyBalanced
      & assertBatchBalanced
      & assertTopBalanced
      & assertSubBalanced

-- Amounts for a transaction that doesn't balance out - neither at top or batch level
genFullyUnbalancedAmounts :: ImpTestM era BatchAmounts
genFullyUnbalancedAmounts = do
  balanced@BatchAmounts {..} <- genFullyBalancedAmounts
  extra <- Coin . getPositive <$> arbitrary
  pure $ balanced {baTopTxIn = baTopTxIn <> extra}

genTopOnlyBalancedAmounts :: ImpTestM era BatchAmounts
genTopOnlyBalancedAmounts = do
  balanced@BatchAmounts {..} <- genFullyBalancedAmounts
  extra <- Coin . getPositive <$> arbitrary
  pure $ balanced {baSubTxIn = baSubTxIn <> extra}

data BatchBalances = BatchBalances
  { bbSubConsumed :: Coin
  , bbSubProduced :: Coin
  , bbTopConsumed :: Coin
  , bbTopProduced :: Coin
  , bbBatchConsumed :: Coin
  , bbBatchProduced :: Coin
  }

assertBatchBalanced :: HasCallStack => BatchAmounts -> BatchAmounts
assertBatchBalanced ba
  | bbBatchConsumed bb == bbBatchProduced bb = ba
  | otherwise =
      error $
        "Impossible: batch amounts are not balanced: consumed = "
          <> show (bbBatchConsumed bb)
          <> ", produced = "
          <> show (bbBatchProduced bb)
  where
    bb = batchBalances False ba

assertTopBalanced :: HasCallStack => BatchAmounts -> BatchAmounts
assertTopBalanced ba
  | bbTopConsumed bb == bbTopProduced bb = ba
  | otherwise =
      error $
        "Impossible: top transaction amounts are not balanced: consumed = "
          <> show (bbTopConsumed bb)
          <> ", produced = "
          <> show (bbTopProduced bb)
  where
    bb = batchBalances False ba

assertSubBalanced :: HasCallStack => BatchAmounts -> BatchAmounts
assertSubBalanced ba
  | bbSubConsumed bb == bbSubProduced bb = ba
  | otherwise =
      error $
        "Impossible: sub-transaction amounts are not balanced: consumed = "
          <> show (bbSubConsumed bb)
          <> ", produced = "
          <> show (bbSubProduced bb)
  where
    bb = batchBalances False ba

batchBalances :: Bool -> BatchAmounts -> BatchBalances
batchBalances isLegacy BatchAmounts {..} =
  let script = if isLegacy then baScriptTxIn else mempty
      subConsumed = baSubTxIn
      subProduced = baSubTxOut <> baSubDirectDeposit
      topConsumed = baTopTxIn <> baTopWithdrawal <> script
      topProduced = baTopTxOut <> baFee <> script
   in BatchBalances
        { bbSubConsumed = subConsumed
        , bbSubProduced = subProduced
        , bbTopConsumed = topConsumed
        , bbTopProduced = topProduced
        , bbBatchConsumed = subConsumed <> topConsumed
        , bbBatchProduced = subProduced <> topProduced
        }
