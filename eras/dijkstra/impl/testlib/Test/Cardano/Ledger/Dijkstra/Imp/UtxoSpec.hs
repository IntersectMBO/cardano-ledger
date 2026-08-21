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
import Control.Exception (assert)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Lens.Micro
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum)

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
      let genTx = do
            poolKh <- freshKeyHash
            tx <- registerPoolTxWithSubTxs [poolKh] [[poolKh], [poolKh]]
            -- just the pool deposits are in `produced` because the transaction is not fixed up
            expectProduced tx $ inject (pp ^. ppPoolDepositL)
            pure tx
      submitTx_ =<< genTx
      submitTx_ =<< switchTxToLegacyMode =<< genTx

    it "counts distinct pool deposits in top and sub separately" $ do
      let genTx = do
            pp <- getsPParams id
            poolA <- freshKeyHash
            poolB <- freshKeyHash
            tx <- registerPoolTxWithSubTxs [poolB, poolA, poolB] [[poolA, poolA, poolB], [poolA, poolB]]
            expectProduced tx $ inject ((2 :: Int) <×> (pp ^. ppPoolDepositL))
            pure tx
      submitTx_ =<< genTx
      submitTx_ =<< switchTxToLegacyMode =<< genTx

    it "includes sub-tx cert deposits when top has no certs" $ do
      pp <- getsPParams id
      let genTx = do
            poolKh <- freshKeyHash
            tx <- registerPoolTxWithSubTxs [] [[poolKh]]
            expectProduced tx $ inject (pp ^. ppPoolDepositL)
            pure tx
      submitTx_ =<< genTx
      submitTx_ =<< switchTxToLegacyMode =<< genTx

    it "does not count re-registrations of an already-registered pool across the batch" $ do
      let genTx = do
            poolKh <- freshKeyHash
            registerPool poolKh
            tx <- registerPoolTxWithSubTxs [poolKh] [[poolKh]]
            expectProduced tx mempty
            pure tx
      submitTx_ =<< genTx
      submitTx_ =<< switchTxToLegacyMode =<< genTx

    it "dedupes across multiple subtransactions registering the same fresh pool" $ do
      pp <- getsPParams id
      let genTx = do
            poolKh <- freshKeyHash
            tx <- registerPoolTxWithSubTxs [] [[poolKh], [poolKh]]
            expectProduced tx $ inject (pp ^. ppPoolDepositL)
            pure tx
      submitTx_ =<< genTx
      submitTx_ =<< switchTxToLegacyMode =<< genTx

    it "sums outputs, fee, treasury donations and deposits across the batch" $ do
      pp <- getsPParams id
      let genTx = do
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
            topFee <- (Coin 1_000_000 <>) <$> arbitrary

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
            pure topTx

      submitTx_ =<< genTx
      submitTx_ =<< switchTxToLegacyMode =<< genTx

    disableInConformanceIt "sums assets burned by the top and the sub transaction" $ do
      let genTx = do
            -- Mint upfront the tokens that the batch is going to burn: one output for the top
            -- transaction to spend and one for the sub transaction.
            policyId <- PolicyID <$> (impAddNativeScript . RequireSignature =<< freshKeyHash)
            assetName <- arbitrary @AssetName
            topBurnAmount <- getPositive <$> arbitrary
            subBurnAmount <- getPositive <$> arbitrary
            tokenAddr <- freshKeyAddr_
            let tokens n = multiAssetFromList [(policyId, assetName, n)]
            mintTx <-
              submitTx $
                mkBasicTx $
                  mkBasicTxBody
                    & mintTxBodyL .~ tokens (topBurnAmount + subBurnAmount)
                    & outputsTxBodyL
                      .~ [ mkBasicTxOut tokenAddr (MaryValue mempty (tokens topBurnAmount))
                         , mkBasicTxOut tokenAddr (MaryValue mempty (tokens subBurnAmount))
                         ]
            topOut <- freshTxOut
            subOut <- freshTxOut
            topFee <- (Coin 1_000_000 <>) <$> arbitrary
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
            pure topTx
      submitTx_ =<< genTx
      submitTx_ =<< switchTxToLegacyMode =<< genTx

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
          scriptTxIn <- produceScriptAt (hashPlutusScript $ alwaysSucceedsWithDatum SPlutusV2) baScriptTxIn
          pure $
            tx
              & bodyTxL . inputsTxBodyL <>~ Set.singleton scriptTxIn
              & bodyTxL . feeTxBodyL <>~ baScriptTxIn

    it "tx balanced across the batch and at the top level - normal mode" $ do
      amounts <- genFullyBalancedAmounts
      topTx <- mkTopTx amounts
      withFixup noBalanceFixup $ submitTx_ topTx

    it "tx balanced across the batch and at the top level - legacy mode" $ do
      amounts <- genFullyBalancedAmounts
      topTx <- mkTopTx amounts
      topTxLegacy <- mkTopTxLegacyMode amounts topTx
      withFixup noBalanceFixup $ submitTx_ topTxLegacy

    it "tx balanced across the batch and unbalanced at the top level - normal mode" $ do
      amounts <- genBatchOnlyBalancedAmounts
      topTx <- mkTopTx amounts
      withFixup noBalanceFixup $ submitTx_ topTx

    it "tx balanced across the batch and unbalanced at the top level - legacy mode" $ do
      amounts <- genBatchOnlyBalancedAmounts
      topTx <- mkTopTx amounts
      topTxLegacy <- mkTopTxLegacyMode amounts topTx
      let balances = batchBalances True amounts
      withFixup noBalanceFixup $
        submitFailingTx
          topTxLegacy
          [ injectFailure $
              ValueNotConservedInLegacy
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
              ValueNotConservedInLegacy
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

    describe "fixup function for balancing subtransactions" $ do
      it "top-only balanced - normal mode" $ do
        amounts <- genTopOnlyBalancedAmounts
        topTx <- mkTopTx amounts
        balanced <- balanceSubTransactions topTx
        withFixup noBalanceFixup $ submitTx_ balanced

      it "top-only balanced - legacy mode" $ do
        amounts <- genTopOnlyBalancedAmounts
        topTx <- mkTopTx amounts
        topTxLegacy <- mkTopTxLegacyMode amounts topTx
        balanced <- balanceSubTransactions topTxLegacy
        withFixup noBalanceFixup $ submitTx_ balanced

      it "balanced on both levels keeps it balanced" $ do
        amounts <- genFullyBalancedAmounts
        topTx <- mkTopTx amounts
        balanced <- balanceSubTransactions topTx
        withFixup noBalanceFixup $ submitTx_ balanced
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
    expectProduced :: Tx TopTx era -> Value era -> ImpTestM era ()
    expectProduced tx expected = do
      pp <- getsPParams id
      pState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certPStateL
      produced pp pState (tx ^. bodyTxL) `shouldBe` expected

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
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut addr (inject amount)]
      txInAt 0 <$> submitTx tx

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

-- A template for creating a transaction with exaclty one subtransaction,
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

assertBatchBalanced :: BatchAmounts -> BatchAmounts
assertBatchBalanced ba =
  let bb = batchBalances False ba
   in assert (bbBatchConsumed bb == bbBatchProduced bb) ba

assertTopBalanced :: BatchAmounts -> BatchAmounts
assertTopBalanced ba =
  let bb = batchBalances False ba
   in assert (bbTopConsumed bb == bbTopProduced bb) ba

assertSubBalanced :: BatchAmounts -> BatchAmounts
assertSubBalanced ba =
  let bb = batchBalances False ba
   in assert (bbSubConsumed bb == bbSubProduced bb) ba

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
