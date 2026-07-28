{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.EntitiesSpec (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraUtxoPredFailure (..),
  EntitiesPredFailure (..),
 )
import Cardano.Ledger.Plutus
import Cardano.Ledger.Val (Val (..))
import qualified Data.Foldable as Foldable
import Data.List ((\\))
import qualified Data.Map.NonEmpty as NE
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "ENTITIES" $ do
  it "Withdrawing from an unregistered staking address" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2

    stakeKey <- freshKeyHash
    accountAddress <- getAccountAddressFor $ KeyHashObj stakeKey
    let
      tx =
        mkBasicTx $
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals [(accountAddress, Coin 20)]
    submitFailingTx
      tx
      [ injectFailure $
          WithdrawalsExceedAccountBalance @era $
            NE.singleton accountAddress $
              Mismatch (Coin 20) mempty
      , injectFailure . WithdrawalsMissingAccounts @era $
          Withdrawals [(accountAddress, Coin 20)]
      ]
    (registeredAccountAddress, reward, stakeKey2) <- setupAccountAddress
    void $ delegateToDRep (KeyHashObj stakeKey2) (Coin 1_000_000) DRepAlwaysNoConfidence
    let
      tx2 =
        mkBasicTx $
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals [(accountAddress, zero), (registeredAccountAddress, reward)]
    submitFailingTx
      tx2
      [ injectFailure . WithdrawalsMissingAccounts @era $
          Withdrawals [(accountAddress, zero)]
      ]

  it "Withdrawing the wrong amount" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2

    (accountAddress1, reward1, stakeKey1) <- setupAccountAddress
    (accountAddress2, reward2, stakeKey2) <- setupAccountAddress
    void $ delegateToDRep (KeyHashObj stakeKey1) (Coin 1_000_000) DRepAlwaysAbstain
    void $ delegateToDRep (KeyHashObj stakeKey2) (Coin 1_000_000) DRepAlwaysAbstain
    submitFailingTx
      ( mkBasicTx $
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals
                [ (accountAddress1, reward1 <+> Coin 1)
                , (accountAddress2, reward2)
                ]
      )
      [ injectFailure $
          WithdrawalsExceedAccountBalance @era $
            NE.singleton accountAddress1 $
              Mismatch (reward1 <+> Coin 1) reward1
      , injectFailure $
          WithdrawalAmountsExceedAccountBalances @era $
            NE.singleton accountAddress1 $
              Mismatch (reward1 <+> Coin 1) reward1
      ]

    -- in legacy mode, we produce `IncompleteWithdrawals` failure
    txIn <- produceScript . hashPlutusScript $ alwaysSucceedsWithDatum SPlutusV2
    submitFailingTx
      ( mkBasicTx $
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals
                [(accountAddress1, zero)]
            & inputsTxBodyL .~ [txIn]
      )
      [ injectFailure . IncompleteWithdrawals @era $
          NE.singleton accountAddress1 $
            Mismatch zero reward1
      ]

    submitTx_ $
      mkBasicTx $
        mkBasicTxBody
          & withdrawalsTxBodyL
            .~ Withdrawals
              [(accountAddress1, zero)]

  it "Legacy top over-drains after a sub-tx already withdrew from the same account" $ do
    (accountAddress, reward, _stakeKey) <- setupAccountAddress
    txIn <- produceScript . hashPlutusScript $ alwaysSucceedsWithDatum SPlutusV2
    let
      partialWdrlSubTx :: Tx SubTx era
      partialWdrlSubTx =
        mkBasicTx $
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals [(accountAddress, Coin 1)]
      tx =
        mkBasicTx $
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals [(accountAddress, reward)]
            & inputsTxBodyL
              .~ [txIn]
            & subTransactionsTxBodyL
              .~ [partialWdrlSubTx]
    expectPredicateFailures
      tx
      [ injectFailure . IncompleteWithdrawals @era $
          NE.singleton accountAddress $
            Mismatch reward (reward <-> Coin 1)
      ]

  it "Legacy top withdraws from an unregistered account" $ do
    stakeKey <- freshKeyHash
    accountAddress <- getAccountAddressFor $ KeyHashObj stakeKey
    txIn <- produceScript . hashPlutusScript $ alwaysSucceedsWithDatum SPlutusV2
    let
      tx =
        mkBasicTx $
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals [(accountAddress, Coin 20)]
            & inputsTxBodyL
              .~ [txIn]
    expectPredicateFailures
      tx
      [ injectFailure . WithdrawalsMissingAccounts @era $ Withdrawals [(accountAddress, Coin 20)]
      ]

  -- needs Imp sub-tx wits fixup + nested inject plumbing to assert
  -- SubEntitiesFailure (SubWithdrawalsMissingAccounts _) from SUBENTITIES
  xit "Sub-tx withdraws from an account unregistered by a prior sub-tx" $ do
    stakingCred <- KeyHashObj <$> freshKeyHash
    accountAddress <- registerStakeCredential stakingCred
    keyDeposit <- getsPParams ppKeyDepositL
    let
      unregSubTx :: Tx SubTx era
      unregSubTx =
        mkBasicTx $
          mkBasicTxBody
            & certsTxBodyL
              .~ [UnRegDepositTxCert stakingCred keyDeposit]
      wdrlSubTx :: Tx SubTx era
      wdrlSubTx =
        mkBasicTx $
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals [(accountAddress, zero)]
      tx =
        mkBasicTx $
          mkBasicTxBody
            & subTransactionsTxBodyL
              .~ [unregSubTx, wdrlSubTx]
    _ <- trySubmitTx tx
    pure ()

  -- Expected success: DD target check uses post-CERTS threaded state
  -- needs Imp fixup
  xit "Cross-sub register-then-direct-deposit: sub_0 registers C, sub_1 DDs into C" $ do
    stakingCred <- KeyHashObj <$> freshKeyHash
    accountAddress <- getAccountAddressFor stakingCred
    keyDeposit <- getsPParams ppKeyDepositL
    let
      regSubTx :: Tx SubTx era
      regSubTx =
        mkBasicTx $
          mkBasicTxBody
            & certsTxBodyL
              .~ [RegDepositTxCert stakingCred keyDeposit]
      ddSubTx :: Tx SubTx era
      ddSubTx =
        mkBasicTx $
          mkBasicTxBody
            & directDepositsTxBodyL
              .~ DirectDeposits [(accountAddress, Coin 50)]
      tx =
        mkBasicTx $
          mkBasicTxBody
            & subTransactionsTxBodyL
              .~ [regSubTx, ddSubTx]
    submitTx_ tx

  -- Expected success: within one sub, CERTS run before DD check
  -- needs Imp fixup
  xit "Intra-sub register-then-direct-deposit: one sub-tx registers C and DDs into C" $ do
    stakingCred <- KeyHashObj <$> freshKeyHash
    accountAddress <- getAccountAddressFor stakingCred
    keyDeposit <- getsPParams ppKeyDepositL
    let
      regAndDDSubTx :: Tx SubTx era
      regAndDDSubTx =
        mkBasicTx $
          mkBasicTxBody
            & certsTxBodyL
              .~ [RegDepositTxCert stakingCred keyDeposit]
            & directDepositsTxBodyL
              .~ DirectDeposits [(accountAddress, Coin 50)]
      tx =
        mkBasicTx $
          mkBasicTxBody
            & subTransactionsTxBodyL
              .~ [regAndDDSubTx]
    submitTx_ tx
  where
    setupAccountAddress :: ImpTestM era (AccountAddress, Coin, KeyHash Staking)
    setupAccountAddress = do
      kh <- freshKeyHash
      let cred = KeyHashObj kh
      ra <- registerStakeCredential cred
      submitAndExpireProposalToMakeReward cred
      b <- getBalance cred
      pure (ra, b, kh)

    -- Poor man's `submitFailingTx` - only checking that the given predicate failures
    -- are part of all the failures returned by submitting the transaction.
    -- TOOD: to be replaced by `submitFailingTx` when the Imp fixup for nested transaction is done.
    expectPredicateFailures tx expected = do
      result <- trySubmitTx tx
      case result of
        Left (predFailures, _) ->
          (expected \\ Foldable.toList predFailures) `shouldBe` []
        Right _ -> expectationFailure "Expected submission to fail, but it succeeded"
