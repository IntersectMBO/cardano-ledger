{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.EntitiesSpec (spec) where

import Cardano.Base.Typeable (Typeable)
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (
  EntitiesPredFailure (..),
  SubEntitiesPredFailure (..),
 )
import Cardano.Ledger.Plutus
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Data.OMap.Strict as OMap
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "ENTITIES" $ do
  it "Batch with successful withdrawals and direct deposits" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    (acc1, reward1, kh1) <- setupAccountAddress
    (acc2, reward2, kh2) <- setupAccountAddress
    (acc3, reward3, kh3) <- setupAccountAddress

    let depositAmount = Coin 50
        partialWithdrawal = reward3 <-> Coin 10
        subDeposit =
          mkBasicTx $
            mkBasicTxBody
              & directDepositsTxBodyL .~ DirectDeposits [(acc1, depositAmount)]
        subWithdraw =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL .~ Withdrawals [(acc2, reward2)]
        topTx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL .~ Withdrawals [(acc3, partialWithdrawal)]
              & subTransactionsTxBodyL .~ [subDeposit, subWithdraw]
    submitTx_ topTx

    finalBalance1 <- getBalance (KeyHashObj kh1)
    finalBalance2 <- getBalance (KeyHashObj kh2)
    finalBalanceD <- getBalance (KeyHashObj kh3)

    finalBalance1 `shouldBe` reward1 <+> depositAmount
    finalBalance2 `shouldBe` mempty
    finalBalanceD `shouldBe` (reward3 <-> partialWithdrawal)

  it "Partial withdrawals" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2

    (account1, reward1, kh1) <- setupAccountAddress
    (account2, reward2, kh2) <- setupAccountAddress
    lessThanReward1 <- Coin <$> choose (1, unCoin reward1 - 1)
    atMostReward2 <- Coin <$> choose (1, unCoin reward2)
    let tx =
          mkTxWithBatchWithdrawals
            (Withdrawals [(account1, lessThanReward1)])
            [Withdrawals [(account2, atMostReward2)]]
    submitTx_ tx
    getBalance (KeyHashObj kh1) `shouldReturn` (reward1 <-> lessThanReward1)
    getBalance (KeyHashObj kh2) `shouldReturn` (reward2 <-> atMostReward2)

    -- restore balances, to test legacy mode
    submitTx_ $
      mkBasicTx $
        mkBasicTxBody
          & directDepositsTxBodyL .~ DirectDeposits [(account1, lessThanReward1), (account2, atMostReward2)]
    legacyTx <- switchToLegacyMode tx
    submitFailingTx
      legacyTx
      [ injectFailure . IncompleteWithdrawals @era $
          NE.singleton account1 $
            Mismatch lessThanReward1 reward1
      ]

    -- drain top withdrawal
    submitTx_
      =<< switchToLegacyMode
        ( mkTxWithBatchWithdrawals
            (Withdrawals [(account1, reward1)])
            [Withdrawals [(account2, atMostReward2)]]
        )
    getBalance (KeyHashObj kh1) `shouldReturn` zero
    getBalance (KeyHashObj kh2) `shouldReturn` (reward2 <-> atMostReward2)

  it "Withdrawals from an unregistered staking address" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2

    account1 <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    account2 <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    amountX <- Coin . getPositive <$> arbitrary
    let
      txBody :: forall l. Typeable l => TxBody l era
      txBody =
        mkBasicTxBody
          & withdrawalsTxBodyL .~ Withdrawals [(account1, amountX), (account2, zero)]
    submitFailingTx
      (mkBasicTx txBody)
      [ injectFailure . MissingAccountsInWithdrawals @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . MissingOriginalAccountsInWithdrawals @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      ]

    account3 <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    amountY <- Coin . getPositive <$> arbitrary
    let subTxOnlyWithdrawal =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL .~ Withdrawals [(account3, amountY)]
    submitFailingTx
      (mkBasicTx $ txBody & subTransactionsTxBodyL .~ [mkBasicTx txBody, subTxOnlyWithdrawal])
      [ injectFailure . MissingAccountsInWithdrawals @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . MissingOriginalAccountsInWithdrawals @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . SubMissingOriginalAccountsInWithdrawals @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . SubMissingAccountsInWithdrawals @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . SubMissingOriginalAccountsInWithdrawals @era $
          Withdrawals [(account3, amountY)]
      , injectFailure . SubMissingAccountsInWithdrawals @era $
          Withdrawals [(account3, amountY)]
      ]

  it "Direct deposits to an unregistered account" $ do
    account <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    amountX <- Coin . getPositive <$> arbitrary
    let txBody :: forall l. Typeable l => TxBody l era
        txBody = mkBasicTxBody & directDepositsTxBodyL .~ DirectDeposits [(account, amountX)]
    submitFailingTx
      (mkBasicTx txBody)
      [ injectFailure . MissingAccountsInDirectDeposits @era $
          DirectDeposits [(account, amountX)]
      ]

    account2 <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    amountY <- Coin . getPositive <$> arbitrary
    amountZ <- Coin . getPositive <$> arbitrary
    let subTxOnlyDirectDeposit =
          mkBasicTx $
            mkBasicTxBody & directDepositsTxBodyL .~ DirectDeposits [(account, amountY), (account2, amountZ)]
    submitFailingTx
      (mkBasicTx $ txBody & subTransactionsTxBodyL .~ [mkBasicTx txBody, subTxOnlyDirectDeposit])
      [ injectFailure . MissingAccountsInDirectDeposits @era $
          DirectDeposits [(account, amountX)]
      , injectFailure . SubMissingAccountsInDirectDeposits @era $
          DirectDeposits [(account, amountX)]
      , injectFailure . SubMissingAccountsInDirectDeposits @era $
          DirectDeposits [(account, amountY), (account2, amountZ)]
      ]

  it "Withdrawals and direct deposits with wrong network id" $ do
    stakeKey <- freshKeyHash
    accountAddress <- registerStakeCredential (KeyHashObj stakeKey)
    let wrongNetworkAccount = accountAddress & accountAddressNetworkIdL .~ Mainnet
    let txBody :: forall l. Typeable l => TxBody l era
        txBody =
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals [(wrongNetworkAccount, mempty)]
            & directDepositsTxBodyL
              .~ DirectDeposits [(wrongNetworkAccount, Coin 50)]

    submitFailingTx
      (mkBasicTx txBody)
      [ injectFailure . WrongNetworkInWithdrawals @era Testnet $ NES.singleton wrongNetworkAccount
      , injectFailure . WrongNetworkInDirectDeposits @era Testnet $ NES.singleton wrongNetworkAccount
      ]

    submitFailingTx
      (mkBasicTx $ txBody & subTransactionsTxBodyL .~ [mkBasicTx txBody])
      [ injectFailure . WrongNetworkInWithdrawals @era Testnet $ NES.singleton wrongNetworkAccount
      , injectFailure . WrongNetworkInDirectDeposits @era Testnet $ NES.singleton wrongNetworkAccount
      , injectFailure . SubWrongNetworkInWithdrawals @era Testnet $ NES.singleton wrongNetworkAccount
      , injectFailure . SubWrongNetworkInDirectDeposits @era Testnet $ NES.singleton wrongNetworkAccount
      ]

  it "Aggregate of top and sub withdrawals exceeds account balance" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    (account, reward, _) <- setupAccountAddress
    (topAmount, subAmount) <- genCoinPairExceeding reward
    let tx =
          mkTxWithBatchWithdrawals
            (Withdrawals [(account, topAmount)])
            [Withdrawals [(account, subAmount)]]
    submitFailingTx
      tx
      [ injectFailure $
          ExceededBalancesInWithdrawals @era $
            fromJust $
              NE.fromMap [(account, Mismatch (topAmount <+> subAmount) reward)]
      ]
    legacyTx <- switchToLegacyMode tx
    submitFailingTx
      legacyTx
      [ injectFailure . IncompleteWithdrawals @era $
          NE.singleton account $
            Mismatch topAmount (reward <-> subAmount)
      ]

  it "Aggregate of sub withdrawals exceeds account balance" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    (account, reward, _) <- setupAccountAddress
    (subAmount1, subAmount2) <- genCoinPairExceeding reward

    (subAmount1 <+> subAmount2) `shouldSatisfy` (> reward)

    let tx =
          mkTxWithBatchWithdrawals
            (Withdrawals [(account, zero)])
            [Withdrawals [(account, subAmount1)], Withdrawals [(account, subAmount2)]]
    submitFailingTx
      tx
      [ injectFailure $
          ExceededBalancesInWithdrawals @era $
            fromJust $
              NE.fromMap [(account, Mismatch (subAmount1 <+> subAmount2) reward)]
      ]
    legacyTx <- switchToLegacyMode tx
    submitFailingTx
      legacyTx
      [ injectFailure . ExceededBalancesInWithdrawals @era $
          NE.singleton account $
            Mismatch (subAmount1 <+> subAmount2) reward
      ]

  it "Individual withdrawal exceeds account balance" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    (account, reward, _) <- setupAccountAddress
    atMostReward <- Coin <$> choose (1, unCoin reward)
    moreThanReward <- (reward <+>) . Coin . getPositive <$> arbitrary

    -- A sub-transaction overdraws
    let subTxOverdraws =
          mkTxWithBatchWithdrawals
            (Withdrawals [(account, atMostReward)])
            [Withdrawals [(account, moreThanReward)]]
    submitFailingTx
      subTxOverdraws
      [ injectFailure $
          ExceededBalancesInWithdrawals @era $
            fromJust $
              NE.fromMap [(account, Mismatch (atMostReward <+> moreThanReward) reward)]
      ]
    legacySubTxOverdraws <- switchToLegacyMode subTxOverdraws
    submitFailingTx
      legacySubTxOverdraws
      [ injectFailure . IncompleteWithdrawals @era $
          NE.singleton account $
            Mismatch atMostReward zero
      , injectFailure $
          ExceededBalancesInWithdrawals @era $
            fromJust $
              NE.fromMap [(account, Mismatch moreThanReward reward)]
      ]

    -- The top transaction overdraws
    let topTxOverdraws =
          mkTxWithBatchWithdrawals
            (Withdrawals [(account, moreThanReward)])
            [Withdrawals [(account, atMostReward)]]
    submitFailingTx
      topTxOverdraws
      [ injectFailure $
          ExceededBalancesInWithdrawals @era $
            fromJust $
              NE.fromMap
                [(account, Mismatch (atMostReward <+> moreThanReward) reward)]
      ]
    legacyTopTxOverdraws <- switchToLegacyMode topTxOverdraws
    submitFailingTx
      legacyTopTxOverdraws
      [ injectFailure . IncompleteWithdrawals @era $
          NE.singleton account $
            Mismatch moreThanReward (reward <-> atMostReward)
      ]
  where
    setupAccountAddress :: ImpTestM era (AccountAddress, Coin, KeyHash Staking)
    setupAccountAddress = do
      kh <- freshKeyHash
      let cred = KeyHashObj kh
      ra <- registerStakeCredential cred
      submitAndExpireProposalToMakeReward cred
      b <- getBalance cred
      pure (ra, b, kh)
    switchToLegacyMode tx = do
      txIn <- produceScript . hashPlutusScript $ alwaysSucceedsWithDatum SPlutusV2
      pure $ tx & bodyTxL . inputsTxBodyL .~ [txIn]
    mkTxWithBatchWithdrawals :: Withdrawals -> [Withdrawals] -> Tx TopTx era
    mkTxWithBatchWithdrawals topWdrls subs =
      mkBasicTx $
        mkBasicTxBody
          & withdrawalsTxBodyL .~ topWdrls
          & subTransactionsTxBodyL .~ OMap.fromFoldable (fmap mkSubTx subs)
      where
        mkSubTx :: Withdrawals -> Tx SubTx era
        mkSubTx w = mkBasicTx (mkBasicTxBody & withdrawalsTxBodyL .~ w)
    genCoinPairExceeding (Coin maxSum) = do
      a <- choose (1, maxSum)
      b <- choose (maxSum - a + 1, maxSum)
      pure (Coin a, Coin b)
