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
    let subAmount = reward <-> Coin 1
    let tx =
          mkTxWithBatchWithdrawals
            (Withdrawals [(account, reward)])
            [Withdrawals [(account, subAmount)]]
    submitFailingTx
      tx
      [ injectFailure $
          ExceededBalancesInWithdrawals @era $
            fromJust $
              NE.fromMap [(account, Mismatch (reward <+> subAmount) reward)]
      ]
    -- legacy mode
    legacyTx <- switchToLegacyMode tx
    submitFailingTx
      legacyTx
      [ injectFailure . IncompleteWithdrawals @era $
          NE.singleton account $
            Mismatch reward (reward <-> subAmount)
      ]

  it "Sub-transaction alone over-draws account" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    (account, reward, _) <- setupAccountAddress

    let moreThanReward = reward <+> Coin 1
    let tx =
          mkTxWithBatchWithdrawals
            (Withdrawals [(account, reward)])
            [Withdrawals [(account, moreThanReward)]]
    submitFailingTx
      tx
      [ injectFailure $
          ExceededBalancesInWithdrawals @era $
            fromJust $
              NE.fromMap [(account, Mismatch (reward <+> moreThanReward) reward)]
      ]
    legacyTx <- switchToLegacyMode tx
    submitFailingTx
      legacyTx
      [ injectFailure . IncompleteWithdrawals @era $
          NE.singleton account $
            Mismatch reward zero
      , injectFailure $
          ExceededBalancesInWithdrawals @era $
            fromJust $
              NE.fromMap [(account, Mismatch moreThanReward reward)]
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
