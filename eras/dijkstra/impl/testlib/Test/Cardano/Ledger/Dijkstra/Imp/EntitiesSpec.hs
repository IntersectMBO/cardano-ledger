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
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraUtxoPredFailure (..),
  EntitiesPredFailure (..),
  SubEntitiesPredFailure (..),
 )
import Cardano.Ledger.Plutus
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "ENTITIES" $ do
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
      [ injectFailure $
          WithdrawalsExceedAccountBalance @era $
            NE.singleton account1 $
              Mismatch amountX mempty
      , injectFailure . MissingAccountsInWithdrawals @era $
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
      [ injectFailure $
          WithdrawalsExceedAccountBalance @era $
            fromJust $
              NE.fromMap $
                Map.fromList
                  [ (account1, Mismatch (amountX <> amountX) mempty)
                  , (account3, Mismatch amountY mempty)
                  ]
      , injectFailure . MissingAccountsInWithdrawals @era $
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
    let
    let
      txBody :: forall l. Typeable l => TxBody l era
      txBody = mkBasicTxBody & directDepositsTxBodyL .~ DirectDeposits [(account, amountX)]
    submitFailingTx
      (mkBasicTx txBody)
      [ injectFailure . MissingAccountsInDirectDeposits @era $
          DirectDeposits [(account, amountX)]
      ]

    account2 <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    amountY <- Coin . getPositive <$> arbitrary
    amountZ <- Coin . getPositive <$> arbitrary
    let
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

  it "Withdrawals of the wrong amount" $ do
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
          ExceededBalancesInWithdrawals @era $
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
      , injectFailure . MissingAccountsInWithdrawals @era $ Withdrawals [(wrongNetworkAccount, mempty)]
      ]

    submitFailingTx
      (mkBasicTx $ txBody & subTransactionsTxBodyL .~ [mkBasicTx txBody])
      [ injectFailure . WrongNetworkInWithdrawals @era Testnet $ NES.singleton wrongNetworkAccount
      , injectFailure . WrongNetworkInDirectDeposits @era Testnet $ NES.singleton wrongNetworkAccount
      , injectFailure . MissingAccountsInWithdrawals @era $ Withdrawals [(wrongNetworkAccount, mempty)]
      , injectFailure . SubWrongNetworkInWithdrawals @era Testnet $ NES.singleton wrongNetworkAccount
      , injectFailure . SubWrongNetworkInDirectDeposits @era Testnet $ NES.singleton wrongNetworkAccount
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
