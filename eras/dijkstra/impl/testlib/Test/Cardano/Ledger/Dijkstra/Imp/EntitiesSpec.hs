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
import qualified Data.Foldable as Foldable
import Data.List ((\\))
import qualified Data.Map.NonEmpty as NE
import qualified Data.Set.NonEmpty as NES
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
      , injectFailure . MissingAccountsInWithdrawals @era $
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
      [ injectFailure . MissingAccountsInWithdrawals @era $
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

  it "Rejects withdrawals and direct deposits with wrong network id" $ do
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

    submitFailingTxIncluding
      (mkBasicTx $ txBody & subTransactionsTxBodyL .~ [mkBasicTx txBody])
      [ injectFailure . SubWrongNetworkInWithdrawals @era Testnet $ NES.singleton wrongNetworkAccount
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

    -- Poor man's `submitFailingTx` - only checking that the given predicate failures
    -- are part of all the failures returned by submitting the transaction.
    -- TOOD: to be replaced by `submitFailingTx` when the Imp fixup for nested transaction is done.
    submitFailingTxIncluding tx expected = do
      result <- trySubmitTx tx
      case result of
        Left (predFailures, _) ->
          (expected \\ Foldable.toList predFailures) `shouldBe` []
        Right _ -> expectationFailure "Expected submission to fail, but it succeeded"
