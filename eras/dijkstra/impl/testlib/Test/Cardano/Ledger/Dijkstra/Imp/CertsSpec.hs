{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.CertsSpec (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraLedgerPredFailure (..), DijkstraUtxoPredFailure (..))
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.NonEmpty as NE
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "Withdrawals" $ do
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
        , injectFailure . DijkstraWithdrawalsMissingAccounts @era $
            Withdrawals [(accountAddress, Coin 20)]
        , injectFailure (DijkstraWdrlNotDelegatedToDRep [stakeKey])
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
        [ injectFailure . DijkstraWithdrawalsMissingAccounts @era $
            Withdrawals [(accountAddress, zero)]
        , injectFailure (DijkstraWdrlNotDelegatedToDRep [stakeKey])
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
            DijkstraIncompleteWithdrawals @era $
              NE.singleton accountAddress1 $
                Mismatch (reward1 <+> Coin 1) reward1
        ]

      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(accountAddress1, zero)]
        )
        [ injectFailure . DijkstraIncompleteWithdrawals @era $
            NE.singleton accountAddress1 $
              Mismatch zero reward1
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
