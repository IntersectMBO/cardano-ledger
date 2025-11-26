{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.CertsSpec (spec) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (EpochInterval (..), Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (ConwayCertsPredFailure (..), ConwayLedgerPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Val (Val (..))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "Withdrawals" $ do
    it "Withdrawing from an unregistered reward account" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      pv <- getsPParams @era ppProtocolVersionL

      stakeKey <- freshKeyHash
      rwdAccount <- getRewardAccountFor $ KeyHashObj stakeKey
      let
        tx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals [(rwdAccount, Coin 20)]
        notInRewardsFailure =
          ( if hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv
              then injectFailure . ConwayWithdrawalsMissingAccounts @era
              else injectFailure . WithdrawalsNotInRewardsCERTS @era
          )
            $ Withdrawals [(rwdAccount, Coin 20)]
       in
        submitBootstrapAware
          (submitTx_ tx)
          (submitFailingTx tx)
          ( FailBootstrapAndPostBootstrap $
              FailBoth
                { bootstrapFailures = [notInRewardsFailure]
                , postBootstrapFailures =
                    [ notInRewardsFailure
                    , injectFailure (ConwayWdrlNotDelegatedToDRep [stakeKey])
                    ]
                }
          )
      (registeredRwdAccount, reward, stakeKey2) <- setupRewardAccount
      void $ delegateToDRep (KeyHashObj stakeKey2) (Coin 1_000_000) DRepAlwaysNoConfidence
      let
        tx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals [(rwdAccount, zero), (registeredRwdAccount, reward)]
        notInRewardsFailure =
          ( if hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv
              then injectFailure . ConwayWithdrawalsMissingAccounts @era
              else injectFailure . WithdrawalsNotInRewardsCERTS @era
          )
            $ Withdrawals [(rwdAccount, zero)]
       in
        submitBootstrapAware
          (submitTx_ tx)
          (submitFailingTx tx)
          ( FailBootstrapAndPostBootstrap $
              FailBoth
                { bootstrapFailures = [notInRewardsFailure]
                , postBootstrapFailures =
                    [ notInRewardsFailure
                    , injectFailure (ConwayWdrlNotDelegatedToDRep [stakeKey])
                    ]
                }
          )

    it "Withdrawing the wrong amount" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      pv <- getsPParams @era ppProtocolVersionL

      (rwdAccount1, reward1, stakeKey1) <- setupRewardAccount
      (rwdAccount2, reward2, stakeKey2) <- setupRewardAccount
      void $ delegateToDRep (KeyHashObj stakeKey1) (Coin 1_000_000) DRepAlwaysAbstain
      void $ delegateToDRep (KeyHashObj stakeKey2) (Coin 1_000_000) DRepAlwaysAbstain
      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [ (rwdAccount1, reward1 <+> Coin 1)
                  , (rwdAccount2, reward2)
                  ]
        )
        [ if hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv
            then
              injectFailure $
                ConwayIncompleteWithdrawals @era [(raCredential rwdAccount1, Mismatch (reward1 <+> Coin 1) reward1)]
            else
              injectFailure . WithdrawalsNotInRewardsCERTS @era $ Withdrawals [(rwdAccount1, reward1 <+> Coin 1)]
        ]

      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(rwdAccount1, zero)]
        )
        [ if hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv
            then
              injectFailure . ConwayIncompleteWithdrawals @era $ [(raCredential rwdAccount1, Mismatch zero reward1)]
            else injectFailure . WithdrawalsNotInRewardsCERTS @era $ Withdrawals [(rwdAccount1, zero)]
        ]
  where
    setupRewardAccount = do
      kh <- freshKeyHash
      let cred = KeyHashObj kh
      ra <- registerStakeCredential cred
      submitAndExpireProposalToMakeReward cred
      b <- getBalance cred
      pure (ra, b, kh)
