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

import Cardano.Ledger.BaseTypes (EpochInterval (..), Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (ConwayCertsPredFailure (..), ConwayLedgerPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.NonEmpty as NEM
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
    it "Withdrawing from an unregistered staking address" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      pv <- getsPParams @era ppProtocolVersionL

      stakeKey <- freshKeyHash
      accountAddress <- getAccountAddressFor $ KeyHashObj stakeKey
      let
        tx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals [(accountAddress, Coin 20)]
        notInRewardsFailure =
          ( if hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv
              then injectFailure . ConwayWithdrawalsMissingAccounts @era
              else injectFailure . WithdrawalsNotInRewardsCERTS @era
          )
            $ Withdrawals [(accountAddress, Coin 20)]
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
      (registeredAccountAddress, reward, stakeKey2) <- setupAccountAddress
      void $ delegateToDRep (KeyHashObj stakeKey2) (Coin 1_000_000) DRepAlwaysNoConfidence
      let
        tx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals [(accountAddress, zero), (registeredAccountAddress, reward)]
        notInRewardsFailure =
          ( if hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv
              then injectFailure . ConwayWithdrawalsMissingAccounts @era
              else injectFailure . WithdrawalsNotInRewardsCERTS @era
          )
            $ Withdrawals [(accountAddress, zero)]
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
        [ if hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv
            then
              injectFailure $
                ConwayIncompleteWithdrawals @era $
                  NEM.singleton accountAddress1 $
                    Mismatch (reward1 <+> Coin 1) reward1
            else
              injectFailure . WithdrawalsNotInRewardsCERTS @era $
                Withdrawals [(accountAddress1, reward1 <+> Coin 1)]
        ]

      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(accountAddress1, zero)]
        )
        [ if hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule pv
            then
              injectFailure . ConwayIncompleteWithdrawals @era $
                NEM.singleton accountAddress1 $
                  Mismatch zero reward1
            else injectFailure . WithdrawalsNotInRewardsCERTS @era $ Withdrawals [(accountAddress1, zero)]
        ]
  where
    setupAccountAddress = do
      kh <- freshKeyHash
      let cred = KeyHashObj kh
      ra <- registerStakeCredential cred
      submitAndExpireProposalToMakeReward cred
      b <- getBalance cred
      pure (ra, b, kh)
