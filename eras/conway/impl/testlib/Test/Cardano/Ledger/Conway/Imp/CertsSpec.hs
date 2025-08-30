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

import Cardano.Ledger.BaseTypes (EpochInterval (..))
import Cardano.Ledger.Coin (Coin (..))
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
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayCertsPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "Withdrawals" $ do
    it "Withdrawing from an unregistered reward account" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2

      stakeKey <- freshKeyHash
      rwdAccount <- getRewardAccountFor $ KeyHashObj stakeKey
      let
        tx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals [(rwdAccount, Coin 20)]
        notInRewardsFailure =
          injectFailure $ WithdrawalsNotInRewardsCERTS $ Withdrawals [(rwdAccount, Coin 20)]
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
      (registeredRwdAccount, reward, _stakeKey2) <-
        setupRewardAccount (Coin 1_000_000) DRepAlwaysNoConfidence
      let
        tx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals [(rwdAccount, zero), (registeredRwdAccount, reward)]
        notInRewardsFailure =
          injectFailure $ WithdrawalsNotInRewardsCERTS $ Withdrawals [(rwdAccount, zero)]
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

      (rwdAccount1, reward1, _stakeKey1) <- setupRewardAccount (Coin 1_000_000) DRepAlwaysAbstain
      (rwdAccount2, reward2, _stakeKey2) <- setupRewardAccount (Coin 1_000_000) DRepAlwaysAbstain
      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [ (rwdAccount1, reward1 <+> Coin 1)
                  , (rwdAccount2, reward2)
                  ]
        )
        [ injectFailure $
            WithdrawalsNotInRewardsCERTS $
              Withdrawals [(rwdAccount1, reward1 <+> Coin 1)]
        ]

      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(rwdAccount1, zero)]
        )
        [injectFailure $ WithdrawalsNotInRewardsCERTS $ Withdrawals [(rwdAccount1, zero)]]
  where
    setupRewardAccount stake dRep = do
      kh <- freshKeyHash
      let cred = KeyHashObj kh
      void $ regDelegToDRep cred stake dRep
      ra <- getRewardAccountFor cred
      submitAndExpireProposalToMakeReward cred
      b <- getBalance cred
      pure (ra, b, kh)
