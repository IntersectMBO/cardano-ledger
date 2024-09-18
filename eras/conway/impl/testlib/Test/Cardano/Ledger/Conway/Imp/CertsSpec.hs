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
import Cardano.Ledger.Conway.Rules (ConwayCertsPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Val (Val (..))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayCertsPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = do
  describe "Withdrawals" $ do
    it "Withdrawing from an unregistered reward account" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2

      rwdAccount <- KeyHashObj <$> freshKeyHash >>= getRewardAccountFor
      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(rwdAccount, Coin 20)]
        )
        [injectFailure $ WithdrawalsNotInRewardsCERTS [(rwdAccount, Coin 20)]]

      (registeredRwdAccount, reward) <- setupRewardAccount
      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(rwdAccount, zero), (registeredRwdAccount, reward)]
        )
        [injectFailure $ WithdrawalsNotInRewardsCERTS [(rwdAccount, zero)]]

    it "Withdrawing the wrong amount" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2

      (rwdAccount1, reward1) <- setupRewardAccount
      (rwdAccount2, reward2) <- setupRewardAccount
      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [ (rwdAccount1, reward1 <+> Coin 1)
                  , (rwdAccount2, reward2)
                  ]
        )
        [injectFailure $ WithdrawalsNotInRewardsCERTS [(rwdAccount1, reward1 <+> Coin 1)]]

      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(rwdAccount1, zero)]
        )
        [injectFailure $ WithdrawalsNotInRewardsCERTS [(rwdAccount1, zero)]]
  where
    setupRewardAccount = do
      cred <- KeyHashObj <$> freshKeyHash
      ra <- registerStakeCredential cred
      submitAndExpireProposalToMakeReward cred
      rw <- lookupReward cred
      pure (ra, rw)
