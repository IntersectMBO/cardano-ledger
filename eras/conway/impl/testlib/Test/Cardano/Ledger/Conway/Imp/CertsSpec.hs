{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.CertsSpec (conwayOnlySpec, spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Plutus (SLanguage (SPlutusV3), hashPlutusScript)
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.NonEmpty as NEM
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsNoDatum)

conwayOnlySpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
conwayOnlySpec = describe "CERTS" $ do
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
        notInRewardsFailure =
          (injectFailure . ConwayWithdrawalsMissingAccounts @era) $
            Withdrawals [(accountAddress, Coin 20)]
       in
        submitBootstrapAware
          (submitTx_ tx)
          (submitFailingSubsetTx tx)
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
          (injectFailure . ConwayWithdrawalsMissingAccounts @era) $
            Withdrawals [(accountAddress, zero)]
       in
        submitBootstrapAware
          (submitTx_ tx)
          (submitFailingSubsetTx tx)
          ( FailBootstrapAndPostBootstrap $
              FailBoth
                { bootstrapFailures = [notInRewardsFailure]
                , postBootstrapFailures =
                    [ notInRewardsFailure
                    , injectFailure (ConwayWdrlNotDelegatedToDRep [stakeKey])
                    ]
                }
          )

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "CERTS" $ do
  describe "Withdrawals" $ do
    it "Withdrawing the wrong amount" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2

      (accountAddress1, reward1, stakeKey1) <- setupAccountAddress
      (accountAddress2, reward2, stakeKey2) <- setupAccountAddress
      void $ delegateToDRep (KeyHashObj stakeKey1) (Coin 1_000_000) DRepAlwaysAbstain
      void $ delegateToDRep (KeyHashObj stakeKey2) (Coin 1_000_000) DRepAlwaysAbstain

      -- Force legacy mode by including a PV3 script in our Tx's
      txIn <- produceScript . hashPlutusScript $ alwaysSucceedsNoDatum SPlutusV3

      submitFailingSubsetTx
        ( mkBasicTx $
            mkBasicTxBody
              & inputsTxBodyL
                .~ [txIn]
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [ (accountAddress1, reward1 <+> Coin 1)
                  , (accountAddress2, reward2)
                  ]
        )
        [ injectFailure . ConwayIncompleteWithdrawals @era $
            NEM.singleton accountAddress1 $
              Mismatch (reward1 <+> Coin 1) reward1
        ]

      submitFailingSubsetTx
        ( mkBasicTx $
            mkBasicTxBody
              & inputsTxBodyL
                .~ [txIn]
              & withdrawalsTxBodyL
                .~ Withdrawals
                  [(accountAddress1, zero)]
        )
        [ injectFailure . ConwayIncompleteWithdrawals @era $
            NEM.singleton accountAddress1 $
              Mismatch zero reward1
        ]

setupAccountAddress ::
  forall era.
  ConwayEraImp era =>
  ImpM (LedgerSpec era) (AccountAddress, Coin, KeyHash Staking)
setupAccountAddress = do
  kh <- freshKeyHash
  let cred = KeyHashObj kh
  ra <- registerStakeCredential cred
  submitAndExpireProposalToMakeReward cred
  b <- getBalance cred
  pure (ra, b, kh)
