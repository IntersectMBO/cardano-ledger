{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Ledger.Shelley.Imp.DelegSpec (
  shelleyEraSpecificSpec,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.State
import Lens.Micro
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

shelleyEraSpecificSpec ::
  forall era.
  ( ShelleyEraImp era
  , ShelleyEraTxCert era
  , InjectRuleFailure "LEDGER" ShelleyDelegPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyPoolPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
shelleyEraSpecificSpec =
  describe "Delegate stake" $ do
    it "Delegate registered stake credentials to registered pool" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [regTxCert]

      poolKh <- freshKeyHash
      registerPool poolKh

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegStakeTxCert cred poolKh]

      expectDelegatedToPool cred poolKh

    it "Register and delegate in the same transaction" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      poolKh <- freshKeyHash
      registerPool poolKh
      freshKeyHash >>= \kh -> do
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [ regTxCert
                 , DelegStakeTxCert cred poolKh
                 ]
        expectDelegatedToPool (KeyHashObj kh) poolKh

    it "Delegate unregistered stake credentials" $ do
      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [DelegStakeTxCert cred poolKh]
        )
        [injectFailure $ StakeKeyNotRegisteredDELEG cred]

      expectNotRegistered cred

    it "Delegate to unregistered pool" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [regTxCert]

      poolKh <- freshKeyHash
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [DelegStakeTxCert cred poolKh]
        )
        [injectFailure $ StakePoolNotRegisteredOnKeyPOOL poolKh]

      expectNotDelegatedToPool cred

    it "Delegate already delegated credentials" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      poolKh <- freshKeyHash
      registerPool poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ regTxCert
               , DelegStakeTxCert cred poolKh
               ]
      expectDelegatedToPool cred poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegStakeTxCert cred poolKh]
      expectDelegatedToPool cred poolKh

      poolKh1 <- freshKeyHash
      registerPool poolKh1
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegStakeTxCert cred poolKh1]
      expectDelegatedToPool cred poolKh1

      poolKh2 <- freshKeyHash
      registerPool poolKh2
      poolKh3 <- freshKeyHash
      registerPool poolKh3

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ DelegStakeTxCert cred poolKh2
               , DelegStakeTxCert cred poolKh3
               ]

      expectDelegatedToPool cred poolKh3

    it "Delegate and unregister" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      unRegTxCert <- genUnRegTxCert cred
      poolKh <- freshKeyHash
      registerPool poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ regTxCert
               , DelegStakeTxCert cred poolKh
               , unRegTxCert
               ]
      expectNotRegistered cred

expectNotRegistered :: (HasCallStack, ShelleyEraImp era) => Credential 'Staking -> ImpTestM era ()
expectNotRegistered cred = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  impAnn (show cred <> " expected to not be in Accounts") $ do
    expectNothingExpr $ lookupAccountState cred accounts

expectNotDelegatedToPool ::
  (HasCallStack, ShelleyEraImp era) => Credential 'Staking -> ImpTestM era ()
expectNotDelegatedToPool cred = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  impAnn (show cred <> " expected to not have delegated to a stake pool") $ do
    accountState <- expectJust $ lookupAccountState cred accounts
    expectNothingExpr (accountState ^. stakePoolDelegationAccountStateL)

expectDelegatedToPool ::
  (HasCallStack, ShelleyEraImp era) => Credential 'Staking -> KeyHash 'StakePool -> ImpTestM era ()
expectDelegatedToPool cred poolKh = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  impAnn (show cred <> " expected to have delegated to " <> show poolKh) $ do
    accountState <- expectJust $ lookupAccountState cred accounts
    accountState ^. stakePoolDelegationAccountStateL `shouldBe` Just poolKh
