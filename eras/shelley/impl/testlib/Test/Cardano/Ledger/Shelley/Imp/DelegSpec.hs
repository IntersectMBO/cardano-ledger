{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Imp.DelegSpec (
  shelleyEraSpecificSpec,
  spec,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.State (ShelleyEraAccounts)
import Cardano.Ledger.Shelley.Transition (shelleyRegisterInitialAccounts)
import Cardano.Ledger.State (accountsL, accountsMapL, stakePoolDelegationAccountStateL)
import qualified Data.ListMap as LM
import qualified Data.Map.Strict as Map
import Lens.Micro
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.ImpTest

shelleyEraSpecificSpec ::
  ( ShelleyEraImp era
  , ShelleyEraAccounts era
  , InjectRuleFailure "LEDGER" ShelleyDelegsPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
shelleyEraSpecificSpec = do
  it "Twice the same certificate in the same transaction" $ do
    freshKeyHash >>= \kh -> do
      regTxCert <- genRegTxCert (KeyHashObj kh)
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [regTxCert, regTxCert]
        )
        [injectFailure $ StakeKeyAlreadyRegisteredDELEG (KeyHashObj kh)]
      expectStakeCredNotRegistered (KeyHashObj kh)

  it "Delegate to unregistered pool" $ do
    cred <- KeyHashObj <$> freshKeyHash
    regTxCert <- genRegTxCert cred
    submitTx_ $
      mkBasicTx mkBasicTxBody
        & bodyTxL . certsTxBodyL .~ [regTxCert]

    poolKh <- freshKeyHash
    submitFailingTx
      ( mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [delegStakeTxCert cred poolKh]
      )
      [injectFailure $ DelegateeNotRegisteredDELEG poolKh]
    expectNotDelegatedToAnyPool cred

  it "Deregistering returns the deposit" $ do
    let keyDeposit = Coin 2
    -- This is paid out as the reward
    let poolDeposit = Coin 3
    modifyPParams $ \pp ->
      pp
        & ppKeyDepositL .~ keyDeposit
        & ppPoolDepositL .~ poolDeposit
    stakeCred <- KeyHashObj <$> freshKeyHash
    rewardAccount <- getRewardAccountFor stakeCred
    otherStakeCred <- KeyHashObj <$> freshKeyHash
    otherRewardAccount <- getRewardAccountFor otherStakeCred
    khStakePool <- freshKeyHash
    registerPool khStakePool
    stakeCredRegTxCert <- genRegTxCert stakeCred
    otherStakeCredRegTxCert <- genRegTxCert otherStakeCred
    submitTx_ . mkBasicTx $
      mkBasicTxBody
        & certsTxBodyL
          .~ [ stakeCredRegTxCert
             , delegStakeTxCert stakeCred khStakePool
             , otherStakeCredRegTxCert
             , delegStakeTxCert otherStakeCred khStakePool
             ]
    expectRegisteredRewardAddress rewardAccount
    expectRegisteredRewardAddress otherRewardAccount
    registerAndRetirePoolToMakeReward otherStakeCred

    getBalance otherStakeCred `shouldReturn` poolDeposit
    unRegTxCert <- genUnRegTxCert stakeCred

    submitTx_ . mkBasicTx $
      mkBasicTxBody
        & certsTxBodyL .~ [unRegTxCert]
        & withdrawalsTxBodyL
          .~ Withdrawals
            ( Map.fromList
                [ (rewardAccount, Coin 0)
                , (otherRewardAccount, poolDeposit)
                ]
            )
    getBalance otherStakeCred `shouldReturn` Coin 0
    expectNotRegisteredRewardAddress rewardAccount

  it "Transition creates the delegations correctly" $ do
    pool1 <- freshKeyHash >>= \kh -> kh <$ registerPool kh
    pool2 <- freshKeyHash >>= \kh -> kh <$ registerPool kh
    pool3 <- freshKeyHash >>= \kh -> kh <$ registerPool kh
    poolParams <- freshKeyHash >>= \kh -> registerRewardAccount >>= freshPoolParams kh
    deleg1 <- freshKeyHash >>= \kh -> kh <$ registerStakeCredential (KeyHashObj kh)
    deleg2 <- freshKeyHash >>= \kh -> kh <$ registerStakeCredential (KeyHashObj kh)
    deleg3 <- freshKeyHash >>= \kh -> kh <$ registerStakeCredential (KeyHashObj kh)
    nes <- getsNES id
    let sgs =
          ShelleyGenesisStaking
            { sgsPools = LM.ListMap [(pool1, poolParams), (pool2, poolParams), (pool3, poolParams)]
            , sgsStake = LM.ListMap [(deleg1, pool1), (deleg2, pool1), (deleg3, pool2)]
            }
    let updatedNES = shelleyRegisterInitialAccounts sgs nes
    delegateStake (KeyHashObj deleg1) pool1
    delegateStake (KeyHashObj deleg2) pool1
    delegateStake (KeyHashObj deleg3) pool2
    getPoolsState <$> (getsNES id) `shouldReturn` getPoolsState updatedNES
    getDelegs deleg1 updatedNES `shouldReturn` Just pool1
    getDelegs deleg2 updatedNES `shouldReturn` Just pool1
    getDelegs deleg3 updatedNES `shouldReturn` Just pool2
  where
    getDelegs kh nes = do
      let accounts = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      pure $ Map.lookup (KeyHashObj kh) accounts >>= (^. stakePoolDelegationAccountStateL)
    getPoolsState nes = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL

spec ::
  ShelleyEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "Register stake credential" $ do
    it "With correct deposit or without any deposit" $ do
      cred <- KeyHashObj <$> freshKeyHash
      -- NOTE: This will always generate certs with deposits post-Conway
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [regTxCert]
      expectStakeCredRegistered cred

    it "When already already registered" $ do
      cred <- ScriptHashObj <$> impAddNativeScript (RequireAllOf [])
      regTxCert <- genRegTxCert cred
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [regTxCert]
      submitTx_ tx
      submitFailingTx
        tx
        [ injectFailure $ StakeKeyAlreadyRegisteredDELEG cred
        ]
      expectStakeCredRegistered cred

  describe "Unregister stake credentials" $ do
    it "When registered" $ do
      cred <- ScriptHashObj <$> impAddNativeScript (RequireAllOf [])
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [regTxCert]
      expectStakeCredRegistered cred

      unRegTxCert <- genUnRegTxCert cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [unRegTxCert]
      expectStakeCredNotRegistered cred

    it "When not registered" $ do
      freshKeyHash >>= \kh -> do
        unRegTxCert <- genUnRegTxCert (KeyHashObj kh)
        submitFailingTx
          ( mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [unRegTxCert]
          )
          [injectFailure $ StakeKeyNotRegisteredDELEG (KeyHashObj kh)]

    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/917
    -- impacts `registerAndRetirePoolToMakeReward`
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "With non-zero reward balance" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [regTxCert]

      registerAndRetirePoolToMakeReward cred

      balance <- getBalance cred
      unRegTxCert <- genUnRegTxCert cred
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ [unRegTxCert]
        )
        [injectFailure $ StakeKeyNonZeroAccountBalanceDELEG balance]
      expectStakeCredRegistered cred

    it "Register and unregister in the same transaction" $ do
      freshKeyHash >>= \kh -> do
        regTxCert <- genRegTxCert (KeyHashObj kh)
        unRegTxCert <- genUnRegTxCert (KeyHashObj kh)
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ [regTxCert, unRegTxCert]
        expectStakeCredNotRegistered (KeyHashObj kh)

  describe "Delegate stake" $ do
    it "Delegate registered stake credentials to registered pool" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [regTxCert]

      poolKh <- freshKeyHash
      registerPool poolKh

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [delegStakeTxCert cred poolKh]
      expectDelegatedToPool cred poolKh

    it "Register and delegate in the same transaction" $ do
      poolKh <- freshKeyHash
      registerPool poolKh
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [regTxCert, delegStakeTxCert cred poolKh]
      expectDelegatedToPool cred poolKh

    it "Delegate unregistered stake credentials" $ do
      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      pv <- getProtVer
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [delegStakeTxCert cred poolKh]
        )
        [ injectFailure $
            if pvMajor pv < natVersion @9
              then StakeDelegationImpossibleDELEG cred
              else StakeKeyNotRegisteredDELEG cred
        ]
      expectStakeCredNotRegistered cred
      expectNotDelegatedToAnyPool cred

    it "Delegate already delegated credentials" $ do
      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      regTxCert <- genRegTxCert cred
      let delegTxCert = delegStakeTxCert cred poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [regTxCert, delegTxCert]
      expectDelegatedToPool cred poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [delegTxCert]
      expectDelegatedToPool cred poolKh

      poolKh1 <- freshKeyHash
      registerPool poolKh1
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [delegStakeTxCert cred poolKh1]
      expectDelegatedToPool cred poolKh1
      expectNotDelegatedToPool cred poolKh

      poolKh2 <- freshKeyHash
      registerPool poolKh2
      poolKh3 <- freshKeyHash
      registerPool poolKh3

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ delegStakeTxCert cred poolKh2
               , delegStakeTxCert cred poolKh3
               ]

      expectDelegatedToPool cred poolKh3
      expectNotDelegatedToPool cred poolKh2
      expectNotDelegatedToPool cred poolKh

    it "Delegate and unregister" $ do
      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      regTxCert <- genRegTxCert cred
      unRegTxCert <- genUnRegTxCert cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [regTxCert, delegStakeTxCert cred poolKh, unRegTxCert]
      expectStakeCredNotRegistered cred
      expectNotDelegatedToAnyPool cred
