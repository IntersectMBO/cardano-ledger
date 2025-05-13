{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.DelegSpec (
  spec,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (EpochInterval (..), StrictMaybe (..), addEpochInterval)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayDelegPredFailure (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus (
  SLanguage (..),
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UMap as UMap
import Cardano.Ledger.Val (Val (..))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (evenRedeemerNoDatum)

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayDelegPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "Register stake credential" $ do
    it "With correct deposit or without any deposit" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      freshKeyHash >>= \kh -> do
        let cred = KeyHashObj kh
        regTxCert <- genRegTxCert cred
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ [regTxCert]
        expectRegistered cred

      freshKeyHash >>= \kh -> do
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [RegDepositTxCert (KeyHashObj kh) expectedDeposit]
        expectRegistered (KeyHashObj kh)

    it "Twice the same certificate in the same transaction" $ do
      -- This is expected behavior because `certsTxBodyL` removes duplicates
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      freshKeyHash >>= \kh -> do
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [ RegDepositTxCert (KeyHashObj kh) expectedDeposit
                 , RegDepositTxCert (KeyHashObj kh) expectedDeposit
                 ]
        expectRegistered (KeyHashObj kh)

    it "When already already registered" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      let sh = hashPlutusScript $ evenRedeemerNoDatum SPlutusV3
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [RegDepositTxCert (ScriptHashObj sh) expectedDeposit]
      submitTx_ tx

      submitFailingTx
        tx
        [ injectFailure $ StakeKeyRegisteredDELEG (ScriptHashObj sh)
        ]
      expectRegistered (ScriptHashObj sh)

    it "With incorrect deposit" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      Positive n <- arbitrary
      let wrongDeposit = expectedDeposit <+> Coin n

      freshKeyHash >>= \kh -> do
        submitFailingTx
          ( mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [RegDepositTxCert (KeyHashObj kh) wrongDeposit]
          )
          [injectFailure $ IncorrectDepositDELEG wrongDeposit]
        expectNotRegistered (KeyHashObj kh)

  describe "Unregister stake credentials" $ do
    it "When registered" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      let sh = ScriptHashObj $ hashPlutusScript (evenRedeemerNoDatum SPlutusV3)
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [RegDepositTxCert sh expectedDeposit]

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [UnRegDepositTxCert sh expectedDeposit]
      expectNotRegistered sh

    it "When not registered" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      freshKeyHash >>= \kh ->
        submitFailingTx
          ( mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [UnRegDepositTxCert (KeyHashObj kh) expectedDeposit]
          )
          [ injectFailure $ StakeKeyNotRegisteredDELEG (KeyHashObj kh)
          ]

    it "With incorrect deposit" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [RegDepositTxCert cred expectedDeposit]

      Positive n <- arbitrary
      let wrongDeposit = expectedDeposit <+> Coin n

      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [UnRegDepositTxCert cred wrongDeposit]
        )
        [injectFailure $ IncorrectDepositDELEG wrongDeposit]

      expectRegistered cred

    it "With non-zero reward balance" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [RegDepositTxCert cred expectedDeposit]

      submitAndExpireProposalToMakeReward cred

      reward <- getReward cred
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ [UnRegDepositTxCert cred expectedDeposit]
        )
        [injectFailure $ StakeKeyHasNonZeroRewardAccountBalanceDELEG reward]
      expectRegistered cred

    it "Register and unregister in the same transaction" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      freshKeyHash >>= \kh -> do
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [ RegDepositTxCert (KeyHashObj kh) expectedDeposit
                 , UnRegDepositTxCert (KeyHashObj kh) expectedDeposit
                 ]
        expectNotRegistered (KeyHashObj kh)

    it "deregistering returns the deposit" $ do
      let
        keyDeposit = Coin 2
        -- This is paid out as the reward
        govActionDeposit = Coin 3
      modifyPParams $ \pp ->
        pp
          & ppKeyDepositL .~ keyDeposit
          & ppGovActionDepositL .~ govActionDeposit
      stakeCred <- KeyHashObj <$> freshKeyHash
      rewardAccount <- registerStakeCredential stakeCred
      otherStakeCred <- KeyHashObj <$> freshKeyHash
      otherRewardAccount <- registerStakeCredential otherStakeCred
      khStakePool <- freshKeyHash
      registerPool khStakePool
      submitTx_ . mkBasicTx $
        mkBasicTxBody
          & certsTxBodyL
            .~ SSeq.fromList
              [ DelegTxCert stakeCred (DelegStakeVote khStakePool DRepAlwaysAbstain)
              , DelegTxCert otherStakeCred (DelegStakeVote khStakePool DRepAlwaysAbstain)
              ]
      expectRegisteredRewardAddress rewardAccount
      expectRegisteredRewardAddress otherRewardAccount
      submitAndExpireProposalToMakeReward otherStakeCred
      getReward otherStakeCred `shouldReturn` govActionDeposit
      unRegTxCert <- genUnRegTxCert stakeCred
      submitTx_ . mkBasicTx $
        mkBasicTxBody
          & certsTxBodyL
            .~ SSeq.fromList [unRegTxCert]
          & withdrawalsTxBodyL
            .~ Withdrawals
              ( Map.fromList
                  [ (rewardAccount, Coin 0)
                  , (otherRewardAccount, govActionDeposit)
                  ]
              )
      getReward otherStakeCred `shouldReturn` Coin 0
      expectNotRegisteredRewardAddress rewardAccount

  describe "Delegate stake" $ do
    it "Delegate registered stake credentials to registered pool" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [RegDepositTxCert cred expectedDeposit]

      poolKh <- freshKeyHash
      registerPool poolKh

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegStake poolKh)]

      expectDelegatedToPool cred poolKh

    it "Register and delegate in the same transaction" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ RegDepositTxCert cred expectedDeposit
               , DelegTxCert cred (DelegStake poolKh)
               ]
      expectDelegatedToPool cred poolKh

      freshKeyHash >>= \kh -> do
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [RegDepositDelegTxCert (KeyHashObj kh) (DelegStake poolKh) expectedDeposit]
        expectDelegatedToPool (KeyHashObj kh) poolKh

      freshKeyHash >>= \kh -> do
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [ RegDepositTxCert (KeyHashObj kh) expectedDeposit
                 , DelegStakeTxCert (KeyHashObj kh) poolKh -- using the pattern from Shelley
                 ]
        expectDelegatedToPool (KeyHashObj kh) poolKh

    it "Delegate unregistered stake credentials" $ do
      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [DelegTxCert cred (DelegStake poolKh)]
        )
        [injectFailure $ StakeKeyNotRegisteredDELEG cred]

      expectNotDelegatedToPool cred

    it "Delegate to unregistered pool" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [RegDepositTxCert cred expectedDeposit]

      poolKh <- freshKeyHash
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [DelegTxCert cred (DelegStake poolKh)]
        )
        [injectFailure $ DelegateeStakePoolNotRegisteredDELEG poolKh]

      expectNotDelegatedToPool cred

    it "Delegate already delegated credentials" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ RegDepositTxCert cred expectedDeposit
               , DelegTxCert cred (DelegStake poolKh)
               ]
      expectDelegatedToPool cred poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegStake poolKh)]
      expectDelegatedToPool cred poolKh

      poolKh1 <- freshKeyHash
      registerPool poolKh1
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegStake poolKh1)]
      expectDelegatedToPool cred poolKh1

      poolKh2 <- freshKeyHash
      registerPool poolKh2
      poolKh3 <- freshKeyHash
      registerPool poolKh3

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ DelegTxCert cred (DelegStake poolKh2)
               , DelegTxCert cred (DelegStake poolKh3)
               ]

      expectDelegatedToPool cred poolKh3

    it "Delegate and unregister" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ RegDepositTxCert cred expectedDeposit
               , DelegTxCert cred (DelegStake poolKh)
               , UnRegDepositTxCert cred expectedDeposit
               ]
      expectNotRegistered cred
      expectNotDelegatedToPool cred

  describe "Delegate vote" $ do
    it "Delegate vote of registered stake credentials to registered drep" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [RegDepositTxCert cred expectedDeposit]

      (drepCred, _, _) <- setupSingleDRep 1_000_000
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegVote (DRepCredential drepCred))]

      expectDelegatedVote cred (DRepCredential drepCred)
      expectNotDelegatedToPool cred
      whenBootstrap $ do
        impAnn "Ensure DRep delegation is populated after bootstrap" $ do
          -- Clear out delegation, in order to check its repopulation from UMap.
          let deleteDelegation =
                Map.adjust (drepDelegsL %~ Set.delete cred) drepCred
          --  Drep delegation for both version 9 and 10 are populating both umap and
          --  `drepDelegs`, so manually modifying the umap in the state is the only way to
          --  test the correct repopulation of `drepDelegs`
          modifyNES $ nesEsL . epochStateRegDrepL %~ deleteDelegation
          hotCreds <- registerInitialCommittee
          (spo, _, _) <- setupPoolWithStake $ Coin 3_000_000_000
          protVer <- getProtVer
          gai <- submitGovAction $ HardForkInitiation SNothing (majorFollow protVer)
          submitYesVoteCCs_ hotCreds gai
          submitYesVote_ (StakePoolVoter spo) gai
          passNEpochs 2
          getLastEnactedHardForkInitiation `shouldReturn` SJust (GovPurposeId gai)
          expectDelegatedVote cred (DRepCredential drepCred)

    it "Delegate vote of registered stake credentials to unregistered drep" $ do
      RewardAccount _ cred <- registerRewardAccount
      drepCred <- KeyHashObj <$> freshKeyHash
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [DelegTxCert cred (DelegVote (DRepCredential drepCred))]
          inBootstrap = do
            submitTx_ tx
            expectDelegatedVote cred (DRepCredential drepCred)
            impAnn "Ensure delegation is cleaned up on the transition out of bootstrap" $ do
              hotCreds <- registerInitialCommittee
              (spo, _, _) <- setupPoolWithStake $ Coin 3_000_000_000
              protVer <- getProtVer
              gai <- submitGovAction $ HardForkInitiation SNothing (majorFollow protVer)
              submitYesVoteCCs_ hotCreds gai
              submitYesVote_ (StakePoolVoter spo) gai
              passNEpochs 2
              getLastEnactedHardForkInitiation `shouldReturn` SJust (GovPurposeId gai)
              expectNotDelegatedVote cred

          outOfBootstrap = do
            submitFailingTx tx [injectFailure $ DelegateeDRepNotRegisteredDELEG drepCred]
            expectNotDelegatedVote cred
      ifBootstrap inBootstrap outOfBootstrap

    it "Delegate vote of unregistered stake credentials" $ do
      cred <- KeyHashObj <$> freshKeyHash
      drepCred <- KeyHashObj <$> registerDRep
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [DelegTxCert cred (DelegVote (DRepCredential drepCred))]
        )
        [injectFailure $ StakeKeyNotRegisteredDELEG cred]

      expectNotDelegatedVote cred

    it "Redelegate vote" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      drepCred <- KeyHashObj <$> registerDRep

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [RegDepositDelegTxCert cred (DelegVote (DRepCredential drepCred)) expectedDeposit]
      expectDelegatedVote cred (DRepCredential drepCred)

      drepCred2 <- KeyHashObj <$> registerDRep
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegVote (DRepCredential drepCred2))]

      expectDelegatedVote cred (DRepCredential drepCred2)

      impAnn "Check that unregistration of previous delegation does not affect current delegation" $ do
        unRegisterDRep drepCred
        -- we need to preserve the buggy behavior until the boostrap phase is over.
        ifBootstrap (expectNotDelegatedVote cred) (expectDelegatedVote cred (DRepCredential drepCred2))

    it "Delegate vote and unregister stake credentials" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      cred <- KeyHashObj <$> freshKeyHash
      drepCred <- KeyHashObj <$> registerDRep
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [RegDepositDelegTxCert cred (DelegVote (DRepCredential drepCred)) expectedDeposit]
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [UnRegDepositTxCert cred expectedDeposit]
      expectNotRegistered cred
      expectNotDelegatedVote cred

  describe "Delegate both stake and vote - separated out for conformance mismatch" $
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/640
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableImpInitExpectLedgerRuleConformance $ do
      it "Delegate, retire and re-register pool" $ do
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
        cred <- KeyHashObj <$> freshKeyHash
        poolKh <- freshKeyHash
        rewardAccount <- registerRewardAccount
        registerPool poolKh
        drepCred <- KeyHashObj <$> registerDRep

        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [ RegDepositDelegTxCert
                    cred
                    (DelegStakeVote poolKh (DRepCredential drepCred))
                    expectedDeposit
                 ]
        expectDelegatedToPool cred poolKh
        expectDelegatedVote cred (DRepCredential drepCred)

        let poolLifetime = 2
        let poolExpiry = getsNES nesELL <&> \n -> addEpochInterval n $ EpochInterval poolLifetime

        poolExpiry >>= \pe ->
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RetirePoolTxCert poolKh pe]

        -- when pool is re-registered after its expiration, all delegations are cleared
        passNEpochs $ fromIntegral poolLifetime
        expectNotDelegatedToPool cred
        registerPoolWithRewardAccount poolKh rewardAccount
        expectNotDelegatedToPool cred
        -- the vote delegation is kept
        expectDelegatedVote cred (DRepCredential drepCred)

        -- re-delegate
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [ DelegTxCert
                    cred
                    (DelegStake poolKh)
                 ]
        expectDelegatedToPool cred poolKh

        -- when pool is re-registered before its expiration, delegations are kept
        poolExpiry >>= \pe ->
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RetirePoolTxCert poolKh pe]
        -- re-register the pool before the expiration time
        passNEpochs $ fromIntegral poolLifetime - 1
        registerPoolWithRewardAccount poolKh rewardAccount
        expectDelegatedToPool cred poolKh
        passNEpochs 2
        expectDelegatedToPool cred poolKh

        -- when pool is retired and re-registered in the same transaction, delegations are kept
        pps <- freshPoolParams poolKh rewardAccount
        poolExpiry >>= \pe ->
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RetirePoolTxCert poolKh pe, RegPoolTxCert pps]

        expectDelegatedToPool cred poolKh
        passNEpochs $ fromIntegral poolLifetime
        expectDelegatedToPool cred poolKh
  describe "Delegate both stake and vote" $ do
    it "Delegate and unregister credentials" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      drepCred <- KeyHashObj <$> registerDRep

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ RegDepositDelegTxCert
                  cred
                  (DelegStakeVote poolKh (DRepCredential drepCred))
                  expectedDeposit
               ]
      expectDelegatedToPool cred poolKh
      expectDelegatedVote cred (DRepCredential drepCred)

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [UnRegDepositTxCert cred expectedDeposit]
      expectNotRegistered cred
      expectNotDelegatedVote cred
      expectNotDelegatedToPool cred

    it "Delegate to DRep and SPO and change delegation to a different SPO" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPool poolKh
      drepCred <- KeyHashObj <$> registerDRep

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ RegDepositDelegTxCert
                  cred
                  (DelegStakeVote poolKh (DRepCredential drepCred))
                  expectedDeposit
               ]
      expectDelegatedToPool cred poolKh
      expectDelegatedVote cred (DRepCredential drepCred)

      poolKh' <- freshKeyHash
      registerPool poolKh'
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegStake poolKh')]
      expectDelegatedToPool cred poolKh'
      expectDelegatedVote cred (DRepCredential drepCred)
  where
    expectRegistered cred = do
      umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      let umapDeposit = rdDepositCoin <$> UMap.lookup cred (RewDepUView umap)
      impAnn
        (show cred <> " expected to be in UMap RewDep with the correct deposit")
        $ umapDeposit `shouldBe` Just expectedDeposit

    expectNotRegistered cred = do
      umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
      impAnn (show cred <> " expected to not be in UMap RewDep") $
        UMap.notMember cred (RewDepUView umap) `shouldBe` True

    expectDelegatedToPool cred poolKh = do
      umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
      impAnn (show cred <> " expected to have stake delegated to " <> show poolKh) $
        Map.lookup cred (sPoolMap umap) `shouldBe` Just poolKh

    expectNotDelegatedToPool cred = do
      umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
      impAnn (show cred <> " expected to not have delegated stake") $
        Map.notMember cred (sPoolMap umap) `shouldBe` True

    expectDelegatedVote cred drep = do
      umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
      dreps <- getsNES $ nesEsL . epochStateRegDrepL
      impAnn (show cred <> " expected to have their vote delegated to " <> show drep) $ do
        Map.lookup cred (dRepMap umap) `shouldBe` Just drep
        case drep of
          DRepCredential drepCred ->
            case Map.lookup drepCred dreps of
              Nothing ->
                whenPostBootstrap $ assertFailure "Expected DRep to be registered"
              Just drepState ->
                assertBool
                  "Expected DRep delegations to contain the stake credential"
                  (cred `Set.member` drepDelegs drepState)
          _ -> pure ()

    expectNotDelegatedVote cred = do
      umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
      impAnn (show cred <> " expected to not have their vote delegated") $
        Map.notMember cred (dRepMap umap) `shouldBe` True
