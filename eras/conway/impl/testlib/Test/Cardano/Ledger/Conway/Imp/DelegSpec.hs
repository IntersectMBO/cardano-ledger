{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.DelegSpec (
  spec,
  conwayEraSpecificSpec,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  Mismatch (..),
  ProtVer (..),
  StrictMaybe (..),
  addEpochInterval,
  natVersion,
 )
import Cardano.Ledger.Coin (Coin (..), compactCoinOrError)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayDelegPredFailure (..))
import Cardano.Ledger.Conway.State hiding (balance)
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus (
  SLanguage (..),
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
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
      pv <- getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL

      Positive n <- arbitrary
      let wrongDeposit = expectedDeposit <+> Coin n

      freshKeyHash >>= \kh -> do
        submitFailingTx
          ( mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [RegDepositTxCert (KeyHashObj kh) wrongDeposit]
          )
          [ injectFailure $
              if hardforkConwayDELEGIncorrectDepositsAndRefunds pv
                then
                  DepositIncorrectDELEG
                    Mismatch
                      { mismatchSupplied = wrongDeposit
                      , mismatchExpected = expectedDeposit
                      }
                else IncorrectDepositDELEG wrongDeposit
          ]
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

    it "With incorrect refund" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      pv <- getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL

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
        [ injectFailure $
            if hardforkConwayDELEGIncorrectDepositsAndRefunds pv
              then
                RefundIncorrectDELEG
                  Mismatch
                    { mismatchSupplied = wrongDeposit
                    , mismatchExpected = expectedDeposit
                    }
              else IncorrectDepositDELEG wrongDeposit
        ]

      expectRegistered cred

    it "With non-zero reward balance" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [RegDepositTxCert cred expectedDeposit]

      submitAndExpireProposalToMakeReward cred

      balance <- getBalance cred
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ [UnRegDepositTxCert cred expectedDeposit]
        )
        [injectFailure $ StakeKeyHasNonZeroRewardAccountBalanceDELEG balance]
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
      rewardAccount <- getRewardAccountFor stakeCred
      otherStakeCred <- KeyHashObj <$> freshKeyHash
      otherRewardAccount <- getRewardAccountFor otherStakeCred
      khStakePool <- freshKeyHash
      registerPoolWithDeposit khStakePool
      submitTx_ . mkBasicTx $
        mkBasicTxBody
          & certsTxBodyL
            .~ SSeq.fromList
              [ RegDepositDelegTxCert stakeCred (DelegStakeVote khStakePool DRepAlwaysAbstain) keyDeposit
              , RegDepositDelegTxCert otherStakeCred (DelegStakeVote khStakePool DRepAlwaysAbstain) keyDeposit
              ]
      expectRegisteredRewardAddress rewardAccount
      expectRegisteredRewardAddress otherRewardAccount
      submitAndExpireProposalToMakeReward otherStakeCred
      getBalance otherStakeCred `shouldReturn` govActionDeposit
      let unRegTxCert = UnRegDepositTxCert stakeCred keyDeposit
      submitTx_ . mkBasicTx $
        mkBasicTxBody
          & certsTxBodyL .~ SSeq.fromList [unRegTxCert]
          & withdrawalsTxBodyL
            .~ Withdrawals
              ( Map.fromList
                  [ (rewardAccount, Coin 0)
                  , (otherRewardAccount, govActionDeposit)
                  ]
              )
      getBalance otherStakeCred `shouldReturn` Coin 0
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
      registerPoolWithDeposit poolKh

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegStake poolKh)]

      expectDelegatedToPool cred poolKh

    it "Register and delegate in the same transaction" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      poolKh <- freshKeyHash
      registerPoolWithDeposit poolKh
      freshKeyHash >>= \kh -> do
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [RegDepositDelegTxCert (KeyHashObj kh) (DelegStake poolKh) expectedDeposit]
        expectDelegatedToPool (KeyHashObj kh) poolKh

    it "Delegate unregistered stake credentials" $ do
      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPoolWithDeposit poolKh
      submitFailingTx
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [DelegTxCert cred (DelegStake poolKh)]
        )
        [injectFailure $ StakeKeyNotRegisteredDELEG cred]

      expectNotRegistered cred

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
      registerPoolWithDeposit poolKh
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
      registerPoolWithDeposit poolKh1
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegStake poolKh1)]
      expectDelegatedToPool cred poolKh1

      poolKh2 <- freshKeyHash
      registerPoolWithDeposit poolKh2
      poolKh3 <- freshKeyHash
      registerPoolWithDeposit poolKh3

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
      registerPoolWithDeposit poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ RegDepositDelegTxCert cred (DelegStake poolKh) expectedDeposit
               , UnRegDepositTxCert cred expectedDeposit
               ]
      expectNotRegistered cred

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
          -- Clear out delegation, in order to check its repopulation from accounts.
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
      RewardAccount _ cred <- registerRewardAccountWithDeposit
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

      expectNotRegistered cred

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
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/917
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "Delegate vote and unregister after hardfork" $ do
      let
        bootstrapVer = ProtVer (natVersion @9) 0
        setProtVer pv = modifyNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL .~ pv
      initialProtVer <- getProtVer
      (_, ccCred, _) <- impAnn "Set up a committee" $ do
        -- Temporarily set protver to 10 to elect a committee
        setProtVer $ ProtVer (natVersion @10) 0
        res <- electBasicCommittee
        setProtVer initialProtVer
        pure res
      (khSPO, _, _) <- setupPoolWithStake $ Coin 10_000_000
      -- Using a lazy pattern match here to prevent evaluation of tuple
      -- unless we actually need a value from it
      ~(drepCred, _, _) <-
        if initialProtVer > bootstrapVer
          then setupSingleDRep 100_000_000
          else pure $ error "drepCred should not be accessed before protver 10"
      passNEpochs 3
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      cred <- KeyHashObj <$> freshKeyHash
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [RegDepositDelegTxCert cred (DelegVote DRepAlwaysAbstain) expectedDeposit]
      registerAndRetirePoolToMakeReward cred
      expectRegistered cred
      expectDelegatedVote cred DRepAlwaysAbstain
      impAnn "Version should be unchanged" $
        getProtVer `shouldReturn` initialProtVer
      let nextVer = majorFollow initialProtVer
      hfGaid <- submitGovAction $ HardForkInitiation SNothing nextVer
      submitVote_ VoteYes (StakePoolVoter khSPO) hfGaid
      submitVote_ VoteYes (CommitteeVoter ccCred) hfGaid
      when (initialProtVer > bootstrapVer) $
        submitVote_ VoteYes (DRepVoter drepCred) hfGaid
      passNEpochs 3
      logRatificationChecks hfGaid
      impAnn "Version should be bumped" $
        getProtVer `shouldReturn` nextVer
      withdrawalAmount <- getsPParams ppPoolDepositL
      rewardAccount <- getRewardAccountFor cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [UnRegDepositTxCert cred expectedDeposit]
          & bodyTxL . withdrawalsTxBodyL
            .~ Withdrawals (Map.singleton rewardAccount withdrawalAmount)
      expectNotRegistered cred
      expectNotDelegatedVote cred
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/916
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "Delegate vote and undelegate after delegating to some stake pools" $ do
      (khSPO, _, _) <- setupPoolWithStake $ Coin 1_000_000
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      cred <- KeyHashObj <$> freshKeyHash
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [RegDepositDelegTxCert cred (DelegVote DRepAlwaysAbstain) expectedDeposit]
      registerAndRetirePoolToMakeReward cred
      expectRegistered cred
      expectDelegatedVote cred DRepAlwaysAbstain
      forM_ @[] [1 .. 3 :: Int] $ \_ -> do
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [DelegTxCert cred (DelegStake khSPO)]
      passNEpochs 3
      withdrawalAmount <- getsPParams ppPoolDepositL
      rewardAccount <- getRewardAccountFor cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [UnRegDepositTxCert cred expectedDeposit]
          & bodyTxL . withdrawalsTxBodyL
            .~ Withdrawals (Map.singleton rewardAccount withdrawalAmount)
      expectNotRegistered cred
      expectNotDelegatedVote cred

    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/640
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "Delegate, retire and re-register pool" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      rewardAccount <- registerRewardAccountWithDeposit
      registerPoolWithDeposit poolKh
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
      registerPoolWithDeposit poolKh
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

    it "Delegate to DRep and SPO and change delegation to a different SPO" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      poolKh <- freshKeyHash
      registerPoolWithDeposit poolKh
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
      registerPoolWithDeposit poolKh'
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegStake poolKh')]
      expectDelegatedToPool cred poolKh'
      expectDelegatedVote cred (DRepCredential drepCred)
  where
    expectNotRegistered :: Credential 'Staking -> ImpTestM era ()
    expectNotRegistered cred = do
      accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
      impAnn (show cred <> " expected to not be in Accounts") $ do
        expectNothingExpr $ lookupAccountState cred accounts

    expectNotDelegatedToPool :: Credential 'Staking -> ImpTestM era ()
    expectNotDelegatedToPool cred = do
      accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
      impAnn (show cred <> " expected to not have delegated to a stake pool") $ do
        accountState <- expectJust $ lookupAccountState cred accounts
        expectNothingExpr (accountState ^. stakePoolDelegationAccountStateL)

    expectDelegatedVote :: HasCallStack => Credential 'Staking -> DRep -> ImpTestM era ()
    expectDelegatedVote cred drep = do
      accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
      dreps <- getsNES $ nesEsL . epochStateRegDrepL
      impAnn (show cred <> " expected to have delegated to " <> show drep) $ do
        accountState <- expectJust $ lookupAccountState cred accounts
        accountState ^. dRepDelegationAccountStateL `shouldBe` Just drep
        case drep of
          DRepCredential drepCred ->
            case Map.lookup drepCred dreps of
              Nothing ->
                whenPostBootstrap $
                  assertFailure $
                    "Expected DRep: " <> show drepCred <> " to be registered"
              Just drepState ->
                assertBool
                  "Expected DRep delegations to contain the stake credential"
                  (cred `Set.member` drepDelegs drepState)
          _ -> pure ()

    expectNotDelegatedVote :: Credential 'Staking -> ImpTestM era ()
    expectNotDelegatedVote cred = do
      accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
      impAnn (show cred <> " expected to not have their vote delegated") $
        expectNothingExpr (lookupDRepDelegation cred accounts)

conwayEraSpecificSpec ::
  forall era.
  ( ConwayEraImp era
  , ShelleyEraTxCert era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
conwayEraSpecificSpec = do
  describe "Register stake credential" $ do
    it "Without any deposit" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL .~ [regTxCert]
      expectRegistered cred

  describe "Delegate stake" $ do
    it "Register and delegate in the same transaction" $ do
      cred1 <- KeyHashObj <$> freshKeyHash
      regTxCert1 <- genRegTxCert cred1
      poolKh <- freshKeyHash
      registerPoolWithDeposit poolKh
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ regTxCert1
               , DelegTxCert cred1 (DelegStake poolKh)
               ]
      expectDelegatedToPool cred1 poolKh

      cred2 <- KeyHashObj <$> freshKeyHash
      regTxCert2 <- genRegTxCert cred2
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [ regTxCert2
               , DelegStakeTxCert cred2 poolKh -- using the pattern from Shelley
               ]
      expectDelegatedToPool cred2 poolKh

expectRegistered :: (HasCallStack, ConwayEraImp era) => Credential 'Staking -> ImpTestM era ()
expectRegistered cred = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

  accountState <- expectJust $ lookupAccountState cred accounts
  impAnn (show cred <> " expected to be in Accounts with the correct deposit") $ do
    accountState ^. depositAccountStateL `shouldBe` compactCoinOrError expectedDeposit

expectDelegatedToPool ::
  (HasCallStack, ConwayEraImp era) => Credential 'Staking -> KeyHash 'StakePool -> ImpTestM era ()
expectDelegatedToPool cred poolKh = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  impAnn (show cred <> " expected to have delegated to " <> show poolKh) $ do
    accountState <- expectJust $ lookupAccountState cred accounts
    accountState ^. stakePoolDelegationAccountStateL `shouldBe` Just poolKh
