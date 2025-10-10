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
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayDelegPredFailure (..))
import Cardano.Ledger.Conway.State hiding (balance)
import Cardano.Ledger.Conway.Transition (conwayRegisterInitialAccounts)
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus (
  SLanguage (..),
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesisStaking (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Val (Val (..))
import qualified Data.ListMap as LM
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
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "Register stake credential" $ do
    it "With correct deposit" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      freshKeyHash >>= \kh -> do
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [RegDepositTxCert (KeyHashObj kh) expectedDeposit]
        expectStakeCredRegistered (KeyHashObj kh)

    it "Twice the same certificate in the same transaction" $ do
      -- This is expected behavior because `certsTxBodyL` removes duplicates
      freshKeyHash >>= \kh -> do
        regTxCert <- genRegTxCert (KeyHashObj kh)
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [regTxCert, regTxCert]
        expectStakeCredRegistered (KeyHashObj kh)
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
        expectStakeCredNotRegistered (KeyHashObj kh)

  describe "Unregister stake credentials" $ do
    it "With incorrect refund" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      pv <- getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL

      let cred = ScriptHashObj $ hashPlutusScript $ evenRedeemerNoDatum SPlutusV3

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

      expectStakeCredRegistered cred

    it "Deregistering returns the deposit" $ do
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
      registerPool khStakePool
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
      unRegTxCert <- genUnRegTxCert stakeCred
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
              .~ [DelegTxCert cred (DelegStake poolKh)]
        )
        [injectFailure $ DelegateeStakePoolNotRegisteredDELEG poolKh]
      expectNotDelegatedToAnyPool cred

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
      expectNotDelegatedToAnyPool cred
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

    it "Redelegate vote to the same DRep" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      cred <- KeyHashObj <$> freshKeyHash
      drepCred <- KeyHashObj <$> registerDRep

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [RegDepositDelegTxCert cred (DelegVote (DRepCredential drepCred)) expectedDeposit]
      expectDelegatedVote cred (DRepCredential drepCred)

      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ [DelegTxCert cred (DelegVote (DRepCredential drepCred))]

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

      expectStakeCredNotRegistered cred

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
        ifBootstrap
          ( do
              -- we cannot `expectNotDelegatedVote` because the delegation is still in the DRepState of the other pool
              accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
              expectNothingExpr (lookupDRepDelegation cred accounts)
              dReps <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
              drepState2 <- expectJust $ Map.lookup drepCred2 dReps
              drepDelegs drepState2 `shouldSatisfy` Set.member cred
          )
          (expectDelegatedVote cred (DRepCredential drepCred2))

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
      expectStakeCredNotRegistered cred
      expectNotDelegatedVote cred
      expectNotDelegatedToAnyPool cred

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
      expectStakeCredRegistered cred
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
      expectStakeCredNotRegistered cred
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
      expectStakeCredRegistered cred
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
      expectStakeCredNotRegistered cred
      expectNotDelegatedVote cred

    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/640
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "Delegate, retire and re-register pool" $ do
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
      expectNotDelegatedToAnyPool cred
      registerPoolWithRewardAccount poolKh rewardAccount
      expectNotDelegatedToAnyPool cred
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
      expectStakeCredNotRegistered cred

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
      expectNotDelegatedToPool cred poolKh
      expectDelegatedVote cred (DRepCredential drepCred)

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
    let updatedNES = conwayRegisterInitialAccounts sgs nes
    delegateStake (KeyHashObj deleg1) pool1
    delegateStake (KeyHashObj deleg2) pool1
    delegateStake (KeyHashObj deleg3) pool2
    getPoolsState <$> (getsNES id) `shouldReturn` getPoolsState updatedNES
    getDelegs deleg1 updatedNES `shouldReturn` Just pool1
    getDelegs deleg2 updatedNES `shouldReturn` Just pool1
    getDelegs deleg3 updatedNES `shouldReturn` Just pool2
  where
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
      dreps <- getsNES $ nesEsL . epochStateRegDrepL
      impAnn (show cred <> " expected to not have their vote delegated") $ do
        expectNothingExpr (lookupDRepDelegation cred accounts)
        assertBool
          ("Expected no drep state delegation to contain the stake credential: " <> show cred)
          (all (Set.notMember cred . drepDelegs) dreps)
    getDelegs kh nes = do
      let accounts = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      pure $ Map.lookup (KeyHashObj kh) accounts >>= (^. stakePoolDelegationAccountStateL)
    getPoolsState nes = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
