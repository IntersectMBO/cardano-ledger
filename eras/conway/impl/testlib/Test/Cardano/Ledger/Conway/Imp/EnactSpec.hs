{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.EnactSpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (Event, ShelleyTickEvent (..))
import Cardano.Ledger.Val (zero, (<->))
import Control.Monad (forM)
import Data.Default (def)
import Data.Foldable as F (foldl', traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Typeable (cast)
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Arbitrary (uniformSubSet)
import Test.Cardano.Ledger.Core.Rational
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  committeeSpec
  treasuryWithdrawalsSpec
  noConfidenceSpec
  hardForkInitiationSpec
  constitutionSpec
  actionPrioritySpec
  hardForkInitiationNoDRepsSpec
  pparamPredictionSpec

treasuryWithdrawalsSpec ::
  forall era. ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
treasuryWithdrawalsSpec =
  describe "Treasury withdrawals" $ do
    -- Treasury withdrawals are disallowed in bootstrap, so we're running these tests only post-bootstrap
    it "Modify EnactState as expected" $ whenPostBootstrap $ do
      rewardAcount1 <- registerRewardAccount
      govActionId <- submitTreasuryWithdrawals [(rewardAcount1, Coin 666)]
      gas <- getGovActionState govActionId
      let govAction = gasAction gas
      enactStateInit <- getEnactState
      let signal =
            EnactSignal
              { esGovActionId = govActionId
              , esGovAction = govAction
              }
          enactState =
            enactStateInit
              { ensTreasury = Coin 1000
              }
      enactState' <- runImpRule @"ENACT" () enactState signal
      ensWithdrawals enactState' `shouldBe` [(raCredential rewardAcount1, Coin 666)]

      rewardAcount2 <- registerRewardAccount
      let withdrawals' =
            [ (rewardAcount1, Coin 111)
            , (rewardAcount2, Coin 222)
            ]
      govActionId' <- submitTreasuryWithdrawals withdrawals'
      gas' <- getGovActionState govActionId'
      let govAction' = gasAction gas'
      let signal' =
            EnactSignal
              { esGovActionId = govActionId'
              , esGovAction = govAction'
              }

      enactState'' <- runImpRule @"ENACT" () enactState' signal'

      ensWithdrawals enactState''
        `shouldBe` [ (raCredential rewardAcount1, Coin 777)
                   , (raCredential rewardAcount2, Coin 222)
                   ]
      ensTreasury enactState'' `shouldBe` Coin 1

    it "Withdrawals exceeding treasury submitted in a single proposal" $ whenPostBootstrap $ do
      disableTreasuryExpansion
      committeeCs <- registerInitialCommittee
      (drepC, _, _) <- setupSingleDRep 1_000_000
      initialTreasury <- getsNES treasuryL
      numWithdrawals <- choose (1, 10)
      withdrawals <- genWithdrawalsExceeding initialTreasury numWithdrawals

      void $ enactTreasuryWithdrawals withdrawals drepC committeeCs
      checkNoWithdrawal initialTreasury withdrawals

      let sumRequested = foldMap snd withdrawals

      impAnn "Submit a treasury donation that can cover the withdrawals" $ do
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . treasuryDonationTxBodyL .~ (sumRequested <-> initialTreasury)
        submitTx_ tx
      passNEpochs 2
      getsNES treasuryL `shouldReturn` zero
      sumRewardAccounts withdrawals `shouldReturn` sumRequested

    it "Withdrawals exceeding maxBound Word64 submitted in a single proposal" $ whenPostBootstrap $ do
      disableTreasuryExpansion
      committeeCs <- registerInitialCommittee
      (drepC, _, _) <- setupSingleDRep 1_000_000
      initialTreasury <- getsNES treasuryL
      numWithdrawals <- choose (1, 10)
      withdrawals <- genWithdrawalsExceeding (Coin (fromIntegral (maxBound :: Word64))) numWithdrawals
      void $ enactTreasuryWithdrawals withdrawals drepC committeeCs
      checkNoWithdrawal initialTreasury withdrawals

    it "Withdrawals exceeding treasury submitted in several proposals within the same epoch" $
      whenPostBootstrap $
        do
          disableTreasuryExpansion
          committeeCs <- registerInitialCommittee
          (drepC, _, _) <- setupSingleDRep 1_000_000
          donateToTreasury $ Coin 5_000_000
          initialTreasury <- getsNES treasuryL
          numWithdrawals <- choose (1, 10)
          withdrawals <- genWithdrawalsExceeding initialTreasury numWithdrawals

          impAnn "submit in individual proposals in the same epoch" $ do
            traverse_
              ( \w -> do
                  gaId <- submitTreasuryWithdrawals @era [w]
                  submitYesVote_ (DRepVoter drepC) gaId
                  submitYesVoteCCs_ committeeCs gaId
              )
              withdrawals
            passNEpochs 2

            let expectedTreasury =
                  F.foldl'
                    ( \acc (_, x) ->
                        if acc >= x
                          then acc <-> x
                          else acc
                    )
                    initialTreasury
                    withdrawals

            getsNES treasuryL `shouldReturn` expectedTreasury
            -- check that the sum of the rewards matches what was spent from the treasury
            sumRewardAccounts withdrawals `shouldReturn` (initialTreasury <-> expectedTreasury)
  where
    sumRewardAccounts withdrawals = mconcat <$> traverse (getAccountBalance . fst) withdrawals
    genWithdrawalsExceeding (Coin val) n = do
      vals <- genValuesExceeding val n
      forM (Coin <$> vals) $ \coin -> (,coin) <$> registerRewardAccount
    checkNoWithdrawal initialTreasury withdrawals = do
      getsNES treasuryL `shouldReturn` initialTreasury
      sumRewardAccounts withdrawals `shouldReturn` zero
    genValuesExceeding val n = do
      pcts <- replicateM (n - 1) $ choose (1, 100)
      let tot = sum pcts
      let amounts = map (\x -> ceiling ((x * val) % tot)) pcts
      let minNeeded = max 0 (val - sum amounts + 1)
      excess <- choose (minNeeded, val + 1)
      pure $ excess : amounts

hardForkInitiationSpec ::
  forall era.
  ( ConwayEraImp era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
hardForkInitiationSpec =
  it "HardForkInitiation" $ whenPostBootstrap $ do
    committeeMembers' <- registerInitialCommittee
    modifyPParams $ \pp ->
      pp
        & ppDRepVotingThresholdsL . dvtHardForkInitiationL .~ 2 %! 3
        & ppPoolVotingThresholdsL . pvtHardForkInitiationL .~ 2 %! 3
    _ <- setupPoolWithStake $ Coin 22_000_000
    (stakePoolId1, _, _) <- setupPoolWithStake $ Coin 22_000_000
    (stakePoolId2, _, _) <- setupPoolWithStake $ Coin 22_000_000
    (dRep1, _, _) <- setupSingleDRep 11_000_000
    (dRep2, _, _) <- setupSingleDRep 11_000_000
    curProtVer <- getProtVer
    nextMajorVersion <- succVersion $ pvMajor curProtVer
    let nextProtVer = curProtVer {pvMajor = nextMajorVersion}
    govActionId <- submitGovAction $ HardForkInitiation SNothing nextProtVer
    submitYesVoteCCs_ committeeMembers' govActionId
    submitYesVote_ (DRepVoter dRep1) govActionId
    submitYesVote_ (StakePoolVoter stakePoolId1) govActionId
    passNEpochs 2
      & impEventsFrom
      >>= expectHardForkEvents <*> pure []
    getProtVer `shouldReturn` curProtVer
    submitYesVote_ (DRepVoter dRep2) govActionId
    passNEpochs 2
      & impEventsFrom
      >>= expectHardForkEvents <*> pure []
    getProtVer `shouldReturn` curProtVer
    submitYesVote_ (StakePoolVoter stakePoolId2) govActionId
    passNEpochs 2
      & impEventsFrom
      >>= expectHardForkEvents
        <*> pure
          [ SomeSTSEvent @era @"TICK" . injectEvent $ ConwayHardForkEvent nextProtVer
          ]

    getProtVer `shouldReturn` nextProtVer

hardForkInitiationNoDRepsSpec ::
  forall era.
  ( ConwayEraImp era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
hardForkInitiationNoDRepsSpec =
  it "HardForkInitiation without DRep voting" $ do
    committeeMembers' <- registerInitialCommittee
    modifyPParams $ ppPoolVotingThresholdsL . pvtHardForkInitiationL .~ 2 %! 3
    whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL . dvtHardForkInitiationL .~ def)
    _ <- setupPoolWithStake $ Coin 22_000_000
    (stakePoolId1, _, _) <- setupPoolWithStake $ Coin 22_000_000
    (stakePoolId2, _, _) <- setupPoolWithStake $ Coin 22_000_000
    curProtVer <- getProtVer
    nextMajorVersion <- succVersion $ pvMajor curProtVer
    let nextProtVer = curProtVer {pvMajor = nextMajorVersion}
    govActionId <- submitGovAction $ HardForkInitiation SNothing nextProtVer
    submitYesVoteCCs_ committeeMembers' govActionId
    submitYesVote_ (StakePoolVoter stakePoolId1) govActionId
    passNEpochs 2
      & impEventsFrom
      >>= expectHardForkEvents <*> pure []
    getProtVer `shouldReturn` curProtVer
    submitYesVote_ (StakePoolVoter stakePoolId2) govActionId
    passNEpochs 2
      & impEventsFrom
      >>= expectHardForkEvents
        <*> pure
          [ SomeSTSEvent @era @"TICK" . injectEvent $ ConwayHardForkEvent nextProtVer
          ]
    getProtVer `shouldReturn` nextProtVer

pparamPredictionSpec ::
  ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
pparamPredictionSpec =
  it "futurePParams" $ do
    committeeMembers' <- registerInitialCommittee
    modifyPParams $ ppPoolVotingThresholdsL . pvtHardForkInitiationL .~ 2 %! 3
    whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL . dvtHardForkInitiationL .~ def)
    _ <- setupPoolWithStake $ Coin 22_000_000
    (stakePoolId1, _, _) <- setupPoolWithStake $ Coin 22_000_000
    (stakePoolId2, _, _) <- setupPoolWithStake $ Coin 22_000_000
    curProtVer <- getProtVer
    nextMajorVersion <- succVersion $ pvMajor curProtVer
    let nextProtVer = curProtVer {pvMajor = nextMajorVersion}
    govActionId <- submitGovAction $ HardForkInitiation SNothing nextProtVer
    submitYesVoteCCs_ committeeMembers' govActionId
    submitYesVote_ (StakePoolVoter stakePoolId1) govActionId
    submitYesVote_ (StakePoolVoter stakePoolId2) govActionId
    passEpoch
    advanceToPointOfNoReturn
    passEpoch
    getProtVer `shouldReturn` nextProtVer

noConfidenceSpec ::
  forall era. ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
noConfidenceSpec =
  it "NoConfidence" $ whenPostBootstrap $ do
    modifyPParams $ \pp ->
      pp
        & ppDRepVotingThresholdsL . dvtCommitteeNoConfidenceL .~ 1 %! 2
        & ppPoolVotingThresholdsL . pvtCommitteeNoConfidenceL .~ 1 %! 2
        & ppCommitteeMaxTermLengthL .~ EpochInterval 200
    let
      assertNoCommittee :: HasCallStack => ImpTestM era ()
      assertNoCommittee =
        do
          committee <- getCommittee
          impAnn "There should not be a committee" $ committee `shouldBe` SNothing
    khCC <- freshKeyHash
    initialCommitteeMembers <- getCommitteeMembers

    (drep, _, _) <- setupSingleDRep 1_000_000
    startEpochNo <- getsNES nesELL
    let committeeMap =
          Map.fromList
            [ (KeyHashObj khCC, addEpochInterval startEpochNo (EpochInterval 50))
            ]
    prevGaidCommittee@(GovPurposeId gaidCommittee) <-
      submitCommitteeElection
        SNothing
        drep
        initialCommitteeMembers
        committeeMap
    (khSPO, _, _) <- setupPoolWithStake $ Coin 42_000_000
    logInstantStake
    submitYesVote_ (StakePoolVoter khSPO) gaidCommittee
    replicateM_ 4 passEpoch
    impAnn "Committee should be elected" $ do
      committee <- getCommittee
      committee `shouldBe` SJust (Committee committeeMap $ 1 %! 2)
    gaidNoConf <- mkProposal (NoConfidence (SJust prevGaidCommittee)) >>= submitProposal
    submitYesVote_ (StakePoolVoter khSPO) gaidNoConf
    submitYesVote_ (DRepVoter drep) gaidNoConf
    replicateM_ 4 passEpoch
    assertNoCommittee

constitutionSpec ::
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
constitutionSpec =
  it "Constitution" $ do
    (committeeMember1 :| [committeeMember2]) <- registerInitialCommittee
    (dRep, _, _) <- setupSingleDRep 1_000_000
    initialConstitution <- getConstitution
    (proposal, constitution) <- mkConstitutionProposal SNothing
    mbGovActionId <-
      submitBootstrapAwareFailingProposal proposal $
        FailBootstrap [injectFailure (DisallowedProposalDuringBootstrap proposal)]
    forM_ mbGovActionId $ \govActionId -> do
      proposalsBeforeVotes <- getsNES $ newEpochStateGovStateL . proposalsGovStateL
      pulserBeforeVotes <- getsNES newEpochStateDRepPulsingStateL

      submitYesVote_ (DRepVoter dRep) govActionId
      submitYesVote_ (CommitteeVoter committeeMember1) govActionId
      submitYesVote_ (CommitteeVoter committeeMember2) govActionId

      proposalsAfterVotes <- getsNES $ newEpochStateGovStateL . proposalsGovStateL
      pulserAfterVotes <- getsNES newEpochStateDRepPulsingStateL

      impAnn "Votes are recorded in the proposals" $ do
        let proposalsWithVotes =
              proposalsAddVote
                (CommitteeVoter committeeMember1)
                VoteYes
                govActionId
                ( proposalsAddVote
                    (CommitteeVoter committeeMember2)
                    VoteYes
                    govActionId
                    ( proposalsAddVote
                        (DRepVoter dRep)
                        VoteYes
                        govActionId
                        proposalsBeforeVotes
                    )
                )
        proposalsAfterVotes `shouldBe` proposalsWithVotes

      impAnn "Pulser has not changed" $
        pulserAfterVotes `shouldBe` pulserBeforeVotes

      passEpoch

      impAnn "New constitution is not enacted after one epoch" $ do
        constitutionAfterOneEpoch <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        constitutionAfterOneEpoch `shouldBe` initialConstitution

      impAnn "Pulser should reflect the constitution to be enacted" $ do
        pulser <- getsNES newEpochStateDRepPulsingStateL
        let ratifyState = extractDRepPulsingState pulser
        gasId <$> rsEnacted ratifyState `shouldBe` govActionId Seq.:<| Seq.Empty
        rsEnactState ratifyState ^. ensConstitutionL `shouldBe` constitution

      passEpoch

      impAnn "Constitution is enacted after two epochs" $ do
        curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        curConstitution `shouldBe` constitution

      impAnn "Pulser is reset" $ do
        pulser <- getsNES newEpochStateDRepPulsingStateL
        let pulserRatifyState = extractDRepPulsingState pulser
        rsEnacted pulserRatifyState `shouldBe` Seq.empty
        enactState <- getEnactState
        rsEnactState pulserRatifyState `shouldBe` enactState

actionPrioritySpec :: forall era. ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
actionPrioritySpec =
  describe "Competing proposals" $ do
    it "higher action priority wins" $ do
      (drepC, _, _) <- setupSingleDRep 1_000_000
      (poolKH, _, _) <- setupPoolWithStake $ Coin 1_000_000
      cc <- KeyHashObj <$> freshKeyHash
      proposal <-
        mkUpdateCommitteeProposal Nothing mempty [(cc, EpochInterval 30)] (1 %! 3)
      mbGai1 <-
        submitBootstrapAwareFailingProposal proposal $
          FailBootstrap [injectFailure $ DisallowedProposalDuringBootstrap proposal]
      forM_ mbGai1 $ \gai1 -> do
        -- gai2 is the first action of a higher priority
        gai2 <- submitGovAction $ NoConfidence SNothing
        gai3 <- submitGovAction $ NoConfidence SNothing
        traverse_ @[]
          ( \gaid -> do
              submitYesVote_ (DRepVoter drepC) gaid
              submitYesVote_ (StakePoolVoter poolKH) gaid
          )
          [gai1, gai2, gai3]
        passNEpochs 2
        getLastEnactedCommittee
          `shouldReturn` SJust (GovPurposeId gai2)
        expectNoCurrentProposals

        committee <-
          getsNES $
            nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
        committee `shouldBe` SNothing

    -- distinct constitutional values for minFee
    let genMinFeeVals =
          (\x y z -> (CoinPerByte $ CompactCoin x, CoinPerByte $ CompactCoin y, CoinPerByte $ CompactCoin z))
            <$> uniformRM (30, 330)
            <*> uniformRM (330, 660)
            <*> uniformRM (660, 1000)

    it "proposals of same priority are enacted in order of submission" $ do
      modifyPParams $ ppPoolVotingThresholdsL . pvtPPSecurityGroupL .~ 1 %! 1
      whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL . dvtPPEconomicGroupL .~ def)
      (val1, val2, val3) <- genMinFeeVals

      committeeCs <- registerInitialCommittee
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      pGai0 <-
        submitParameterChange
          SNothing
          $ def & ppuTxFeePerByteL .~ SJust val1
      pGai1 <-
        submitParameterChange
          (SJust pGai0)
          $ def & ppuTxFeePerByteL .~ SJust val2
      pGai2 <-
        submitParameterChange
          (SJust pGai1)
          $ def & ppuTxFeePerByteL .~ SJust val3
      traverse_ @[]
        ( \gaid -> do
            submitYesVote_ (StakePoolVoter spoC) gaid
            submitYesVoteCCs_ committeeCs gaid
        )
        [pGai0, pGai1, pGai2]
      passNEpochs 2
      getLastEnactedParameterChange
        `shouldReturn` SJust (GovPurposeId pGai2)
      expectNoCurrentProposals
      getsNES (nesEsL . curPParamsEpochStateL . ppTxFeePerByteL)
        `shouldReturn` val3

    it "only the first action of a transaction gets enacted" $ do
      modifyPParams $ ppPoolVotingThresholdsL . pvtPPSecurityGroupL .~ 1 %! 1
      whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL . dvtPPEconomicGroupL .~ def)
      (val1, val2, val3) <- genMinFeeVals

      committeeCs <- registerInitialCommittee
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      policy <- getGovPolicy
      gaids <-
        submitGovActions $
          NE.fromList
            [ ParameterChange
                SNothing
                (def & ppuTxFeePerByteL .~ SJust val1)
                policy
            , ParameterChange
                SNothing
                (def & ppuTxFeePerByteL .~ SJust val2)
                policy
            , ParameterChange
                SNothing
                (def & ppuTxFeePerByteL .~ SJust val3)
                policy
            ]
      traverse_
        ( \gaid -> do
            submitYesVote_ (StakePoolVoter spoC) gaid
            submitYesVoteCCs_ committeeCs gaid
        )
        gaids
      passNEpochs 2
      getsNES (nesEsL . curPParamsEpochStateL . ppTxFeePerByteL)
        `shouldReturn` val1
      expectNoCurrentProposals

expectHardForkEvents ::
  forall era.
  ( ConwayEraImp era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  ) =>
  [SomeSTSEvent era] -> [SomeSTSEvent era] -> ImpTestM era ()
expectHardForkEvents actual expected =
  filter isHardForkEvent actual `shouldBeExpr` expected
  where
    isHardForkEvent (SomeSTSEvent ev)
      | Just
          (TickNewEpochEvent (EpochEvent (HardForkEvent (ConwayHardForkEvent _))) :: ShelleyTickEvent era) <-
          cast ev =
          True
      | otherwise = False

committeeSpec :: ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
committeeSpec =
  describe "Committee enactment" $ do
    it "Enact UpdateCommitee with lengthy lifetime" $ do
      NonNegative n <- arbitrary
      passNEpochs n
      (drepCred, _, _) <- setupSingleDRep 1_000_000
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      cc <- KeyHashObj <$> freshKeyHash
      EpochInterval committeeMaxTermLength <-
        getsNES $ nesEsL . curPParamsEpochStateL . ppCommitteeMaxTermLengthL
      proposal <-
        mkUpdateCommitteeProposal Nothing mempty [(cc, EpochInterval (committeeMaxTermLength + 2))] (1 %! 2)
      mbSecondAddCCGaid <-
        submitBootstrapAwareFailingProposal proposal $
          FailBootstrap [injectFailure $ DisallowedProposalDuringBootstrap proposal]
      forM_ mbSecondAddCCGaid $ \secondAddCCGaid -> do
        submitYesVote_ (DRepVoter drepCred) secondAddCCGaid
        submitYesVote_ (StakePoolVoter spoC) secondAddCCGaid
        passNEpochs 2
        -- Due to longer than allowed lifetime we have to wait an extra epoch for this new action to be enacted
        expectCommitteeMemberAbsence cc
        passEpoch
        expectCommitteeMemberPresence cc

    -- A CC that has resigned will need to be first voted out and then voted in to be considered active
    it "CC re-election" $ do
      (drepCred, _, _) <- setupSingleDRep 1_000_000
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      passNEpochs 2
      -- Add a fresh CC
      cc <- KeyHashObj <$> freshKeyHash
      proposal <-
        mkUpdateCommitteeProposal Nothing mempty [(cc, EpochInterval 10)] (1 %! 2)
      mbAddCCGaid <-
        submitBootstrapAwareFailingProposal proposal $
          FailBootstrap [injectFailure $ DisallowedProposalDuringBootstrap proposal]
      forM_ mbAddCCGaid $ \addCCGaid -> do
        submitYesVote_ (DRepVoter drepCred) addCCGaid
        submitYesVote_ (StakePoolVoter spoC) addCCGaid
        passNEpochs 2
        -- Confirm that they are added
        expectCommitteeMemberPresence cc
        -- Confirm their hot key registration
        _hotKey <- registerCommitteeHotKey cc
        ccShouldNotBeResigned cc
        -- Have them resign
        _ <- resignCommitteeColdKey cc SNothing
        ccShouldBeResigned cc
        -- Re-add the same CC
        reAddCCGaid <- submitUpdateCommittee Nothing mempty [(cc, EpochInterval 20)] (1 %! 2)
        submitYesVote_ (DRepVoter drepCred) reAddCCGaid
        submitYesVote_ (StakePoolVoter spoC) reAddCCGaid
        passNEpochs 2
        -- Confirm that they are still resigned
        ccShouldBeResigned cc
        -- Remove them
        removeCCGaid <-
          submitUpdateCommittee (Just (SJust $ GovPurposeId reAddCCGaid)) (Set.singleton cc) [] (1 %! 2)
        submitYesVote_ (DRepVoter drepCred) removeCCGaid
        submitYesVote_ (StakePoolVoter spoC) removeCCGaid
        passNEpochs 2
        -- Confirm that they have been removed
        expectCommitteeMemberAbsence cc
        secondAddCCGaid <-
          submitUpdateCommittee Nothing mempty [(cc, EpochInterval 20)] (1 %! 2)
        submitYesVote_ (DRepVoter drepCred) secondAddCCGaid
        submitYesVote_ (StakePoolVoter spoC) secondAddCCGaid
        passNEpochs 2
        -- Confirm that they have been added
        expectCommitteeMemberPresence cc
        -- Confirm that after registering a hot key, they are active
        _hotKey <- registerCommitteeHotKey cc
        ccShouldNotBeResigned cc
    describe "Removing CC with UpdateCommittee" $ do
      it "Non registered" $ do
        (drepCred, _, _) <- setupSingleDRep 1_000_000
        (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
        passNEpochs 2
        initialCommittee <- getCommitteeMembers
        logToExpr initialCommittee
        initialCommittee `shouldSatisfy` not . Set.null
        proposal <-
          mkUpdateCommitteeProposal Nothing initialCommittee mempty (1 %! 2)
        mbRemoveCCGaid <-
          submitBootstrapAwareFailingProposal proposal $
            FailBootstrap [injectFailure $ DisallowedProposalDuringBootstrap proposal]
        forM_ mbRemoveCCGaid $ \removeCCGaid -> do
          submitYesVote_ (DRepVoter drepCred) removeCCGaid
          submitYesVote_ (StakePoolVoter spoC) removeCCGaid
          passNEpochs 2
          finalCommittee <- getCommitteeMembers
          logToExpr finalCommittee
          finalCommittee `shouldSatisfy` Set.null
      it "Registered" $ do
        (drepCred, _, _) <- setupSingleDRep 1_000_000
        (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
        passNEpochs 2
        initialCommittee <- getCommitteeMembers
        logToExpr initialCommittee
        initialCommittee `shouldSatisfy` not . Set.null
        forM_ (Set.toList initialCommittee) $ \kh -> do
          ccHotCred <- KeyHashObj <$> freshKeyHash
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ SSeq.singleton (AuthCommitteeHotKeyTxCert kh ccHotCred)
        newCommittee <- arbitrary
        initialSubCommittee <- askStatefulGen >>= uniformSubSet Nothing initialCommittee
        proposal <-
          mkUpdateCommitteeProposal Nothing initialSubCommittee newCommittee (1 %! 2)
        mbRemoveCCGaid <-
          submitBootstrapAwareFailingProposal proposal $
            FailBootstrap [injectFailure $ DisallowedProposalDuringBootstrap proposal]
        forM_ mbRemoveCCGaid $ \removeCCGaid -> do
          submitYesVote_ (DRepVoter drepCred) removeCCGaid
          submitYesVote_ (StakePoolVoter spoC) removeCCGaid
          passNEpochs 2
          finalCommittee <- getCommitteeMembers
          logToExpr finalCommittee
          finalCommittee
            `shouldBe` Set.union (initialCommittee Set.\\ initialSubCommittee) (Set.fromList $ fst <$> newCommittee)
