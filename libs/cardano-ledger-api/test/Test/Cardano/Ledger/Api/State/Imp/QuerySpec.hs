{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Api.State.Imp.QuerySpec where

import Cardano.Ledger.Api.State.Query (
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),
  queryCommitteeMembersState,
  queryDRepState,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovPurposeId (..),
  Voter (StakePoolVoter),
 )
import Cardano.Ledger.Conway.PParams (ppDRepActivityL)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.DRep
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Shelley.HardForks as HF
import Cardano.Ledger.Shelley.LedgerState
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spec = do
  describe "DRep" $ do
    describe "Expiries are reported correctly" $ do
      let drepStateFromQuery ::
            (HasCallStack, Monad m) =>
            Credential 'DRepRole (EraCrypto era) ->
            NewEpochState era ->
            m (DRepState (EraCrypto era))
          drepStateFromQuery drep nes =
            case Map.lookup drep (queryDRepState nes mempty) of
              Nothing -> error $ "Expected for DRep " ++ show drep ++ " to be present in the query result"
              Just state -> pure state
      it "simple expiry" $ do
        curEpochNo <- getsNES nesELL
        let drepActivity = 3
        modifyPParams $ ppDRepActivityL .~ EpochInterval drepActivity
        (drep, _, _) <- setupSingleDRep 1_000_000
        nes <- getsNES id
        drepState <- drepStateFromQuery drep nes
        drepState ^. drepExpiryL `shouldBe` addEpochInterval curEpochNo (EpochInterval drepActivity)
        let n = 4
        passNEpochsChecking n $
          isDRepExpired drep `shouldReturn` False
        expectDRepExpiry drep $ addEpochInterval curEpochNo $ EpochInterval drepActivity
        expectActualDRepExpiry drep $
          addEpochInterval curEpochNo $
            EpochInterval (drepActivity + fromIntegral n)

      it "dRep registered when there are dormant epochs" $ do
        let drepActivity = 3
        modifyPParams $ ppDRepActivityL .~ EpochInterval drepActivity
        let n = 2
        passNEpochs n
        expectNumDormantEpochs $ EpochNo (fromIntegral n)
        (drep, _, _) <- setupSingleDRep 1_000_000

        let expectedExpiry = do
              epochNo <- getsNES nesELL
              let tot = addEpochInterval epochNo (EpochInterval drepActivity)
              pv <- getProtVer
              pure $
                if HF.bootstrapPhase pv
                  then binOpEpochNo (+) tot (fromIntegral n)
                  else tot

        expectedExpiry >>= expectActualDRepExpiry drep

        nes <- getsNES id
        void $ submitParameterChange SNothing $ def & ppuMinFeeAL .~ SJust (Coin 3000)

        expectedExpiry >>= expectDRepExpiry drep
        drepState <- drepStateFromQuery drep nes
        expectedExpiry >>= shouldBe (drepState ^. drepExpiryL)

      it "proposals are made and numDormantEpochs are added" $ do
        curEpochNo <- getsNES nesELL
        let drepActivity = 3
        modifyPParams $ ppDRepActivityL .~ EpochInterval drepActivity
        let submitParamChangeProposal =
              submitParameterChange SNothing $ def & ppuMinFeeAL .~ SJust (Coin 3000)
        (drep, _, _) <- setupSingleDRep 1_000_000
        nes <- getsNES id
        drepState <- drepStateFromQuery drep nes
        drepState ^. drepExpiryL `shouldBe` addEpochInterval curEpochNo (EpochInterval drepActivity)
        let n = 2
            actualExpiry = addEpochInterval curEpochNo $ EpochInterval (drepActivity + fromIntegral n)
        passNEpochsChecking n $
          isDRepExpired drep `shouldReturn` False
        expectActualDRepExpiry drep actualExpiry
        expectDRepExpiry drep $ addEpochInterval curEpochNo $ EpochInterval drepActivity
        void submitParamChangeProposal
        expectDRepExpiry drep actualExpiry
        nes1 <- getsNES id
        drepState1 <- drepStateFromQuery drep nes1
        drepState1 ^. drepExpiryL `shouldBe` actualExpiry
        passNEpochsChecking (fromIntegral drepActivity) $
          isDRepExpired drep `shouldReturn` False
        passEpoch
        isDRepExpired drep `shouldReturn` True
      it "update certificates are submitted and proposals are made" $ do
        curEpochNo <- getsNES nesELL
        let drepActivity = 3
        modifyPParams $ ppDRepActivityL .~ EpochInterval drepActivity
        let submitParamChangeProposal =
              submitParameterChange SNothing $ def & ppuMinFeeAL .~ SJust (Coin 3000)
        (drep, _, _) <- setupSingleDRep 1_000_000
        nes <- getsNES id
        drepState <- drepStateFromQuery drep nes
        drepState ^. drepExpiryL `shouldBe` addEpochInterval curEpochNo (EpochInterval drepActivity)
        let n = 3
        passNEpochsChecking n $
          isDRepExpired drep `shouldReturn` False
        expectNumDormantEpochs $ EpochNo (fromIntegral n)
        expectDRepExpiry drep $ addEpochInterval curEpochNo $ EpochInterval drepActivity
        expectActualDRepExpiry drep $
          addEpochInterval curEpochNo $
            EpochInterval (drepActivity + fromIntegral n)
        updateDRep drep
        expectDRepExpiry drep $ addEpochInterval curEpochNo $ EpochInterval drepActivity
        expectActualDRepExpiry drep $
          addEpochInterval curEpochNo $
            EpochInterval (drepActivity + fromIntegral n)
        expectNumDormantEpochs $ EpochNo (fromIntegral n)
        nes1 <- getsNES id
        drepState1 <- drepStateFromQuery drep nes1
        drepState1
          ^. drepExpiryL
            `shouldBe` addEpochInterval
              curEpochNo
              (EpochInterval (drepActivity + fromIntegral n))
        expectDRepExpiry drep $ addEpochInterval curEpochNo $ EpochInterval drepActivity
        passEpoch
        expectNumDormantEpochs $ EpochNo (fromIntegral n + 1)
        void submitParamChangeProposal
        expectNumDormantEpochs $ EpochNo 0
        nes2 <- getsNES id
        drepState2 <- drepStateFromQuery drep nes2
        let drepExpiry2 = addEpochInterval curEpochNo $ EpochInterval (drepActivity + fromIntegral n + 1)
        drepState2 ^. drepExpiryL `shouldBe` drepExpiry2
        expectActualDRepExpiry drep drepExpiry2
        passNEpochsChecking (fromIntegral drepActivity) $ do
          isDRepExpired drep `shouldReturn` False
        passEpoch
        isDRepExpired drep `shouldReturn` True
  describe "Committee members hot key pre-authorization" $ do
    it "authorized members not elected get removed in the next epoch" $ do
      whenPostBootstrap $ do
        c1 <- KeyHashObj <$> freshKeyHash
        submitGovAction_ $
          UpdateCommittee SNothing mempty (Map.singleton c1 (EpochNo 4321)) (1 %! 1)
        hk1 <- registerCommitteeHotKey c1
        expectQueryResult (Set.singleton c1) mempty mempty $
          [(c1, CommitteeMemberState (MemberAuthorized hk1) Unrecognized Nothing ToBeRemoved)]
        passEpoch
        expectQueryResult (Set.singleton c1) mempty mempty Map.empty

    it "members should remain authorized if authorized during the epoch after their election" $
      whenPostBootstrap $ do
        (drep, _, _) <- setupSingleDRep 1_000_000
        (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000

        c1 <- KeyHashObj <$> freshKeyHash
        c1Expiry <- flip addEpochInterval (EpochInterval 10) <$> getsNES nesELL

        initialCommitteeMembers <- getCommitteeMembers
        GovPurposeId gid <-
          electCommittee
            SNothing
            drep
            initialCommitteeMembers
            [(c1, c1Expiry)]
        submitYesVote_ (StakePoolVoter spoC) gid

        passEpoch
        hk1 <- registerCommitteeHotKey c1
        expectQueryResult (Set.singleton c1) mempty mempty $
          [(c1, CommitteeMemberState (MemberAuthorized hk1) Unrecognized Nothing ToBeEnacted)]
        passEpoch
        expectQueryResult (Set.singleton c1) mempty mempty $
          [(c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just c1Expiry) NoChangeExpected)]

  it "Committee queries" $ whenPostBootstrap $ do
    (drep, _, _) <- setupSingleDRep 1_000_000
    (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
    curEpochNo <- getsNES nesELL
    let offsetEpochInterval n = addEpochInterval curEpochNo (EpochInterval n)
    let cExpiry n =
          (,)
            <$> (KeyHashObj <$> freshKeyHash)
            <*> pure (offsetEpochInterval n)
    (c1, c1Expiry) <- cExpiry 12
    (c2, c2Expiry) <- cExpiry 2
    (c3, c3Expiry) <- cExpiry 7
    (c4, c4Expiry) <- cExpiry 5
    c5 <- KeyHashObj <$> freshKeyHash
    c6 <- KeyHashObj <$> freshKeyHash
    c7 <- KeyHashObj <$> freshKeyHash
    c8 <- KeyHashObj <$> freshKeyHash
    let newMembers =
          [ (c1, c1Expiry)
          , (c2, c2Expiry)
          , (c3, c3Expiry)
          , (c4, c4Expiry)
          ]
    initialMembers <- getCommitteeMembers

    ga1@(GovPurposeId gaid1) <-
      electCommittee
        SNothing
        drep
        initialMembers
        newMembers
    submitYesVote_ (StakePoolVoter spoC) gaid1

    expectMembers initialMembers
    passNEpochs 2 -- epoch 2
    expectMembers $ Map.keysSet newMembers
    -- members for which the expiration epoch is the current epoch are `ToBeExpired`
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState MemberNotAuthorized Active (Just c1Expiry) NoChangeExpected)
      , (c2, CommitteeMemberState MemberNotAuthorized Active (Just c2Expiry) ToBeExpired)
      , (c3, CommitteeMemberState MemberNotAuthorized Active (Just c3Expiry) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just c4Expiry) NoChangeExpected)
      ]
    -- hot cred status of members with registered hot keys becomes `MemberAuthorized`
    hk1 <- registerCommitteeHotKey c1
    hk2 <- registerCommitteeHotKey c2
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just c1Expiry) NoChangeExpected)
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Active (Just c2Expiry) ToBeExpired)
      , (c3, CommitteeMemberState MemberNotAuthorized Active (Just c3Expiry) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just c4Expiry) NoChangeExpected)
      ]
    expectQueryResult
      [c2, c3, c5]
      mempty
      [Active, Unrecognized]
      [ (c3, CommitteeMemberState MemberNotAuthorized Active (Just c3Expiry) NoChangeExpected)
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Active (Just c2Expiry) ToBeExpired)
      ]

    c3Anchor <- arbitrary
    _ <- resignCommitteeColdKey c3 (SJust c3Anchor)
    _ <-
      submitGovAction $
        UpdateCommittee
          (SJust ga1)
          mempty
          (Map.fromList [(c5, offsetEpochInterval 10), (c8, offsetEpochInterval 10)])
          (1 %! 1)
    hk5 <- registerCommitteeHotKey c5
    passTick
    -- hot cred status of resigned member becomes `Resigned`
    -- registering a hot key for a credential that's not part of the committee will yield `Unrecognized` member status
    -- and expected change of `ToBeRemoved`
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just c1Expiry) NoChangeExpected)
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Active (Just c2Expiry) ToBeExpired)
      , (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just c3Expiry) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just c4Expiry) NoChangeExpected)
      , (c5, CommitteeMemberState (MemberAuthorized hk5) Unrecognized Nothing ToBeRemoved)
      ]
    expectQueryResult
      [c2, c3, c5]
      [hk5]
      [Unrecognized]
      ( Map.singleton
          c5
          (CommitteeMemberState (MemberAuthorized hk5) Unrecognized Nothing ToBeRemoved)
      )

    passEpoch -- epoch 3
    -- the `Unrecognized` member gets removed from the query result
    -- the member which in the previous epoch was expected `ToBeEpired`, has now MemberStatus `Expired` and `NoChangeExpected`
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just c1Expiry) NoChangeExpected)
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Expired (Just c2Expiry) NoChangeExpected)
      , (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just c3Expiry) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just c4Expiry) NoChangeExpected)
      ]

    -- elect new committee to be: c1 (term extended ), c3 (no changes), c4 (term shortened, expiring next epoch), c6, c7 (new)
    let c1NewExpiry = offsetEpochInterval 13
        c4NewExpiry = offsetEpochInterval 4
        c6Expiry = offsetEpochInterval 6
        c7Expiry = offsetEpochInterval 7
    ga2@(GovPurposeId gaid2) <-
      electCommittee
        (SJust ga1)
        drep
        [c2]
        [ (c1, c1NewExpiry)
        , (c4, c4NewExpiry)
        , (c6, c6Expiry)
        , (c7, c7Expiry)
        ]
    submitYesVote_ (StakePoolVoter spoC) gaid2
    passEpoch -- epoch 4
    hk6 <- registerCommitteeHotKey c6
    hk8 <- registerCommitteeHotKey c8

    -- in the next epoch after the election, the old committee is still in place
    expectMembers [c1, c2, c3, c4]

    -- members that are not be part of the next committee are `ToBeRemoved`
    -- members that are part of both current and next committee have `NoChangeExpected` or `TermAdjusted`
    -- members that part of only next committee are `ToBeEnacted`
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just c1Expiry) (TermAdjusted c1NewExpiry))
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Expired (Just c2Expiry) ToBeRemoved)
      , (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just c3Expiry) NoChangeExpected)
      , -- though its term was adjusted, `ToBeExpired` takes precedence
        (c4, CommitteeMemberState MemberNotAuthorized Active (Just c4Expiry) ToBeExpired)
      , (c6, CommitteeMemberState (MemberAuthorized hk6) Unrecognized Nothing ToBeEnacted)
      , (c7, CommitteeMemberState MemberNotAuthorized Unrecognized Nothing ToBeEnacted)
      , (c8, CommitteeMemberState (MemberAuthorized hk8) Unrecognized Nothing ToBeRemoved)
      ]
    expectQueryResult
      [c1]
      mempty
      mempty
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just c1Expiry) (TermAdjusted c1NewExpiry))
      ]
    expectQueryResult
      [c2]
      [hk2]
      [Expired]
      ( Map.singleton
          c2
          (CommitteeMemberState (MemberAuthorized hk2) Expired (Just c2Expiry) ToBeRemoved)
      )

    passNEpochs 2 -- epoch 6
    -- the new committee is in place with the adjusted terms
    expectMembers [c1, c3, c4, c6, c7]
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just c1NewExpiry) NoChangeExpected)
      , (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just c3Expiry) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Expired (Just c4NewExpiry) NoChangeExpected)
      , (c6, CommitteeMemberState (MemberAuthorized hk6) Active (Just c6Expiry) ToBeExpired)
      , (c7, CommitteeMemberState MemberNotAuthorized Active (Just c7Expiry) NoChangeExpected)
      ]
    expectQueryResult
      Set.empty
      Set.empty
      [Unrecognized]
      Map.empty

    -- elect new committee to be:
    -- c4 (which is presently `Expired`, set a new term),
    -- c6 (which is presently `ToBeExpired`, set a new term)
    -- c7 (which will become `ToBeExpired` in the next epoch)
    -- c3 (which would become `ToBeExpired` in the next epoch, but set a new term)
    let c3NewExpiry = offsetEpochInterval 9
        c4NewNewExpiry = offsetEpochInterval 9
        c6NewExpiry = offsetEpochInterval 9
    GovPurposeId gaid3 <-
      electCommittee
        (SJust ga2)
        drep
        [c1]
        [ (c3, c3NewExpiry)
        , (c4, c4NewNewExpiry)
        , (c6, c6NewExpiry)
        ]
    submitYesVote_ (StakePoolVoter spoC) gaid3
    passEpoch -- epoch 7
    -- members whose term changed have next epoch change `TermAdjusted`
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just c1NewExpiry) ToBeRemoved)
      ,
        ( c3
        , CommitteeMemberState
            (MemberResigned (Just c3Anchor))
            Active
            (Just c3Expiry)
            (TermAdjusted c3NewExpiry)
        )
      ,
        ( c4
        , CommitteeMemberState MemberNotAuthorized Expired (Just c4NewExpiry) (TermAdjusted c4NewNewExpiry)
        )
      , (c6, CommitteeMemberState (MemberAuthorized hk6) Expired (Just c6Expiry) (TermAdjusted c6NewExpiry))
      , (c7, CommitteeMemberState MemberNotAuthorized Active (Just c7Expiry) ToBeExpired)
      ]
    passEpoch -- epoch 8
    expectMembers [c3, c4, c6, c7]
    expectNoFilterQueryResult
      [
        ( c3
        , CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just c3NewExpiry) NoChangeExpected
        )
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just c4NewNewExpiry) NoChangeExpected)
      , (c6, CommitteeMemberState (MemberAuthorized hk6) Active (Just c6NewExpiry) NoChangeExpected)
      , (c7, CommitteeMemberState MemberNotAuthorized Expired (Just c7Expiry) NoChangeExpected)
      ]
  where
    expectQueryResult ::
      HasCallStack =>
      Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) ->
      Set.Set (Credential 'HotCommitteeRole (EraCrypto era)) ->
      Set.Set MemberStatus ->
      Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) (CommitteeMemberState (EraCrypto era)) ->
      ImpTestM era ()
    expectQueryResult ckFilter hkFilter statusFilter expResult = do
      nes <- use impNESL
      let CommitteeMembersState {csCommittee} =
            queryCommitteeMembersState
              ckFilter
              hkFilter
              statusFilter
              nes
      impAnn "Expecting query result" $
        csCommittee `shouldBe` expResult

    expectNoFilterQueryResult ::
      HasCallStack =>
      Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) (CommitteeMemberState (EraCrypto era)) ->
      ImpTestM era ()
    expectNoFilterQueryResult =
      expectQueryResult mempty mempty mempty
