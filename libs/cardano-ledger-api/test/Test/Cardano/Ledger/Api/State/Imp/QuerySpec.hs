{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Api.State.Imp.QuerySpec where

import Cardano.Ledger.Api.State.Query (
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),
  queryCommitteeMembersState,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  ConwayEraGov (..),
  ConwayGovState,
  EraGov (..),
 )
import Cardano.Ledger.Conway.PParams (
  dvtCommitteeNoConfidence,
  dvtCommitteeNormal,
  dvtUpdateToConstitution,
  ppCommitteeMaxTermLengthL,
  ppDRepVotingThresholdsL,
  ppGovActionDepositL,
  ppGovActionLifetimeL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Keys (
  KeyRole (..),
 )
import Cardano.Ledger.Shelley.LedgerState
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Lens.Micro.Mtl
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  , GovState era ~ ConwayGovState era
  ) =>
  SpecWith (ImpTestState era)
spec = do
  describe "Committee members hot key pre-authorization" $ do
    it "authorized members not elected get removed in the next epoch" $ do
      setPParams

      c1 <- KeyHashObj <$> freshKeyHash
      hk1 <- registerCommitteeHotKey c1
      expectQueryResult (Set.singleton c1) mempty mempty $
        [(c1, CommitteeMemberState (MemberAuthorized hk1) Unrecognized Nothing ToBeRemoved)]
      passEpoch
      expectQueryResult (Set.singleton c1) mempty mempty Map.empty

    it "members should remain authorized if authorized during the epoch after their election" $ do
      setPParams
      (drep, _, _) <- setupSingleDRep 1_000_000

      c1 <- KeyHashObj <$> freshKeyHash
      _ <- electCommittee SNothing drep Set.empty [(c1, EpochNo 12)]
      passEpoch
      hk1 <- registerCommitteeHotKey c1
      expectQueryResult (Set.singleton c1) mempty mempty $
        [(c1, CommitteeMemberState (MemberAuthorized hk1) Unrecognized Nothing ToBeEnacted)]
      passEpoch
      expectQueryResult (Set.singleton c1) mempty mempty $
        [(c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just 12) NoChangeExpected)]

  it "Committee queries" $ do
    setPParams
    (drep, _, _) <- setupSingleDRep 1_000_000
    c1 <- KeyHashObj <$> freshKeyHash
    c2 <- KeyHashObj <$> freshKeyHash
    c3 <- KeyHashObj <$> freshKeyHash
    c4 <- KeyHashObj <$> freshKeyHash
    c5 <- KeyHashObj <$> freshKeyHash
    c6 <- KeyHashObj <$> freshKeyHash
    c7 <- KeyHashObj <$> freshKeyHash
    c8 <- KeyHashObj <$> freshKeyHash
    let newMembers =
          [ (c1, EpochNo 12)
          , (c2, EpochNo 2)
          , (c3, EpochNo 7)
          , (c4, EpochNo 5)
          ]
    initialMembers <- getCommitteeMembers

    ga1 <-
      electCommittee
        SNothing
        drep
        initialMembers
        newMembers

    expectMembers initialMembers
    passNEpochs 2 -- epoch 2
    expectMembers $ Map.keysSet newMembers
    -- members for which the expiration epoch is the current epoch are `ToBeExpired`
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState MemberNotAuthorized Active (Just 12) NoChangeExpected)
      , (c2, CommitteeMemberState MemberNotAuthorized Active (Just 2) ToBeExpired)
      , (c3, CommitteeMemberState MemberNotAuthorized Active (Just 7) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 5) NoChangeExpected)
      ]
    -- hot cred status of members with registered hot keys becomes `MemberAuthorized`
    hk1 <- registerCommitteeHotKey c1
    hk2 <- registerCommitteeHotKey c2
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just 12) NoChangeExpected)
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Active (Just 2) ToBeExpired)
      , (c3, CommitteeMemberState MemberNotAuthorized Active (Just 7) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 5) NoChangeExpected)
      ]
    expectQueryResult
      [c2, c3, c5]
      mempty
      [Active, Unrecognized]
      [ (c3, CommitteeMemberState MemberNotAuthorized Active (Just 7) NoChangeExpected)
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Active (Just 2) ToBeExpired)
      ]

    c3Anchor <- arbitrary
    _ <- resignCommitteeColdKey c3 (SJust c3Anchor)
    hk5 <- registerCommitteeHotKey c5
    passTick
    -- hot cred status of resigned member becomes `Resigned`
    -- registering a hot key for a credential that's not part of the committee will yield `Unrecognized` member status
    -- and expected change of `ToBeRemoved`
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just 12) NoChangeExpected)
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Active (Just 2) ToBeExpired)
      , (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just 7) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 5) NoChangeExpected)
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
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just 12) NoChangeExpected)
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Expired (Just 2) NoChangeExpected)
      , (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just 7) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 5) NoChangeExpected)
      ]

    -- elect new committee to be: c1 (term extended ), c3 (no changes), c4 (term shortened, expiring next epoch), c6, c7 (new)
    ga2 <-
      electCommittee
        (SJust ga1)
        drep
        [c2]
        [ (c1, 13)
        , (c4, 4)
        , (c6, 6)
        , (c7, 7)
        ]
    passEpoch -- epoch 4
    hk6 <- registerCommitteeHotKey c6
    hk8 <- registerCommitteeHotKey c8

    -- in the next epoch after the election, the old committee is still in place
    expectMembers [c1, c2, c3, c4]
    -- members that are not be part of the next committee are `ToBeRemoved`
    -- members that are part of both current and next committee have `NoChangeExpected` or `TermAdjusted`
    -- members that part of only next committee are `ToBeEnacted`
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just 12) (TermAdjusted 13))
      , (c2, CommitteeMemberState (MemberAuthorized hk2) Expired (Just 2) ToBeRemoved)
      , (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just 7) NoChangeExpected)
      , -- though its term was adjusted, `ToBeExpired` takes precedence
        (c4, CommitteeMemberState MemberNotAuthorized Active (Just 5) ToBeExpired)
      , (c6, CommitteeMemberState (MemberAuthorized hk6) Unrecognized Nothing ToBeEnacted)
      , (c7, CommitteeMemberState MemberNotAuthorized Unrecognized Nothing ToBeEnacted)
      , (c8, CommitteeMemberState (MemberAuthorized hk8) Unrecognized Nothing ToBeRemoved)
      ]
    expectQueryResult
      [c2]
      [hk2]
      [Expired]
      ( Map.singleton
          c2
          (CommitteeMemberState (MemberAuthorized hk2) Expired (Just 2) ToBeRemoved)
      )

    passNEpochs 2 -- epoch 6
    -- the new committee is in place with the adjusted terms
    expectMembers [c1, c3, c4, c6, c7]
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just 13) NoChangeExpected)
      , (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just 7) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Expired (Just 4) NoChangeExpected)
      , (c6, CommitteeMemberState (MemberAuthorized hk6) Active (Just 6) ToBeExpired)
      , (c7, CommitteeMemberState MemberNotAuthorized Active (Just 7) NoChangeExpected)
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
    _ <-
      electCommittee
        (SJust ga2)
        drep
        [c1]
        [ (c3, 9)
        , (c4, 9)
        , (c6, 9)
        ]
    passEpoch -- epoch 7
    -- members whose term changed have next epoch change `TermAdjusted`
    expectNoFilterQueryResult
      [ (c1, CommitteeMemberState (MemberAuthorized hk1) Active (Just 13) ToBeRemoved)
      , (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just 7) (TermAdjusted 9))
      , (c4, CommitteeMemberState MemberNotAuthorized Expired (Just 4) (TermAdjusted 9))
      , (c6, CommitteeMemberState (MemberAuthorized hk6) Expired (Just 6) (TermAdjusted 9))
      , (c7, CommitteeMemberState MemberNotAuthorized Active (Just 7) ToBeExpired)
      ]
    passEpoch -- epoch 8
    expectMembers [c3, c4, c6, c7]
    expectNoFilterQueryResult
      [ (c3, CommitteeMemberState (MemberResigned (Just c3Anchor)) Active (Just 9) NoChangeExpected)
      , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 9) NoChangeExpected)
      , (c6, CommitteeMemberState (MemberAuthorized hk6) Active (Just 9) NoChangeExpected)
      , (c7, CommitteeMemberState MemberNotAuthorized Expired (Just 7) NoChangeExpected)
      ]
  where
    expectMembers ::
      HasCallStack => Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) -> ImpTestM era ()
    expectMembers expKhs = do
      committee <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
      let members = Map.keysSet $ foldMap' committeeMembers committee
      impAnn "Expecting committee members" $ members `shouldBe` expKhs

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

setPParams ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  ImpTestM era ()
setPParams = do
  modifyPParams $ \pp ->
    pp
      & ppDRepVotingThresholdsL
        .~ def
          { dvtCommitteeNormal = 1 %! 1
          , dvtCommitteeNoConfidence = 1 %! 2
          , dvtUpdateToConstitution = 1 %! 2
          }
      & ppCommitteeMaxTermLengthL .~ EpochInterval 100
      & ppGovActionLifetimeL .~ EpochInterval 2
      & ppGovActionDepositL .~ Coin 123
