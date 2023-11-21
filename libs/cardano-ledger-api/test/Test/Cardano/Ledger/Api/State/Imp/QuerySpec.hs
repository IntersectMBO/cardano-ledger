{-# LANGUAGE DataKinds #-}
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
  ConwayGovState,
  EraGov (..),
  GovAction (..),
  GovActionPurpose (..),
  PrevGovActionId (..),
  Voter (DRepVoter),
  cgEnactStateL,
  ensCommitteeL,
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
  KeyHash,
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
spec =
  it "Committee queries" $ do
    setPParams
    drep <- setupSingleDRep
    c1 <- freshKeyHash
    c2 <- freshKeyHash
    c3 <- freshKeyHash
    c4 <- freshKeyHash
    c5 <- freshKeyHash
    c6 <- freshKeyHash
    c7 <- freshKeyHash
    c8 <- freshKeyHash
    let newMembers =
          Map.fromList
            [ (c1, EpochNo 12)
            , (c2, EpochNo 2)
            , (c3, EpochNo 11)
            , (c4, EpochNo 6)
            ]
    ga1 <-
      electCommittee
        SNothing
        drep
        Set.empty
        newMembers

    expectMembers Set.empty
    expectNoFilterQueryResult Nothing
    passEpoch >> passEpoch
    -- epoch 2
    expectMembers $ Map.keysSet newMembers
    expectNoFilterQueryResult $
      Just $
        Map.fromList
          [ (c1, CommitteeMemberState MemberNotAuthorized Active (Just 12) NoChangeExpected)
          , (c2, CommitteeMemberState MemberNotAuthorized Active (Just 2) ToBeExpired)
          , (c3, CommitteeMemberState MemberNotAuthorized Active (Just 11) NoChangeExpected)
          , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 6) NoChangeExpected)
          ]
    hk1 <- registerCommitteeHotKey c1
    hk2 <- registerCommitteeHotKey c2
    expectNoFilterQueryResult $
      Just $
        Map.fromList
          [ (c1, CommitteeMemberState (MemberAuthorized (KeyHashObj hk1)) Active (Just 12) NoChangeExpected)
          , (c2, CommitteeMemberState (MemberAuthorized (KeyHashObj hk2)) Active (Just 2) ToBeExpired)
          , (c3, CommitteeMemberState MemberNotAuthorized Active (Just 11) NoChangeExpected)
          , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 6) NoChangeExpected)
          ]
    expectQueryResult
      (Set.fromList [c2, c3, c5])
      mempty
      (Set.fromList [Active, Unrecognized])
      ( Just $
          Map.fromList
            [ (c3, CommitteeMemberState MemberNotAuthorized Active (Just 11) NoChangeExpected)
            , (c2, CommitteeMemberState (MemberAuthorized (KeyHashObj hk2)) Active (Just 2) ToBeExpired)
            ]
      )

    _ <- resignCommitteeColdKey c3
    hk5 <- registerCommitteeHotKey c5
    passTick
    expectNoFilterQueryResult $
      Just $
        Map.fromList
          [ (c1, CommitteeMemberState (MemberAuthorized (KeyHashObj hk1)) Active (Just 12) NoChangeExpected)
          , (c2, CommitteeMemberState (MemberAuthorized (KeyHashObj hk2)) Active (Just 2) ToBeExpired)
          , (c3, CommitteeMemberState MemberResigned Active (Just 11) NoChangeExpected)
          , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 6) NoChangeExpected)
          , (c5, CommitteeMemberState (MemberAuthorized (KeyHashObj hk5)) Unrecognized Nothing ToBeRemoved)
          ]
    expectQueryResult
      (Set.fromList [c2, c3, c5])
      (Set.singleton hk5)
      (Set.fromList [Unrecognized])
      ( Just $
          Map.singleton c5 (CommitteeMemberState (MemberAuthorized (KeyHashObj hk5)) Unrecognized Nothing ToBeRemoved)
      )

    _ <-
      electCommittee -- elect new committee to be: c1, c4, c6, c7
        (SJust ga1)
        drep
        (Set.fromList [c2, c3])
        ( Map.fromList
            [ (c6, EpochNo 10)
            , (c7, EpochNo 10)
            ]
        )
    passEpoch
    -- epoch 3
    hk6 <- registerCommitteeHotKey c6
    hk8 <- registerCommitteeHotKey c8

    expectMembers $ Set.fromList [c1, c2, c3, c4]
    expectNoFilterQueryResult $
      Just $
        Map.fromList
          [ (c1, CommitteeMemberState (MemberAuthorized (KeyHashObj hk1)) Active (Just 12) NoChangeExpected)
          , (c2, CommitteeMemberState (MemberAuthorized (KeyHashObj hk2)) Expired (Just 2) ToBeRemoved)
          , (c3, CommitteeMemberState MemberResigned Active (Just 11) ToBeRemoved)
          , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 6) NoChangeExpected)
          , (c6, CommitteeMemberState (MemberAuthorized (KeyHashObj hk6)) Unrecognized Nothing ToBeEnacted)
          , (c7, CommitteeMemberState MemberNotAuthorized Unrecognized Nothing ToBeEnacted)
          , (c8, CommitteeMemberState (MemberAuthorized (KeyHashObj hk8)) Unrecognized Nothing ToBeRemoved)
          ]
    expectQueryResult
      (Set.fromList [c2])
      (Set.singleton hk2)
      (Set.fromList [Expired])
      ( Just $
          Map.singleton c2 (CommitteeMemberState (MemberAuthorized (KeyHashObj hk2)) Expired (Just 2) ToBeRemoved)
      )

    passEpoch
    -- epoch 4
    expectMembers $ Set.fromList [c1, c4, c6, c7]
    expectNoFilterQueryResult $
      Just $
        Map.fromList
          [ (c1, CommitteeMemberState (MemberAuthorized (KeyHashObj hk1)) Active (Just 12) NoChangeExpected)
          , (c4, CommitteeMemberState MemberNotAuthorized Active (Just 6) NoChangeExpected)
          , (c6, CommitteeMemberState (MemberAuthorized (KeyHashObj hk6)) Active (Just 10) NoChangeExpected)
          , (c7, CommitteeMemberState MemberNotAuthorized Active (Just 10) NoChangeExpected)
          ]
    expectQueryResult
      Set.empty
      Set.empty
      (Set.fromList [Unrecognized])
      (Just Map.empty)
  where
    expectMembers ::
      HasCallStack => Set.Set (KeyHash 'ColdCommitteeRole (EraCrypto era)) -> ImpTestM era ()
    expectMembers expKhs = do
      committee <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
      let members = Map.keysSet $ foldMap' committeeMembers committee
      impAnn "Expecting committee members" $ members `shouldBe` Set.map KeyHashObj expKhs

    expectQueryResult ::
      HasCallStack =>
      Set.Set (KeyHash 'ColdCommitteeRole (EraCrypto era)) ->
      Set.Set (KeyHash 'HotCommitteeRole (EraCrypto era)) ->
      Set.Set MemberStatus ->
      Maybe (Map.Map (KeyHash 'ColdCommitteeRole (EraCrypto era)) (CommitteeMemberState (EraCrypto era))) ->
      ImpTestM era ()
    expectQueryResult ckFilter hkFilter statusFilter expResult = do
      nes <- use impNESL
      let res =
            queryCommitteeMembersState
              (Set.map KeyHashObj ckFilter)
              (Set.map KeyHashObj hkFilter)
              statusFilter
              nes
      impAnn "Expecting query result" $
        csCommittee <$> res `shouldBe` Map.mapKeys KeyHashObj <$> expResult

    expectNoFilterQueryResult ::
      HasCallStack =>
      Maybe (Map.Map (KeyHash 'ColdCommitteeRole (EraCrypto era)) (CommitteeMemberState (EraCrypto era))) ->
      ImpTestM era ()
    expectNoFilterQueryResult =
      expectQueryResult mempty mempty mempty

electCommittee ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)) ->
  KeyHash 'DRepRole (EraCrypto era) ->
  Set.Set (KeyHash 'ColdCommitteeRole (EraCrypto era)) ->
  Map.Map (KeyHash 'ColdCommitteeRole (EraCrypto era)) EpochNo ->
  ImpTestM era (PrevGovActionId 'CommitteePurpose (EraCrypto era))
electCommittee prevGovId drep toRemove toAdd = do
  let
    committeeAction =
      UpdateCommittee
        prevGovId
        (Set.map KeyHashObj toRemove)
        (Map.mapKeys KeyHashObj toAdd)
        (1 %! 2)
  gaidCommitteeProp <- submitGovAction committeeAction
  submitYesVote_ (DRepVoter $ KeyHashObj drep) gaidCommitteeProp
  pure (PrevGovActionId gaidCommitteeProp)

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
      & ppCommitteeMaxTermLengthL .~ 10
      & ppGovActionLifetimeL .~ 2
      & ppGovActionDepositL .~ Coin 123
