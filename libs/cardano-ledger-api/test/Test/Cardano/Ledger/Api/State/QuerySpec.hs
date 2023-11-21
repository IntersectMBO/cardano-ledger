{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Api.State.QuerySpec (spec) where

import Cardano.Ledger.Api.State.Query (
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),
  filterStakePoolDelegsAndRewards,
  queryCommitteeMembersState,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  ConwayEraGov,
  ConwayGovState,
  DRepPulsingState (..),
  RatifyState (..),
  cgEnactStateL,
  ensCommitteeL,
  newEpochStateDRepPulsingStateL,
  rsEnactStateL,
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UMap (UMap)
import Data.Default.Class (Default (..))
import Data.Foldable (foldMap')
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Api.Arbitrary ()
import Test.Cardano.Ledger.Api.State.Query
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary (genValidUMapWithCreds)
import Test.Cardano.Ledger.Shelley.Arbitrary ()

spec :: Spec
spec = do
  describe "GetFilteredDelegationsAndRewardAccounts" $ do
    prop "filterStakePoolDelegsAndRewards same as getFilteredDelegationsAndRewardAccounts" $
      forAll genValidUMapWithCreds $ \(umap :: UMap StandardCrypto, creds) ->
        filterStakePoolDelegsAndRewards umap creds
          `shouldBe` getFilteredDelegationsAndRewardAccounts umap creds

  describe "GetCommitteeMembersState" $ do
    committeeMembersStateSpec @Conway

committeeMembersStateSpec ::
  forall era.
  ( ConwayEraGov era
  , EraTxOut era
  , Default (EpochState era)
  , Default (StashedAVVMAddresses era)
  , GovState era ~ ConwayGovState era
  ) =>
  Spec
committeeMembersStateSpec =
  prop "CommitteeMembersState Query" $ \statusFilter -> do
    forAll genCommittee $ \committee ->
      -- half of the committee members in the next epoch will overlap with the current ones
      forAll (genNextCommittee @era committee) $ \nextCommittee ->
        -- replace some arbitrary number of cold keys from the committeeState with the
        -- ones from the committee so we can have Active members
        forAll (genRelevantCommitteeState @era committee nextCommittee) $ \committeeState -> do
          let nes =
                defNewEpochState
                  & nesEpochStateL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
                    .~ committeeState
                  & newEpochStateDRepPulsingStateL .~ DRComplete def nextRatifyState
                  & nesEpochStateL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
                    .~ maybeToStrictMaybe committee
              nextRatifyState =
                (def @(RatifyState era))
                  & rsEnactStateL . ensCommitteeL .~ maybeToStrictMaybe nextCommittee
              defNewEpochState = NewEpochState @era (EpochNo 0) (BlocksMade def) (BlocksMade def) def def (PoolDistr def) def
          -- replace some cold and hot keys from the filter with known ones from both
          -- committee and committeeState
          forAll (genRelevantColdCredsFilter committee committeeState) $ \ckFilter ->
            forAll (genRelevantHotCredsFilter committeeState) $ \hkFilter -> do
              propEmpty nes
              propComplete nes
              propAuthorized nes
              propActiveAuthorized nes
              propNotAuthorized nes
              propResigned nes
              propUnrecognized nes
              propNextEpoch nes
              propNoExpiration nes
              propFilters ckFilter hkFilter statusFilter nes

propEmpty ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propEmpty nes = do
  withCommitteeInfo nes $
    \(Committee comMembers _) (CommitteeState comStateMembers) nextComMembers noFilterResult -> do
      Map.null (csCommittee noFilterResult)
        `shouldBe` (Map.null comMembers && Map.null comStateMembers && Map.null nextComMembers)

propComplete ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propComplete nes = do
  withCommitteeInfo nes $
    \(Committee comMembers _) (CommitteeState comStateMembers) nextComMembers noFilterResult -> do
      -- if a credential appears in either Committee or CommitteeState, it should appear
      -- in the result
      Set.unions [Map.keysSet comMembers, Map.keysSet nextComMembers, Map.keysSet comStateMembers]
        `shouldBe` Map.keysSet (csCommittee noFilterResult)

propNotAuthorized ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propNotAuthorized nes = do
  withCommitteeInfo nes $
    \_ (CommitteeState comStateMembers) _ noFilterResult -> do
      let notAuthorized =
            Map.filter
              ( \case
                  CommitteeMemberState MemberNotAuthorized _ _ _ -> True
                  _ -> False
              )
              (csCommittee noFilterResult)
      -- if the member is NotAuthorized, it should not have an associated hot credential in the committeeState
      Map.intersection comStateMembers notAuthorized `shouldBe` Map.empty

propAuthorized ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propAuthorized nes = do
  withCommitteeInfo nes $
    \_ (CommitteeState comStateMembers) _ noFilterResult -> do
      let ckHk =
            Map.mapMaybe
              ( \case
                  CommitteeMemberState (MemberAuthorized hk) _ _ _ -> Just hk
                  _ -> Nothing
              )
              (csCommittee noFilterResult)
      -- if the member is Authorized, it should appear in the commiteeState
      Map.mapMaybe id comStateMembers `shouldBe` ckHk

propResigned ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propResigned nes = do
  withCommitteeInfo nes $
    \_ (CommitteeState comStateMembers) _ noFilterResult -> do
      let resigned =
            Map.filter
              ( \case
                  CommitteeMemberState MemberResigned _ _ _ -> True
                  _ -> False
              )
              (csCommittee noFilterResult)
      -- if the member is Resignd, it should appear in the commiteeState as Nothing
      Map.keysSet (Map.filter isNothing comStateMembers) `shouldBe` Map.keysSet resigned

propUnrecognized ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propUnrecognized nes = do
  withCommitteeInfo nes $
    \(Committee comMembers _) (CommitteeState comStateMembers) nextComMembers' noFilterResult -> do
      let unrecognized =
            Map.filter
              ( \case
                  CommitteeMemberState _ Unrecognized _ _ -> True
                  _ -> False
              )
              (csCommittee noFilterResult)
      let nextComMembers = Map.keysSet nextComMembers'
      -- if the member is Unrecognized, it should not be in the committe, but it should be
      -- in the committeeState or in the nextCommittee
      Map.intersection comMembers unrecognized `shouldBe` Map.empty
      Map.keysSet unrecognized
        `shouldSatisfy` (`Set.isSubsetOf` (Map.keysSet comStateMembers `Set.union` nextComMembers))
      -- all Unrecognized members will be either enacted or removed in the next epoch
      Set.fromList (cmsNextEpochChange <$> Map.elems unrecognized)
        `shouldSatisfy` (`Set.isSubsetOf` Set.fromList [ToBeEnacted, ToBeRemoved])
      Map.keysSet (Map.filter (\x -> cmsNextEpochChange x == ToBeEnacted) unrecognized)
        `shouldSatisfy` (`Set.isSubsetOf` nextComMembers)
      Map.keysSet (Map.filter (\x -> cmsNextEpochChange x == ToBeRemoved) unrecognized)
        `shouldSatisfy` (\s -> Set.null s || not (s `Set.isSubsetOf` nextComMembers))

propActiveAuthorized ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propActiveAuthorized nes = do
  withCommitteeInfo nes $
    \(Committee comMembers comQuorum) (CommitteeState comStateMembers) _ noFilterResult -> do
      let activeAuthorized =
            Map.mapMaybe
              ( \case
                  CommitteeMemberState (MemberAuthorized hk) Active _ _ -> Just hk
                  _ -> Nothing
              )
              (csCommittee noFilterResult)
      let epochNo = nes ^. nesELL

      -- if a member is active and authorized, then it should be:
      --   - in Committee and not expired
      --   - in CommitteeState, not empty
      Map.keysSet activeAuthorized
        `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet comMembers)
      Map.keysSet activeAuthorized
        `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet comStateMembers)
      Map.intersection comMembers activeAuthorized
        `shouldSatisfy` all (>= epochNo)
      Map.intersection comStateMembers activeAuthorized
        `shouldSatisfy` all isJust
      csEpochNo noFilterResult `shouldBe` epochNo
      csQuorum noFilterResult `shouldBe` comQuorum

propFilters ::
  forall era.
  EraGov era =>
  Set (Credential 'ColdCommitteeRole (EraCrypto era)) ->
  Set (Credential 'HotCommitteeRole (EraCrypto era)) ->
  Set MemberStatus ->
  NewEpochState era ->
  Expectation
propFilters ckFilter hkFilter statusFilter nes = do
  let qRes = queryCommitteeMembersState @era ckFilter hkFilter statusFilter nes
  forM_ qRes $ \(CommitteeMembersState result _ _) -> do
    let allCks = Map.keysSet result
    let (allHks, allMemberStatuses) =
          foldMap'
            ( \case
                CommitteeMemberState (MemberAuthorized hk) ms _ _ -> (Set.singleton hk, Set.singleton ms)
                CommitteeMemberState _ ms _ _ -> (Set.empty, Set.singleton ms)
            )
            result
    unless (Set.null ckFilter) $
      result `shouldSatisfy` const (allCks `Set.isSubsetOf` ckFilter)
    unless (Set.null hkFilter) $
      result `shouldSatisfy` const (allHks `Set.isSubsetOf` hkFilter)
    unless (Set.null statusFilter) $
      result `shouldSatisfy` const (allMemberStatuses `Set.isSubsetOf` statusFilter)

propNextEpoch ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propNextEpoch nes = do
  withCommitteeInfo nes $
    \(Committee comMembers' _) (CommitteeState comStateMembers') nextComMembers' noFilterResult -> do
      let comMembers = Map.keysSet comMembers'
      let comStateMembers = Map.keysSet comStateMembers'
      let nextComMembers = Map.keysSet nextComMembers'

      filterNext ToBeEnacted noFilterResult
        `shouldSatisfy` (\res -> Map.keysSet res == nextComMembers `Set.difference` comMembers)

      filterNext ToBeRemoved noFilterResult
        `shouldSatisfy` (\res -> Map.keysSet res == (comMembers `Set.union` comStateMembers) `Set.difference` nextComMembers)

      -- members who are both in current and nextCommittee are either ToBeExpired or NoChangeExpected
      Map.keysSet (filterNext NoChangeExpected noFilterResult)
        `Set.union` Map.keysSet (filterNext ToBeExpired noFilterResult)
        `shouldSatisfy` (== (comMembers `Set.intersection` nextComMembers))

      let currentEpoch = csEpochNo noFilterResult
      let expiring =
            Map.keysSet $
              Map.union
                (Map.filter (== currentEpoch) comMembers')
                (Map.filter (== currentEpoch) nextComMembers')

      -- members ToBeExpired have the expiry set to currentEpoch, either in the current committee or in the next one
      Map.keysSet (filterNext ToBeExpired noFilterResult)
        `shouldSatisfy` (`Set.isSubsetOf` expiring)

      cmsExpiration
        <$> filterNext NoChangeExpected noFilterResult
          `shouldSatisfy` all (all (>= currentEpoch + 1))
  where
    filterNext nextEpochChange cms =
      Map.filter
        ( \case
            CommitteeMemberState _ _ _ nextEpochChange' ->
              nextEpochChange == nextEpochChange'
        )
        (csCommittee cms)

propNoExpiration ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propNoExpiration nes =
  withCommitteeInfo nes $
    \_ _ _ noFilterResult -> do
      let noExpiration = Map.filter (isNothing . cmsExpiration) (csCommittee noFilterResult)
      unless (Map.null noExpiration) $
        -- only Unrecognized members should have no expiration
        Set.fromList (cmsStatus <$> Map.elems noExpiration) `shouldBe` Set.singleton Unrecognized

genCommittee ::
  forall era.
  Era era =>
  Gen (Maybe (Committee era))
genCommittee = frequency [(1, pure Nothing), (9, Just <$> genCommittee' arbitrary)]

genRelevantCommitteeState ::
  forall era.
  EraTxOut era =>
  Maybe (Committee era) ->
  Maybe (Committee era) ->
  Gen (CommitteeState era)
genRelevantCommitteeState maybeCm maybeNextCm = do
  membersRetaining <-
    (++)
      <$> genMembersRetaining maybeCm
      <*> genMembersRetaining maybeNextCm
  pairs <- zip membersRetaining <$> arbitrary
  pure $ CommitteeState $ Map.fromList pairs

genNextCommittee ::
  forall era.
  EraTxOut era =>
  Maybe (Committee era) ->
  Gen (Maybe (Committee era))
genNextCommittee maybeCm =
  oneof [pure Nothing, Just <$> genCommittee' (genMembersRetaining maybeCm)]

genCommittee' :: Gen [Credential 'ColdCommitteeRole (EraCrypto era)] -> Gen (Committee era)
genCommittee' genCreds = do
  creds <- genCreds
  m <- zip creds <$> listOf (EpochNo <$> chooseBoundedIntegral (0, 20))
  Committee (Map.fromList m) <$> arbitrary

genRelevantColdCredsFilter ::
  forall era.
  EraTxOut era =>
  Maybe (Committee era) ->
  CommitteeState era ->
  Gen (Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)))
genRelevantColdCredsFilter maybeCm (CommitteeState comStateMembers) = do
  creds <-
    (++)
      <$> genMembersRetaining maybeCm
      <*> genRetaining (Map.keys comStateMembers)
  pure $ Set.fromList creds

genRelevantHotCredsFilter ::
  forall era.
  EraTxOut era =>
  CommitteeState era ->
  Gen (Set.Set (Credential 'HotCommitteeRole (EraCrypto era)))
genRelevantHotCredsFilter (CommitteeState comStateMembers) =
  Set.fromList <$> genRetaining (Map.elems (Map.mapMaybe id comStateMembers))

genMembersRetaining ::
  forall era.
  EraTxOut era =>
  Maybe (Committee era) ->
  Gen [Credential 'ColdCommitteeRole (EraCrypto era)]
genMembersRetaining maybeCm =
  genRetaining $ Map.keys $ foldMap' committeeMembers maybeCm

genRetaining :: Arbitrary a => [a] -> Gen [a]
genRetaining ret = do
  retSize <- choose (0, length ret)
  new <- arbitrary
  pure $ new <> take retSize ret

withCommitteeInfo ::
  EraGov era =>
  NewEpochState era ->
  ( Committee era ->
    CommitteeState era ->
    Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo -> -- next epoch committee members
    CommitteeMembersState (EraCrypto era) ->
    Expectation
  ) ->
  Expectation
withCommitteeInfo nes expectation =
  case committeeInfo nes of
    Nothing -> queryCommitteeMembersStateNoFilters nes `shouldBe` Nothing
    Just (committee, comState, nextComMembers) ->
      case queryCommitteeMembersStateNoFilters nes of
        Just noFilterQueryResult ->
          expectation committee comState nextComMembers noFilterQueryResult
        Nothing ->
          expectationFailure "Expected queryCommitteeMembersState to return a Just value"

committeeInfo ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Maybe
    ( Committee era
    , CommitteeState era
    , Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo
    )
committeeInfo nes = do
  (comMembers, comQurum) <-
    getCommitteeMembers (nes ^. nesEpochStateL . esLStateL . lsUTxOStateL . utxosGovStateL)
  let ledgerState = nes ^. nesEpochStateL . esLStateL
  let nextCommitteeMembers =
        maybe
          Map.empty
          fst
          $ getNextEpochCommitteeMembers (ledgerState ^. lsUTxOStateL . utxosGovStateL)
  let comState = ledgerState ^. lsCertStateL . certVStateL . vsCommitteeStateL
  pure (Committee comMembers comQurum, comState, nextCommitteeMembers)

queryCommitteeMembersStateNoFilters ::
  forall era. EraGov era => NewEpochState era -> Maybe (CommitteeMembersState (EraCrypto era))
queryCommitteeMembersStateNoFilters =
  queryCommitteeMembersState @era
    Set.empty
    Set.empty
    Set.empty
