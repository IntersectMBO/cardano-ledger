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
    \(Committee cm _) (CommitteeState comStateMembers') nextComMembers' noFilterResult -> do
      let comMembers = Map.keysSet cm
      let comStateMembers = Map.keysSet comStateMembers'
      let nextComMembers = Map.keysSet nextComMembers'

      filterNext ToBeEnacted noFilterResult
        `shouldSatisfy` (\res -> Map.keysSet res == nextComMembers `Set.difference` comMembers)

      filterNext ToBeRemoved noFilterResult
        `shouldSatisfy` (\res -> Map.keysSet res == (comMembers `Set.union` comStateMembers) `Set.difference` nextComMembers)

      filterNext NoChangeExpected noFilterResult
        `shouldSatisfy` (\res -> Map.keysSet res == (comMembers `Set.intersection` nextComMembers))
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
genCommittee = frequency [(1, pure Nothing), (9, Just <$> arbitrary)]

genRelevantCommitteeState ::
  forall era.
  EraTxOut era =>
  Maybe (Committee era) ->
  Maybe (Committee era) ->
  Gen (CommitteeState era)
genRelevantCommitteeState maybeCm nextCom = do
  CommitteeState comStateMembers <- arbitrary @(CommitteeState era)
  let comMembers = Map.keysSet (foldMap' committeeMembers maybeCm)
      nextComMembers = Map.keysSet (foldMap' committeeMembers nextCom)
      third = Set.size comMembers `div` 3
      chosen = Set.toList $ Set.take third comMembers
      nextChosen = Set.toList $ Set.take third nextComMembers
      x =
        Map.fromList $
          zipWith
            (\k1 (_, v2) -> (k1, v2))
            (chosen <> nextChosen)
            (Map.assocs comStateMembers)
  pure $ CommitteeState $ x `Map.union` Map.drop (length chosen + length nextChosen) comStateMembers

genNextCommittee ::
  forall era.
  EraTxOut era =>
  Maybe (Committee era) ->
  Gen (Maybe (Committee era))
genNextCommittee maybeCm =
  oneof [pure Nothing, Just <$> genCom]
  where
    genCom = do
      let comMembers = foldMap' committeeMembers maybeCm
      new <- arbitrary @(Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
      q <- arbitrary
      retainSize <- choose (0, Map.size comMembers)
      pure $ Committee (Map.union new (Map.take retainSize comMembers)) q

genRelevantColdCredsFilter ::
  forall era.
  EraTxOut era =>
  Maybe (Committee era) ->
  CommitteeState era ->
  Gen (Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)))
genRelevantColdCredsFilter maybeCm (CommitteeState comStateMembers) = do
  flt <- arbitrary
  s <- choose (0, Set.size flt)
  let cm1 = Map.keysSet $ Map.take (s `div` 2) (foldMap' committeeMembers maybeCm)
  let cm2 = Map.keysSet $ Map.take (s `div` 2) comStateMembers
  pure $ Set.unions [Set.drop s flt, cm1, cm2]

genRelevantHotCredsFilter ::
  forall era.
  EraTxOut era =>
  CommitteeState era ->
  Gen (Set.Set (Credential 'HotCommitteeRole (EraCrypto era)))
genRelevantHotCredsFilter (CommitteeState comStateMembers) = do
  flt <- arbitrary
  s <- choose (0, Set.size flt)
  let cm = Set.fromList $ Map.elems (Map.mapMaybe id (Map.take s comStateMembers))
  pure $ Set.drop s flt `Set.union` cm

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
