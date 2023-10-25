{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.State.QuerySpec (spec) where

import Cardano.Ledger.Api.State.Query (
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  filterStakePoolDelegsAndRewards,
  queryCommitteeMembersState,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UMap (UMap)
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
  ( EraTxOut era
  , EraGov era
  , Arbitrary (PParams era)
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (GovState era)
  , Arbitrary (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  ) =>
  Spec
committeeMembersStateSpec =
  prop "CommitteeMembersState Query" $ \nes' ckFilter' hkFilter' statusFilter -> do
    case committeeInfo nes' of
      Nothing -> property ()
      Just (comMembers, _, comStateMembers) -> do
        -- replace some arbitrary number of cold keys from the committeeState with the
        -- ones from the committee so we can have Active members
        forAll (genCommonMembers comMembers comStateMembers) $ \newCommitteeState ->
          let nes :: NewEpochState era
              nes =
                nes'
                  & nesEpochStateL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
                    .~ newCommitteeState
           in -- replace some cold and hot keys from the filter with known ones from both
              -- committee and committeeState
              forAll (genRelevantColdCredsFilter ckFilter' comMembers comStateMembers) $ \ckFilter ->
                forAll (genRelevantHotCredsFilter hkFilter' comStateMembers) $ \hkFilter -> do
                  propEmpty nes
                  propComplete nes
                  propAuthorized nes
                  propActiveAuthorized nes
                  propNotAuthorized nes
                  propResigned nes
                  propUnrecognized nes
                  propFilters ckFilter hkFilter statusFilter nes

propEmpty ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propEmpty nes = do
  withCommitteeInfo nes $ \comMembers _ comStateMembers noFilterResult -> do
    Map.null (csCommittee noFilterResult)
      `shouldBe` (Map.null comMembers && Map.null comStateMembers)

propComplete ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propComplete nes = do
  withCommitteeInfo nes $ \comMembers _ comStateMembers noFilterResult -> do
    -- if a credential appears in either Committee or CommitteeState, it should appear
    -- in the result
    Map.keysSet comMembers
      `Set.union` Map.keysSet comStateMembers
      `shouldBe` Map.keysSet (csCommittee noFilterResult)

propNotAuthorized ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propNotAuthorized nes = do
  withCommitteeInfo nes $ \_comMembers _ comStateMembers noFilterResult -> do
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
  withCommitteeInfo nes $ \_comMembers _ comStateMembers noFilterResult -> do
    let ckHk =
          Map.foldMapWithKey
            ( \ck cms ->
                case cms of
                  CommitteeMemberState (MemberAuthorized hk) _ _ _ -> [(ck, Just hk)]
                  _ -> []
            )
            (csCommittee noFilterResult)
    -- if the member is Authorized, it should appear in the commiteeState
    Map.filter isJust comStateMembers `shouldBe` Map.fromList ckHk

propResigned ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propResigned nes = do
  withCommitteeInfo nes $ \_comMembers _ comStateMembers noFilterResult -> do
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
  withCommitteeInfo nes $ \comMembers _ comStateMembers noFilterResult -> do
    let unrecognized =
          Map.filter
            ( \case
                CommitteeMemberState _ Unrecognized _ _ -> True
                _ -> False
            )
            (csCommittee noFilterResult)
    -- if the member is Unrecognized, it should not be in the committe, but it should be
    -- in the committeeState
    Map.intersection comMembers unrecognized `shouldBe` Map.empty
    Map.keysSet unrecognized `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet comStateMembers)

propActiveAuthorized ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Expectation
propActiveAuthorized nes = do
  withCommitteeInfo nes $ \comMembers comQuorum comStateMembers noFilterResult -> do
    let activeAuthorized =
          Map.fromList $
            Map.foldMapWithKey
              ( \ck cms ->
                  case cms of
                    CommitteeMemberState (MemberAuthorized hk) Active _ _ -> [(ck, hk)]
                    _ -> []
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

genCommonMembers ::
  Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo ->
  Map.Map
    (Credential 'ColdCommitteeRole (EraCrypto era))
    (Maybe (Credential 'HotCommitteeRole (EraCrypto era))) ->
  Gen (CommitteeState era)
genCommonMembers comMembers comStateMembers = do
  s <- choose (0, Map.size comMembers)
  let chosen = Map.take s comMembers
  let x =
        Map.fromList $
          zipWith
            (\(k1, _) (_, v2) -> (k1, v2))
            (Map.assocs chosen)
            (Map.assocs comStateMembers)
  pure $ CommitteeState $ x `Map.union` (Map.drop s comStateMembers)

genRelevantColdCredsFilter ::
  Set.Set (Credential 'ColdCommitteeRole c) ->
  Map.Map (Credential 'ColdCommitteeRole c) EpochNo ->
  Map.Map (Credential 'ColdCommitteeRole c) (Maybe (Credential 'HotCommitteeRole c)) ->
  Gen (Set.Set (Credential 'ColdCommitteeRole c))
genRelevantColdCredsFilter flt comMembers comStateMembers = do
  s <- choose (0, Set.size flt)
  let cm1 = Map.keysSet $ Map.take (s `div` 2) comMembers
  let cm2 = Map.keysSet $ Map.take (s `div` 2) comStateMembers
  pure $ Set.unions [Set.drop s flt, cm1, cm2]

genRelevantHotCredsFilter ::
  Set.Set (Credential 'HotCommitteeRole c) ->
  Map.Map (Credential 'ColdCommitteeRole c) (Maybe (Credential 'HotCommitteeRole c)) ->
  Gen (Set.Set (Credential 'HotCommitteeRole c))
genRelevantHotCredsFilter flt comStateMembers = do
  s <- choose (0, Set.size flt)
  let cm = Set.fromList $ Map.elems (Map.mapMaybe id (Map.take s comStateMembers))
  pure $ Set.drop s flt `Set.union` cm

withCommitteeInfo ::
  EraGov era =>
  NewEpochState era ->
  ( Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo ->
    UnitInterval ->
    Map.Map
      (Credential 'ColdCommitteeRole (EraCrypto era))
      (Maybe (Credential 'HotCommitteeRole (EraCrypto era))) ->
    CommitteeMembersState (EraCrypto era) ->
    Expectation
  ) ->
  Expectation
withCommitteeInfo nes expectation =
  case committeeInfo nes of
    Nothing -> queryCommitteeMembersStateNoFilters nes `shouldBe` Nothing
    Just (comMembers, comQuorum, comStateMembers) ->
      case queryCommitteeMembersStateNoFilters nes of
        Just noFilterQueryResult ->
          expectation comMembers comQuorum comStateMembers noFilterQueryResult
        Nothing ->
          expectationFailure "Expected queryCommitteeMembersState to return a Just value"

committeeInfo ::
  forall era.
  EraGov era =>
  NewEpochState era ->
  Maybe
    ( Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo
    , UnitInterval
    , Map.Map
        (Credential 'ColdCommitteeRole (EraCrypto era))
        (Maybe (Credential 'HotCommitteeRole (EraCrypto era)))
    )
committeeInfo nes = do
  (comMembers, comQurum) <-
    getCommitteeMembers (nes ^. nesEpochStateL . esLStateL . lsUTxOStateL . utxosGovStateL)
  let comStateMembers =
        csCommitteeCreds $
          nes ^. nesEpochStateL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
  pure (comMembers, comQurum, comStateMembers)

queryCommitteeMembersStateNoFilters ::
  forall era. EraGov era => NewEpochState era -> Maybe (CommitteeMembersState (EraCrypto era))
queryCommitteeMembersStateNoFilters =
  queryCommitteeMembersState @era
    Set.empty
    Set.empty
    Set.empty
