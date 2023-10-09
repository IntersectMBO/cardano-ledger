{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UMap (UMap)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Api.Arbitrary ()
import Test.Cardano.Ledger.Api.State.Query
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary (genValidUMapWithCreds)
import Test.Cardano.Ledger.Shelley.Arbitrary ()

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Data.Foldable (foldMap')
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)

spec :: Spec
spec = do
  describe "GetFilteredDelegationsAndRewardAccounts" $ do
    prop "filterStakePoolDelegsAndRewards same as getFilteredDelegationsAndRewardAccounts" $
      forAll genValidUMapWithCreds $ \(umap :: UMap StandardCrypto, creds) ->
        filterStakePoolDelegsAndRewards umap creds
          `shouldBe` getFilteredDelegationsAndRewardAccounts umap creds

  describe "GetCommitteeMembersState" $ do
    committeeMembersStateSpec

committeeMembersStateSpec :: Spec
committeeMembersStateSpec =
  prop "CommitteeMembersState Query" $ do
    forAll (arbitrary @(NewEpochState Conway)) $ \nes' -> do
      forAll
        ( arbitrary
            @( Set (Credential 'ColdCommitteeRole StandardCrypto)
             , Set (Credential 'HotCommitteeRole StandardCrypto)
             , Set MemberStatus
             )
        )
        $ \(ckFilter', hkFilter', statusFilter) -> do
          -- replace some arbitrary number of cold keys from the committeeState with the ones from the committee
          -- so we can have Active members
          let (comMembers, comStateMembers) = committeeInfo nes'
          forAll (genCommonMembers comMembers comStateMembers) $ \newCommitteeState -> do
            let nes =
                  nes'
                    & nesEpochStateL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
                      .~ newCommitteeState
            -- replace some cold and hot keys from the filter with known ones from both committee and committeeState
            forAll
              ( (,)
                  <$> genRelevantColdCredsFilter ckFilter' comMembers comStateMembers
                  <*> genRelevantHotCredsFilter hkFilter' comStateMembers
              )
              $ \(ckFilter, hkFilter) -> do
                propEmpty nes
                propComplete nes
                propAuthorized nes
                propActiveAuthorized nes
                propNotAuthorized nes
                propResigned nes
                propUnrecognized nes
                propFilters ckFilter hkFilter statusFilter nes

propEmpty :: NewEpochState Conway -> Expectation
propEmpty nes = do
  let (comMembers, comStateMembers) = committeeInfo nes
  let CommitteeMembersState result _ = queryCommitteeMembersStateNoFilters nes
  Map.null result `shouldBe` (Map.null comMembers && Map.null comStateMembers)

propComplete :: NewEpochState Conway -> Expectation
propComplete nes = do
  let (comMembers, comStateMembers) = committeeInfo nes
  let CommitteeMembersState result _ = queryCommitteeMembersStateNoFilters nes
  -- if a credential appears in either Committee or CommitteeState, it should appear in the result
  Map.keysSet comMembers
    `Set.union` Map.keysSet comStateMembers
    `shouldBe` Map.keysSet result

propNotAuthorized :: NewEpochState Conway -> Expectation
propNotAuthorized nes = do
  let (_, comStateMembers) = committeeInfo nes
  let CommitteeMembersState result _ = queryCommitteeMembersStateNoFilters nes
  let notAuthorized =
        Map.filter
          ( \case
              CommitteeMemberState MemberNotAuthorized _ _ _ -> True
              _ -> False
          )
          result
  -- if the member is NotAuthorized, it should not have an associated hot credential in the committeeState
  Map.intersection comStateMembers notAuthorized `shouldBe` Map.empty

propAuthorized :: NewEpochState Conway -> Expectation
propAuthorized nes = do
  let (_, comStateMembers) = committeeInfo nes
  let CommitteeMembersState result _ = queryCommitteeMembersStateNoFilters nes
  let ckHk =
        Map.foldMapWithKey
          ( \ck cms ->
              case cms of
                CommitteeMemberState (MemberAuthorized hk) _ _ _ -> [(ck, Just hk)]
                _ -> []
          )
          result
  -- if the member is Authorized, it should appear in the commiteeState
  Map.filter isJust comStateMembers `shouldBe` Map.fromList ckHk

propResigned :: NewEpochState Conway -> Expectation
propResigned nes = do
  let (_, comStateMembers) = committeeInfo nes
  let CommitteeMembersState result _ = queryCommitteeMembersStateNoFilters nes
  let resigned =
        Map.filter
          ( \case
              CommitteeMemberState MemberResigned _ _ _ -> True
              _ -> False
          )
          result
  -- if the member is Resignd, it should appear in the commiteeState as Nothing
  Map.keysSet (Map.filter isNothing comStateMembers) `shouldBe` Map.keysSet resigned

propUnrecognized :: NewEpochState Conway -> Expectation
propUnrecognized nes = do
  let (comMembers, comStateMembers) = committeeInfo nes
  let CommitteeMembersState result _ = queryCommitteeMembersStateNoFilters nes
  let unrecognized =
        Map.filter
          ( \case
              CommitteeMemberState _ Unrecognized _ _ -> True
              _ -> False
          )
          result
  -- if the member is Unrecognized, it should not be in the committe, but it should be in the committeeState
  Map.intersection comMembers unrecognized `shouldBe` Map.empty
  Map.keysSet unrecognized `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet comStateMembers)

propActiveAuthorized :: NewEpochState Conway -> Expectation
propActiveAuthorized nes = do
  let (comMembers, comStateMembers) = committeeInfo nes
  let CommitteeMembersState result _ = queryCommitteeMembersStateNoFilters nes
  let activeAuthorized =
        Map.fromList $
          Map.foldMapWithKey
            ( \ck cms ->
                case cms of
                  CommitteeMemberState (MemberAuthorized hk) Active _ _ -> [(ck, hk)]
                  _ -> []
            )
            result
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

propFilters ::
  Set (Credential 'ColdCommitteeRole StandardCrypto) ->
  Set (Credential 'HotCommitteeRole StandardCrypto) ->
  Set MemberStatus ->
  NewEpochState Conway ->
  Expectation
propFilters ckFilter hkFilter statusFilter nes = do
  let CommitteeMembersState result _ =
        queryCommitteeMembersState @Conway
          ckFilter
          hkFilter
          statusFilter
          nes
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
  Map.Map (Credential 'ColdCommitteeRole StandardCrypto) EpochNo ->
  Map.Map (Credential 'ColdCommitteeRole StandardCrypto) (Maybe (Credential 'HotCommitteeRole StandardCrypto)) ->
  Gen (CommitteeState Conway)
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
  Set.Set (Credential 'ColdCommitteeRole StandardCrypto) ->
  Map.Map (Credential 'ColdCommitteeRole StandardCrypto) EpochNo ->
  Map.Map (Credential 'ColdCommitteeRole StandardCrypto) (Maybe (Credential 'HotCommitteeRole StandardCrypto)) ->
  Gen (Set.Set (Credential 'ColdCommitteeRole StandardCrypto))
genRelevantColdCredsFilter flt comMembers comStateMembers = do
  s <- choose (0, Set.size flt)
  let cm1 = Map.keysSet $ Map.take (s `div` 2) comMembers
  let cm2 = Map.keysSet $ Map.take (s `div` 2) comStateMembers
  pure $ Set.unions [Set.drop s flt, cm1, cm2]

genRelevantHotCredsFilter ::
  Set.Set (Credential 'HotCommitteeRole StandardCrypto) ->
  Map.Map (Credential 'ColdCommitteeRole StandardCrypto) (Maybe (Credential 'HotCommitteeRole StandardCrypto)) ->
  Gen (Set.Set (Credential 'HotCommitteeRole StandardCrypto))
genRelevantHotCredsFilter flt comStateMembers = do
  s <- choose (0, Set.size flt)
  let cm = Set.fromList $ Map.elems (Map.mapMaybe id (Map.take s comStateMembers))
  pure $ Set.drop s flt `Set.union` cm

committeeInfo ::
  NewEpochState Conway ->
  ( Map.Map (Credential 'ColdCommitteeRole StandardCrypto) EpochNo
  , Map.Map (Credential 'ColdCommitteeRole StandardCrypto) (Maybe (Credential 'HotCommitteeRole StandardCrypto))
  )
committeeInfo nes =
  let comMembers = getCommitteeMembers (nes ^. nesEpochStateL . esLStateL . lsUTxOStateL . utxosGovStateL)
      comStateMembers =
        csCommitteeCreds $
          nes ^. nesEpochStateL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
   in (comMembers, comStateMembers)

queryCommitteeMembersStateNoFilters :: NewEpochState Conway -> CommitteeMembersState StandardCrypto
queryCommitteeMembersStateNoFilters =
  queryCommitteeMembersState @Conway
    Set.empty
    Set.empty
    Set.empty
