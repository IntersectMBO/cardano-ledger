{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Governance.ProceduresSpec (spec) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  GovActionId,
  Voter,
  VotingProcedure,
  VotingProcedures (..),
  foldlVotingProcedures,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Cardano.Base.QuickCheck (testLawsGroup)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck.Classes (
  exponentialSemigroupLaws,
  monoidLaws,
  semigroupLaws,
  semigroupMonoidLaws,
 )

spec :: Spec
spec = describe "VotingProcedures" $ do
  describe "Semigroup and Monoid" $
    -- `<>` for `VotingProcedures` is not commutative, so
    -- `commutativeSemigroupLaws` and `commutativeMonoidLaws` must not be applied
    -- here.
    testLawsGroup @(VotingProcedures ConwayEra)
      [ semigroupLaws
      , exponentialSemigroupLaws
      , monoidLaws
      , semigroupMonoidLaws
      ]
  -- Needs the overlapping 'VotingProcedures' generator, or else we won't be able to assert that <> is not commutative (disjoint operands *do* commute)
  prop "<> is not commutative" $
    expectFailure $
      -- Use `forAllBlind` and `==` instead of `forAll` and `===` because an
      -- expected failure would print the whole counterexample on every run.
      forAllBlind genOverlappingVotingProcedures $ \(vps1, vps2) ->
        vps1 <> vps2 == vps2 <> vps1
  prop "<> is a right-biased union per (Voter, GovActionId)" $
    forAll genOverlappingVotingProcedures $ \(vps1, vps2) ->
      -- Map.union is right-biased
      flattenVotes (vps1 <> vps2) === Map.union (flattenVotes vps2) (flattenVotes vps1)

-- | This function flattens the nested 'Map' from 'VotingProcedures' to a single
-- 'Map'. With a single 'Map', 'VotingProcedures' represents a partial function
-- @(Voter, GovActionId) -> VotingProcedure@.
--
-- Asserting against this model rather than against the nested 'Map's is what
-- makes the property above reject an outer-level union that drops a shared
-- voter's other votes. For example, a wrong @(\<>)@ that implements
-- @vps1 \<> vps2@ as @Map.union@ only unions the outer, 'Voter'-keyed 'Map'.
-- For a voter @v@ who voted on @gaid1 :: GovActionId@ in @vps1@ and on
-- @gaid2 :: GovActionId@ in @vps2@, 'Map.union' keeps @vps1@'s whole inner map
-- @Map GovActionId (VotingProcedure era)@ for @v@ and discards @vps2@'s inner
-- map for @v@, so @v@'s vote on @gaid2@ vanishes from the result even though
-- it never collided with anything. Comparing against 'flattenVotes' models
-- instead catches this immediately, because @(v, gaid2)@ is simply a missing
-- key in the flattened union.
flattenVotes :: VotingProcedures era -> Map (Voter, GovActionId) (VotingProcedure era)
flattenVotes = foldlVotingProcedures (\acc voter gaid vp -> Map.insert (voter, gaid) vp acc) Map.empty

-- | A pair of 'VotingProcedures' that overlaps in terms of 'Voter' and
-- 'GovActionId'. Two independent 'arbitrary' values of 'VotingProcedures' don't
-- have 'Voter' or 'GovActionId' that overlap, since both keys are random
-- hashes, so a property over them would only ever exercise the disjoint case.
--
-- A generated pair covers all three interesting shapes at once, each ruling out a different wrong
-- implementation of `<>`:
--
--   * a 'Voter' only one side of `<>` has;
--   * a 'Voter' both sides of `<>` have, with a 'GovActionId' only one side has;
--   * a 'Voter' and 'GovActionId' both sides of `<>` have;
genOverlappingVotingProcedures :: Gen (VotingProcedures ConwayEra, VotingProcedures ConwayEra)
genOverlappingVotingProcedures = do
  -- Voters both sides have
  sharedVoters <- Map.fromList <$> scale (min 6) (listOf ((,) <$> arbitrary <*> genOverlappingVotes))
  -- Voters only one side has. Their 'Voter' keys are random hashes, so they don't collide with
  -- `sharedVoters`.
  leftOnlyVoters <- unVotingProcedures <$> arbitrary
  rightOnlyVoters <- unVotingProcedures <$> arbitrary
  pure
    ( VotingProcedures $ Map.union (fmap fst sharedVoters) leftOnlyVoters
    , VotingProcedures $ Map.union (fmap snd sharedVoters) rightOnlyVoters
    )

-- | The two sets of votes that a shared 'Voter' could cast, one for each side
-- of the generated pair.
--
-- Both sides of the pair vote on the same generated `sharedGovActionIds`, but
-- with independently generated 'VotingProcedure's, so the two sides disagree
-- and it becomes observable which of them the `<>` implementation keeps. Each
-- side also votes on 'GovActionId' the other side doesn't have.
genOverlappingVotes ::
  Gen (Map GovActionId (VotingProcedure ConwayEra), Map GovActionId (VotingProcedure ConwayEra))
genOverlappingVotes = do
  -- Non-empty, so that a shared voter always does share a gov action, and so
  -- that neither side can come out with no votes at all.
  sharedGovActionIds <- scale (min 6) (listOf1 arbitrary)
  let genVotes = do
        sharedVotes <- traverse (\gaid -> (,) gaid <$> arbitrary) sharedGovActionIds
        uniqueVotes <- Map.fromList <$> scale (min 6) (listOf ((,) <$> arbitrary <*> arbitrary))
        pure $ Map.fromList sharedVotes `Map.union` uniqueVotes
  (,) <$> genVotes <*> genVotes
