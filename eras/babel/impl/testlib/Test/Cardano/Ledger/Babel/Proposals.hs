{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Babel.Proposals where

import Cardano.Ledger.Babel
import Cardano.Ledger.Conway.Governance (
  GovActionState (..),
  Proposals,
  Vote,
  Voter,
  proposalsActionsMap,
  proposalsAddAction,
  proposalsAddVote,
  proposalsApplyEnactment,
  proposalsIds,
  proposalsRemoveWithDescendants,
  proposalsSize,
  toGovRelationTreeEither,
 )
import Control.DeepSeq (force)
import Control.Exception (AssertionFailed (..), evaluate)
import Data.Either (isRight)
import Data.Foldable (foldl', toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (fromList)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Babel.Arbitrary (
  ProposalsForEnactment (..),
  ProposalsNewActions (..),
 )
import Test.Cardano.Ledger.Common

spec :: Spec
spec = do
  describe "Proposals" $ do
    describe "Construction" $ do
      prop "Adding new nodes keeps Proposals consistent" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions Babel) ->
          let ps' =
                foldl'
                  (\p action -> fromMaybe (error "Unable to add action") $ proposalsAddAction action p)
                  ps
                  actions
              actionsMap = foldl' (\accum gas -> Map.insert (gasId gas) gas accum) Map.empty actions
           in actionsMap `shouldBe` (actionsMap `Map.intersection` proposalsActionsMap ps')
    describe "Removal" $ do
      prop "Removing leaf nodes keeps Proposals consistent" $
        \(ps :: Proposals Babel) -> do
          let gais = Set.fromList $ toList $ SSeq.takeLast 4 $ proposalsIds ps
              ps' = fst $ proposalsRemoveWithDescendants gais ps
          proposalsSize ps' `shouldBe` proposalsSize ps - Set.size gais
      prop "Removing root nodes keeps Proposals consistent" $
        \(ps :: Proposals Babel) -> do
          let gais = Set.fromList $ toList $ SSeq.take 4 $ proposalsIds ps
              ps' = fst $ proposalsRemoveWithDescendants gais ps
          proposalsSize ps' `shouldSatisfy` (<= proposalsSize ps)
      prop "Removing non-member nodes throws an AssertionFailure" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions Babel) ->
          (evaluate . force) (proposalsRemoveWithDescendants (Set.fromList $ gasId <$> actions) ps)
            `shouldThrow` \AssertionFailed {} -> True
    describe "Enactment" $ do
      prop "Adding votes preserves consistency" $
        \(ProposalsForEnactment ps gass _ :: ProposalsForEnactment Babel, voter :: Voter era, vote :: Vote) -> do
          case gass of
            gas Seq.:<| _gass -> isRight . toGovRelationTreeEither $ proposalsAddVote voter vote (gasId gas) ps
            _ -> True
      prop "Enacting exhaustive lineages reduces Proposals to their roots" $
        \(ProposalsForEnactment ps gass _ :: ProposalsForEnactment Babel) -> do
          let toEnact = Set.fromList $ toList gass
              (_ps', enactedRemoved, expiredRemoved) = proposalsApplyEnactment gass Set.empty ps
          expiredRemoved `shouldSatisfy` Map.null
          toEnact `shouldSatisfy` (`Set.isSubsetOf` Set.fromList (Map.elems enactedRemoved))
      prop "Enacting non-member nodes throws an AssertionFailure" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions Babel) ->
          (evaluate . force) (proposalsApplyEnactment (fromList actions) Set.empty ps)
            `shouldThrow` \AssertionFailed {} -> True
      prop "Expiring compliments of exhaustive lineages keeps proposals consistent" $
        \(ProposalsForEnactment ps _ gais :: ProposalsForEnactment Babel) -> do
          let (_ps', enactedRemoved, expiredRemoved) = proposalsApplyEnactment Seq.Empty gais ps
          enactedRemoved `shouldSatisfy` Map.null
          gais `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet expiredRemoved)
      prop "Expiring non-member nodes throws an AssertionFailure" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions Babel) ->
          (evaluate . force) (proposalsApplyEnactment Seq.Empty (Set.fromList $ gasId <$> actions) ps)
            `shouldThrow` \AssertionFailed {} -> True
      prop "Enacting and expiring exhaustive lineages reduces Proposals to their roots" $
        \(ProposalsForEnactment ps toEnact toExpire :: ProposalsForEnactment Babel) -> do
          let (ps', enactedRemoved, expiredRemoved) = proposalsApplyEnactment toEnact toExpire ps
          Set.fromList (toList toEnact)
            `shouldSatisfy` (`Set.isSubsetOf` Set.fromList (Map.elems enactedRemoved))
          Set.fromList (toList toExpire) `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet expiredRemoved)
          proposalsSize ps' `shouldBe` 0
