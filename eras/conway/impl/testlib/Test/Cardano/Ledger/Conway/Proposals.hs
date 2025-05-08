{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Proposals where

import Cardano.Ledger.Conway.Governance
import Control.DeepSeq (force)
import Control.Exception (AssertionFailed (..), evaluate)
import Data.Either (isRight)
import Data.Foldable as F (foldl', toList)
import qualified Data.Map.Strict as Map
import Data.MapExtras (fromElems)
import Data.Maybe (fromMaybe)
import Data.Sequence (fromList)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary (
  ProposalsForEnactment (..),
  ProposalsNewActions (..),
 )
import Test.Cardano.Ledger.Conway.Era (ConwayEraTest)

spec :: forall era. ConwayEraTest era => Spec
spec = do
  describe "Proposals" $ do
    describe "Construction" $ do
      prop "Adding new nodes keeps Proposals consistent" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions era) ->
          let ps' =
                F.foldl'
                  (\p action -> fromMaybe (error "Unable to add action") $ proposalsAddAction action p)
                  ps
                  actions
              actionsMap = F.foldl' (\accum gas -> Map.insert (gasId gas) gas accum) Map.empty actions
           in actionsMap `shouldBe` (actionsMap `Map.intersection` proposalsActionsMap ps')
    describe "Removal" $ do
      prop "Removing leaf nodes keeps Proposals consistent" $
        \(ps :: Proposals era) -> do
          let gais = Set.fromList $ toList $ SSeq.takeLast 4 $ proposalsIds ps
              ps' = fst $ proposalsRemoveWithDescendants gais ps
          proposalsSize ps' `shouldBe` proposalsSize ps - Set.size gais
      prop "Removing root nodes keeps Proposals consistent" $
        \(ps :: Proposals era) -> do
          let gais = Set.fromList $ toList $ SSeq.take 4 $ proposalsIds ps
              ps' = fst $ proposalsRemoveWithDescendants gais ps
          proposalsSize ps' `shouldSatisfy` (<= proposalsSize ps)
      prop "Removing non-member nodes throws an AssertionFailure" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions era) ->
          (evaluate . force) (proposalsRemoveWithDescendants (Set.fromList $ gasId <$> actions) ps)
            `shouldThrow` \AssertionFailed {} -> True
    describe "Enactment" $ do
      prop "Adding votes preserves consistency" $
        \( ProposalsForEnactment {pfeProposals, pfeToEnact} :: ProposalsForEnactment era
           , voter :: Voter
           , vote :: Vote
           ) -> do
            case pfeToEnact of
              gas Seq.:<| _gass -> isRight . toGovRelationTreeEither $ proposalsAddVote voter vote (gasId gas) pfeProposals
              _ -> True
      prop "Enacting exhaustive lineages reduces Proposals to their roots" $
        \( ProposalsForEnactment {pfeProposals, pfeToEnact, pfeToRemove, pfeToRetain} ::
             ProposalsForEnactment era
           ) -> do
            let (ps', enacted, removedDueToEnactment, expiredRemoved) = proposalsApplyEnactment pfeToEnact Set.empty pfeProposals
            expiredRemoved `shouldSatisfy` Map.null
            enacted `shouldBe` fromElems gasId pfeToEnact
            Map.keysSet removedDueToEnactment `shouldBe` pfeToRemove
            proposalsSize ps' `shouldBe` Set.size pfeToRetain
      prop "Enacting non-member nodes throws an AssertionFailure" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions era) ->
          (evaluate . force) (proposalsApplyEnactment (fromList actions) Set.empty ps)
            `shouldThrow` \AssertionFailed {} -> True
      prop "Expiring compliments of exhaustive lineages keeps proposals consistent" $
        \( ProposalsForEnactment {pfeProposals, pfeToEnact, pfeToRemove, pfeToRetain} ::
             ProposalsForEnactment era
           ) -> do
            let (ps', enacted, removedDueToEnactment, expiredRemoved) =
                  proposalsApplyEnactment Seq.Empty pfeToRemove pfeProposals
            enacted `shouldBe` mempty
            removedDueToEnactment `shouldBe` mempty
            Map.keysSet expiredRemoved `shouldBe` pfeToRemove
            ps' `shouldBe` fst (proposalsRemoveWithDescendants pfeToRemove pfeProposals)
            let enactMap = fromElems gasId pfeToEnact
            let (emptyProposals, enactedMap) = proposalsRemoveWithDescendants (Map.keysSet enactMap) ps'
            proposalsSize emptyProposals `shouldBe` Set.size pfeToRetain
            enactedMap `shouldBe` enactMap
      prop "Expiring non-member nodes throws an AssertionFailure" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions era) ->
          (evaluate . force) (proposalsApplyEnactment Seq.Empty (Set.fromList $ gasId <$> actions) ps)
            `shouldThrow` \AssertionFailed {} -> True
      prop "Enacting and expiring conflicting proposals does not lead to removal due to enactment" $
        \( ProposalsForEnactment {pfeProposals, pfeToEnact, pfeToRemove, pfeToRetain} ::
             ProposalsForEnactment era
           ) -> do
            let (ps', enacted, enactedRemoved, expiredRemoved) = proposalsApplyEnactment pfeToEnact pfeToRemove pfeProposals
            Map.keysSet expiredRemoved `shouldBe` pfeToRemove
            enactedRemoved `shouldBe` mempty
            enacted `shouldBe` fromElems gasId pfeToEnact
            proposalsSize ps' `shouldBe` Set.size pfeToRetain
