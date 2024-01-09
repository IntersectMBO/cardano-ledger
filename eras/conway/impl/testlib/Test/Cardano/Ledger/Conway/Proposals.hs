{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Proposals where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Governance
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable (foldl', toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (fromList)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq (Empty, (:<|), (:|>)))
import qualified Data.Set as Set
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary (
  ProposalsForEnactment (..),
  ProposalsNewActions (..),
 )

spec :: Spec
spec = do
  describe "Proposals" $ do
    context "Construction" $ do
      prop "Generator generates consistent Proposals" $
        \(ps :: Proposals Conway) ->
          ps `shouldSatisfy` isConsistent_
      prop "Adding new nodes keeps Proposals consistent" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions Conway) ->
          let ps' =
                foldl'
                  (\p action -> fromMaybe (error "Unable to add action") $ proposalsAddAction action p)
                  ps
                  actions
           in ps' `shouldSatisfy` isConsistent_
    context "Removal" $ do
      prop "Removing leaf nodes keeps Proposals consistent" $
        \(ps :: Proposals Conway) ->
          let test gais =
                let ps' = fst $ proposalsRemoveDescendentIds gais ps
                 in do
                      proposalsSize ps' `shouldBe` proposalsSize ps - Set.size gais
                      ps' `shouldSatisfy` isConsistent_
           in case proposalsIds ps of
                Empty -> True `shouldBe` True
                _pids :|> pid :|> pid1 :|> pid2 :|> pid3 -> test $ Set.fromList [pid, pid1, pid2, pid3]
                _pids :|> pid :|> pid1 :|> pid2 -> test $ Set.fromList [pid, pid1, pid2]
                _pids :|> pid :|> pid1 -> test $ Set.fromList [pid, pid1]
                _pids :|> pid -> test $ Set.singleton pid
      prop "Removing root nodes keeps Proposals consistent" $
        \(ps :: Proposals Conway) ->
          let test gais =
                let ps' = fst $ proposalsRemoveDescendentIds gais ps
                 in do
                      proposalsSize ps' `shouldSatisfy` (< proposalsSize ps)
                      ps' `shouldSatisfy` isConsistent_
           in case proposalsIds ps of
                Empty -> True `shouldBe` True
                pid :<| pid1 :<| pid2 :<| pid3 :<| _pids -> test $ Set.fromList [pid, pid1, pid2, pid3]
                pid :<| pid1 :<| pid2 :<| _pids -> test $ Set.fromList [pid, pid1, pid2]
                pid :<| pid1 :<| _pids -> test $ Set.fromList [pid, pid1]
                pid :<| _pids -> test $ Set.singleton pid
      prop "Removing non-member nodes is a no-op" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions Conway) ->
          fst (proposalsRemoveDescendentIds (Set.fromList $ gasId <$> actions) ps) `shouldBe` ps
    context "Enactment" $ do
      prop "Adding votes preserves consistency" $
        \(ProposalsForEnactment ps gais _ :: ProposalsForEnactment Conway, voter :: Voter era, vote :: Vote) -> do
          case gais of
            gai Seq.:<| _gais -> isConsistent_ $ proposalsAddVote voter vote gai ps
            _ -> True
      prop "Enacting exhaustive lineages reduces Proposals to their roots" $
        \(ProposalsForEnactment ps gais _ :: ProposalsForEnactment Conway) -> do
          let toEnact = Set.fromList $ toList gais
              (ps', enactedRemoved, expiredRemoved) = proposalsApplyEnactment gais Set.empty ps
          ps' `shouldSatisfy` isConsistent_
          expiredRemoved `shouldSatisfy` Map.null
          toEnact `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet enactedRemoved)
      prop "Enacting non-member nodes is an error" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions Conway) ->
          (evaluate . force) (proposalsApplyEnactment (fromList $ gasId <$> actions) Set.empty ps)
            `shouldThrow` anyErrorCall
      prop "Expiring compliments of exhaustive lineages keeps proposals consistent" $
        \(ProposalsForEnactment ps _ gais :: ProposalsForEnactment Conway) -> do
          let (ps', enactedRemoved, expiredRemoved) = proposalsApplyEnactment Seq.Empty gais ps
          ps' `shouldSatisfy` isConsistent_
          enactedRemoved `shouldSatisfy` Map.null
          gais `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet expiredRemoved)
      prop "Expiring non-member nodes is a no-op" $
        \(ProposalsNewActions ps actions :: ProposalsNewActions Conway) -> do
          let (ps', enactedRemoved, expiredRemoved) = proposalsApplyEnactment Seq.Empty (Set.fromList $ gasId <$> actions) ps
          ps' `shouldBe` ps
          enactedRemoved `shouldSatisfy` Map.null
          expiredRemoved `shouldSatisfy` Map.null
      prop "Enacting and expiring exhaustive lineages reduces Proposals to their roots" $
        \(ProposalsForEnactment ps toEnact toExpire :: ProposalsForEnactment Conway) -> do
          let (ps', enactedRemoved, expiredRemoved) = proposalsApplyEnactment toEnact toExpire ps
          ps' `shouldSatisfy` isConsistent_
          Set.fromList (toList toEnact) `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet enactedRemoved)
          Set.fromList (toList toExpire) `shouldSatisfy` (`Set.isSubsetOf` Map.keysSet expiredRemoved)
          proposalsSize ps' `shouldBe` 0
