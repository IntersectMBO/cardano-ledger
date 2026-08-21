{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SnapSpec (spec) where

import Cardano.Ledger.Val ((<->))
import Control.Monad (forM)
import Test.Cardano.Ledger.Conway.Imp.SnapSpec (
  getActiveProposalDeposits,
  getDRepVotingStake,
  getLeaderElectionStake,
  getSpoVotingStake,
  isPoolInLeaderDistr,
  isPoolInRewardSnapshot,
  setupCombinedScenario,
  setupExpiredRefundScenario,
  setupReapedPoolScenario,
  setupRetiredPoolInLeaderDistr,
  setupWithdrawalScenario,
 )
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  DijkstraEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "SNAP" $ do
  it "SPO voting stake no longer lags DRep voting stake by the refunded deposit" $ do
    (pool, drep, _) <- setupExpiredRefundScenario
    drepVotingStake <- getDRepVotingStake drep
    spoVotingStake <- getSpoVotingStake pool
    (drepVotingStake <-> spoVotingStake) `shouldBe` mempty
    activeProposalDeposits <- getActiveProposalDeposits pool
    passEpoch
    leaderElectionStake <- getLeaderElectionStake pool
    (spoVotingStake <-> leaderElectionStake) `shouldBe` activeProposalDeposits

  it "SPO voting stake no longer lags DRep voting stake by a reaped pool's refunded deposit" $ do
    (poolActive, drep, _) <- setupReapedPoolScenario
    drepVotingStake <- getDRepVotingStake drep
    spoVotingStake <- getSpoVotingStake poolActive
    (drepVotingStake <-> spoVotingStake) `shouldBe` mempty
    activeProposalDeposits <- getActiveProposalDeposits poolActive
    passEpoch
    leaderElectionStake <- getLeaderElectionStake poolActive
    (spoVotingStake <-> leaderElectionStake) `shouldBe` activeProposalDeposits

  it "SPO voting stake no longer lags DRep voting stake by an enacted treasury withdrawal" $
    whenPostBootstrap $ do
      (pool, drep, _) <- setupWithdrawalScenario
      drepVotingStake <- getDRepVotingStake drep
      spoVotingStake <- getSpoVotingStake pool
      (drepVotingStake <-> spoVotingStake) `shouldBe` mempty
      activeProposalDeposits <- getActiveProposalDeposits pool
      passEpoch
      leaderElectionStake <- getLeaderElectionStake pool
      (spoVotingStake <-> leaderElectionStake) `shouldBe` activeProposalDeposits

  it "SPO voting stake no longer lags DRep voting stake by the combined refunds and withdrawal" $
    whenPostBootstrap $ do
      (poolActive, drep, _) <- setupCombinedScenario
      drepVotingStake <- getDRepVotingStake drep
      spoVotingStake <- getSpoVotingStake poolActive
      (drepVotingStake <-> spoVotingStake) `shouldBe` mempty
      activeProposalDeposits <- getActiveProposalDeposits poolActive
      passEpoch
      leaderElectionStake <- getLeaderElectionStake poolActive
      (spoVotingStake <-> leaderElectionStake) `shouldBe` activeProposalDeposits

  it "A reaped pool leaves the leader-election distribution one epoch earlier" $ do
    pool <- setupRetiredPoolInLeaderDistr
    passEpoch
    isPoolInLeaderDistr pool `shouldReturn` False

  it "A reaped pool leaves the reward stake snapshot one epoch earlier" $ do
    pool <- setupRetiredPoolInLeaderDistr
    isPoolInRewardSnapshot pool `shouldReturn` True
    passNEpochs 2
    isPoolInRewardSnapshot pool `shouldReturn` False

  it "SPO and DRep voting stake agree for every shared delegator" $ do
    pairs <- forM [1 .. 4 :: Integer] $ \i -> do
      (drep, cred, _) <- setupSingleDRep (i * 100_000_000)
      pool <- freshKeyHash
      registerPool pool
      delegateStake cred pool
      pure (pool, drep)
    passNEpochs 2
    forM_ pairs $ \(pool, drep) -> do
      spoVotingStake <- getSpoVotingStake pool
      drepVotingStake <- getDRepVotingStake drep
      spoVotingStake `shouldBe` drepVotingStake
