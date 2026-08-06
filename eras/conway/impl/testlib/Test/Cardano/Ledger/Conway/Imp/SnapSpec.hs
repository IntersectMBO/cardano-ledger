{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Imp.SnapSpec (spec, conwayOnlySpec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), addEpochInterval)
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Val ((<->))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "SNAP" $ do
  let getSpoVotingStake :: KeyHash StakePool -> ImpTestM era Coin
      getSpoVotingStake pool = do
        poolDistr <- psPoolDistr . fst . finishDRepPulser <$> getsNES (nesEsL . epochStateDRepPulsingStateL)
        pure $ fromCompact $ poolDistr Map.! pool
  it "SPO voting stake exceeds leader election stake by the active proposal deposit" $ do
    modifyPParams $ \pp ->
      pp
        & ppGovActionLifetimeL .~ EpochInterval 10
        & ppGovActionDepositL .~ Coin 1_000_000
    govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL

    (pool, _paymentCred, stakingCred) <- setupPoolWithStake (Coin 500_000_000)
    returnAddr <- getAccountAddressFor stakingCred
    _govActionId <- submitProposal =<< mkProposalWithAccountAddress InfoAction returnAddr

    passEpoch
    spoVotingStakeThisEpoch <- getSpoVotingStake pool
    passEpoch
    leaderElectionStakeNextEpoch <-
      fromCompact . individualTotalPoolStake . (Map.! pool) . unPoolDistr <$> getsNES nesPdL
    (spoVotingStakeThisEpoch <-> leaderElectionStakeNextEpoch) `shouldBe` govActionDeposit

conwayOnlySpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
conwayOnlySpec = describe "SNAP" $ do
  let getSpoVotingStake :: KeyHash StakePool -> ImpTestM era Coin
      getSpoVotingStake pool = do
        poolDistr <- psPoolDistr . fst . finishDRepPulser <$> getsNES (nesEsL . epochStateDRepPulsingStateL)
        pure $ fromCompact $ poolDistr Map.! pool
      getDRepVotingStake :: Credential DRepRole -> ImpTestM era Coin
      getDRepVotingStake drep = do
        drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . psDRepDistrG
        pure $ fromCompact $ drepDistr Map.! DRepCredential drep
  it "Reproduces #5014: SPO voting stake lags DRep voting stake by the refunded deposit" $ do
    modifyPParams $ \pp ->
      pp
        & ppGovActionLifetimeL .~ EpochInterval 1
        & ppGovActionDepositL .~ Coin 1_000_000
    govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL

    (drep, cred, _) <- setupSingleDRep 500_000_000
    pool <- freshKeyHash
    registerPool pool
    delegateStake cred pool
    returnAddr <- getAccountAddressFor cred

    govActionId <- submitProposal =<< mkProposalWithAccountAddress InfoAction returnAddr
    expectPresentGovActionId govActionId
    passNEpochs 3
    expectMissingGovActionId govActionId

    drepVotingStake <- getDRepVotingStake drep
    spoVotingStake <- getSpoVotingStake pool
    impAnn "SPO voting stake is behind by the refunded deposit" $
      (drepVotingStake <-> spoVotingStake) `shouldBe` govActionDeposit

    passEpoch
    spoVotingStakeNextEpoch <- getSpoVotingStake pool
    drepVotingStakeNextEpoch <- getDRepVotingStake drep
    impAnn "SPO voting stake catches up in the next epoch" $
      spoVotingStakeNextEpoch `shouldBe` drepVotingStakeNextEpoch

  it "Reproduces #5014: SPO voting stake lags DRep voting stake by a reaped pool's refunded deposit" $ do
    poolDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppPoolDepositL
    (drep, cred, _) <- setupSingleDRep 500_000_000
    poolActive <- freshKeyHash
    registerPool poolActive
    delegateStake cred poolActive
    registerAndRetirePoolToMakeReward cred
    drepVotingStake <- getDRepVotingStake drep
    spoVotingStake <- getSpoVotingStake poolActive
    (drepVotingStake <-> spoVotingStake) `shouldBe` poolDeposit

  it "Reproduces #5014: SPO voting stake lags DRep voting stake by an enacted treasury withdrawal" $
    whenPostBootstrap $ do
      modifyPParams $ \pp -> pp & ppGovActionLifetimeL .~ EpochInterval 30
      committeeCs <- registerInitialCommittee
      (drep, cred, _) <- setupSingleDRep 500_000_000
      pool <- freshKeyHash
      registerPool pool
      delegateStake cred pool
      returnAddr <- getAccountAddressFor cred
      submitTx_ $ mkBasicTx mkBasicTxBody & bodyTxL . treasuryDonationTxBodyL .~ Coin 1_000_000
      govActionId <- submitTreasuryWithdrawals [(returnAddr, Coin 1_000_000)]
      submitYesVote_ (DRepVoter drep) govActionId
      submitYesVoteCCs_ committeeCs govActionId
      passNEpochs 2
      drepVotingStake <- getDRepVotingStake drep
      spoVotingStake <- getSpoVotingStake pool
      (drepVotingStake <-> spoVotingStake) `shouldBe` Coin 1_000_000

  it
    "Reproduces #5014: SPO voting stake lags DRep voting stake by the combined refunds and withdrawal"
    $ whenPostBootstrap
    $ do
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL .~ EpochInterval 1
          & ppGovActionDepositL .~ Coin 1_000_000
      committeeCs <- registerInitialCommittee
      (drep, cred, _) <- setupSingleDRep 500_000_000
      poolActive <- freshKeyHash
      registerPool poolActive
      delegateStake cred poolActive
      returnAddr <- getAccountAddressFor cred
      submitTx_ $ mkBasicTx mkBasicTxBody & bodyTxL . treasuryDonationTxBodyL .~ Coin 1_000_000
      _ <- submitProposal =<< mkProposalWithAccountAddress InfoAction returnAddr
      poolToRetire <- freshKeyHash
      registerPoolWithAccountAddress poolToRetire returnAddr
      passEpoch
      curEpochNo <- getsNES nesELL
      submitTxAnn_ "Retire the temporary pool" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ SSeq.singleton (RetirePoolTxCert poolToRetire (addEpochInterval curEpochNo (EpochInterval 2)))
      modifyPParams $ \pp -> pp & ppGovActionLifetimeL .~ EpochInterval 30
      govActionId <- submitTreasuryWithdrawals [(returnAddr, Coin 1_000_000)]
      submitYesVote_ (DRepVoter drep) govActionId
      submitYesVoteCCs_ committeeCs govActionId
      passNEpochs 2
      govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      poolDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppPoolDepositL
      drepVotingStake <- getDRepVotingStake drep
      spoVotingStake <- getSpoVotingStake poolActive
      (drepVotingStake <-> spoVotingStake) `shouldBe` (govActionDeposit <> poolDeposit <> Coin 1_000_000)
