{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.SnapSpec (
  spec,
  conwayOnlySpec,
  getSpoVotingStake,
  getDRepVotingStake,
  setupExpiredRefundScenario,
  setupReapedPoolScenario,
  setupWithdrawalScenario,
  setupCombinedScenario,
  getLeaderElectionStake,
  getActiveProposalDeposits,
  isPoolInLeaderDistr,
  isPoolInRewardSnapshot,
  setupRetiredPoolInLeaderDistr,
) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), addEpochInterval, epochInfoPure)
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.API.Forecast (EraForecast)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Slot (epochInfoFirst)
import Cardano.Ledger.Val ((<->))
import Control.Monad.Reader (runReader)
import Control.State.Transition.Extended (TRC (..), applySTS)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import Data.Void (absurd)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

getSpoVotingStake :: ConwayEraImp era => KeyHash StakePool -> ImpTestM era Coin
getSpoVotingStake pool = do
  poolDistr <- psPoolDistr . fst . finishDRepPulser <$> getsNES (nesEsL . epochStateDRepPulsingStateL)
  pure $ fromCompact $ poolDistr Map.! pool

getDRepVotingStake :: ConwayEraImp era => Credential DRepRole -> ImpTestM era Coin
getDRepVotingStake drep = do
  drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . psDRepDistrG
  pure $ fromCompact $ drepDistr Map.! DRepCredential drep

getLeaderElectionStake :: KeyHash StakePool -> ImpTestM era Coin
getLeaderElectionStake pool =
  fromCompact . individualTotalPoolStake . (Map.! pool) . unPoolDistr <$> getsNES nesPdL

getActiveProposalDeposits :: ConwayEraImp era => KeyHash StakePool -> ImpTestM era Coin
getActiveProposalDeposits pool = do
  proposals <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  pure $
    foldMap fromCompact $
      Map.filterWithKey
        (\cred _ -> lookupStakePoolDelegation cred accounts == Just pool)
        (proposalsDeposits proposals)

setupDelegatorAndPool ::
  ConwayEraImp era => ImpTestM era (KeyHash StakePool, Credential DRepRole, Credential Staking)
setupDelegatorAndPool = do
  (drep, cred, _) <- setupSingleDRep 500_000_000
  pool <- freshKeyHash
  registerPool pool
  delegateStake cred pool
  pure (pool, drep, cred)

setupExpiredRefundScenario ::
  ConwayEraImp era => ImpTestM era (KeyHash StakePool, Credential DRepRole, Coin)
setupExpiredRefundScenario = do
  modifyPParams $ \pp ->
    pp
      & ppGovActionLifetimeL .~ EpochInterval 1
      & ppGovActionDepositL .~ Coin 1_000_000
  govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  (pool, drep, cred) <- setupDelegatorAndPool
  returnAddr <- getAccountAddressFor cred
  govActionId <- submitProposal =<< mkProposalWithAccountAddress InfoAction returnAddr
  expectPresentGovActionId govActionId
  passNEpochs 3
  expectMissingGovActionId govActionId
  pure (pool, drep, govActionDeposit)

setupReapedPoolScenario ::
  ConwayEraImp era => ImpTestM era (KeyHash StakePool, Credential DRepRole, Coin)
setupReapedPoolScenario = do
  poolDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppPoolDepositL
  (poolActive, drep, cred) <- setupDelegatorAndPool
  registerAndRetirePoolToMakeReward cred
  pure (poolActive, drep, poolDeposit)

setupWithdrawalScenario ::
  ConwayEraImp era => ImpTestM era (KeyHash StakePool, Credential DRepRole, Coin)
setupWithdrawalScenario = do
  modifyPParams $ \pp -> pp & ppGovActionLifetimeL .~ EpochInterval 30
  committeeCs <- registerInitialCommittee
  (pool, drep, cred) <- setupDelegatorAndPool
  returnAddr <- getAccountAddressFor cred
  submitTx_ $ mkBasicTx mkBasicTxBody & bodyTxL . treasuryDonationTxBodyL .~ Coin 1_000_000
  govActionId <- submitTreasuryWithdrawals [(returnAddr, Coin 1_000_000)]
  submitYesVote_ (DRepVoter drep) govActionId
  submitYesVoteCCs_ committeeCs govActionId
  passNEpochs 2
  expectMissingGovActionId govActionId
  pure (pool, drep, Coin 1_000_000)

setupCombinedScenario ::
  ConwayEraImp era => ImpTestM era (KeyHash StakePool, Credential DRepRole, Coin)
setupCombinedScenario = do
  modifyPParams $ \pp ->
    pp
      & ppGovActionLifetimeL .~ EpochInterval 1
      & ppGovActionDepositL .~ Coin 1_000_000
  committeeCs <- registerInitialCommittee
  (poolActive, drep, cred) <- setupDelegatorAndPool
  returnAddr <- getAccountAddressFor cred
  submitTx_ $ mkBasicTx mkBasicTxBody & bodyTxL . treasuryDonationTxBodyL .~ Coin 1_000_000
  infoActionId <- submitProposal =<< mkProposalWithAccountAddress InfoAction returnAddr
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
  expectMissingGovActionId infoActionId
  expectMissingGovActionId govActionId
  govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  poolDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppPoolDepositL
  pure (poolActive, drep, govActionDeposit <> poolDeposit <> Coin 1_000_000)

isPoolInLeaderDistr :: KeyHash StakePool -> ImpTestM era Bool
isPoolInLeaderDistr pool = Map.member pool . unPoolDistr <$> getsNES nesPdL

isPoolInRewardSnapshot :: KeyHash StakePool -> ImpTestM era Bool
isPoolInRewardSnapshot pool =
  Map.member pool . unPoolDistr . calculatePoolDistr <$> getsNES (nesEsL . esSnapshotsL . ssStakeGoL)

setupRetiredPoolInLeaderDistr :: ConwayEraImp era => ImpTestM era (KeyHash StakePool)
setupRetiredPoolInLeaderDistr = do
  (pool, _, _) <- setupDelegatorAndPool
  passNEpochs 3
  isPoolInLeaderDistr pool `shouldReturn` True
  curEpochNo <- getsNES nesELL
  submitTxAnn_ "Retire the pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (RetirePoolTxCert pool (addEpochInterval curEpochNo (EpochInterval 1)))
  passEpoch
  isPoolInLeaderDistr pool `shouldReturn` True
  pure pool

spec ::
  forall era.
  (ConwayEraImp era, EraForecast era) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "SNAP" $ do
  -- 'TICKF' skips 'SNAP', because a forecast never needs the new mark snapshot
  -- that 'SNAP' spends its time aggregating. It must still perform the cheap
  -- half, the rotation, since a forecast across an epoch boundary reads the set
  -- snapshot: Dijkstra's takes the Leios voting committee from it.
  it "TICKF rotates the stake snapshots" $ do
    -- Delegate stake and then cross a boundary, so that the mark snapshot holds
    -- that stake while set and go do not. Without divergent snapshots, rotating
    -- them would be unobservable and the assertions below vacuous --- which the
    -- 'shouldNotBe' guards against.
    _ <- setupPoolWithStake (Coin 500_000_000)
    passEpoch
    nes <- getsNES id
    globals <- use impGlobalsL
    -- The first slot of the next epoch, where TICKF crosses the boundary.
    let nextEpochStart =
          epochInfoFirst (epochInfoPure globals) (succ (nes ^. nesELL))
        ticked =
          either (absurd . NE.head) id
            . flip runReader globals
            $ applySTS @(EraRule "TICKF" era) (TRC ((), nes, nextEpochStart))
        snapshotsOf s = s ^. nesEsL . esSnapshotsL
    snapshotsOf nes ^. ssStakeMarkL `shouldNotBe` snapshotsOf nes ^. ssStakeSetL
    snapshotsOf ticked ^. ssStakeSetL `shouldBe` snapshotsOf nes ^. ssStakeMarkL
    snapshotsOf ticked ^. ssStakeGoL `shouldBe` snapshotsOf nes ^. ssStakeSetL

  it "SPO voting stake exceeds leader election stake by the active proposal deposit" $ do
    modifyPParams $ \pp ->
      pp
        & ppGovActionLifetimeL .~ EpochInterval 10
        & ppGovActionDepositL .~ Coin 1_000_000
    (pool, _paymentCred, stakingCred) <- setupPoolWithStake (Coin 500_000_000)
    returnAddr <- getAccountAddressFor stakingCred
    _govActionId <- submitProposal =<< mkProposalWithAccountAddress InfoAction returnAddr
    passEpoch
    spoVotingStakeThisEpoch <- getSpoVotingStake pool
    activeProposalDeposits <- getActiveProposalDeposits pool
    passEpoch
    leaderElectionStakeNextEpoch <- getLeaderElectionStake pool
    (spoVotingStakeThisEpoch <-> leaderElectionStakeNextEpoch) `shouldBe` activeProposalDeposits

conwayOnlySpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
conwayOnlySpec = describe "SNAP" $ do
  it "Reproduces #5014: SPO voting stake lags DRep voting stake by the refunded deposit" $ do
    (pool, drep, govActionDeposit) <- setupExpiredRefundScenario
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
    (poolActive, drep, poolDeposit) <- setupReapedPoolScenario
    drepVotingStake <- getDRepVotingStake drep
    spoVotingStake <- getSpoVotingStake poolActive
    (drepVotingStake <-> spoVotingStake) `shouldBe` poolDeposit

  it "Reproduces #5014: SPO voting stake lags DRep voting stake by an enacted treasury withdrawal" $
    whenPostBootstrap $ do
      (pool, drep, amount) <- setupWithdrawalScenario
      drepVotingStake <- getDRepVotingStake drep
      spoVotingStake <- getSpoVotingStake pool
      (drepVotingStake <-> spoVotingStake) `shouldBe` amount

  it
    "Reproduces #5014: SPO voting stake lags DRep voting stake by the combined refunds and withdrawal"
    $ whenPostBootstrap
    $ do
      (poolActive, drep, combinedDeposit) <- setupCombinedScenario
      drepVotingStake <- getDRepVotingStake drep
      spoVotingStake <- getSpoVotingStake poolActive
      (drepVotingStake <-> spoVotingStake) `shouldBe` combinedDeposit

  it "A reaped pool remains in the leader-election distribution for an extra epoch" $ do
    pool <- setupRetiredPoolInLeaderDistr
    passEpoch
    isPoolInLeaderDistr pool `shouldReturn` True
    passEpoch
    isPoolInLeaderDistr pool `shouldReturn` False

  it "A reaped pool remains in the reward stake snapshot for an extra epoch" $ do
    pool <- setupRetiredPoolInLeaderDistr
    isPoolInRewardSnapshot pool `shouldReturn` True
    passNEpochs 2
    isPoolInRewardSnapshot pool `shouldReturn` True
    passEpoch
    isPoolInRewardSnapshot pool `shouldReturn` False
