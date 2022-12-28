{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Shelley.LedgerState.PulsingReward (
  startStep,
  pulseStep,
  completeStep,
  createRUpd,
  completeRupd,
  circulation,
  updateNonMyopic,
  decayFactor,
)
where

import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  BlocksMade (..),
  BoundedRational (..),
  ShelleyBase,
  activeSlotVal,
 )
import Cardano.Ledger.Coin (
  Coin (..),
  DeltaCoin (..),
  rationalToCoinViaFloor,
  toDeltaCoin,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.DPState (
  DPState (..),
  rewards,
 )
import Cardano.Ledger.EpochBoundary (
  SnapShot (..),
  SnapShots (..),
  Stake (..),
  sumAllStake,
  sumStakePerPool,
 )
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.Shelley.PoolRank (
  Likelihood (..),
  NonMyopic (..),
  applyDecay,
  leaderProbability,
  likelihood,
 )
import Cardano.Ledger.Shelley.RewardUpdate (
  FreeVars (..),
  Pulser,
  PulsingRewUpdate (..),
  RewardAns (..),
  RewardEvent,
  RewardPulser (..),
  RewardSnapShot (..),
  RewardUpdate (..),
 )
import Cardano.Ledger.Shelley.Rewards (
  PoolRewardInfo (..),
  StakeShare (..),
  leaderRewardToGeneral,
  mkPoolRewardInfo,
  sumRewards,
 )
import Cardano.Ledger.Shelley.TxBody (
  PoolParams (..),
  RewardAcnt (..),
  getRwdCred,
 )
import Cardano.Ledger.Slot (EpochSize (..))
import qualified Cardano.Ledger.UMapCompact as UM
import Cardano.Ledger.Val ((<->))
import Data.Group (invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (Pulsable (..), completeM)
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import Data.Word (Word64)
import Lens.Micro ((^.))

-- =============================
-- To prevent a huge pause, at the stability point, we spread out the
-- Calculation of rewards over many blocks. We do this in 3 phases. Phase 1
-- of a reward upate is a pure computation, computing some parameters which
-- become fixed at the time when we reach the stability point. One of these
-- parameters is a Pulser, i.e. a computation that when pulseM'ed computes
-- a portion of what is required, so that the whole compuation can be spread out in time.

startStep ::
  forall era.
  EraPParams era =>
  EpochSize ->
  BlocksMade (EraCrypto era) ->
  EpochState era ->
  Coin ->
  ActiveSlotCoeff ->
  Word64 ->
  PulsingRewUpdate (EraCrypto era)
startStep slotsPerEpoch b@(BlocksMade b') es@(EpochState acnt ss ls pr _ nm) maxSupply asc secparam =
  let SnapShot stake' delegs' poolParams = ssStakeGo ss
      numStakeCreds, k :: Rational
      numStakeCreds = fromIntegral (VMap.size $ unStake stake')
      k = fromIntegral secparam
      -- We expect approximately 10k-many blocks to be produced each epoch.
      -- The reward calculation begins (4k/f)-many slots into the epoch,
      -- and we guarantee that it ends (2k/f)-many slots before the end
      -- of the epoch (to allow tools such as db-sync to see the reward
      -- values in advance of them being applied to the ledger state).
      --
      -- Therefore to evenly space out the reward calculation, we divide
      -- the number of stake credentials by 4k in order to determine how many
      -- stake credential rewards we should calculate each block.
      -- If it does not finish in this amount of time, the calculation is
      -- forced to completion.
      pulseSize = max 1 (ceiling (numStakeCreds / (4 * k)))
      -- We now compute the amount of total rewards that can potentially be given
      -- out this epoch, and the adjustments to the reserves and the treasury.
      Coin reserves = asReserves acnt
      ds = dpsDState $ lsDPState ls
      -- reserves and rewards change
      deltaR1 =
        rationalToCoinViaFloor $
          min 1 eta
            * unboundRational (pr ^. ppRhoL)
            * fromIntegral reserves
      d = unboundRational (pr ^. ppDG)
      expectedBlocks =
        floor $
          (1 - d) * unboundRational (activeSlotVal asc) * fromIntegral slotsPerEpoch
      -- TODO asc is a global constant, and slotsPerEpoch should not change often at all,
      -- it would be nice to not have to compute expectedBlocks every epoch
      blocksMade = fromIntegral $ Map.foldr (+) 0 b' :: Integer
      eta
        | unboundRational (pr ^. ppDG) >= 0.8 = 1
        | otherwise = blocksMade % expectedBlocks
      Coin rPot = ssFee ss <> deltaR1
      deltaT1 = floor $ unboundRational (pr ^. ppTauL) * fromIntegral rPot
      _R = Coin $ rPot - deltaT1
      -- We now compute stake pool specific values that are needed for computing
      -- member and leader rewards.
      activestake = sumAllStake stake'
      _totalStake = circulation es maxSupply
      stakePerPool = sumStakePerPool delegs' stake'
      mkPoolRewardInfoCurry =
        mkPoolRewardInfo
          pr
          _R
          b
          (fromIntegral blocksMade)
          stake'
          delegs'
          stakePerPool
          _totalStake
          activestake
      -- We map over the registered stake pools to compute the revelant
      -- stake pool specific values.
      allPoolInfo = VMap.map mkPoolRewardInfoCurry poolParams

      -- Stake pools that do not produce any blocks get no rewards,
      -- but some information is still needed from non-block-producing
      -- pools for the ranking algorithm used by the wallets.
      blockProducingPoolInfo = VMap.toMap $ VMap.mapMaybe (either (const Nothing) Just) allPoolInfo
      getSigma = unStakeShare . poolRelativeStake
      makeLikelihoods = \case
        -- This pool produced no blocks this epoch
        Left (StakeShare sigma) ->
          likelihood
            0
            (leaderProbability asc sigma $ pr ^. ppDG)
            slotsPerEpoch
        -- This pool produced at least one block this epoch
        Right info ->
          likelihood
            (poolBlocks info)
            (leaderProbability asc (getSigma info) $ pr ^. ppDG)
            slotsPerEpoch
      newLikelihoods = VMap.toMap $ VMap.map makeLikelihoods allPoolInfo
      -- We now compute the leader rewards for each stake pool.
      collectLRs acc poolRI =
        let rewardAcnt = getRwdCred . ppRewardAcnt . poolPs $ poolRI
            packageLeaderReward = Set.singleton . leaderRewardToGeneral . poolLeaderReward
         in if HardForks.forgoRewardPrefilter (pr ^. ppProtocolVersionL) || rewardAcnt `UM.member` rewards ds
              then
                Map.insertWith
                  Set.union
                  rewardAcnt
                  (packageLeaderReward poolRI)
                  acc
              else acc
      -- The data in 'RewardSnapShot' will be used to finish up the reward calculation
      -- once all the member rewards are complete.
      rewsnap =
        RewardSnapShot
          { rewFees = ssFee ss
          , rewprotocolVersion = pr ^. ppProtocolVersionL
          , rewNonMyopic = nm
          , rewDeltaR1 = deltaR1
          , rewR = _R
          , rewDeltaT1 = Coin deltaT1
          , rewLikelihoods = newLikelihoods
          , rewLeaders = Map.foldl' collectLRs mempty blockProducingPoolInfo
          }
      -- The data in 'FreeVars' to supply individual stake pool members with
      -- the neccessary information to compute their individual rewards.
      free =
        FreeVars
          delegs'
          (UM.domain $ rewards ds)
          (unCoin _totalStake)
          (pr ^. ppProtocolVersionL)
          blockProducingPoolInfo
      pulser :: Pulser (EraCrypto era)
      pulser =
        RSLP
          pulseSize
          free
          (unStake stake')
          (RewardAns Map.empty Map.empty)
   in Pulsing rewsnap pulser

-- Phase 2

-- | Run the pulser for a bit. If is has nothing left to do, complete it.
pulseStep ::
  PulsingRewUpdate c ->
  ShelleyBase (PulsingRewUpdate c, RewardEvent c)
pulseStep (Complete r_) = pure (Complete r_, mempty)
pulseStep p@(Pulsing _ pulser) | done pulser = completeStep p
pulseStep (Pulsing rewsnap pulser) = do
  -- The pulser might compute provenance, but using pulseM here does not compute it
  p2@(RSLP _ _ _ (RewardAns _ event)) <- pulseM pulser
  pure (Pulsing rewsnap p2, event)

-- Phase 3

completeStep ::
  PulsingRewUpdate c ->
  ShelleyBase (PulsingRewUpdate c, RewardEvent c)
completeStep (Complete r) = pure (Complete r, mempty)
completeStep (Pulsing rewsnap pulser) = do
  (p2, !event) <- completeRupd (Pulsing rewsnap pulser)
  pure (Complete p2, event)

-- | Phase 3 of reward update has several parts
--   a) completeM the pulser (in case there are still computions to run)
--   b) Combine the pulser provenance with the RewardProvenance
--   c) Construct the final RewardUpdate
--   d) Add the leader rewards to both the events and the computed Rewards
completeRupd ::
  PulsingRewUpdate c ->
  ShelleyBase (RewardUpdate c, RewardEvent c)
completeRupd (Complete x) = pure (x, mempty)
completeRupd
  ( Pulsing
      RewardSnapShot
        { rewDeltaR1 = deltaR1
        , rewFees = feesSS
        , rewR = oldr
        , rewDeltaT1 = (Coin deltaT1)
        , rewNonMyopic = nm
        , rewLikelihoods = newLikelihoods
        , rewLeaders = lrewards
        , rewprotocolVersion = protVer
        }
      pulser@(RSLP _size _free _source (RewardAns prev _now)) -- If prev is Map.empty, we have never pulsed.
    ) = do
    RewardAns rs_ events <- completeM pulser
    let rs' = Map.map Set.singleton rs_
    let rs'' = Map.unionWith Set.union rs' lrewards
    let !events' = Map.unionWith Set.union events lrewards

    let deltaR2 = oldr <-> sumRewards protVer rs''
    let neverpulsed = Map.null prev
        !newevent =
          if neverpulsed -- If we have never pulsed then everything in the computed needs to added to the event
            then Map.unionWith Set.union rs' events'
            else events'
    pure
      ( RewardUpdate
          { deltaT = DeltaCoin deltaT1
          , deltaR = invert (toDeltaCoin deltaR1) <> toDeltaCoin deltaR2
          , rs = rs''
          , deltaF = invert (toDeltaCoin feesSS)
          , nonMyopic = updateNonMyopic nm oldr newLikelihoods
          }
      , newevent
      )

-- | To create a reward update, run all 3 phases
--   This function is not used in the rules, so it ignores RewardEvents
createRUpd ::
  forall era.
  EraPParams era =>
  EpochSize ->
  BlocksMade (EraCrypto era) ->
  EpochState era ->
  Coin ->
  ActiveSlotCoeff ->
  Word64 ->
  ShelleyBase (RewardUpdate (EraCrypto era))
createRUpd slotsPerEpoch blocksmade epstate maxSupply asc secparam = do
  let step1 = startStep slotsPerEpoch blocksmade epstate maxSupply asc secparam
  (step2, _event) <- pulseStep step1
  case step2 of
    (Complete r) -> pure r
    (Pulsing rewsnap pulser) -> fst <$> completeRupd (Pulsing rewsnap pulser)

-- | Calculate the current circulation
--
-- This is used in the rewards calculation, and for API endpoints for pool ranking.
circulation :: EpochState era -> Coin -> Coin
circulation (EpochState acnt _ _ _ _ _) supply =
  supply <-> asReserves acnt

decayFactor :: Float
decayFactor = 0.9

updateNonMyopic ::
  NonMyopic c ->
  Coin ->
  Map (KeyHash 'StakePool c) Likelihood ->
  NonMyopic c
updateNonMyopic nm rPot_ newLikelihoods =
  nm
    { likelihoodsNM = updatedLikelihoods
    , rewardPotNM = rPot_
    }
  where
    history = likelihoodsNM nm
    performance kh newPerf =
      maybe
        mempty
        (applyDecay decayFactor)
        (Map.lookup kh history)
        <> newPerf
    updatedLikelihoods = Map.mapWithKey performance newLikelihoods
