{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.LedgerState.PulsingReward
  ( startStep,
    pulseStep,
    completeStep,
    createRUpd,
    completeRupd,
    circulation,
    updateNonMyopic,
    decayFactor,
  )
where

import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    BlocksMade (..),
    BoundedRational (..),
    NonNegativeInterval,
    ProtVer (..),
    ShelleyBase,
    UnitInterval,
    activeSlotVal,
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    DeltaCoin (..),
    rationalToCoinViaFloor,
    toDeltaCoin,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.Shelley.EpochBoundary
  ( SnapShot (..),
    SnapShots (..),
    Stake (..),
    sumAllStake,
    sumStakePerPool,
  )
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState.DPState
  ( DPState (..),
    rewards,
  )
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.Shelley.PoolRank
  ( Likelihood (..),
    NonMyopic (..),
    applyDecay,
    leaderProbability,
    likelihood,
  )
import Cardano.Ledger.Shelley.RewardProvenance (RewardProvenance (..))
import qualified Cardano.Ledger.Shelley.RewardProvenance as RP
import Cardano.Ledger.Shelley.RewardUpdate
  ( FreeVars (..),
    Pulser,
    PulsingRewUpdate (..),
    RewardAns (..),
    RewardEvent,
    RewardPulser (..),
    RewardSnapShot (..),
    RewardUpdate (..),
  )
import Cardano.Ledger.Shelley.Rewards
  ( PoolRewardInfo (..),
    StakeShare (..),
    leaderRewardToGeneral,
    mkPoolRewardInfo,
    sumRewards,
  )
import Cardano.Ledger.Shelley.TxBody
  ( PoolParams (..),
    RewardAcnt (..),
    getRwdCred,
  )
import Cardano.Ledger.Slot
  ( EpochSize (..),
  )
import Cardano.Ledger.Val ((<->))
import Control.Monad.Trans
import Control.Provenance (ProvM, modifyM, runProvM)
import Data.Default.Class (def)
import Data.Group (invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (Pulsable (..), completeM)
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.UMap as UM
import qualified Data.VMap as VMap
import Data.Word (Word64)
import GHC.Records (HasField (..))
import Numeric.Natural (Natural)

-- =============================
-- To prevent a huge pause, at the stability point, we spread out the
-- Calculation of rewards over many blocks. We do this in 3 phases. Phase 1
-- of a reward upate is a pure computation, computing some parameters which
-- become fixed at the time when we reach the stability point. One of these
-- parameters is a Pulser, i.e. a computation that when pulseM'ed computes
-- a portion of what is required, so that the whole compuation can be spread out in time.

-- | The EpochState has a field which is (Core.PParams era). We need these
--     fields, a subset of the fields in PParams, in: startStep and createRUpd.
type UsesPP era =
  ( HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_nOpt" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  )

-- | Assemble the components for, and then create, a Pulser.
startStep ::
  forall era.
  UsesPP era =>
  EpochSize ->
  BlocksMade (Crypto era) ->
  EpochState era ->
  Coin ->
  ActiveSlotCoeff ->
  Word64 ->
  (PulsingRewUpdate (Crypto era), RewardProvenance (Crypto era))
startStep slotsPerEpoch b@(BlocksMade b') es@(EpochState acnt ss ls pr _ nm) maxSupply asc secparam =
  let SnapShot stake' delegs' poolParams = _pstakeGo ss
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
      Coin reserves = _reserves acnt
      ds = dpsDState $ lsDPState ls
      -- reserves and rewards change
      deltaR1_ =
        rationalToCoinViaFloor $
          min 1 eta_
            * unboundRational (getField @"_rho" pr)
            * fromIntegral reserves
      d_ = unboundRational (getField @"_d" pr)
      expectedBlocks =
        floor $
          (1 - d_) * unboundRational (activeSlotVal asc) * fromIntegral slotsPerEpoch
      -- TODO asc is a global constant, and slotsPerEpoch should not change often at all,
      -- it would be nice to not have to compute expectedBlocks every epoch
      blocksMade = fromIntegral $ Map.foldr (+) 0 b' :: Integer
      eta_
        | unboundRational (getField @"_d" pr) >= 0.8 = 1
        | otherwise = blocksMade % expectedBlocks
      Coin rPot_ = _feeSS ss <> deltaR1_
      deltaT1_ = floor $ unboundRational (getField @"_tau" pr) * fromIntegral rPot_
      _R = Coin $ rPot_ - deltaT1_

      -- We now compute stake pool specific values that are needed for computing
      -- member and leader rewards.
      activestake = sumAllStake stake'
      totalStake_ = circulation es maxSupply
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
          totalStake_
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
            (leaderProbability asc sigma $ getField @"_d" pr)
            slotsPerEpoch
        -- This pool produced at least one block this epoch
        Right info ->
          likelihood
            (poolBlocks info)
            (leaderProbability asc (getSigma info) $ getField @"_d" pr)
            slotsPerEpoch
      newLikelihoods = VMap.toMap $ VMap.map makeLikelihoods allPoolInfo

      -- We now compute the leader rewards for each stake pool.
      collectLRs acc poolRI =
        let rewardAcnt = getRwdCred . _poolRAcnt . poolPs $ poolRI
            packageLeaderReward = Set.singleton . leaderRewardToGeneral . poolLeaderReward
         in if HardForks.forgoRewardPrefilter pr || rewardAcnt `UM.member` rewards ds
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
          { rewFees = _feeSS ss,
            rewprotocolVersion = getField @"_protocolVersion" pr,
            rewNonMyopic = nm,
            rewDeltaR1 = deltaR1_,
            rewR = _R,
            rewDeltaT1 = Coin deltaT1_,
            rewLikelihoods = newLikelihoods,
            rewLeaders = Map.foldl' collectLRs mempty blockProducingPoolInfo
          }

      -- The data in 'FreeVars' to supply individual stake pool members with
      -- the neccessary information to compute their individual rewards.
      free =
        FreeVars
          delegs'
          (UM.domain $ rewards ds)
          (unCoin totalStake_)
          (getField @"_protocolVersion" pr)
          blockProducingPoolInfo
      pulser :: Pulser (Crypto era)
      pulser =
        RSLP
          pulseSize
          free
          (unStake stake')
          (RewardAns Map.empty Map.empty)
      provenance =
        def
          { spe = case slotsPerEpoch of EpochSize n -> n,
            blocks = b,
            blocksCount = blocksMade,
            maxLL = maxSupply,
            deltaR1 = deltaR1_,
            RP.r = _R,
            RP.totalStake = totalStake_,
            RP.activeStake = activestake,
            d = d_,
            expBlocks = expectedBlocks,
            eta = eta_,
            rPot = Coin rPot_,
            deltaT1 = Coin deltaT1_
            -- The reward provenance is in the process of being deprecated,
            -- some fields are not populated anymore, such as the pool provenance
            -- and the desireabilities.
          }
   in (Pulsing rewsnap pulser, provenance)

-- Phase 2

-- | Run the pulser for a bit. If is has nothing left to do, complete it.
pulseStep ::
  PulsingRewUpdate crypto ->
  ShelleyBase (PulsingRewUpdate crypto, RewardEvent crypto)
pulseStep (Complete r_) = pure (Complete r_, mempty)
pulseStep p@(Pulsing _ pulser) | done pulser = completeStep p
pulseStep (Pulsing rewsnap pulser) = do
  -- The pulser might compute provenance, but using pulseM here does not compute it
  p2@(RSLP _ _ _ (RewardAns _ event)) <- pulseM pulser
  pure (Pulsing rewsnap p2, event)

-- Phase 3

completeStep ::
  PulsingRewUpdate crypto ->
  ShelleyBase (PulsingRewUpdate crypto, RewardEvent crypto)
completeStep (Complete r_) = pure (Complete r_, mempty)
completeStep (Pulsing rewsnap pulser) = do
  (p2, !event) <- runProvM (completeRupd (Pulsing rewsnap pulser))
  pure (Complete p2, event)

-- | Phase 3 of reward update has several parts
--   a) completeM the pulser (in case there are still computions to run)
--   b) Combine the pulser provenance with the RewardProvenance
--   c) Construct the final RewardUpdate
--   d) Add the leader rewards to both the events and the computed Rewards
completeRupd ::
  PulsingRewUpdate crypto ->
  ProvM (RewardProvenance crypto) ShelleyBase (RewardUpdate crypto, RewardEvent crypto)
completeRupd (Complete x) = pure (x, mempty)
completeRupd
  ( Pulsing
      rewsnap@RewardSnapShot
        { rewDeltaR1 = deltaR1_,
          rewFees = feesSS,
          rewR = oldr,
          rewDeltaT1 = (Coin deltaT1_),
          rewNonMyopic = nm,
          rewLikelihoods = newLikelihoods,
          rewLeaders = lrewards
        }
      pulser@(RSLP _size _free _source (RewardAns prev _now)) -- If prev is Map.empty, we have never pulsed.
    ) = do
    RewardAns rs_ events <- lift (completeM pulser)
    let rs' = Map.map Set.singleton rs_
    let rs'' = Map.unionWith Set.union rs' lrewards
    let !events' = Map.unionWith Set.union events lrewards

    let deltaR2_ = oldr <-> sumRewards rewsnap rs''
    modifyM (\rp -> rp {deltaR2 = deltaR2_})
    let neverpulsed = Map.null prev
        !newevent =
          if neverpulsed -- If we have never pulsed then everything in the computed needs to added to the event
            then Map.unionWith Set.union rs' events'
            else events'
    pure
      ( RewardUpdate
          { deltaT = DeltaCoin deltaT1_,
            deltaR = invert (toDeltaCoin deltaR1_) <> toDeltaCoin deltaR2_,
            rs = rs'',
            deltaF = invert (toDeltaCoin feesSS),
            nonMyopic = updateNonMyopic nm oldr newLikelihoods
          },
        newevent
      )

-- | To create a reward update, run all 3 phases
--   This function is not used in the rules, so it ignores RewardEvents
createRUpd ::
  forall era.
  (UsesPP era) =>
  EpochSize ->
  BlocksMade (Crypto era) ->
  EpochState era ->
  Coin ->
  ActiveSlotCoeff ->
  Word64 ->
  ProvM (RewardProvenance (Crypto era)) ShelleyBase (RewardUpdate (Crypto era))
createRUpd slotsPerEpoch blocksmade epstate maxSupply asc secparam = do
  let (step1, initialProvenance) = startStep slotsPerEpoch blocksmade epstate maxSupply asc secparam
  modifyM (const initialProvenance)
  (step2, _event) <- lift (pulseStep step1)
  case step2 of
    (Complete r_) -> pure r_
    (Pulsing rewsnap pulser) -> fst <$> completeRupd (Pulsing rewsnap pulser)

-- | Calculate the current circulation
--
-- This is used in the rewards calculation, and for API endpoints for pool ranking.
circulation :: EpochState era -> Coin -> Coin
circulation (EpochState acnt _ _ _ _ _) supply =
  supply <-> _reserves acnt

decayFactor :: Float
decayFactor = 0.9

updateNonMyopic ::
  NonMyopic crypto ->
  Coin ->
  Map (KeyHash 'StakePool crypto) Likelihood ->
  NonMyopic crypto
updateNonMyopic nm rPot_ newLikelihoods =
  nm
    { likelihoodsNM = updatedLikelihoods,
      rewardPotNM = rPot_
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
