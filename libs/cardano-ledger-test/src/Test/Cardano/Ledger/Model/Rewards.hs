{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Model.Rewards where

import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    UnitInterval,
    activeSlotVal,
    unboundRational,
  )
import Cardano.Ledger.Coin (Coin, coinToRational, rationalToCoinViaFloor)
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (EpochSize)
import Control.Lens
import Control.Monad (unless)
import Control.Monad.State.Class (MonadState)
import Data.Foldable (fold, toList)
import Data.Group (Group (..))
import Data.Group.GrpMap (GrpMap (..), mkGrpMap)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Natural (Natural)
import qualified GHC.Records as GHC
import Test.Cardano.Ledger.Model.Acnt
  ( ModelAcntF (..),
    modelAcnt_reserves,
    modelAcnt_treasury,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelBlocksMade (..),
    ModelPoolId,
    PositiveRational,
    boundPositiveRational,
    unboundPositiveRational,
  )
import Test.Cardano.Ledger.Model.FeatureSet (ScriptFeature)
import Test.Cardano.Ledger.Model.LedgerState
  ( ModelDPState (..),
    ModelDState (..),
    ModelEpochState (..),
    ModelLState (..),
    ModelRewardUpdate (..),
    ModelSnapshot (..),
    ModelSnapshotStake (..),
    ModelSnapshots (..),
    modelDPStatedpsDState,
    modelDState_rewards,
    modelEpochState_acnt,
    modelEpochState_ls,
    modelLState_dpstate,
    modelLState_utxoSt,
    modelUTxOState_fees,
  )
import Test.Cardano.Ledger.Model.PParams (UsesModelPP)
import Test.Cardano.Ledger.Model.Prov (MonadModelProvenance (..))
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential,
    liftModelCredential,
  )
import Test.Cardano.Ledger.Model.Snapshot
  ( SnapshotQueue (..),
  )
import Test.Cardano.Ledger.Model.Tx
  ( ModelPoolParams (..),
  )
import Test.Cardano.Ledger.Rational (unsafeFromRational)

-- | fig 50
-- | Reward Update Application
-- [SL-D5] Figure 52
applyRUpd :: (MonadState (ModelEpochState era) m) => ModelRewardUpdate era -> m ()
applyRUpd (ModelRewardUpdate deltaT deltaR (GrpMap rs) deltaF) = do
  rewards <- use $ modelEpochState_ls . modelLState_dpstate . modelDPStatedpsDState . modelDState_rewards
  let regRU = Map.intersection rs rewards
      unregRU = Map.difference rs rewards
      unregRU' = fold unregRU
  modelEpochState_acnt . modelAcnt_treasury <>= deltaT <> unregRU'
  modelEpochState_acnt . modelAcnt_reserves <>= deltaR
  modelEpochState_ls . modelLState_dpstate . modelDPStatedpsDState . modelDState_rewards
    %= Map.unionWith (<>) regRU
  modelEpochState_ls . modelLState_utxoSt . modelUTxOState_fees <>= deltaF

validModelRewardUpdate ::
  ModelRewardUpdate era ->
  Either (NonEmpty.NonEmpty String) (ModelRewardUpdate era)
validModelRewardUpdate ru@(ModelRewardUpdate dTreasury dReserves dRewards dFees) =
  case NonEmpty.nonEmpty $
    concat
      [ ["unbalanced" | dTreasury <> dReserves <> fold dRewards <> dFees /= mempty],
        ["treasury<0" | dTreasury < mempty],
        ["reserves>0" | dReserves > mempty],
        ["fees>0" | dFees > mempty],
        ["negative rewards" | not $ Map.null $ Map.filter (< mempty) $ unGrpMap dRewards]
      ] of
    Nothing -> Right ru
    Just errors -> Left errors

-- | calculation to create a reward update
-- [SL-D5] Figure 51
createRUpd :: MonadModelProvenance era provM => ModelRUpdEnv era -> EpochSize -> Coin -> ActiveSlotCoeff -> provM (ModelRewardUpdate era)
createRUpd
  ( ModelRUpdEnv
      { _modelRUpdEnv_b = b,
        _modelRUpdEnv_es =
          ModelEpochState
            { _modelEpochState_acnt = ModelAcnt {_modelAcnt_reserves = reserves},
              _modelEpochState_ss =
                ModelSnapshots
                  { _modelSnapshots_pstake =
                      SnapshotQueue
                        { _snapshotQueue_go =
                            ModelSnapshot
                              { _modelSnapshot_stake = stake,
                                _modelSnapshot_delegations = delegs,
                                _modelSnapshot_pools = poolSS
                              }
                        },
                    _modelSnapshots_feeSS = feeSS
                  },
              _modelEpochState_ls =
                ModelLState
                  { _modelLState_dpstate =
                      ModelDPState
                        { _modelDPStatedpsDState =
                            ModelDState
                              { _modelDState_rewards = rewards
                              }
                        }
                  },
              _modelEpochState_prevPp = prevPp
            }
      }
    )
  (toRational -> slotsPerEpoch)
  total
  (unboundRational . activeSlotVal -> activeSlotCoeff') = do
    let d = unboundRational $ GHC.getField @"_d" prevPp
        tau = unboundRational $ GHC.getField @"_tau" prevPp
        rho = unboundRational $ GHC.getField @"_rho" prevPp

        deltaR1 = rationalToCoinViaFloor (min 1 eta * rho * coinToRational reserves)
        eta
          | d >= 0.8 = 1
          | otherwise = blocksMade / toRational (floor $ (1 - d) * slotsPerEpoch * activeSlotCoeff' :: Integer)
        rewardPot = feeSS <> deltaR1
        deltaT1 = rationalToCoinViaFloor (tau * coinToRational rewardPot)
        r = rewardPot ~~ deltaT1
        circulation = total ~~ reserves
        blocksMade = toRational $ sum (unModelBlocksMade b)
    rs <- rewardModel prevPp b r (Map.keysSet rewards) poolSS stake delegs circulation
    let deltaR2 = r ~~ fold rs
    pure $
      ModelRewardUpdate
        { _modelRewardUpdate_treasury = deltaT1,
          _modelRewardUpdate_reserves = deltaR2 ~~ deltaR1,
          _modelRewardUpdate_rewards = mkGrpMap rs,
          _modelRewardUpdate_fees = invert feeSS
        }

data ModelRUpdEnv era = ModelRUpdEnv
  { _modelRUpdEnv_b :: !ModelBlocksMade,
    _modelRUpdEnv_es :: !(ModelEpochState era)
  }
  deriving (Show)

-- |
-- [SL-D5] Figure 46
-- [SL-D1] 5.5.2 f(s, σ)
maxPoolModel :: UsesModelPP pparams => pparams -> Coin -> PositiveRational -> UnitInterval -> Coin
maxPoolModel
  pp
  (coinToRational -> r)
  (unboundPositiveRational -> sigma)
  (unboundRational -> pr) =
    rationalToCoinViaFloor (r / (1 + a0) * (sigma' + p' * a0 * (sigma' - p' * (z0 - sigma') / z0) / z0))
    where
      a0 = unboundRational $ GHC.getField @"_a0" pp
      nOpt = toRational $ GHC.getField @"_nOpt" pp
      z0 = 1 / nOpt
      sigma' = min sigma z0
      p' = min pr z0

-- |
-- [SL-D5] Figure 46
-- [SL-D1] 5.5.2 (p-hat)
mkApparentPerformance ::
  UnitInterval ->
  Rational ->
  Natural ->
  Natural ->
  Rational
mkApparentPerformance (unboundRational -> d) sigma (toRational -> n) (toRational -> nbar)
  | d < 0.8 = beta / sigma
  | otherwise = 1
  where
    beta = n / max 1 nbar

-- |
-- [SL-D5] Figure 47
-- [SL-D1] 5.5.3
r_operator :: MonadModelProvenance era provM => ModelSnapshotStake -> Coin -> ModelPoolParams era -> Rational -> PositiveRational -> provM Coin
r_operator stk f_hat (ModelPoolParams {_mppId = poolId, _mppCost = c, _mppMargin = (unboundRational -> m), _mppRAcnt = ra}) s (unboundPositiveRational -> sigma) = do
  unless (f_hat <= mempty) $ rewardOperatorProvenance poolId $ Map.singleton ra $ _modelSnapshotStake_utxos stk
  pure $
    if (f_hat <= c)
      then f_hat
      else c <> rationalToCoinViaFloor ((coinToRational $ f_hat ~~ c) * (m + (1 - m) * s / sigma))

-- |
-- [SL-D5] Figure 47
-- [SL-D1] 5.5.3
r_member :: MonadModelProvenance era provM => (ModelCredential 'Staking (ScriptFeature era), ModelSnapshotStake) -> Coin -> ModelPoolParams era -> Rational -> PositiveRational -> provM Coin
r_member (hk, stk) f_hat (ModelPoolParams {_mppId = poolId, _mppCost = c, _mppMargin = (unboundRational -> m)}) t (unboundPositiveRational -> sigma)
  | f_hat <= c = pure $ Val.zero
  | otherwise = do
      unless (f_hat <= mempty) $ rewardMemberProvenance poolId $ Map.singleton hk $ _modelSnapshotStake_utxos stk
      pure $ rationalToCoinViaFloor ((coinToRational $ f_hat ~~ c) * (1 - m) * t / sigma)

-- Fig48 [SL-D5]
rewardOnePoolModel ::
  ( UsesModelPP pparams,
    MonadModelProvenance era provM
  ) =>
  pparams ->
  Coin ->
  Natural ->
  Natural ->
  ModelPoolParams era ->
  Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelSnapshotStake ->
  PositiveRational ->
  Rational ->
  Coin ->
  Set (ModelCredential 'Staking (ScriptFeature era)) ->
  provM (Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin)
rewardOnePoolModel
  pp
  r
  n
  nbar
  pool@(ModelPoolParams {_mppPledge = (coinToRational -> pledge)})
  stake
  sigma
  sigma_a
  (coinToRational -> tot)
  addrs_rew = do
    let poolOwners = Set.fromList $ fmap liftModelCredential $ _mppOwners pool
        ostake' = fold $ Map.restrictKeys stake poolOwners
        ostake = coinToRational $ _modelSnapshotStake_balance ostake'
        p_r = unsafeFromRational "rewardOnePoolModel::p_r" $ pledge / tot

        maxP
          | pledge <= ostake = maxPoolModel pp r sigma p_r
          | otherwise = Val.zero

        appPerf = mkApparentPerformance (GHC.getField @"_d" pp) sigma_a n nbar
        poolR = rationalToCoinViaFloor (appPerf * coinToRational maxP)

    mRewards <-
      fmap Map.fromList $
        sequence
          [ (,) hk <$> r_member (hk, stk) poolR pool c_over_tot sigma
            | (hk, stk) <- Map.toList stake,
              let c = coinToRational $ _modelSnapshotStake_balance stk,
              let c_over_tot = c / tot,
              not (Set.member hk poolOwners)
          ]
    iReward <-
      r_operator
        ostake'
        poolR
        pool
        -- (unsafeFromRational "rewardOnePoolModel::ostake/tot" $ ostake / tot)
        (ostake / tot)
        sigma
    let potentialRewards = Map.unionWith (<>) mRewards (Map.singleton (_mppRAcnt pool) iReward)
        rewards = Map.restrictKeys potentialRewards addrs_rew
    pure rewards

-- |
-- [SL-D5] Figure 34
poolStakeModel ::
  ModelPoolId ->
  Map.Map (ModelCredential 'Staking sf) ModelPoolId ->
  Map.Map (ModelCredential 'Staking sf) a ->
  Map.Map (ModelCredential 'Staking sf) a
poolStakeModel hk delegs stake = Map.intersection stake (Map.filter (== hk) delegs)

-- Fig48[SL-D5]
rewardModel ::
  forall era pparams provM.
  ( UsesModelPP pparams,
    MonadModelProvenance era provM
  ) =>
  pparams ->
  ModelBlocksMade ->
  Coin ->
  Set (ModelCredential 'Staking (ScriptFeature era)) ->
  Map.Map ModelPoolId (ModelPoolParams era) ->
  Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelSnapshotStake ->
  Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelPoolId ->
  Coin ->
  provM (Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin)
rewardModel
  pp
  (ModelBlocksMade blocks)
  r
  addrs_rew
  poolParams
  stake
  delegs
  total = do
    let total_a = coinToRational $ foldMap _modelSnapshotStake_balance stake
        nbar = sum blocks
        pdata :: Map.Map ModelPoolId (ModelPoolParams era, Natural, Map.Map (ModelCredential 'Staking (ScriptFeature era)) ModelSnapshotStake)
        pdata =
          Map.intersectionWithKey
            (\hk p n -> (p, n, poolStakeModel hk delegs stake))
            poolParams
            blocks

    results ::
      Map.Map ModelPoolId (Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin) <-
      Map.fromList
        <$> sequence
          [ (,) hk <$> rewardOnePoolModel pp r n nbar p s sigma sigma_a total addrs_rew
            | (hk, (p, n, s)) <- Map.toList pdata,
              let sbar = coinToRational (foldMap _modelSnapshotStake_balance s),
              total_a > 0,
              total /= mempty,
              sigma_a <- [sa | let sa = sbar / total_a, sa >= 0],
              {-sigma_a <- case boundRational' $ sbar / total_a of
                Underflow -> []
                InBounds x -> [x]
                Overflow -> error $ "sigma_a overflow: " <> show (sbar / total_a), -}
              sigma <- toList $ boundPositiveRational (sbar / coinToRational total)
          ]
    let rewards = Map.unionsWith (<>) results
    pure rewards
