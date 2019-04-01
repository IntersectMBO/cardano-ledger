{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.PoolReap
  ( POOLREAP
  ) where

import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set

import           Lens.Micro              ((^.), (&), (.~), (%~))

import           LedgerState
import           PParams
import           Slot
import           Delegation.Certificates
import           Coin

import           Control.State.Transition

data POOLREAP

instance STS POOLREAP where
  type State POOLREAP = (AccountState, DState, PState)
  type Signal POOLREAP = Epoch
  type Environment POOLREAP = PParams
  data PredicateFailure POOLREAP = FailurePOOLREAP
                                   deriving (Show, Eq)
  initialRules = [pure (emptyAccount, emptyDState, emptyPState)]
  transitionRules = [poolReapTransition]

poolReapTransition :: TransitionRule POOLREAP
poolReapTransition = do
  TRC (pp, (a, d, p), eNew) <- judgmentContext
  let retired = Map.keysSet $ Map.filter (== eNew) $ p ^. retiring
  let pr = poolRefunds pp (p ^. retiring) (firstSlot eNew)
  let relevant = Map.restrictKeys (p ^. pParams) retired
  let rewardAcnts =
        Map.mapMaybeWithKey
          (\k v ->
             case Map.lookup k relevant of
               Nothing -> Nothing
               Just _  -> Just (v ^. poolRAcnt))
          (p ^. pParams)
  let rewardAcnts' = Map.restrictKeys rewardAcnts (Map.keysSet pr)
  let refunds' =
        Map.foldlWithKey
          (\m k addr -> Map.insert addr (pr Map.! k) m)
          Map.empty
          rewardAcnts' -- not yet restricted to a in dom(rewards)
  let domRewards = Map.keysSet (d ^. rewards)
  let (refunds, unclaimed') =
        Map.partitionWithKey (\k _ -> k `Set.member` domRewards) refunds'
  let unclaimed = Map.foldl (+) (Coin 0) unclaimed'
  let StakePools stakePools = p ^. stPools
  let Avgs averages = p ^. avgs
  pure
    ( a & treasury %~ (+) unclaimed
    , d & rewards %~ flip Map.union refunds &
      delegations %~ flip Map.withoutKeys retired
    , p & stPools .~ (StakePools $ Map.withoutKeys stakePools retired) &
      pParams %~ flip Map.withoutKeys retired &
      retiring %~ flip Map.withoutKeys retired &
      avgs .~ (Avgs $ Map.withoutKeys averages retired))
