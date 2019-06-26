{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.PoolReap
  ( POOLREAP
  )
where

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           Lens.Micro                     ( (^.)
                                                , (&)
                                                , (.~)
                                                , (%~)
                                                )

import           LedgerState
import           PParams
import           Slot
import           Delegation.Certificates
import           Coin

import           Control.State.Transition

import           Ledger.Core ((◁))

data POOLREAP hashAlgo dsignAlgo

instance STS (POOLREAP hashAlgo dsignAlgo) where
  type State (POOLREAP hashAlgo dsignAlgo)
    = (AccountState, DState hashAlgo dsignAlgo, PState hashAlgo dsignAlgo)
  type Signal (POOLREAP hashAlgo dsignAlgo) = Epoch
  type Environment (POOLREAP hashAlgo dsignAlgo) = PParams
  data PredicateFailure (POOLREAP hashAlgo dsignAlgo)
    = FailurePOOLREAP
    deriving (Show, Eq)
  initialRules = [pure (emptyAccount, emptyDState, emptyPState)]
  transitionRules = [poolReapTransition]

poolReapTransition :: TransitionRule (POOLREAP hashAlgo dsignAlgo)
poolReapTransition = do
  TRC (pp, (a, ds, ps), e) <- judgmentContext
  let retired  = Map.keysSet $ Map.filter (== e) $ ps ^. retiring
  let pr       = poolRefunds pp (ps ^. retiring) (firstSlot e)
  let relevant = retired ◁ (ps ^. pParams)
  let rewardAcnts = Map.mapMaybeWithKey
        (\k v -> case Map.lookup k relevant of
          Nothing -> Nothing
          Just _  -> Just (v ^. poolRAcnt)
        )
        (ps ^. pParams)
  let rewardAcnts' = (Map.keysSet pr) ◁ rewardAcnts
  let refunds' = Map.foldlWithKey
        (\m k addr -> Map.insert addr (pr Map.! k) m)
        Map.empty
        rewardAcnts' -- not yet restricted to a in dom(rewards)
  let domRewards = Map.keysSet (ds ^. rewards)
  let (refunds, unclaimed') =
        Map.partitionWithKey (\k _ -> k `Set.member` domRewards) refunds'
  let unclaimed             = Map.foldl (+) (Coin 0) unclaimed'
  let StakePools stakePools = ps ^. stPools
  pure
    ( a & treasury %~ (+) unclaimed
    , ds
    &  rewards
    %~ flip Map.union refunds
    &  delegations
    %~ flip Map.withoutKeys retired
    , ps
    &  stPools
    .~ (StakePools $ Map.withoutKeys stakePools retired)
    &  pParams
    %~ flip Map.withoutKeys retired
    &  retiring
    %~ flip Map.withoutKeys retired
    )
