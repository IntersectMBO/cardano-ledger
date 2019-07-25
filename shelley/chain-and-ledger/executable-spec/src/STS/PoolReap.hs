{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.PoolReap
  ( POOLREAP
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Lens.Micro ((^.))

import           Coin
import           Delegation.Certificates
import           LedgerState
import           PParams
import           Slot

import           Control.State.Transition

import           Ledger.Core ((∪+), (⋪), (◁))

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
  let rewardAcnts' = Map.keysSet pr ◁ rewardAcnts
  let refunds' = Map.foldlWithKey
        (\m k addr -> Map.insert addr (pr Map.! k) m)
        Map.empty
        rewardAcnts' -- not yet restricted to a in dom(rewards)
  let domRewards = Map.keysSet (ds ^. rewards)
  let (refunds, unclaimed') =
        Map.partitionWithKey (\k _ -> k `Set.member` domRewards) refunds'
  let unclaimed             = Map.foldl (+) (Coin 0) unclaimed'
  let StakePools stakePools = ps ^. stPools

  let treasury' = _treasury a + unclaimed

  let rewards'  = _rewards ds ∪+ refunds
  let delegations' = Map.filter (`Set.notMember` retired) (_delegations ds)

  let stPools' = StakePools $ retired ⋪ stakePools
  let pParams' = retired ⋪ _pParams ps
  let retiring' = retired ⋪ _retiring ps
  let cs' = retired ⋪ _cCounters ps
  pure
    ( a { _treasury = treasury' }
    , ds { _rewards = rewards'
         , _delegations = delegations' }
    , ps { _stPools = stPools'
         , _pParams = pParams'
         , _retiring = retiring'
         , _cCounters = cs'})
