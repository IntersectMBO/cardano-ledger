{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.PoolReap
  ( POOLREAP
  , PoolreapState(..)
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Lens.Micro ((^.))

import           Delegation.Certificates
import           LedgerState
import           PParams
import           Slot
import           TxData (_poolRAcnt)

import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Hedgehog (Gen)

import           Ledger.Core (dom, (∈), (∪+), (⋪), (⋫), (▷), (◁))

data POOLREAP hashAlgo dsignAlgo vrfAlgo

data PoolreapState hashAlgo dsignAlgo vrfAlgo = PoolreapState
  { prUTxOSt :: UTxOState hashAlgo dsignAlgo vrfAlgo
  , prAcnt   :: AccountState
  , prDState :: DState hashAlgo dsignAlgo
  , prPState :: PState hashAlgo dsignAlgo vrfAlgo
  }
  deriving (Show, Eq)

instance STS (POOLREAP hashAlgo dsignAlgo vrfAlgo) where
  type State (POOLREAP hashAlgo dsignAlgo vrfAlgo) = PoolreapState hashAlgo dsignAlgo vrfAlgo
  type Signal (POOLREAP hashAlgo dsignAlgo vrfAlgo) = Epoch
  type Environment (POOLREAP hashAlgo dsignAlgo vrfAlgo) = PParams
  data PredicateFailure (POOLREAP hashAlgo dsignAlgo vrfAlgo)
    = FailurePOOLREAP
    deriving (Show, Eq)
  initialRules = [pure $ PoolreapState emptyUTxOState emptyAccount emptyDState emptyPState]
  transitionRules = [poolReapTransition]

poolReapTransition :: TransitionRule (POOLREAP hashAlgo dsignAlgo vrfAlgo)
poolReapTransition = do
  TRC (pp, PoolreapState us a ds ps, e) <- judgmentContext

  let retired = dom $ (ps ^. retiring) ▷ Set.singleton e
      StakePools stPools' = ps ^. stPools
      pr = poolRefunds pp (retired ◁ stPools') (firstSlot e)
      rewardAcnts = Map.map _poolRAcnt $ retired ◁ (ps ^. pParams)
      rewardAcnts' = Map.fromList . Map.elems $ Map.intersectionWith (,) rewardAcnts pr

      domRewards = dom (ds ^. rewards)
      (refunds, mRefunds) = Map.partitionWithKey (\k _ -> k ∈ domRewards) rewardAcnts'
      refunded = sum $ Map.elems refunds
      unclaimed = sum $ Map.elems mRefunds

      StakePools stakePools = ps ^. stPools

  pure $ PoolreapState
    us { _deposited = _deposited us - (unclaimed + refunded)}
    a { _treasury = _treasury a + unclaimed }
    ds { _rewards = _rewards ds ∪+ refunds
       , _delegations = _delegations ds ⋫ retired }
    ps { _stPools = StakePools $ retired ⋪ stakePools
       , _pParams = retired ⋪ _pParams ps
       , _retiring = retired ⋪ _retiring ps
       }

instance HasTrace (POOLREAP hashAlgo dsignAlgo vrfAlgo) where
  envGen _ = undefined :: Gen PParams
  sigGen _ _ = undefined :: Gen Epoch
