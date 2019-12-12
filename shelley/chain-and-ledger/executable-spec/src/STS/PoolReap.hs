{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.PoolReap
  ( POOLREAP
  , PoolreapState(..)
  )
where

import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Lens.Micro ((^.))
import           BaseTypes
import           Delegation.Certificates
import           EpochBoundary (poolRefunds)
import           LedgerState
import           PParams
import           Slot
import           TxData (_poolRAcnt)
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace(..), envGen, sigGen)

import           Hedgehog (Gen)

import           Ledger.Core (dom, (∈), (∪+), (⋪), (⋫), (▷), (◁))

data POOLREAP crypto

data PoolreapState crypto = PoolreapState
  { prUTxOSt :: UTxOState crypto
  , prAcnt   :: AccountState
  , prDState :: DState crypto
  , prPState :: PState crypto
  }
  deriving (Show, Eq)

instance STS (POOLREAP crypto) where
  type State (POOLREAP crypto) = PoolreapState crypto
  type Signal (POOLREAP crypto) = EpochNo
  type Environment (POOLREAP crypto) = PParams
  type BaseM (POOLREAP crypto) = ShelleyBase
  data PredicateFailure (POOLREAP crypto)
    = FailurePOOLREAP
    deriving (Show, Eq)
  initialRules = [pure $ PoolreapState emptyUTxOState emptyAccount emptyDState emptyPState]
  transitionRules = [poolReapTransition]

poolReapTransition :: TransitionRule (POOLREAP crypto)
poolReapTransition = do
  TRC (pp, PoolreapState us a ds ps, e) <- judgmentContext

  firstSlot <- liftSTS $ do
    ei <- asks epochInfo
    epochInfoFirst ei e
  let retired = dom $ (ps ^. retiring) ▷ Set.singleton e
      StakePools stPools' = ps ^. stPools
      pr = poolRefunds pp (retired ◁ stPools') firstSlot
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

instance HasTrace (POOLREAP crypto) where
  envGen _ = undefined :: Gen PParams
  sigGen _ _ = undefined :: Gen EpochNo

  type BaseEnv (POOLREAP crypto) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
