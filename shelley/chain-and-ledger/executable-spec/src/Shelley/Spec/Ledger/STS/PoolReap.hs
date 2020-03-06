{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.PoolReap
  ( POOLREAP
  , PoolreapState(..)
  )
where

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Ledger.Core (dom, (∈), (∪+), (⋪), (⋫), (▷), (◁))
import           Lens.Micro ((^.))
import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.Delegation.Certificates
import           Shelley.Spec.Ledger.EpochBoundary (poolRefunds)
import           Shelley.Spec.Ledger.LedgerState
import           Shelley.Spec.Ledger.PParams
import           Shelley.Spec.Ledger.Slot
import           Shelley.Spec.Ledger.TxData (_poolRAcnt)

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
    deriving (Show, Eq, Generic)
  initialRules = [pure $ PoolreapState emptyUTxOState emptyAccount emptyDState emptyPState]
  transitionRules = [poolReapTransition]

instance NoUnexpectedThunks (PredicateFailure (POOLREAP crypto))

poolReapTransition :: TransitionRule (POOLREAP crypto)
poolReapTransition = do
  TRC (pp, PoolreapState us a ds ps, e) <- judgmentContext

  firstSlot <- liftSTS $ do
    ei <- asks epochInfo
    epochInfoFirst ei e
  let retired = dom $ (ps ^. retiring) ▷ Set.singleton e
      StakePools stpools = _stPools ps
      pr = poolRefunds pp (retired ◁ stpools) firstSlot
      rewardAcnts = Map.map _poolRAcnt $ retired ◁ (ps ^. pParams)
      rewardAcnts' = Map.fromList . Map.elems $ Map.intersectionWith (,) rewardAcnts pr
      (refunds, mRefunds) = Map.partitionWithKey (\k _ -> k ∈  dom (_rewards ds)) rewardAcnts'
      refunded = sum $ Map.elems refunds
      unclaimed = sum $ Map.elems mRefunds

  pure $ PoolreapState
    us { _deposited = _deposited us - (unclaimed + refunded)}
    a { _treasury = _treasury a + unclaimed }
    ds { _rewards = _rewards ds ∪+ refunds
       , _delegations = _delegations ds ⋫ retired }
    ps { _stPools = StakePools $ retired ⋪ stpools
       , _pParams = retired ⋪ _pParams ps
       , _retiring = retired ⋪ _retiring ps
       }
