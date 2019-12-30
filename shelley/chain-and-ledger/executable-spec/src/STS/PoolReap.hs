{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module STS.PoolReap
  ( POOLREAP
  , PoolreapState(..)
  )
where

import           BaseTypes
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace (..), envGen, sigGen)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Delegation.Certificates
import           EpochBoundary (poolRefunds)
import           GHC.Generics (Generic)
import           Hedgehog (Gen)
import           Ledger.Core (dom, (∈), (∪+), (⋪), (⋫), (▷), (◁))
import           LedgerState
import           Lens.Micro ((^.))
import           PParams
import           Slot
import           TxData (_poolRAcnt)

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

instance HasTrace (POOLREAP crypto) where
  envGen _ = undefined :: Gen PParams
  sigGen _ _ = undefined :: Gen EpochNo

  type BaseEnv (POOLREAP crypto) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
