{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.PoolReap
  ( POOLREAP,
    PoolreapState (..),
    PredicateFailure,
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.Core (dom, (∈), (∪+), (⋪), (⋫), (▷), (◁))
import Shelley.Spec.Ledger.Delegation.Certificates (StakePools (..))
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DState (..),
    PState (..),
    UTxOState (..),
    emptyAccount,
    emptyDState,
    emptyPState,
    emptyUTxOState,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.Slot (EpochNo (..))
import Shelley.Spec.Ledger.TxData (_poolRAcnt)

data POOLREAP crypto

data PoolreapState crypto = PoolreapState
  { prUTxOSt :: UTxOState crypto,
    prAcnt :: AccountState,
    prDState :: DState crypto,
    prPState :: PState crypto
  }
  deriving (Show, Eq)

instance Typeable crypto => STS (POOLREAP crypto) where
  type State (POOLREAP crypto) = PoolreapState crypto
  type Signal (POOLREAP crypto) = EpochNo
  type Environment (POOLREAP crypto) = PParams
  type BaseM (POOLREAP crypto) = ShelleyBase
  data PredicateFailure (POOLREAP crypto) -- No predicate Falures
    deriving (Show, Eq, Generic)
  initialRules =
    [ pure $
        PoolreapState emptyUTxOState emptyAccount emptyDState emptyPState
    ]
  transitionRules = [poolReapTransition]

instance NoUnexpectedThunks (PredicateFailure (POOLREAP crypto))

poolReapTransition :: TransitionRule (POOLREAP crypto)
poolReapTransition = do
  TRC (pp, PoolreapState us a ds ps, e) <- judgmentContext

  let retired = dom $ (_retiring ps) ▷ Set.singleton e
      StakePools stpools = _stPools ps
      pr = Map.fromList $ fmap (\kh -> (kh, _poolDeposit pp)) (Set.toList retired)
      rewardAcnts = Map.map _poolRAcnt $ retired ◁ (_pParams ps)
      rewardAcnts' =
        Map.fromListWith (+)
          . Map.elems
          $ Map.intersectionWith (,) rewardAcnts pr
      (refunds, mRefunds) =
        Map.partitionWithKey
          (\k _ -> k ∈ dom (_rewards ds))
          rewardAcnts'
      refunded = sum $ Map.elems refunds
      unclaimed = sum $ Map.elems mRefunds

  pure $
    PoolreapState
      us {_deposited = _deposited us - (unclaimed + refunded)}
      a {_treasury = _treasury a + unclaimed}
      ds
        { _rewards = _rewards ds ∪+ refunds,
          _delegations = _delegations ds ⋫ retired
        }
      ps
        { _stPools = StakePools $ retired ⋪ stpools,
          _pParams = retired ⋪ _pParams ps,
          _fPParams = retired ⋪ _fPParams ps,
          _retiring = retired ⋪ _retiring ps
        }
