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
import Control.Iterate.SetAlgebra (dom, eval, (∈), (∪+), (⋪), (⋫), (▷), (◁))
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
import Shelley.Spec.Ledger.TxData (getRwdCred, _poolRAcnt)
import Shelley.Spec.Ledger.Value

data POOLREAP crypto v

data PoolreapState crypto v = PoolreapState
  { prUTxOSt :: UTxOState crypto v,
    prAcnt :: AccountState,
    prDState :: DState crypto,
    prPState :: PState crypto
  }
  deriving (Show, Eq)

instance CV crypto v => STS (POOLREAP crypto v) where
  type State (POOLREAP crypto v) = PoolreapState crypto v
  type Signal (POOLREAP crypto v) = EpochNo
  type Environment (POOLREAP crypto v) = PParams
  type BaseM (POOLREAP crypto v) = ShelleyBase
  data PredicateFailure (POOLREAP crypto v) -- No predicate Falures
    deriving (Show, Eq, Generic)
  initialRules =
    [ pure $
        PoolreapState emptyUTxOState emptyAccount emptyDState emptyPState
    ]
  transitionRules = [poolReapTransition]

instance NoUnexpectedThunks (PredicateFailure (POOLREAP crypto v))

poolReapTransition :: TransitionRule (POOLREAP crypto v)
poolReapTransition = do
  TRC (pp, PoolreapState us a ds ps, e) <- judgmentContext

  let retired = eval (dom ((_retiring ps) ▷ Set.singleton e))
      pr = Map.fromList $ fmap (\kh -> (kh, _poolDeposit pp)) (Set.toList retired)
      rewardAcnts = Map.map _poolRAcnt $ eval (retired ◁ (_pParams ps))
      rewardAcnts' =
        Map.fromListWith (+)
          . Map.elems
          $ Map.intersectionWith (,) rewardAcnts pr
      (refunds, mRefunds) =
        Map.partitionWithKey
          (\k _ -> eval (k ∈ dom (_rewards ds)))
          (Map.mapKeys getRwdCred rewardAcnts')
      refunded = sum $ Map.elems refunds
      unclaimed = sum $ Map.elems mRefunds

  pure $
    PoolreapState
      us {_deposited = _deposited us - (unclaimed + refunded)}
      a {_treasury = _treasury a + unclaimed}
      ds
        { _rewards = eval (_rewards ds ∪+ refunds),
          _delegations = eval (_delegations ds ⋫ retired)
        }
      ps
        { _pParams = eval (retired ⋪ _pParams ps),
          _fPParams = eval (retired ⋪ _fPParams ps),
          _retiring = eval (retired ⋪ _retiring ps)
        }
