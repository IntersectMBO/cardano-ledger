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
  ( Assertion (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.EpochBoundary (obligation)
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

  assertions =
    [ PostCondition
        "Deposit pot must equal obligation"
        ( \(TRC (pp, _, _)) st ->
            obligation pp (_rewards $ prDState st) (_pParams $ prPState st)
              == _deposited (prUTxOSt st)
        )
    , PostCondition
        "PoolReap may not create or remove reward accounts"
        ( \(TRC (_, st, _)) st' ->
            let r = _rewards . prDState
             in length (r st) == length (r st')
        )
    ]

instance NoUnexpectedThunks (PredicateFailure (POOLREAP crypto))

poolReapTransition :: TransitionRule (POOLREAP crypto)
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
