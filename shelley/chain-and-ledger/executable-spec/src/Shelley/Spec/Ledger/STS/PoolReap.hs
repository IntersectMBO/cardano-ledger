{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.PoolReap
  ( POOLREAP,
    PoolreapState (..),
    PredicateFailure,
    PoolreapPredicateFailure,
  )
where

import Cardano.Ledger.Constraints (TransValue)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Val ((<+>), (<->))
import Control.SetAlgebra (dom, eval, setSingleton, (∈), (∪+), (⋪), (⋫), (▷), (◁))
import Control.State.Transition
  ( Assertion (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
  )
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
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
import Shelley.Spec.Ledger.TxBody (getRwdCred, _poolRAcnt)

-- =================================================================

data POOLREAP era

data PoolreapState era = PoolreapState
  { prUTxOSt :: UTxOState era,
    prAcnt :: AccountState,
    prDState :: DState (Crypto era),
    prPState :: PState (Crypto era)
  }

deriving stock instance
  (TransValue Show era) =>
  Show (PoolreapState era)

data PoolreapPredicateFailure era -- No predicate Falures
  deriving (Show, Eq, Generic)

instance NoThunks (PoolreapPredicateFailure era)

instance Typeable era => STS (POOLREAP era) where
  type State (POOLREAP era) = PoolreapState era
  type Signal (POOLREAP era) = EpochNo
  type Environment (POOLREAP era) = PParams era
  type BaseM (POOLREAP era) = ShelleyBase
  type PredicateFailure (POOLREAP era) = PoolreapPredicateFailure era
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
        ),
      PostCondition
        "PoolReap may not create or remove reward accounts"
        ( \(TRC (_, st, _)) st' ->
            let r = _rewards . prDState
             in length (r st) == length (r st')
        )
    ]

poolReapTransition :: TransitionRule (POOLREAP era)
poolReapTransition = do
  TRC (pp, PoolreapState us a ds ps, e) <- judgmentContext

  let retired = eval (dom ((_retiring ps) ▷ setSingleton e))
      pr = Map.fromList $ fmap (\kh -> (kh, _poolDeposit pp)) (Set.toList retired)
      rewardAcnts = Map.map _poolRAcnt $ eval (retired ◁ (_pParams ps))
      rewardAcnts' =
        Map.fromListWith (<+>)
          . Map.elems
          $ Map.intersectionWith (,) rewardAcnts pr
      (refunds, mRefunds) =
        Map.partitionWithKey
          (\k _ -> eval (k ∈ dom (_rewards ds)))
          (Map.mapKeys getRwdCred rewardAcnts')
      refunded = fold $ Map.elems refunds
      unclaimed = fold $ Map.elems mRefunds

  pure $
    PoolreapState
      us {_deposited = _deposited us <-> (unclaimed <+> refunded)}
      a {_treasury = _treasury a <+> unclaimed}
      ds
        { _rewards = eval (_rewards ds ∪+ refunds),
          _delegations = eval (_delegations ds ⋫ retired)
        }
      ps
        { _pParams = eval (retired ⋪ _pParams ps),
          _fPParams = eval (retired ⋪ _fPParams ps),
          _retiring = eval (retired ⋪ _retiring ps)
        }
