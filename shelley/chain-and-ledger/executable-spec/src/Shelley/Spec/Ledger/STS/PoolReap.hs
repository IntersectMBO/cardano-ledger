{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.PoolReap
  ( POOLREAP,
    PoolreapState (..),
    PredicateFailure,
    PoolreapPredicateFailure,
  )
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.Val ((<+>), (<->))
import Control.SetAlgebra (dom, eval, setSingleton, (∈), (∪+), (⋪), (⋫), (▷), (◁))
import Control.State.Transition
  ( Assertion (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
  )
import Data.Default.Class (Default, def)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.EpochBoundary (obligation)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DState (..),
    PState (..),
    TransUTxOState,
    UTxOState (..),
  )
import Shelley.Spec.Ledger.TxBody (getRwdCred, _poolRAcnt)

data POOLREAP era

data PoolreapState era = PoolreapState
  { prUTxOSt :: UTxOState era,
    prAcnt :: AccountState,
    prDState :: DState (Crypto era),
    prPState :: PState (Crypto era)
  }

deriving stock instance
  (TransUTxOState Show era) =>
  Show (PoolreapState era)

data PoolreapPredicateFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

instance NoThunks (PoolreapPredicateFailure era)

instance Default (UTxOState era) => Default (PoolreapState era) where
  def = PoolreapState def def def def

instance
  forall era.
  ( Typeable era,
    Default (PoolreapState era),
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_keyDeposit" (Core.PParams era) Coin
  ) =>
  STS (POOLREAP era)
  where
  type State (POOLREAP era) = PoolreapState era
  type Signal (POOLREAP era) = EpochNo
  type Environment (POOLREAP era) = Core.PParams era
  type BaseM (POOLREAP era) = ShelleyBase
  type PredicateFailure (POOLREAP era) = PoolreapPredicateFailure era
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

poolReapTransition ::
  HasField "_poolDeposit" (Core.PParams era) Coin =>
  TransitionRule (POOLREAP era)
poolReapTransition = do
  TRC (pp, PoolreapState us a ds ps, e) <- judgmentContext

  let retired = eval (dom ((_retiring ps) ▷ setSingleton e))
      pr = Map.fromList $ fmap (\kh -> (kh, getField @"_poolDeposit" pp)) (Set.toList retired)
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
