{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.PoolReap
  ( ShelleyPOOLREAP,
    ShelleyPoolreapEvent (..),
    ShelleyPoolreapState (..),
    PredicateFailure,
    ShelleyPoolreapPredFailure,
  )
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import Cardano.Ledger.Shelley.Era (ShelleyPOOLREAP)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DState (..),
    PState (..),
    UTxOState (..),
    rewards,
  )
import Cardano.Ledger.Shelley.TxBody (RewardAcnt, getRwdCred, _poolRAcnt)
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.UnifiedMap (View (..))
import Cardano.Ledger.Val ((<+>), (<->))
import Control.SetAlgebra (dom, eval, setSingleton, (∈), (⋪), (▷), (◁))
import Control.State.Transition
  ( Assertion (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    tellEvent,
  )
import Data.Default.Class (Default, def)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Typeable (Typeable)
import qualified Data.UMap as UM
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Control.DeepSeq (NFData (..))

data ShelleyPoolreapState era = PoolreapState
  { prUTxOSt :: UTxOState era,
    prAcnt :: AccountState,
    prDState :: DState (EraCrypto era),
    prPState :: PState (EraCrypto era)
  }

deriving stock instance Show (UTxOState era) => Show (ShelleyPoolreapState era)

data ShelleyPoolreapPredFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

data ShelleyPoolreapEvent era = RetiredPools
  { refundPools :: Map.Map (Credential 'Staking (EraCrypto era)) (Map.Map (KeyHash 'StakePool (EraCrypto era)) Coin),
    unclaimedPools :: Map.Map (Credential 'Staking (EraCrypto era)) (Map.Map (KeyHash 'StakePool (EraCrypto era)) Coin),
    epochNo :: EpochNo
  }

instance NoThunks (ShelleyPoolreapPredFailure era)

deriving instance NFData (ShelleyPoolreapPredFailure era)

instance Default (UTxOState era) => Default (ShelleyPoolreapState era) where
  def = PoolreapState def def def def

instance
  forall era.
  ( Typeable era,
    Default (ShelleyPoolreapState era),
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_keyDeposit" (PParams era) Coin
  ) =>
  STS (ShelleyPOOLREAP era)
  where
  type State (ShelleyPOOLREAP era) = ShelleyPoolreapState era
  type Signal (ShelleyPOOLREAP era) = EpochNo
  type Environment (ShelleyPOOLREAP era) = PParams era
  type BaseM (ShelleyPOOLREAP era) = ShelleyBase
  type PredicateFailure (ShelleyPOOLREAP era) = ShelleyPoolreapPredFailure era
  type Event (ShelleyPOOLREAP era) = ShelleyPoolreapEvent era
  transitionRules = [poolReapTransition]
  assertions =
    [ PostCondition
        "Deposit pot must equal obligation"
        ( \(TRC (pp, _, _)) st ->
            obligation pp (rewards $ prDState st) (_pParams $ prPState st)
              == _deposited (prUTxOSt st)
        ),
      PostCondition
        "PoolReap may not create or remove reward accounts"
        ( \(TRC (_, st, _)) st' ->
            let r = rewards . prDState
             in length (r st) == length (r st')
        )
    ]

poolReapTransition ::
  forall era.
  HasField "_poolDeposit" (PParams era) Coin =>
  TransitionRule (ShelleyPOOLREAP era)
poolReapTransition = do
  TRC (pp, PoolreapState us a ds ps, e) <- judgmentContext

  let retired :: Set (KeyHash 'StakePool (EraCrypto era))
      retired = eval (dom (_retiring ps ▷ setSingleton e))
      pr :: Map.Map (KeyHash 'StakePool (EraCrypto era)) Coin
      pr = Map.fromSet (const (getField @"_poolDeposit" pp)) retired
      rewardAcnts :: Map.Map (KeyHash 'StakePool (EraCrypto era)) (RewardAcnt (EraCrypto era))
      rewardAcnts = Map.map _poolRAcnt $ eval (retired ◁ _pParams ps)
      rewardAcnts_ :: Map.Map (KeyHash 'StakePool (EraCrypto era)) (RewardAcnt (EraCrypto era), Coin)
      rewardAcnts_ = Map.intersectionWith (,) rewardAcnts pr
      rewardAcnts' :: Map.Map (RewardAcnt (EraCrypto era)) Coin
      rewardAcnts' =
        Map.fromListWith (<+>)
          . Map.elems
          $ rewardAcnts_
      refunds :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
      mRefunds :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
      (refunds, mRefunds) =
        Map.partitionWithKey
          (\k _ -> eval (k ∈ dom (rewards ds)))
          (Map.mapKeys getRwdCred rewardAcnts')
      refunded = fold $ Map.elems refunds
      unclaimed = fold $ Map.elems mRefunds

  tellEvent $
    let rewardAcntsWithPool =
          Map.foldlWithKey'
            ( \acc sp (ra, coin) ->
                Map.insertWith (Map.unionWith (<>)) (getRwdCred ra) (Map.singleton sp coin) acc
            )
            Map.empty
            rewardAcnts_
        (refundPools', unclaimedPools') =
          Map.partitionWithKey
            (\k _ -> eval (k ∈ dom (rewards ds)))
            rewardAcntsWithPool
     in RetiredPools
          { refundPools = refundPools',
            unclaimedPools = unclaimedPools',
            epochNo = e
          }

  pure $
    PoolreapState
      us {_deposited = _deposited us <-> (unclaimed <+> refunded)}
      a {_treasury = _treasury a <+> unclaimed}
      ( let u0 = _unified ds
            u1 = (Rewards u0 UM.∪+ refunds)
            u2 = (Delegations u1 UM.⋫ retired)
         in ds {_unified = u2}
      )
      ps
        { _pParams = eval (retired ⋪ _pParams ps),
          _fPParams = eval (retired ⋪ _fPParams ps),
          _retiring = eval (retired ⋪ _retiring ps)
        }
