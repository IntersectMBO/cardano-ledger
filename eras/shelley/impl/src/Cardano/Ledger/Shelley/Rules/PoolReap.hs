{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.PoolReap (
  ShelleyPOOLREAP,
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
import Cardano.Ledger.Shelley.Era (ShelleyPOOLREAP)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  CertState (..),
  DState (..),
  PState (..),
  UTxOState (..),
  obligationCertState,
  rewards,
 )
import Cardano.Ledger.Shelley.TxBody (RewardAcnt, getRwdCred, ppRewardAcnt)
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.UMap (View (Delegations, RewardDeposits), compactCoinOrError)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val ((<+>), (<->))
import Control.SetAlgebra (dom, eval, setSingleton, (⋪), (▷), (◁))
import Control.State.Transition (
  Assertion (..),
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
import qualified Data.Set as Set (member)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data ShelleyPoolreapState era = PoolreapState
  { prUTxOSt :: UTxOState era
  , prAcnt :: AccountState
  , prDState :: DState era
  , prPState :: PState era
  }

deriving stock instance Show (UTxOState era) => Show (ShelleyPoolreapState era)

data ShelleyPoolreapPredFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

data ShelleyPoolreapEvent era = RetiredPools
  { refundPools :: Map.Map (Credential 'Staking (EraCrypto era)) (Map.Map (KeyHash 'StakePool (EraCrypto era)) Coin)
  , unclaimedPools :: Map.Map (Credential 'Staking (EraCrypto era)) (Map.Map (KeyHash 'StakePool (EraCrypto era)) Coin)
  , epochNo :: EpochNo
  }

instance NoThunks (ShelleyPoolreapPredFailure era)

instance Default (UTxOState era) => Default (ShelleyPoolreapState era) where
  def = PoolreapState def def def def

instance (Default (ShelleyPoolreapState era), EraPParams era) => STS (ShelleyPOOLREAP era) where
  type State (ShelleyPOOLREAP era) = ShelleyPoolreapState era
  type Signal (ShelleyPOOLREAP era) = EpochNo
  type Environment (ShelleyPOOLREAP era) = PParams era
  type BaseM (ShelleyPOOLREAP era) = ShelleyBase
  type PredicateFailure (ShelleyPOOLREAP era) = ShelleyPoolreapPredFailure era
  type Event (ShelleyPOOLREAP era) = ShelleyPoolreapEvent era
  transitionRules = [poolReapTransition]
  assertions =
    [ PostCondition
        "Deposit pot must equal obligation (PoolReap)"
        ( \(TRC (_, _, _)) st ->
            obligationCertState (CertState def (prPState st) (prDState st)) == utxosDeposited (prUTxOSt st)
        )
    , PostCondition
        "PoolReap may not create or remove reward accounts"
        ( \(TRC (_, st, _)) st' ->
            let r = rewards . prDState
             in length (r st) == length (r st')
        )
    ]

poolReapTransition :: forall era. TransitionRule (ShelleyPOOLREAP era)
poolReapTransition = do
  TRC (_pp, PoolreapState us a ds ps, e) <- judgmentContext

  let
    -- The set of pools retiring this epoch
    retired :: Set (KeyHash 'StakePool (EraCrypto era))
    retired = eval (dom (psRetiring ps ▷ setSingleton e))
    -- The Map of pools (retiring this epoch) to their deposits
    retiringDeposits, remainingDeposits :: Map.Map (KeyHash 'StakePool (EraCrypto era)) Coin
    (retiringDeposits, remainingDeposits) =
      Map.partitionWithKey (\k _ -> Set.member k retired) (psDeposits ps)
    rewardAcnts :: Map.Map (KeyHash 'StakePool (EraCrypto era)) (RewardAcnt (EraCrypto era))
    rewardAcnts = Map.map ppRewardAcnt $ eval (retired ◁ psStakePoolParams ps)
    rewardAcnts_ :: Map.Map (KeyHash 'StakePool (EraCrypto era)) (RewardAcnt (EraCrypto era), Coin)
    rewardAcnts_ = Map.intersectionWith (,) rewardAcnts retiringDeposits
    rewardAcnts' :: Map.Map (RewardAcnt (EraCrypto era)) Coin
    rewardAcnts' =
      Map.fromListWith (<+>)
        . Map.elems
        $ rewardAcnts_
    refunds :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
    mRefunds :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
    (refunds, mRefunds) =
      Map.partitionWithKey
        (\k _ -> UM.member k (rewards ds)) -- (k ∈ dom (rewards ds))
        (Map.mapKeys getRwdCred rewardAcnts')
    refunded = fold refunds
    unclaimed = fold mRefunds

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
            (\k _ -> UM.member k (rewards ds)) -- (k ∈ dom (rewards ds))
            rewardAcntsWithPool
     in RetiredPools
          { refundPools = refundPools'
          , unclaimedPools = unclaimedPools'
          , epochNo = e
          }
  pure $
    PoolreapState
      us {utxosDeposited = utxosDeposited us <-> (unclaimed <+> refunded)}
      a {asTreasury = asTreasury a <+> unclaimed}
      ( let u0 = dsUnified ds
            u1 = RewardDeposits u0 UM.∪+ Map.map compactCoinOrError refunds
            u2 = (Delegations u1 UM.⋫ retired)
         in ds {dsUnified = u2}
      )
      ps
        { psStakePoolParams = eval (retired ⋪ psStakePoolParams ps)
        , psFutureStakePoolParams = eval (retired ⋪ psFutureStakePoolParams ps)
        , psRetiring = eval (retired ⋪ psRetiring ps)
        , psDeposits = remainingDeposits
        }
