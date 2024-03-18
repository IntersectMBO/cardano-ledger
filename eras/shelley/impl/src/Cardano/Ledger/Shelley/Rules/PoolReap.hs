{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.PoolReap (
  ShelleyPOOLREAP,
  ShelleyPoolreapEvent (..),
  ShelleyPoolreapState (..),
  ShelleyPoolreapEnv (..),
  PredicateFailure,
  ShelleyPoolreapPredFailure,

  -- * Deprecations
  prAcnt,
)
where

import Cardano.Ledger.Address (RewardAccount, raCredential)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.CertState (VState)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.PoolParams (ppRewardAccount)
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyPOOLREAP)
import Cardano.Ledger.Shelley.Governance (EraGov)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  CertState (..),
  DState (..),
  PState (..),
  UTxOState (..),
  allObligations,
  rewards,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.LedgerState.Types (potEqualsObligation)
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.UMap (UView (RewDepUView, SPoolUView), compactCoinOrError)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val ((<+>), (<->))
import Control.DeepSeq (NFData)
import Control.SetAlgebra (dom, eval, setSingleton, (⋪), (▷), (◁))
import Control.State.Transition (
  Assertion (..),
  AssertionViolation (..),
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
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data ShelleyPoolreapState era = PoolreapState
  { prUTxOSt :: UTxOState era
  , prAccountState :: AccountState
  , prDState :: DState era
  , prPState :: PState era
  }

prAcnt :: ShelleyPoolreapState era -> AccountState
prAcnt = prAccountState
{-# DEPRECATED prAcnt "Use `prAccountState` instead" #-}

newtype ShelleyPoolreapEnv era = ShelleyPoolreapEnv
  { speVState :: VState era
  -- ^ This enviroment field is only needed for assertions.
  }

deriving stock instance Eq (PParams era) => Eq (ShelleyPoolreapEnv era)

deriving stock instance Show (PParams era) => Show (ShelleyPoolreapEnv era)

deriving stock instance Show (UTxOState era) => Show (ShelleyPoolreapState era)

data ShelleyPoolreapPredFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

instance NFData (ShelleyPoolreapPredFailure era)

data ShelleyPoolreapEvent era = RetiredPools
  { refundPools ::
      Map.Map (Credential 'Staking (EraCrypto era)) (Map.Map (KeyHash 'StakePool (EraCrypto era)) Coin)
  , unclaimedPools ::
      Map.Map (Credential 'Staking (EraCrypto era)) (Map.Map (KeyHash 'StakePool (EraCrypto era)) Coin)
  , epochNo :: EpochNo
  }
  deriving (Generic)

deriving instance Eq (ShelleyPoolreapEvent era)

instance NFData (ShelleyPoolreapEvent era)

instance NoThunks (ShelleyPoolreapPredFailure era)

instance Default (UTxOState era) => Default (ShelleyPoolreapState era) where
  def = PoolreapState def def def def

type instance EraRuleEvent "POOLREAP" (ShelleyEra c) = ShelleyPoolreapEvent (ShelleyEra c)

instance
  ( Default (ShelleyPoolreapState era)
  , EraPParams era
  , EraGov era
  ) =>
  STS (ShelleyPOOLREAP era)
  where
  type State (ShelleyPOOLREAP era) = ShelleyPoolreapState era
  type Signal (ShelleyPOOLREAP era) = EpochNo
  type Environment (ShelleyPOOLREAP era) = ShelleyPoolreapEnv era
  type BaseM (ShelleyPOOLREAP era) = ShelleyBase
  type PredicateFailure (ShelleyPOOLREAP era) = ShelleyPoolreapPredFailure era
  type Event (ShelleyPOOLREAP era) = ShelleyPoolreapEvent era
  transitionRules = [poolReapTransition]

  renderAssertionViolation = renderPoolReapViolation
  assertions =
    [ PostCondition
        "Deposit pot must equal obligation (PoolReap)"
        ( \(TRC (env, _, _)) st ->
            potEqualsObligation
              (CertState (speVState env) (prPState st) (prDState st))
              (prUTxOSt st)
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
  TRC (_, PoolreapState us a ds ps, e) <- judgmentContext

  let
    -- The set of pools retiring this epoch
    retired :: Set (KeyHash 'StakePool (EraCrypto era))
    retired = eval (dom (psRetiring ps ▷ setSingleton e))
    -- The Map of pools (retiring this epoch) to their deposits
    retiringDeposits, remainingDeposits :: Map.Map (KeyHash 'StakePool (EraCrypto era)) Coin
    (retiringDeposits, remainingDeposits) =
      Map.partitionWithKey (\k _ -> Set.member k retired) (psDeposits ps)
    rewardAccounts :: Map.Map (KeyHash 'StakePool (EraCrypto era)) (RewardAccount (EraCrypto era))
    rewardAccounts = Map.map ppRewardAccount $ eval (retired ◁ psStakePoolParams ps)
    rewardAccounts_ ::
      Map.Map (KeyHash 'StakePool (EraCrypto era)) (RewardAccount (EraCrypto era), Coin)
    rewardAccounts_ = Map.intersectionWith (,) rewardAccounts retiringDeposits
    rewardAccounts' :: Map.Map (RewardAccount (EraCrypto era)) Coin
    rewardAccounts' =
      Map.fromListWith (<+>)
        . Map.elems
        $ rewardAccounts_
    refunds :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
    mRefunds :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
    (refunds, mRefunds) =
      Map.partitionWithKey
        (\k _ -> UM.member k (rewards ds)) -- (k ∈ dom (rewards ds))
        (Map.mapKeys raCredential rewardAccounts')
    refunded = fold refunds
    unclaimed = fold mRefunds

  tellEvent $
    let rewardAccountsWithPool =
          Map.foldlWithKey'
            ( \acc sp (ra, coin) ->
                Map.insertWith (Map.unionWith (<>)) (raCredential ra) (Map.singleton sp coin) acc
            )
            Map.empty
            rewardAccounts_
        (refundPools', unclaimedPools') =
          Map.partitionWithKey
            (\k _ -> UM.member k (rewards ds)) -- (k ∈ dom (rewards ds))
            rewardAccountsWithPool
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
            u1 = RewDepUView u0 UM.∪+ Map.map compactCoinOrError refunds
            u2 = SPoolUView u1 UM.⋫ retired
         in ds {dsUnified = u2}
      )
      ps
        { psStakePoolParams = eval (retired ⋪ psStakePoolParams ps)
        , -- TODO: redundant
          psFutureStakePoolParams = eval (retired ⋪ psFutureStakePoolParams ps)
        , psRetiring = eval (retired ⋪ psRetiring ps)
        , psDeposits = remainingDeposits
        }

renderPoolReapViolation ::
  ( EraGov era
  , Environment t ~ ShelleyPoolreapEnv era
  , State t ~ ShelleyPoolreapState era
  ) =>
  AssertionViolation t ->
  String
renderPoolReapViolation
  AssertionViolation {avSTS, avMsg, avCtx = TRC (ShelleyPoolreapEnv vs, poolreapst, _)} =
    let certst = CertState vs (prPState poolreapst) (prDState poolreapst)
        obligations = allObligations certst (prUTxOSt poolreapst ^. utxosGovStateL)
     in "\n\nAssertionViolation ("
          <> avSTS
          <> ")\n   "
          <> avMsg
          <> "\npot (utxosDeposited) = "
          <> show (utxosDeposited (prUTxOSt poolreapst))
          <> show obligations
