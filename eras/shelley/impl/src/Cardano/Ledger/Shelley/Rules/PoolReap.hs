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
  PredicateFailure,
  ShelleyPoolreapPredFailure,
) where

import Cardano.Ledger.Address (RewardAccount, raCredential)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin, compactCoinOrError)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.PoolParams (ppRewardAccount)
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyPOOLREAP)
import Cardano.Ledger.Shelley.LedgerState (
  UTxOState (..),
  allObligations,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.LedgerState.Types (potEqualsObligation)
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.State
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
import Data.Default (Default, def)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set (member)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data ShelleyPoolreapState era = PoolreapState
  { prUTxOSt :: UTxOState era
  , prChainAccountState :: ChainAccountState
  , prCertState :: CertState era
  }

deriving stock instance
  (Show (UTxOState era), Show (CertState era)) => Show (ShelleyPoolreapState era)

data ShelleyPoolreapPredFailure era -- No predicate failures
  deriving (Show, Eq, Generic)

instance NFData (ShelleyPoolreapPredFailure era)

data ShelleyPoolreapEvent era = RetiredPools
  { refundPools ::
      Map.Map (Credential 'Staking) (Map.Map (KeyHash 'StakePool) Coin)
  , unclaimedPools ::
      Map.Map (Credential 'Staking) (Map.Map (KeyHash 'StakePool) Coin)
  , epochNo :: EpochNo
  }
  deriving (Generic)

deriving instance Eq (ShelleyPoolreapEvent era)

instance NFData (ShelleyPoolreapEvent era)

instance NoThunks (ShelleyPoolreapPredFailure era)

instance (Default (UTxOState era), Default (CertState era)) => Default (ShelleyPoolreapState era) where
  def = PoolreapState def def def

type instance EraRuleEvent "POOLREAP" ShelleyEra = ShelleyPoolreapEvent ShelleyEra

instance
  ( Default (ShelleyPoolreapState era)
  , EraPParams era
  , EraGov era
  , EraCertState era
  ) =>
  STS (ShelleyPOOLREAP era)
  where
  type State (ShelleyPOOLREAP era) = ShelleyPoolreapState era
  type Signal (ShelleyPOOLREAP era) = EpochNo
  type Environment (ShelleyPOOLREAP era) = ()
  type BaseM (ShelleyPOOLREAP era) = ShelleyBase
  type PredicateFailure (ShelleyPOOLREAP era) = ShelleyPoolreapPredFailure era
  type Event (ShelleyPOOLREAP era) = ShelleyPoolreapEvent era
  transitionRules = [poolReapTransition]

  renderAssertionViolation = renderPoolReapViolation
  assertions =
    [ PostCondition
        "Deposit pot must equal obligation (PoolReap)"
        ( \_trc st ->
            potEqualsObligation
              (prCertState st)
              (prUTxOSt st)
        )
    , PostCondition
        "PoolReap may not create or remove reward accounts"
        ( \(TRC (_, st, _)) st' ->
            let accountsCount prState =
                  Map.size (prCertState prState ^. certDStateL . accountsL . accountsMapL)
             in accountsCount st == accountsCount st'
        )
    ]

poolReapTransition :: forall era. EraCertState era => TransitionRule (ShelleyPOOLREAP era)
poolReapTransition = do
  TRC (_, PoolreapState us a cs, e) <- judgmentContext

  let
    ps = cs ^. certPStateL
    ds = cs ^. certDStateL
    -- The set of pools retiring this epoch
    retired :: Set (KeyHash 'StakePool)
    retired = eval (dom (psRetiring ps ▷ setSingleton e))
    -- The Map of pools (retiring this epoch) to their deposits
    retiringDeposits, remainingDeposits :: Map.Map (KeyHash 'StakePool) Coin
    (retiringDeposits, remainingDeposits) =
      Map.partitionWithKey (\k _ -> Set.member k retired) (psDeposits ps)
    rewardAccounts :: Map.Map (KeyHash 'StakePool) RewardAccount
    rewardAccounts = Map.map ppRewardAccount $ eval (retired ◁ psStakePoolParams ps)
    rewardAccounts_ :: Map.Map (KeyHash 'StakePool) (RewardAccount, Coin)
    rewardAccounts_ = Map.intersectionWith (,) rewardAccounts retiringDeposits
    rewardAccounts' :: Map.Map RewardAccount Coin
    rewardAccounts' = Map.fromListWith (<+>) $ Map.elems rewardAccounts_
    accounts = ds ^. accountsL
    depositRefunds :: Map.Map RewardAccount Coin
    unclaimedDepositRefunds :: Map.Map RewardAccount Coin
    (depositRefunds, unclaimedDepositRefunds) =
      Map.partitionWithKey
        (\ra _ -> isAccountRegistered (raCredential ra) accounts) -- (k ∈ dom (rewards ds))
        rewardAccounts'
    refunded = fold depositRefunds
    unclaimed = fold unclaimedDepositRefunds

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
            (\cred _ -> isAccountRegistered cred accounts) -- (k ∈ dom (rewards ds))
            rewardAccountsWithPool
     in RetiredPools
          { refundPools = refundPools'
          , unclaimedPools = unclaimedPools'
          , epochNo = e
          }
  pure $
    PoolreapState
      us {utxosDeposited = utxosDeposited us <-> (unclaimed <+> refunded)}
      a {casTreasury = casTreasury a <+> unclaimed}
      ( cs
          & certDStateL . accountsL
            %~ removeStakePoolDelegations retired
              . addToBalanceAccounts
                ( Map.map compactCoinOrError $
                    Map.mapKeys raCredential depositRefunds
                )
          & certPStateL . psStakePoolParamsL %~ (eval . (retired ⋪))
          & certPStateL . psFutureStakePoolParamsL %~ (eval . (retired ⋪))
          & certPStateL . psRetiringL %~ (eval . (retired ⋪))
          & certPStateL . psDepositsL .~ remainingDeposits
      )

renderPoolReapViolation ::
  ( EraGov era
  , State t ~ ShelleyPoolreapState era
  , EraCertState era
  ) =>
  AssertionViolation t ->
  String
renderPoolReapViolation
  AssertionViolation {avSTS, avMsg, avCtx = TRC (_, poolreapst, _)} =
    let obligations =
          allObligations (prCertState poolreapst) (prUTxOSt poolreapst ^. utxosGovStateL)
     in "\n\nAssertionViolation ("
          <> avSTS
          <> ")\n   "
          <> avMsg
          <> "\npot (utxosDeposited) = "
          <> show (utxosDeposited (prUTxOSt poolreapst))
          <> show obligations
