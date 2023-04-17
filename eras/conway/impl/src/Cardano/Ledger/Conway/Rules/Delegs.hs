{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Delegs (
  ConwayDELEGS,
  ConwayDelegsPredFailure (..),
  ConwayDelegsEvent (..),
) where

import Cardano.Ledger.Address (mkRwdAcnt)
import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase, mkCertIxPartial)
import Cardano.Ledger.CertState (PState, rewards)
import Cardano.Ledger.Conway.Era (ConwayDELEGS)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (
  CertState (..),
  Coin,
  DCert,
  DState (..),
  DelegEnv,
  DelegsEnv (..),
  DelplEnv (..),
  KeyHash,
  KeyRole (..),
  Network,
  PoolEnv,
  Ptr (..),
  RewardAcnt,
  ShelleyDELPL,
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody)
import Cardano.Ledger.Shelley.Rules (ShelleyDelplEvent, ShelleyDelplPredFailure, drainedRewardAccounts, isDelegationRegistered, isSubmapOfUM)
import qualified Cardano.Ledger.UMap as UM
import Control.Arrow (ArrowChoice (..))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (Embed (..), STS (..), TRC (..), TransitionRule, judgmentContext, liftSTS, trans, (?!), (?!:))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import GHC.Generics (Generic)
import Lens.Micro ((^.))

data ConwayDelegsPredFailure era
  = DelegateeNotRegisteredDELEG
      !(KeyHash 'StakePool (EraCrypto era)) -- target pool which is not registered
  | WithdrawalsNotInRewardsDELEGS
      !(Map (RewardAcnt (EraCrypto era)) Coin) -- withdrawals that are missing or do not withdrawal the entire amount
  | CertFailure (PredicateFailure (EraRule "CERT" era)) -- Subtransition Failures
  deriving (Generic)

deriving instance
  Eq (PredicateFailure (EraRule "CERT" era)) =>
  Eq (ConwayDelegsPredFailure era)
deriving instance
  Show (PredicateFailure (EraRule "CERT" era)) =>
  Show (ConwayDelegsPredFailure era)

newtype ConwayDelegsEvent era = CertEvent (Event (EraRule "CERT" era))

instance
  ( EraTx era
  , ShelleyEraTxBody era
  , State (EraRule "CERT" era) ~ CertState era
  , Signal (EraRule "CERT" era) ~ DCert (EraCrypto era)
  , Environment (EraRule "CERT" era) ~ DelplEnv era
  , EraRule "DELEGS" era ~ ConwayDELEGS era
  , Embed (EraRule "CERT" era) (ConwayDELEGS era)
  ) =>
  STS (ConwayDELEGS era)
  where
  type State (ConwayDELEGS era) = CertState era
  type Signal (ConwayDELEGS era) = Seq (DCert (EraCrypto era))
  type Environment (ConwayDELEGS era) = DelegsEnv era
  type BaseM (ConwayDELEGS era) = ShelleyBase
  type
    PredicateFailure (ConwayDELEGS era) =
      ConwayDelegsPredFailure era
  type Event (ConwayDELEGS era) = ConwayDelegsEvent era

  transitionRules = [conwayDelegsTransition @era]

conwayDelegsTransition ::
  forall era.
  ( EraTx era
  , ShelleyEraTxBody era
  , State (EraRule "CERT" era) ~ CertState era
  , Embed (EraRule "CERT" era) (ConwayDELEGS era)
  , Environment (EraRule "CERT" era) ~ DelplEnv era
  , Signal (EraRule "CERT" era) ~ DCert (EraCrypto era)
  , EraRule "DELEGS" era ~ ConwayDELEGS era
  ) =>
  TransitionRule (ConwayDELEGS era)
conwayDelegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx acnt), dpstate, certificates) <- judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> zeroRewards dpstate tx network
    gamma :|> c -> do
      dpstate' <-
        trans @(ConwayDELEGS era) $ TRC (env, dpstate, gamma)
      left DelegateeNotRegisteredDELEG (isDelegationRegistered @era dpstate' c)
        ?!: id
      -- It is impossible to have 65535 number of certificates in a
      -- transaction, therefore partial function is justified.
      let ptr = Ptr slot txIx (mkCertIxPartial $ toInteger $ length gamma)
      trans @(EraRule "CERT" era) $
        TRC (DelplEnv slot ptr pp acnt, dpstate', c)

zeroRewards ::
  forall era.
  ( EraTx era
  , State (EraRule "DELEGS" era) ~ CertState era
  , PredicateFailure (EraRule "DELEGS" era) ~ ConwayDelegsPredFailure era
  ) =>
  CertState era ->
  Tx era ->
  Network ->
  TransitionRule (EraRule "DELEGS" era)
zeroRewards dpstate tx network = do
  let ds = certDState dpstate
      wdrls = unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL)
      rewards' = rewards ds
  isSubmapOfUM @era wdrls rewards' -- withdrawals_ ⊆ rewards
    ?! WithdrawalsNotInRewardsDELEGS @era
      ( Map.differenceWith
          (\x y -> if x /= y then Just x else Nothing)
          wdrls
          (Map.mapKeys (mkRwdAcnt network) (UM.rewView (dsUnified ds)))
      )
  let unified' = rewards' UM.⨃ drainedRewardAccounts @era wdrls
  pure $ dpstate {certDState = ds {dsUnified = unified'}}

instance
  ( Era era
  , Embed (EraRule "POOL" era) (ShelleyDELPL era)
  , Embed (EraRule "DELEG" era) (ShelleyDELPL era)
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "DELEG" era) ~ DState era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "DELEG" era) ~ DelegEnv era
  , Signal (EraRule "POOL" era) ~ DCert (EraCrypto era)
  , Signal (EraRule "DELEG" era) ~ DCert (EraCrypto era)
  , PredicateFailure (EraRule "CERT" era) ~ ShelleyDelplPredFailure era
  , Event (EraRule "CERT" era) ~ ShelleyDelplEvent era
  ) =>
  Embed (ShelleyDELPL era) (ConwayDELEGS era)
  where
  wrapFailed = CertFailure
  wrapEvent = CertEvent
