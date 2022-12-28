{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Protocol.TPraos.Rules.Updn (
  UPDN,
  UpdnEnv (..),
  UpdnState (..),
  PredicateFailure,
  UpdnPredicateFailure,
)
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Crypto
import Cardano.Ledger.Slot
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data UPDN c

newtype UpdnEnv
  = -- | New nonce
    UpdnEnv
      Nonce

data UpdnState = UpdnState Nonce Nonce
  deriving (Show, Eq)

data UpdnPredicateFailure c -- No predicate failures
  deriving (Generic, Show, Eq)

instance NoThunks (UpdnPredicateFailure c)

newtype UpdnEvent c = NewEpoch EpochNo

instance
  (Crypto c) =>
  STS (UPDN c)
  where
  type State (UPDN c) = UpdnState
  type Signal (UPDN c) = SlotNo
  type Environment (UPDN c) = UpdnEnv
  type BaseM (UPDN c) = ShelleyBase
  type PredicateFailure (UPDN c) = UpdnPredicateFailure c
  type Event (UPDN c) = UpdnEvent c
  initialRules =
    [ pure
        ( UpdnState
            initialNonce
            initialNonce
        )
    ]
    where
      initialNonce = mkNonceFromNumber 0
  transitionRules = [updTransition]

updTransition :: Crypto c => TransitionRule (UPDN c)
updTransition = do
  TRC (UpdnEnv eta, UpdnState eta_v eta_c, s) <- judgmentContext
  ei <- liftSTS $ asks epochInfoPure
  sp <- liftSTS $ asks stabilityWindow
  EpochNo e <- liftSTS $ epochInfoEpoch ei s
  let newEpochNo = EpochNo (e + 1)
  firstSlotNextEpoch <- liftSTS $ epochInfoFirst ei newEpochNo
  tellEvent $ NewEpoch newEpochNo
  pure $
    UpdnState
      (eta_v ⭒ eta)
      ( if s +* Duration sp < firstSlotNextEpoch
          then eta_v ⭒ eta
          else eta_c
      )
