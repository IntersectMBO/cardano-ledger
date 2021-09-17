{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Updn
  ( UPDN,
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

data UPDN crypto

newtype UpdnEnv
  = -- | New nonce
    UpdnEnv
      Nonce

data UpdnState = UpdnState Nonce Nonce
  deriving (Show, Eq)

data UpdnPredicateFailure crypto -- No predicate failures
  deriving (Generic, Show, Eq)

instance NoThunks (UpdnPredicateFailure crypto)

newtype UpdnEvent crypto = NewEpoch EpochNo

instance
  (Crypto crypto) =>
  STS (UPDN crypto)
  where
  type State (UPDN crypto) = UpdnState
  type Signal (UPDN crypto) = SlotNo
  type Environment (UPDN crypto) = UpdnEnv
  type BaseM (UPDN crypto) = ShelleyBase
  type PredicateFailure (UPDN crypto) = UpdnPredicateFailure crypto
  type Event (UPDN crypto) = UpdnEvent crypto
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

updTransition :: Crypto crypto => TransitionRule (UPDN crypto)
updTransition = do
  TRC (UpdnEnv eta, UpdnState eta_v eta_c, s) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
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
