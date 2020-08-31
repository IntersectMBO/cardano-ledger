{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Updn
  ( UPDN,
    UpdnEnv (..),
    UpdnState (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.Era
import Cardano.Prelude (NoUnexpectedThunks)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Slot

data UPDN era

newtype UpdnEnv
  = -- | New nonce
    UpdnEnv
      Nonce

data UpdnState = UpdnState Nonce Nonce
  deriving (Show, Eq)

instance
  (Era era) =>
  STS (UPDN era)
  where
  type State (UPDN era) = UpdnState
  type Signal (UPDN era) = SlotNo
  type Environment (UPDN era) = UpdnEnv
  type BaseM (UPDN era) = ShelleyBase
  data PredicateFailure (UPDN era) -- No predicate failures
    deriving (Generic, Show, Eq)
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

instance NoUnexpectedThunks (PredicateFailure (UPDN era))

updTransition :: Era era => TransitionRule (UPDN era)
updTransition = do
  TRC (UpdnEnv eta, UpdnState eta_v eta_c, s) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sp <- liftSTS $ asks stabilityWindow
  EpochNo e <- liftSTS $ epochInfoEpoch ei s
  firstSlotNextEpoch <- liftSTS $ epochInfoFirst ei (EpochNo (e + 1))
  pure $
    UpdnState
      (eta_v ⭒ eta)
      ( if s +* Duration sp < firstSlotNextEpoch
          then eta_v ⭒ eta
          else eta_c
      )
