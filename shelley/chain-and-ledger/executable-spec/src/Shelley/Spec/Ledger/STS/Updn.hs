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

import Cardano.Ledger.Crypto
import Cardano.Prelude (NoUnexpectedThunks)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Slot

data UPDN crypto

newtype UpdnEnv
  = -- | New nonce
    UpdnEnv
      Nonce

data UpdnState = UpdnState Nonce Nonce
  deriving (Show, Eq)

instance
  (Crypto crypto) =>
  STS (UPDN crypto)
  where
  type State (UPDN crypto) = UpdnState
  type Signal (UPDN crypto) = SlotNo
  type Environment (UPDN crypto) = UpdnEnv
  type BaseM (UPDN crypto) = ShelleyBase
  data PredicateFailure (UPDN crypto) -- No predicate failures
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

instance NoUnexpectedThunks (PredicateFailure (UPDN crypto))

updTransition :: Crypto crypto => TransitionRule (UPDN crypto)
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
