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

import Cardano.Prelude (NoUnexpectedThunks)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Slot

data UPDN crypto

data UpdnEnv
  = UpdnEnv
      Nonce
      -- ^ New nonce
      PParams
      Nonce
      -- ^ Current previous hash nonce
      Bool
      -- ^ Is new epoch

data UpdnState = UpdnState Nonce Nonce Nonce Nonce
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
  TRC (UpdnEnv eta pp eta_ph ne, UpdnState eta_0 eta_v eta_c eta_h, s) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sp <- liftSTS $ asks stabilityWindow
  EpochNo e <- liftSTS $ epochInfoEpoch ei s
  firstSlotNextEpoch <- liftSTS $ epochInfoFirst ei (EpochNo (e + 1))
  pure $
    UpdnState
      (if ne then eta_c ⭒ eta_h ⭒ _extraEntropy pp else eta_0)
      (eta_v ⭒ eta)
      ( if s +* Duration sp < firstSlotNextEpoch
          then eta_v ⭒ eta
          else eta_c
      )
      (if ne then eta_ph else eta_h)
