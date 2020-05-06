{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Slot

data UPDN crypto

data UpdnEnv crypto = UpdnEnv Nonce PParams (PrevHash crypto) Bool

data UpdnState = UpdnState Nonce Nonce Nonce Nonce
  deriving (Show, Eq)

instance
  (Crypto crypto) =>
  STS (UPDN crypto)
  where
  type State (UPDN crypto) = UpdnState
  type Signal (UPDN crypto) = SlotNo
  type Environment (UPDN crypto) = UpdnEnv crypto
  type BaseM (UPDN crypto) = ShelleyBase
  data PredicateFailure (UPDN crypto)
    deriving (Generic, Show, Eq)
  initialRules = [pure (UpdnState (mkNonce 0) (mkNonce 0) (mkNonce 0) (mkNonce 0))]
  transitionRules = [updTransition]

instance NoUnexpectedThunks (PredicateFailure (UPDN crypto))

prevHashToNonce ::
  PrevHash crypto ->
  Nonce
prevHashToNonce = \case
  GenesisHash -> NeutralNonce -- This case can only happen when starting Shelley from genesis,
    -- setting the intial chain state to some epoch e,
    -- and having the first block be in epoch e+1.
    -- In this edge case there is no need to add any extra
    -- entropy via the previous header hash to the next epoch nonce,
    -- so using the neutral nonce is appropriate.
  BlockHash ph -> hashHeaderToNonce ph

updTransition :: Crypto crypto => TransitionRule (UPDN crypto)
updTransition = do
  TRC (UpdnEnv eta pp ph ne, UpdnState eta_0 eta_v eta_c eta_h, s) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sp <- liftSTS $ asks slotsPrior
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
      (if ne then (prevHashToNonce ph) else eta_h)
