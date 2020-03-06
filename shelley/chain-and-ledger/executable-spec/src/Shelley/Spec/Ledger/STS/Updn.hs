{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Updn
  ( UPDN
  , UpdnEnv(..)
  , UpdnState(..)
  )
where

import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks)
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition
import           GHC.Generics (Generic)
import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.BlockChain
import           Shelley.Spec.Ledger.PParams
import           Shelley.Spec.Ledger.Slot

data UPDN crypto

data UpdnEnv crypto = UpdnEnv Nonce PParams (HashHeader crypto) Bool
data UpdnState = UpdnState Nonce Nonce Nonce Nonce
  deriving (Show, Eq)

instance
  (Crypto crypto)
  => STS (UPDN crypto) where
  type State (UPDN crypto) = UpdnState
  type Signal (UPDN crypto) = SlotNo
  type Environment (UPDN crypto) = UpdnEnv crypto
  type BaseM (UPDN crypto) = ShelleyBase
  data PredicateFailure (UPDN crypto) = FailureUPDN
                               deriving (Generic, Show, Eq)
  initialRules = [pure (UpdnState (mkNonce 0) (mkNonce 0) (mkNonce 0) (mkNonce 0))]
  transitionRules = [updTransition]

instance NoUnexpectedThunks (PredicateFailure (UPDN crypto))

updTransition :: Crypto crypto => TransitionRule (UPDN crypto)
updTransition = do
  TRC (UpdnEnv eta pp h ne, UpdnState eta_0 eta_v eta_c eta_h, s) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sp <- liftSTS $ asks slotsPrior
  EpochNo e <- liftSTS $ epochInfoEpoch ei s
  firstSlotNextEpoch <- liftSTS $ epochInfoFirst ei (EpochNo (e + 1))
  pure $ UpdnState
    (if ne then eta_c ⭒ eta_h ⭒ _extraEntropy pp else eta_0)
    (eta_v ⭒ eta)
    (if s +* Duration sp < firstSlotNextEpoch
      then eta_v ⭒ eta
      else eta_c
    )
    (if ne then (hashHeaderToNonce h) else eta_h)
