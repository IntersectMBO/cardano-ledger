{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Updn
  ( UPDN
  , UpdnEnv(..)
  , UpdnState(..)
  )
where

import           BaseTypes
import           BlockChain
import           PParams
import           Slot

import           Cardano.Ledger.Shelley.Crypto
import           Control.State.Transition

data UPDN crypto

data UpdnEnv crypto = UpdnEnv Nonce PParams (HashHeader crypto) Bool
data UpdnState = UpdnState Nonce Nonce Nonce Nonce
  deriving (Show, Eq)

instance
  (Crypto crypto)
  => STS (UPDN crypto) where
  type State (UPDN crypto) = UpdnState
  type Signal (UPDN crypto) = Slot.Slot
  type Environment (UPDN crypto) = UpdnEnv crypto
  data PredicateFailure (UPDN crypto) = FailureUPDN
                               deriving (Show, Eq)
  initialRules = [pure (UpdnState (mkNonce 0) (mkNonce 0) (mkNonce 0) (mkNonce 0))]
  transitionRules = [updTransition]

updTransition :: TransitionRule (UPDN crypto)
updTransition = do
  TRC (UpdnEnv eta pp h ne, UpdnState eta_0 eta_v eta_c eta_h, s) <- judgmentContext
  let Epoch e = epochFromSlot s
  pure $ UpdnState
    (if ne then eta_c ⭒ eta_h ⭒ _extraEntropy pp else eta_0)
    (eta_v ⭒ eta)
    (if s +* slotsPrior < firstSlot (Epoch (e + 1))
      then eta_v ⭒ eta
      else eta_c
    )
    (if ne then (hashHeaderToNonce h) else eta_h)
