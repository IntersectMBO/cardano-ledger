{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Delegs
  ( DELEGS
  , DelegsEnv (..)
  , PredicateFailure(..)
  )
where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLen, decodeWord,
                     encodeListLen, matchSize)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.State.Transition
import           Data.Sequence (Seq (..))
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Ledger.Core (dom, (∈), (⊆), (⨃))
import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.Coin (Coin)
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.Delegation.Certificates
import           Shelley.Spec.Ledger.LedgerState (DPState (..), emptyDelegation, _dstate, _rewards,
                     _stPools)
import           Shelley.Spec.Ledger.PParams
import           Shelley.Spec.Ledger.Slot
import           Shelley.Spec.Ledger.STS.Delpl
import           Shelley.Spec.Ledger.Tx
import           Shelley.Spec.Ledger.TxData

data DELEGS crypto

data DelegsEnv crypto
  = DelegsEnv
    { delegsSlotNo   :: SlotNo
    , delegsIx       :: Ix
    , delegspp       :: PParams
    , delegsTx       :: (Tx crypto)
    , delegsReserves :: Coin
    }
  deriving Show

instance
  Crypto crypto
  => STS (DELEGS crypto)
 where
  type State (DELEGS crypto) = DPState crypto
  type Signal (DELEGS crypto) = Seq (DCert crypto)
  type Environment (DELEGS crypto) = DelegsEnv crypto
  type BaseM (DELEGS crypto) = ShelleyBase
  data PredicateFailure (DELEGS crypto)
    = DelegateeNotRegisteredDELEG
    | WithdrawalsNotInRewardsDELEGS
    | DelplFailure (PredicateFailure (DELPL crypto))
    deriving (Show, Eq, Generic)

  initialRules    = [ pure emptyDelegation ]
  transitionRules = [ delegsTransition     ]

instance NoUnexpectedThunks (PredicateFailure (DELEGS crypto))

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (PredicateFailure (DELEGS crypto))
 where
   toCBOR = \case
      DelegateeNotRegisteredDELEG   -> encodeListLen 1 <> toCBOR (0 :: Word8)
      WithdrawalsNotInRewardsDELEGS -> encodeListLen 1 <> toCBOR (1 :: Word8)
      (DelplFailure a)              -> encodeListLen 2 <> toCBOR (2 :: Word8)
                                        <> toCBOR a

instance
  (Crypto crypto)
  => FromCBOR (PredicateFailure (DELEGS crypto))
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> matchSize "DelegateeNotRegisteredDELEG" 1 n >>
             pure DelegateeNotRegisteredDELEG
      1 -> matchSize "WithdrawalsNotInRewardsDELEGS" 1 n >>
             pure WithdrawalsNotInRewardsDELEGS
      2 -> do
        matchSize "DelplFailure" 2 n
        a <- fromCBOR
        pure $ DelplFailure a
      k -> invalidKey k

delegsTransition
  :: forall crypto
   . Crypto crypto
  => TransitionRule (DELEGS crypto)
delegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx reserves), dpstate, certificates) <- judgmentContext

  case certificates of
    Empty -> do
      let ds       = _dstate dpstate
          wdrls_   = unWdrl $ _wdrls (_body tx)
          rewards = _rewards ds

      wdrls_ ⊆ rewards ?! WithdrawalsNotInRewardsDELEGS

      let rewards' = rewards ⨃ [(w, 0) | w <- Set.toList (dom wdrls_)]

      pure $ dpstate { _dstate = ds { _rewards = rewards' } }

    gamma :|> c -> do
      dpstate' <-
        trans @(DELEGS crypto) $ TRC (env, dpstate, gamma)

      let isDelegationRegistered = case c of
            DCertDeleg (Delegate deleg) ->
              let StakePools stPools_ = _stPools $ _pstate dpstate' in
              _delegatee deleg ∈ dom stPools_
            _ -> True
      isDelegationRegistered ?! DelegateeNotRegisteredDELEG

      let ptr = Ptr slot txIx (fromIntegral $ length gamma)
      trans @(DELPL crypto)
        $ TRC (DelplEnv slot ptr pp reserves, dpstate', c)

instance
  Crypto crypto
  => Embed (DELPL crypto) (DELEGS crypto)
 where
  wrapFailed = DelplFailure
