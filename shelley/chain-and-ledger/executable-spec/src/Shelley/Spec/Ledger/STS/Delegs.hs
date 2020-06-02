{-# LANGUAGE DataKinds #-}
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
  ( DELEGS,
    DelegsEnv (..),
    PredicateFailure (..),
  )
where

import Byron.Spec.Ledger.Core (dom, (∈), (⊆), (⨃))
import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord,
    encodeListLen,
    matchSize,
  )
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition ((?!), (?!:), Embed (..), STS (..), TRC (..), TransitionRule, judgmentContext, trans)
import Data.Map as Map
import Data.Sequence (Seq (..))
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, invalidKey)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState (..),
    _dstate,
    _rewards,
    _stPools,
    emptyDelegation,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Delpl (DELPL, DelplEnv (..))
import Shelley.Spec.Ledger.Serialization (mapFromCBOR, mapToCBOR)
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxData (DCert (..), DelegCert (..), Delegation (..), Ix, Ptr (..), RewardAcnt, StakePools (..), TxBody (..), Wdrl (..))

data DELEGS crypto

data DelegsEnv crypto = DelegsEnv
  { delegsSlotNo :: SlotNo,
    delegsIx :: Ix,
    delegspp :: PParams,
    delegsTx :: (Tx crypto),
    delegsAccount :: AccountState
  }
  deriving (Show)

instance
  Crypto crypto =>
  STS (DELEGS crypto)
  where
  type State (DELEGS crypto) = DPState crypto
  type Signal (DELEGS crypto) = Seq (DCert crypto)
  type Environment (DELEGS crypto) = DelegsEnv crypto
  type BaseM (DELEGS crypto) = ShelleyBase
  data PredicateFailure (DELEGS crypto)
    = DelegateeNotRegisteredDELEG
        !(KeyHash 'StakePool crypto) -- target pool which is not registered
    | WithdrawalsNotInRewardsDELEGS
        !(Map (RewardAcnt crypto) Coin) -- withdrawals that are missing or do not withdrawl the entire amount
    | DelplFailure (PredicateFailure (DELPL crypto)) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = [pure emptyDelegation]
  transitionRules = [delegsTransition]

instance NoUnexpectedThunks (PredicateFailure (DELEGS crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (DELEGS crypto))
  where
  toCBOR = \case
    DelegateeNotRegisteredDELEG kh -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR kh
    WithdrawalsNotInRewardsDELEGS ws -> encodeListLen 2 <> toCBOR (1 :: Word8) <> mapToCBOR ws
    (DelplFailure a) ->
      encodeListLen 2 <> toCBOR (2 :: Word8)
        <> toCBOR a

instance
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (DELEGS crypto))
  where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "DelegateeNotRegisteredDELEG" 2 n
        kh <- fromCBOR
        pure $ DelegateeNotRegisteredDELEG kh
      1 -> do
        matchSize "WithdrawalsNotInRewardsDELEGS" 2 n
        ws <- mapFromCBOR
        pure $ WithdrawalsNotInRewardsDELEGS ws
      2 -> do
        matchSize "DelplFailure" 2 n
        a <- fromCBOR
        pure $ DelplFailure a
      k -> invalidKey k

delegsTransition ::
  forall crypto.
  Crypto crypto =>
  TransitionRule (DELEGS crypto)
delegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx reserves), dpstate, certificates) <- judgmentContext

  case certificates of
    Empty -> do
      let ds = _dstate dpstate
          wdrls_ = unWdrl $ _wdrls (_body tx)
          rewards = _rewards ds

      wdrls_ ⊆ rewards
        ?! WithdrawalsNotInRewardsDELEGS
          (Map.differenceWith (\x y -> if x /= y then Just x else Nothing) wdrls_ rewards)

      let rewards' = rewards ⨃ [(w, 0) | w <- Set.toList (dom wdrls_)]

      pure $ dpstate {_dstate = ds {_rewards = rewards'}}
    gamma :|> c -> do
      dpstate' <-
        trans @(DELEGS crypto) $ TRC (env, dpstate, gamma)

      let isDelegationRegistered = case c of
            DCertDeleg (Delegate deleg) ->
              let StakePools stPools_ = _stPools $ _pstate dpstate'
                  targetPool = _delegatee deleg
               in case targetPool ∈ dom stPools_ of
                    True -> Right ()
                    False -> Left $ DelegateeNotRegisteredDELEG targetPool
            _ -> Right ()
      isDelegationRegistered ?!: id

      let ptr = Ptr slot txIx (fromIntegral $ length gamma)
      trans @(DELPL crypto) $
        TRC (DelplEnv slot ptr pp reserves, dpstate', c)

instance
  Crypto crypto =>
  Embed (DELPL crypto) (DELEGS crypto)
  where
  wrapFailed = DelplFailure
