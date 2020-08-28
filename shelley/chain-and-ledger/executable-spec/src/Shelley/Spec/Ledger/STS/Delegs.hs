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

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Prelude (NoUnexpectedThunks (..), asks)
import Control.Iterate.SetAlgebra (dom, eval, (∈), (⨃))
import Control.State.Transition (Embed (..), STS (..), TRC (..), TransitionRule, judgmentContext, liftSTS, trans, (?!), (?!:))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Address (mkRwdAcnt)
import Shelley.Spec.Ledger.BaseTypes
  ( ShelleyBase,
    invalidKey,
    networkId,
  )
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState (..),
    RewardAccounts,
    emptyDelegation,
    _dstate,
    _pParams,
    _rewards,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Delpl (DELPL, DelplEnv (..))
import Shelley.Spec.Ledger.Serialization (decodeRecordSum, mapFromCBOR, mapToCBOR)
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxData
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    Ix,
    Ptr (..),
    RewardAcnt (..),
    TxBody (..),
    Wdrl (..),
  )

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
  fromCBOR =
    decodeRecordSum "PredicateFailure" $
      ( \case
          0 -> do
            kh <- fromCBOR
            pure (2, DelegateeNotRegisteredDELEG kh)
          1 -> do
            ws <- mapFromCBOR
            pure (2, WithdrawalsNotInRewardsDELEGS ws)
          2 -> do
            a <- fromCBOR
            pure (2, DelplFailure a)
          k -> invalidKey k
      )

delegsTransition ::
  forall crypto.
  Crypto crypto =>
  TransitionRule (DELEGS crypto)
delegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx acnt), dpstate, certificates) <- judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> do
      let ds = _dstate dpstate
          wdrls_ = unWdrl $ _wdrls (_body tx)
          rewards = _rewards ds

      isSubmapOf wdrls_ rewards -- wdrls_ ⊆ rewards
        ?! WithdrawalsNotInRewardsDELEGS
          ( Map.differenceWith
              (\x y -> if x /= y then Just x else Nothing)
              wdrls_
              (Map.mapKeys (mkRwdAcnt network) rewards)
          )

      let wdrls_' :: RewardAccounts crypto
          wdrls_' =
            Map.foldrWithKey
              ( \(RewardAcnt _ cred) _coin ->
                  Map.insert cred 0
              )
              Map.empty
              wdrls_
          rewards' = eval (rewards ⨃ wdrls_')
      pure $ dpstate {_dstate = ds {_rewards = rewards'}}
    gamma :|> c -> do
      dpstate' <-
        trans @(DELEGS crypto) $ TRC (env, dpstate, gamma)

      let isDelegationRegistered = case c of
            DCertDeleg (Delegate deleg) ->
              let stPools_ = _pParams $ _pstate dpstate'
                  targetPool = _delegatee deleg
               in case eval (targetPool ∈ dom stPools_) of
                    True -> Right ()
                    False -> Left $ DelegateeNotRegisteredDELEG targetPool
            _ -> Right ()
      isDelegationRegistered ?!: id

      let ptr = Ptr slot txIx (fromIntegral $ length gamma)
      trans @(DELPL crypto) $
        TRC (DelplEnv slot ptr pp acnt, dpstate', c)
  where
    -- @wdrls_@ is small and @rewards@ big, better to transform the former
    -- than the latter into the right shape so we can call 'Map.isSubmapOf'.
    isSubmapOf :: Map (RewardAcnt crypto) Coin -> RewardAccounts crypto -> Bool
    isSubmapOf wdrls_ rewards = wdrls_' `Map.isSubmapOf` rewards
      where
        wdrls_' =
          Map.fromList
            [ (cred, coin)
              | (RewardAcnt _ cred, coin) <- Map.toList wdrls_
            ]

instance
  Crypto crypto =>
  Embed (DELPL crypto) (DELEGS crypto)
  where
  wrapFailed = DelplFailure
