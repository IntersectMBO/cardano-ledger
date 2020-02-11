{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Delegs
  ( DELEGS
  , DelegsEnv (..)
  , PredicateFailure(..)
  )
where

import           BaseTypes
import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLen, decodeWord,
                     encodeListLen, matchSize)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin (Coin)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition
import           Control.State.Transition.Generator
import           Data.Functor.Identity (runIdentity)
import           Data.Sequence (Seq (..))
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           Delegation.Certificates
import           GHC.Generics (Generic)
import           Hedgehog (Gen)
import           Ledger.Core (dom, (∈), (⊆), (⨃))
import           LedgerState (DPState (..), emptyDelegation, _dstate, _rewards, _stPools)
import           PParams
import           Slot
import           STS.Delpl
import           Tx
import           TxData

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
          wdrls_   = _wdrls (_body tx)
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


instance Crypto crypto
  => HasTrace (DELEGS crypto) where
  envGen _ = undefined :: Gen (DelegsEnv crypto)
  sigGen _ _ = undefined :: Gen (Seq (DCert crypto))

  type BaseEnv (DELEGS crypto) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
