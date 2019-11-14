{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Delegs
  ( DELEGS
  , DelegsEnv (..)
  , PredicateFailure(..)
  )
where

import           Data.Data (Data)
import           Data.Sequence (Seq (..))
import qualified Data.Set as Set

import           Coin (Coin)
import           Delegation.Certificates
import           LedgerState
import           PParams
import           Slot
import           Tx
import           TxData

import           STS.Delpl

import           Cardano.Ledger.Shelley.Crypto
import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Ledger.Core (dom, (∈), (⊆), (⨃))

import           Hedgehog (Gen)

data DELEGS crypto
  deriving Data

data DelegsEnv crypto
  = DelegsEnv
    { delegsSlot :: Slot
    , delegsIx   :: Ix
    , delegspp   :: PParams
    , delegsTx   :: (Tx crypto)
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
  data PredicateFailure (DELEGS crypto)
    = DelegateeNotRegisteredDELEG
    | WithrawalsNotInRewardsDELEGS
    | DelplFailure (PredicateFailure (DELPL crypto))
    deriving (Show, Eq, Data)

  initialRules    = [ pure emptyDelegation ]
  transitionRules = [ delegsTransition     ]

delegsTransition
  :: forall crypto
   . Crypto crypto
  => TransitionRule (DELEGS crypto)
delegsTransition = do
  TRC (env@(DelegsEnv _slot txIx pp (Tx txbody _ _) _reserves), dpstate, certificates) <- judgmentContext

  case certificates of
    Empty -> do
      let ds       = _dstate dpstate
          rewards_ = _rewards ds
          wdrls_   = _wdrls txbody

      wdrls_ ⊆ rewards_ ?! WithrawalsNotInRewardsDELEGS

      let rewards' = rewards_ ⨃ [(w, 0) | w <- Set.toList (dom wdrls_)]

      pure $ dpstate { _dstate = ds { _rewards = rewards' } }

    certs_ :|> cert -> do
      dpstate' <-
        trans @(DELEGS crypto) $ TRC (env, dpstate, certs_)

      let ptr = Ptr _slot txIx (fromIntegral $ length certs_)

          isDelegationRegistered = case cert of
            Delegate deleg ->
              let StakePools stPools_ = _stPools $ _pstate dpstate' in
              _delegatee deleg ∈ dom stPools_
            _ -> True

      isDelegationRegistered ?! DelegateeNotRegisteredDELEG

      trans @(DELPL crypto)
        $ TRC (DelplEnv _slot ptr pp _reserves, dpstate', cert)

instance
  Crypto crypto
  => Embed (DELPL crypto) (DELEGS crypto)
 where
  wrapFailed = DelplFailure


instance Crypto crypto
  => HasTrace (DELEGS crypto) where
  envGen _ = undefined :: Gen (DelegsEnv crypto)
  sigGen _ _ = undefined :: Gen (Seq (DCert crypto))
