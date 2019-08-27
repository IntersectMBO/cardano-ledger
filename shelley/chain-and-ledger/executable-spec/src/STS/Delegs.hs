{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Delegs
  ( DELEGS
  , DelegsEnv (..)
  )
where

import qualified Data.Set as Set

import           Data.Sequence (Seq (..))

import           Delegation.Certificates
import           Keys
import           LedgerState
import           PParams
import           Slot
import           Tx
import           TxData

import           STS.Delpl

import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Ledger.Core (dom, (∈), (⊆), (⨃))

import           Hedgehog (Gen)

data DELEGS hashAlgo dsignAlgo vrfAlgo

data DelegsEnv hashAlgo dsignAlgo vrfAlgo
  = DelegsEnv Slot Ix PParams (Tx hashAlgo dsignAlgo vrfAlgo)
  deriving Show

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => STS (DELEGS hashAlgo dsignAlgo vrfAlgo)
 where
  type State (DELEGS hashAlgo dsignAlgo vrfAlgo) = DPState hashAlgo dsignAlgo vrfAlgo
  type Signal (DELEGS hashAlgo dsignAlgo vrfAlgo) = Seq (DCert hashAlgo dsignAlgo vrfAlgo)
  type Environment (DELEGS hashAlgo dsignAlgo vrfAlgo) = DelegsEnv hashAlgo dsignAlgo vrfAlgo
  data PredicateFailure (DELEGS hashAlgo dsignAlgo vrfAlgo)
    = DelegateeNotRegisteredDELEG
    | WithrawalsNotInRewardsDELEGS
    | DelplFailure (PredicateFailure (DELPL hashAlgo dsignAlgo vrfAlgo))
    deriving (Show, Eq)

  initialRules    = [ pure emptyDelegation ]
  transitionRules = [ delegsTransition     ]

delegsTransition
  :: forall hashAlgo dsignAlgo vrfAlgo
   . (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TransitionRule (DELEGS hashAlgo dsignAlgo vrfAlgo)
delegsTransition = do
  TRC (env@(DelegsEnv _slot txIx pp (Tx txbody _ _)), dpstate, certificates) <- judgmentContext

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
        trans @(DELEGS hashAlgo dsignAlgo vrfAlgo) $ TRC (env, dpstate, certs_)

      let ptr = Ptr _slot txIx (fromIntegral $ length certs_)

          isDelegationRegistered = case cert of
            Delegate deleg ->
              let StakePools stPools_ = _stPools $ _pstate dpstate' in
              _delegatee deleg ∈ dom stPools_
            _ -> True

      isDelegationRegistered ?! DelegateeNotRegisteredDELEG

      trans @(DELPL hashAlgo dsignAlgo vrfAlgo)
        $ TRC (DelplEnv _slot ptr pp, dpstate', cert)

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Embed (DELPL hashAlgo dsignAlgo vrfAlgo) (DELEGS hashAlgo dsignAlgo vrfAlgo)
 where
  wrapFailed = DelplFailure


instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => HasTrace (DELEGS hashAlgo dsignAlgo vrfAlgo) where
  envGen _ = undefined :: Gen (DelegsEnv hashAlgo dsignAlgo vrfAlgo)
  sigGen _ _ = undefined :: Gen (Seq (DCert hashAlgo dsignAlgo vrfAlgo))
