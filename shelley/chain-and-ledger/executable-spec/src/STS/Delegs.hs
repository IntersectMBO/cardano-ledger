{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Delegs
  ( DELEGS
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Data.Sequence (Seq (..))

import           Delegation.Certificates
import           Keys
import           LedgerState
import           PParams
import           Slot
import           Tx
import           TxData

import           Control.State.Transition

import           STS.Delpl

import           Ledger.Core (dom, (∈), (⊆), (⨃))

data DELEGS hashAlgo dsignAlgo

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => STS (DELEGS hashAlgo dsignAlgo)
 where
  type State (DELEGS hashAlgo dsignAlgo) = DPState hashAlgo dsignAlgo
  type Signal (DELEGS hashAlgo dsignAlgo) = Seq (DCert hashAlgo dsignAlgo)
  type Environment (DELEGS hashAlgo dsignAlgo)
    = (Slot, Ix, PParams, Tx hashAlgo dsignAlgo)
  data PredicateFailure (DELEGS hashAlgo dsignAlgo)
    = DelegateeNotRegisteredDELEG
    | WithrawalsNotInRewardsDELEGS
    | DelplFailure (PredicateFailure (DELPL hashAlgo dsignAlgo))
    deriving (Show, Eq)

  initialRules    = [ pure emptyDelegation ]
  transitionRules = [ delegsTransition     ]

delegsTransition
  :: forall hashAlgo dsignAlgo
   . (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TransitionRule (DELEGS hashAlgo dsignAlgo)
delegsTransition = do
  TRC (env@(_slot, txIx, pp, Tx txbody _ _), dpstate, certificates) <- judgmentContext

  case certificates of
    Empty -> do
      let ds       = _dstate dpstate
          rewards_ = _rewards ds
          fdms_    = _fdms ds
          Dms dms_ = _dms ds
          wdrls_   = _wdrls txbody

      wdrls_ ⊆ rewards_ ?! WithrawalsNotInRewardsDELEGS

      let rewards' = rewards_ ⨃ [(w, 0) | w <- Set.toList (dom wdrls_)]

      let (curr, fdmsMinCurr) =
            if Map.null fdms_
              then (Map.empty, fdms_)
              else
                -- maximum exists as fdms isn't empty here
                let sMax = maximum [s | (s, _) <- Set.toList (dom fdms_)] in
                Map.partitionWithKey (\(s, _) _ -> s >= _slot && s == sMax) fdms_

      let dms' = [(gk, vk) | ((_, gk), vk) <- Map.toList curr]

      pure $ dpstate { _dstate = ds { _rewards = rewards'
                                    , _fdms = fdmsMinCurr
                                    , _dms = Dms $ dms_ ⨃ dms'}}

    certs_ :|> cert -> do
      dpstate' <-
        trans @(DELEGS hashAlgo dsignAlgo) $ TRC (env, dpstate, certs_)

      let ptr = Ptr _slot txIx (fromIntegral $ length certs_)

          isDelegationRegistered = case cert of
            Delegate deleg ->
              let StakePools stPools_ = _stPools $ _pstate dpstate' in
              _delegatee deleg ∈ dom stPools_
            _ -> True

      isDelegationRegistered ?! DelegateeNotRegisteredDELEG

      dpstate'' <-
        trans @(DELPL hashAlgo dsignAlgo)
          $ TRC ((_slot, ptr, pp), dpstate', cert)

      pure dpstate''

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Embed (DELPL hashAlgo dsignAlgo) (DELEGS hashAlgo dsignAlgo)
 where
  wrapFailed = DelplFailure
