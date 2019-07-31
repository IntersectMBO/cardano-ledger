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

import           Delegation.Certificates
import           Keys
import           LedgerState
import           PParams
import           Slot
import           Tx
import           TxData

import           Control.State.Transition

import           STS.Delpl

import           Ledger.Core ((∪))

data DELEGS hashAlgo dsignAlgo

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => STS (DELEGS hashAlgo dsignAlgo)
 where
  type State (DELEGS hashAlgo dsignAlgo) = DPState hashAlgo dsignAlgo
  type Signal (DELEGS hashAlgo dsignAlgo) = [DCert hashAlgo dsignAlgo]
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
    [] -> do
      let wdrls' = _wdrls txbody
      let rews = _rewards $ _dstate dpstate
      wdrls' `Map.isSubmapOf` rews ?! WithrawalsNotInRewardsDELEGS
      let rewards' = Map.union (Map.fromList [(w, 0) | w <- Map.keys wdrls']) rews
      let ds = _dstate dpstate
      let fdms' = _fdms ds

      let (curr, fdms'') =
            if Map.null fdms' then (Map.empty, fdms')
                                  -- maximum exists as fdms isn't empty here
             else let sMax = maximum [s | (s, _) <- Map.keys fdms'] in
              Map.partitionWithKey (\(s, _) _ -> s >= _slot && s == sMax) fdms'

      let Dms dms' = _dms ds
      let dms'' = Map.fromList [(gk, vk) | ((_, gk), vk) <- Map.toList curr]
      pure $ dpstate { _dstate = ds { _rewards = rewards'
                                    , _fdms = fdms''
                                    , _dms = Dms $ dms'' ∪ dms'}}
    cert:_certs -> do
      dpstate' <-
        trans @(DELEGS hashAlgo dsignAlgo) $ TRC (env, dpstate, _certs)

      let ptr = Ptr _slot txIx (fromIntegral $ length _certs)
      let isDelegationRegistered = case cert of
            Delegate deleg ->
              let StakePools sp = _stPools $ _pstate dpstate' in
              Map.member (_delegatee deleg) sp
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
