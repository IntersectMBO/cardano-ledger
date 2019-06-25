{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Delegs
  ( DELEGS
  )
where

import qualified Data.Map.Strict as Map

import           Delegation.Certificates
import           Delegation.PoolParams
import           Keys
import           LedgerState
import           PParams hiding (d)
import           Slot
import           UTxO

import           Control.State.Transition

import           STS.Delpl

data DELEGS

instance STS DELEGS where
    type State DELEGS       = DPState
    type Signal DELEGS      = [DCert]
    type Environment DELEGS = (Slot, Ix, PParams, Tx)
    data PredicateFailure DELEGS = DelegateeNotRegisteredDELEG
                                 | WithrawalsNotInRewardsDELEGS
                                 | DelplFailure (PredicateFailure DELPL)
                    deriving (Show, Eq)

    initialRules    = [ pure emptyDelegation ]
    transitionRules = [ delegsTransition     ]

delegsTransition :: TransitionRule DELEGS
delegsTransition = do
  TRC (env@(_slot, txIx, pp, Tx txbody _), dpstate, certificates) <- judgmentContext
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
                                    , _dms = Dms $ Map.union dms'' dms'}}
    cert:_certs -> do
      let ptr = Ptr _slot txIx (fromIntegral $ length _certs)
      let isDelegationRegistered = case cert of
            Delegate deleg ->
              let StakePools sp = _stPools $ _pstate dpstate in
              Map.member (hashKey $ _delegatee deleg) sp
            _ -> True
      isDelegationRegistered ?! DelegateeNotRegisteredDELEG
      dpstate' <- trans @DELEGS $ TRC (env, dpstate, _certs)
      dpstate'' <- trans @DELPL $ TRC ((_slot, ptr, pp), dpstate', cert)
      pure dpstate''


instance Embed DELPL DELEGS where
  wrapFailed = DelplFailure
