{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Spec.Chain.STS.Rule.BBody where

import Control.Lens ((^.))
import Data.Set (Set)

import Control.State.Transition
import Ledger.Core
import Ledger.Delegation
import Ledger.Update

import Cardano.Spec.Chain.STS.Block

data BBODY

instance STS BBODY where
  type Environment BBODY
    = ( Epoch
      , Slot
      , PParams
      , Set VKeyGenesis
      )

  type State BBODY = DIState

  type Signal BBODY = Block

  data PredicateFailure BBODY
    = InvalidBlockSize
    | DelegationFailure (PredicateFailure DELEG)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((e, s, pps, gks), ds, b) <- judgmentContext
        bSize b <= pps ^. maxBkSz ?! InvalidBlockSize
        let diEnv
              = DSEnv
              { _dSEnvAllowedDelegators = gks
              , _dSEnvEpoch = e
              , _dSEnvSlot = s
              , _dSEnvLiveness = pps ^. dLiveness
              }
        ds' <- trans @DELEG
                     $ TRC (diEnv, ds, b ^. bBody . bDCerts)
        return $! ds'
    ]

instance Embed DELEG BBODY where
  wrapFailed = DelegationFailure
