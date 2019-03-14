{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Delegs
  ( DELEGS
  ) where

import           Control.Monad           (foldM)

import           LedgerState
import           Delegation.Certificates
import           PParams
import           Slot
import           UTxO

import           Control.State.Transition

import           STS.Delpl

data DELEGS

instance STS DELEGS where
    type State DELEGS       = DWState
    type Signal DELEGS      = [DCert]
    type Environment DELEGS = (Slot, Ix, PParams)
    data PredicateFailure DELEGS = DelplFailure (PredicateFailure DELPL)
                    deriving (Show, Eq)

    initialRules    = [ pure emptyDelegation ]
    transitionRules = [ delegsTransition     ]

delegsTransition :: TransitionRule DELEGS
delegsTransition = do
  TRC((slot, ix, pp), d, certificates) <- judgmentContext
  foldM (\d' (clx, c) ->
             trans @DELPL $
                   TRC((Ptr slot ix clx, pp), d', c)) d $ zip [0..] certificates

instance Embed DELPL DELEGS where
  wrapFailed = DelplFailure
