{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Ledger.Delegation.Test (coverDelegFailures) where

import qualified Control.State.Transition.Generator as Generator
import           Data.Data (Data)
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (CoverPercentage)

import           Ledger.Delegation (EpochDiff(..)
                                   , PredicateFailure (DoesNotVerify, EpochInThePast, EpochPastNextEpoch, HasAlreadyDelegated, IsAlreadyScheduled, IsNotGenesisKey))

coverDelegFailures
  :: forall m a
   .  ( MonadTest m
      , HasCallStack
      , Data a
      )
  => CoverPercentage
  -> a
  -> m ()
coverDelegFailures coverPercentage =
  Generator.coverFailures
    coverPercentage
    [ EpochInThePast (EpochDiff 0 0) -- The value here is ignored, only the constructor is compared
    , EpochPastNextEpoch (EpochDiff 0 0 ) -- The value here is ignored, only the constructor is compared
    , IsAlreadyScheduled
    , IsNotGenesisKey
    , HasAlreadyDelegated
    , DoesNotVerify
    ]
