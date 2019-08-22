{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Ledger.Update.Test
  ( coverUpiregFailures
  , coverUpivoteFailures
  )
where

import qualified Control.State.Transition.Generator as Generator
import           Data.Data (Data)
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (CoverPercentage)

import           Ledger.Update (PredicateFailure (AVSigDoesNotVerify, AlreadyProposedPv, AlreadyProposedSv, CannotFollowPv, CannotFollowSv, CannotUpdatePv, DoesNotVerify, InvalidApplicationName, InvalidSystemTags, NoUpdateProposal, NotGenesisDelegate))
import           Ledger.Update (UpId (UpId))

-- | Check that all the relevant predicate failures are covered.
coverUpiregFailures
  :: forall m a
   .  ( MonadTest m
      , HasCallStack
      , Data a
      )
  => CoverPercentage
  -- ^ Minimum percentage that each failure must occur.
  -> a
  -- ^ Structure containing the failures
  -> m ()
coverUpiregFailures coverPercentage someData = do
  Generator.coverFailures
    coverPercentage
    [ CannotFollowPv
    , CannotUpdatePv []
    , AlreadyProposedPv
    ]
    someData

  Generator.coverFailures
    coverPercentage
    [ AlreadyProposedSv
    , CannotFollowSv
    , InvalidApplicationName
    , InvalidSystemTags
    ]
    someData

  Generator.coverFailures
    coverPercentage
    [ NotGenesisDelegate
    , DoesNotVerify
    ]
    someData


-- | See 'coverUpiregFailures'.
coverUpivoteFailures
  :: forall m a
   .  ( MonadTest m
      , HasCallStack
      , Data a
      )
  => CoverPercentage
  -> a
  -> m ()
coverUpivoteFailures coverPercentage someData =
  Generator.coverFailures
    coverPercentage
    [ AVSigDoesNotVerify
    , NoUpdateProposal (UpId 0) -- We need to pass a dummy update id here.
    ]
    someData
