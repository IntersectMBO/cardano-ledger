{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Byron.Spec.Ledger.Update.Test
  ( coverUpiregFailures
  , coverUpivoteFailures
  )
where

import qualified Control.State.Transition.Generator as Generator
import           Data.Data (Data)
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (CoverPercentage)

import           Byron.Spec.Ledger.Update

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
  Generator.coverFailures @_ @UPPVV
    coverPercentage
    [ CannotFollowPv
    , CannotUpdatePv []
    , AlreadyProposedPv
    ]
    someData

  Generator.coverFailures @_ @UPSVV
    coverPercentage
    [ AlreadyProposedSv
    , CannotFollowSv
    , InvalidApplicationName
    , InvalidSystemTags
    ]
    someData

  Generator.coverFailures @_ @UPREG
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
coverUpivoteFailures coverPercentage =
  Generator.coverFailures @_ @ADDVOTE
    coverPercentage
    [ AVSigDoesNotVerify
    , NoUpdateProposal (UpId 0) -- We need to pass a dummy update id here.
    ]
